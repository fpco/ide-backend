-- | This is a modified copy of compiler/ghci/Debugger.hs
{-# LANGUAGE MagicHash, CPP #-}
module Break (
    evaluateIds
  , resolveNames
  , parseNames
  ) where

import Prelude hiding (id)

import Linker
import RtClosureInspect

import GhcMonad
import HscTypes
import Id
import Name
import Var hiding ( varName )
import VarSet
import UniqSupply
import TcType (
    emptyTvSubst
  , TvSubst
  , substTy
  , isUnliftedTypeKind
  , unionTvSubst
  )
import GHC
import MonadUtils

import Control.Monad
import Data.List
import Data.Maybe
import Data.IORef

import GhcShim

-------------------------------------
-- | The :print & friends commands
-------------------------------------

-- | This is basically 'pprintClosureCommand' from Debugger.hs except that
-- we don't take a string as argument but a set of names (and defined a
-- separate 'parseNames' and 'resolveNames' if we do want to start with a
-- String), and that we return a set of terms rather than pretty-printing
-- them directly to stdout.
evaluateIds :: GhcMonad m => Bool -> Bool -> [Id] -> m [(Id, Term)]
evaluateIds bindThings force ids = do
  -- Obtain the terms and the recovered type information
  (subst, terms) <- mapAccumLM go emptyTvSubst ids

  -- Apply the substitutions obtained after recovering the types
  modifySession $ \hsc_env ->
    hsc_env{hsc_IC = substInteractiveContext (hsc_IC hsc_env) subst}

  return (zip ids terms)
 where
   -- Do the obtainTerm--bindSuspensions-computeSubstitution dance
   go :: GhcMonad m => TvSubst -> Id -> m (TvSubst, Term)
   go subst id = do
       let id' = id `setIdType` substTy subst (idType id)
       term_    <- GHC.obtainTermFromId maxBound force id'
       term     <- tidyTermTyVars term_
       term'    <- if bindThings &&
                      False == isUnliftedTypeKind (termType term)
                     then bindSuspensions term
                     else return term
     -- Before leaving, we compare the type obtained to see if it's more specific
     --  Then, we extract a substitution,
     --  mapping the old tyvars to the reconstructed types.
       let reconstructed_type = termType term
       hsc_env <- getSession
       case (improveRTTIType hsc_env (idType id) (reconstructed_type)) of
         Nothing     -> return (subst, term')
         Just subst' -> return (subst `unionTvSubst` subst', term')

   tidyTermTyVars :: GhcMonad m => Term -> m Term
   tidyTermTyVars t =
     withSession $ \hsc_env -> do
     let env_tvs      = tyThingsTyVars $ ic_tythings $ hsc_IC hsc_env
         my_tvs       = termTyVars t
         tvs          = env_tvs `minusVarSet` my_tvs
         tyvarOccName = nameOccName . tyVarName
         tidyEnv      = (initTidyOccEnv (map tyvarOccName (varSetElems tvs))
                        , env_tvs `intersectVarSet` my_tvs)
     return$ mapTermType (snd . tidyOpenType tidyEnv) t

-- | Give names, and bind in the interactive environment, to all the suspensions
--   included (inductively) in a term
bindSuspensions :: GhcMonad m => Term -> m Term
#if __GLASGOW_HASKELL__ >= 710
-- TODO: Not implemented for 7.10 and up
bindSuspensions _ = undefined
#else
bindSuspensions t = do
      hsc_env <- getSession
      inScope <- GHC.getBindings
      let ictxt        = hsc_IC hsc_env
          prefix       = "_t"
          alreadyUsedNames = map (occNameString . nameOccName . getName) inScope
          availNames   = map ((prefix++) . show) [(1::Int)..] \\ alreadyUsedNames
      availNames_var  <- liftIO $ newIORef availNames
      (t', stuff)     <- liftIO $ foldTerm (nameSuspensionsAndGetInfos availNames_var) t
      let (names, tys, hvals) = unzip3 stuff
      let ids = [ mkVanillaGlobal name ty
                | (name,ty) <- zip names tys]
          new_ic = extendInteractiveContext ictxt (map AnId ids)
      liftIO $ extendLinkEnv (zip names hvals)
      modifySession $ \_ -> hsc_env {hsc_IC = new_ic }
      return t'
     where

--    Processing suspensions. Give names and recopilate info
        nameSuspensionsAndGetInfos :: IORef [String] ->
                                       TermFold (IO (Term, [(Name,Type,HValue)]))
        nameSuspensionsAndGetInfos freeNames = TermFold
                      {
                        fSuspension = doSuspension freeNames
                      , fTerm = \ty dc v acts -> do
                                    tt' <- sequence acts
                                    let (terms,names) = unzip tt'
                                    return (Term ty dc v terms, concat names)
                      , fPrim    = \ty n ->return (Prim ty n,[])
                      , fNewtypeWrap  =
                                \ty dc act -> do
                                    (term, names) <- act
                                    return (NewtypeWrap ty dc term, names)
                      , fRefWrap = \ty act -> do
                                    (term, names) <- act
                                    return (RefWrap ty term, names)
                      }
        doSuspension freeNames ct ty hval _name = do
          name <- atomicModifyIORef freeNames (\x->(tail x, head x))
          n <- newGrimName name
          return (Suspension ct ty hval (Just n), [(n,ty,hval)])
#endif

--    Create new uniques and give them sequentially numbered names
newGrimName :: MonadIO m => String -> m Name
newGrimName userName  = do
    us <- liftIO $ mkSplitUniqSupply 'b'
    let unique  = uniqFromSupply us
        occname = mkOccName varName userName
        name    = mkInternalName unique occname noSrcSpan
    return name

{------------------------------------------------------------------------------
  Resolving names
------------------------------------------------------------------------------}

resolveNames :: GhcMonad m => [Name] -> m [Id]
resolveNames names = do
    tyThings <- catMaybes `liftM` mapM lookupName names
    return $ idsFromTyThings tyThings
  where
    idsFromTyThings :: [TyThing] -> [Id]
    idsFromTyThings = catMaybes . map aux
      where
        aux (AnId id) = Just id
        aux _         = Nothing

parseNames :: GhcMonad m => String -> m [Name]
parseNames str = concatMapM parseName (words str)

