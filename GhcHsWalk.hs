{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving #-}
module GhcHsWalk (IdentMap, extractIdsPlugin) where

import Prelude hiding (span, id)
import Control.Monad (forM_)
import Control.Monad.Writer (MonadWriter, WriterT, execWriterT, tell)
import Control.Monad.Trans.Class (MonadTrans, lift)
import System.IO (withFile, IOMode(AppendMode), hPutStr)

import GHC
import TcRnTypes
import Outputable
import HscPlugin
import DynFlags
import Var
import MonadUtils (MonadIO(..))
import Bag
import TypeRep
import TyCon

type IdentMap = [(SrcSpan, Id)]

-- Define type synonym to avoid orphan instances
newtype ExtractIdsT m a = ExtractIdsT { runExtractIdsT :: WriterT IdentMap m a }
  deriving (Functor, Monad, MonadWriter IdentMap, MonadTrans)

execExtractIdsT :: Monad m => ExtractIdsT m () -> m IdentMap
execExtractIdsT = execWriterT . runExtractIdsT

class ExtractIds a where
  extractIds :: (Functor m, MonadIO m, HasDynFlags m) => a -> ExtractIdsT m ()

instance (HasDynFlags m, Monad m) => HasDynFlags (ExtractIdsT m) where
  getDynFlags = lift getDynFlags

-- This is not the standard MonadIO, but the MonadIO from GHC!
instance MonadIO m => MonadIO (ExtractIdsT m) where
  liftIO = lift . liftIO

debugPP :: (MonadIO m, HasDynFlags m, Outputable a) => String -> a -> m ()
debugPP header val = do
  dynFlags <- getDynFlags
  liftIO $ appendFile "/tmp/ghc.log" (header ++ showSDoc dynFlags (ppr val) ++ "\n")

extractIdsPlugin :: HscPlugin
extractIdsPlugin = HscPlugin $ \env -> do
  dynFlags <- getDynFlags
  identMap <- execExtractIdsT $ extractIds (tcg_binds env)

  let _ = identMap :: IdentMap

  liftIO $ withFile "/tmp/ghc.log" AppendMode $ \h ->
    forM_ identMap $ \(span, id) -> do
      hPutStr h $ showSDoc dynFlags (ppr span) ++ ": "
      hPutStr h $ showSDoc dynFlags (ppr (varName id)) ++ " :: "
      hPutStr h $ showSDoc dynFlags (ppr (varType id))
      hPutStr h $ " (" ++ showSDoc dynFlags (ppr (nameSrcSpan (varName id))) ++ ")\n"

  return env

instance ExtractIds (LHsBinds Id) where
  extractIds = mapM_ extractIds . bagToList

instance ExtractIds (LHsBind Id) where
  extractIds (L _span bind@(FunBind {})) = do
    tell [(getLoc (fun_id bind), unLoc (fun_id bind))]
    extractIds (fun_matches bind)
  extractIds (L _span bind@(PatBind {})) =
    fail "extractIds: unsupported PatBind"
  extractIds (L span bind@(VarBind {})) =
    fail "extractIds: unsupported VarBind"
  extractIds (L _span bind@(AbsBinds {})) =
    extractIds (abs_binds bind)

instance ExtractIds (MatchGroup Id) where
  extractIds (MatchGroup matches _postTcType) = do
    mapM_ extractIds matches
    -- We ignore the postTcType, as it doesn't have location information

instance ExtractIds (LMatch Id) where
  extractIds (L _span (Match pats _type rhss)) =
    extractIds rhss

instance ExtractIds (GRHSs Id) where
  extractIds (GRHSs rhss binds) =
    mapM_ extractIds rhss

instance ExtractIds (LGRHS Id) where
  extractIds (L _span (GRHS _guards rhs)) = extractIds rhs

instance ExtractIds (HsLocalBinds Id) where
  extractIds EmptyLocalBinds =
    return ()
  extractIds (HsValBinds (ValBindsIn _ _)) =
    fail "extractIds: Unexpected ValBindsIn (after renamer these should not exist)"
  extractIds (HsValBinds (ValBindsOut binds _sigs)) =
    mapM_ (extractIds . snd) binds -- "fst" is 'rec flag'
  extractIds (HsIPBinds _) =
    fail "extractIds: unsupported HsIPBinds"

instance ExtractIds (LHsExpr Id) where
  extractIds (L _ (HsPar expr)) =
    extractIds expr
  extractIds (L _ (ExprWithTySigOut expr _type)) =
    extractIds expr
  extractIds (L _ (HsOverLit _ )) =
    return ()
  extractIds (L _ (OpApp left op _fix right)) = do
    extractIds left
    extractIds op
    extractIds right
  extractIds (L span (HsVar id)) =
    tell [(span, id)]
  extractIds (L span (HsWrap _wrapper expr)) =
    extractIds (L span expr)
  extractIds (L _ (HsLet binds expr)) = do
    extractIds binds
    extractIds expr

  -- We should ignore unrecognized expressions rather than throw an error
  -- However, for writing this code in the first place it's useful to know
  -- which constructor we fail to support
  extractIds (L _ (HsIPVar _ )) = fail "extractIds: unsupported HsIPVar"
  extractIds (L _ (HsLit _)) = fail "extractIds: unsupported HsLit"
  extractIds (L _ (HsLam _)) = fail "extractIds: unsupported HsLam"
  extractIds (L _ (HsLamCase _ _ )) = fail "extractIds: unsupported HsLamCase"
  extractIds (L _ (HsApp _ _)) = fail "extractIds: unsupported HsApp"
  extractIds (L _ (NegApp _ _)) = fail "extractIds: unsupported NegApp"
  extractIds (L _ (SectionL _ _)) = fail "extractIds: unsupported SectionL"
  extractIds (L _ (SectionR _ _)) = fail "extractIds: unsupported SectionR"
  extractIds (L _ (ExplicitTuple _ _)) = fail "extractIds: unsupported ExplicitTuple"
  extractIds (L _ (HsCase _ _)) = fail "extractIds: unsupported HsCase"
  extractIds (L _ (HsIf _ _ _ _)) = fail "extractIds: unsupported HsIf"
  extractIds (L _ (HsMultiIf _ _)) = fail "extractIds: unsupported HsMultiIf"
  extractIds (L _ (HsDo _ _ _)) = fail "extractIds: unsupported HsDo"
  extractIds (L _ (ExplicitList _ _)) = fail "extractIds: unsupported ExplicitList"
  extractIds (L _ (ExplicitPArr _ _)) = fail "extractIds: unsupported ExplicitPArr"
  extractIds (L _ (RecordCon _ _ _)) = fail "extractIds: unsupported RecordCon"
  extractIds (L _ (RecordUpd _ _ _ _ _)) = fail "extractIds: unsupported RecordUpd"
  extractIds (L _ (ExprWithTySig _ _)) = fail "extractIds: unsupported ExprWithTySig"
  extractIds (L _ (ArithSeq _ _ )) = fail "extractIds: unsupported ArithSeq"
  extractIds (L _ (PArrSeq _ _)) = fail "extractIds: unsupported PArrSeq"
  extractIds (L _ (HsSCC _ _)) = fail "extractIds: unsupported HsSCC"
  extractIds (L _ (HsCoreAnn _ _)) = fail "extractIds: unsupported HsCoreAnn"
  extractIds (L _ (HsBracket _)) = fail "extractIds: unsupported HsBracket"
  extractIds (L _ (HsBracketOut _ _)) = fail "extractIds: unsupported HsBracketOut"
  extractIds (L _ (HsSpliceE _)) = fail "extractIds: unsupported HsSpliceE"
  extractIds (L _ (HsQuasiQuoteE _ )) = fail "extractIds: unsupported HsQuasiQuoteE"
  extractIds (L _ (HsProc _ _)) = fail "extractIds: unsupported HsProc"
  extractIds (L _ (HsArrApp _ _ _ _ _)) = fail "extractIds: unsupported HsArrApp"
  extractIds (L _ (HsArrForm _ _ _)) = fail "extractIds: unsupported HsArrForm"
  extractIds (L _ (HsTick _ _)) = fail "extractIds: unsupported HsTick"
  extractIds (L _ (HsBinTick _ _ _)) = fail "extractIds: unsupported HsBinTick"
  extractIds (L _ (HsTickPragma _ _)) = fail "extractIds: unsupported HsTickPragma"
  extractIds (L _ (EWildPat)) = fail "extractIds: unsupported EWildPat"
  extractIds (L _ (EAsPat _ _)) = fail "extractIds: unsupported EAsPat"
  extractIds (L _ (EViewPat _ _)) = fail "extractIds: unsupported EViewPat"
  extractIds (L _ (ELazyPat _)) = fail "extractIds: unsupported ELazyPat"
  extractIds (L _ (HsType _ )) = fail "extractIds: unsupported HsType"
