{-# LANGUAGE CPP, DeriveDataTypeable, FlexibleInstances,
             GeneralizedNewtypeDeriving, MultiParamTypeClasses, TemplateHaskell,
             TypeSynonymInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-- | A few specialized walks over the Haskell AST. Heavily depends
-- on the GHC patches.
--
-- Only @IdeSession.GHC.Run@ and @IdeSession.GHC.HsWalk@ should import
-- any modules from the ghc package and the modules should not be reexported
-- anywhere else, with the exception of @IdeSession.GHC.Server@.
module HsWalk
  ( extractIdsPlugin
  , extractSourceSpan
  , idInfoForName
  , constructExplicitSharingCache
  , PluginResult(..)
  , IsBinder(..)
  ) where

#define DEBUG 0

import Control.Monad (liftM)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT, local)
import Control.Monad.State.Class (MonadState(..))
import Control.Applicative (Applicative, (<$>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSSC
import Data.Maybe (fromMaybe, fromJust)
import Prelude hiding (id, mod, span, writeFile, appendFile)
import System.IO.Unsafe (unsafePerformIO)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Debug.Trace as Debug
import Control.Exception (evaluate)

#if DEBUG
import System.IO.UTF8 (writeFile, appendFile)
#endif

import IdeSession.Types.Private
import IdeSession.Strict.Container
import qualified IdeSession.Strict.IntMap as IntMap
import qualified IdeSession.Strict.Map    as Map
import qualified IdeSession.Strict.Maybe  as Maybe
import IdeSession.Strict.IORef
import IdeSession.Strict.StateT
import IdeSession.Strict.Pair

import GHC hiding (PackageId, idType, moduleName, ModuleName)
import qualified GHC
import HscPlugin
import qualified Module
import MonadUtils (MonadIO (..))
import qualified Name
import OccName
import Outputable hiding (trace)
import qualified RdrName
import TcRnTypes
import Var hiding (idInfo)
import VarEnv (TidyEnv, emptyTidyEnv)
import Unique (Unique, Uniquable, getUnique, getKey)
import HscTypes (TypeEnv, HscEnv(hsc_dflags), mkPrintUnqualified)
import NameEnv (nameEnvUniqueElts)
import DataCon (dataConRepType)

import Conv
import Haddock
import FilePathCaching
import IdPropCaching
import TraceMonad
import GhcShim

{------------------------------------------------------------------------------
  Caching

  We use explicit sharing so that we can maintain sharing through the JSON
  serialization.
------------------------------------------------------------------------------}

-- | Construct the explicit sharing cache
--
-- TODO: We should remove entries from the cache that are no longer necessary
-- (from modules in the home package that got unloaded, or from modules
-- in other packages that are no longer imported); moreover, we should avoid
-- sending the entire cache over on every call to compile.
constructExplicitSharingCache :: IO ExplicitSharingCache
constructExplicitSharingCache = do
    -- TODO: keep two refs and wipe on that for local ids, to avoid blowup
    -- for long-running sessions with many added and removed definitions.
    idPropCache       <- getIdPropCache
    (filePathHash, _) <- toLazyPair `liftM` getFilePathCache

    let filePathCache = IntMap.fromList . map convert $ HashMap.toList filePathHash
    return ExplicitSharingCache {..}
  where
    convert :: (FilePath, Int) -> (Int, ByteString)
    convert (path, i) = (i, BSSC.pack path)

{------------------------------------------------------------------------------
  Environment mapping source locations to info
------------------------------------------------------------------------------}

fromGhcNameSpace :: Name.NameSpace -> IdNameSpace
fromGhcNameSpace ns
  | ns == Name.varName  = VarName
  | ns == Name.dataName = DataName
  | ns == Name.tvName   = TvName
  | ns == Name.tcName   = TcClsName
  | otherwise = error "fromGhcNameSpace"

{------------------------------------------------------------------------------
  Extract an IdMap from information returned by the ghc type checker
------------------------------------------------------------------------------}

-- | We collect quasi quotes as the type checker expands them
qqRef :: StrictIORef IdList
{-# NOINLINE qqRef #-}
qqRef = unsafePerformIO $ newIORef []

data PluginResult = PluginResult {
    -- TODO: Why aren't we using strict types for the first two fields?
    pluginIdList   :: !IdList
  , pluginExpTypes :: ![(SourceSpan, Text)]
  , pluginPkgDeps  :: !(Strict [] PackageId)
  , pluginUseSites :: !UseSites
  }

extractIdsPlugin :: StrictIORef (Strict (Map ModuleName) PluginResult) -> HscPlugin
extractIdsPlugin symbolRef = HscPlugin {..}
  where
    runHscQQ :: forall m. MonadIO m => Env TcGblEnv TcLclEnv
                                    -> HsQuasiQuote Name
                                    -> m (HsQuasiQuote Name)
    runHscQQ env qq@(HsQuasiQuote quoter _span _str) = liftIO $ do
#if DEBUG
      appendFile "/tmp/ghc.qq" $ showSDoc (ppr qq)
#endif

      let dflags  =               hsc_dflags  (env_top env)
          rdrEnv  =               tcg_rdr_env (env_gbl env)
          current =               tcg_mod     (env_gbl env)
          pkgDeps = imp_dep_pkgs (tcg_imports (env_gbl env))

      idInfo           <- readIORef qqRef
      ProperSpan span' <- extractSourceSpan $ tcl_loc (env_lcl env)
      linkEnv          <- liftIO $ linkEnvForDeps dflags pkgDeps

      (idProp, Just idScope) <- idInfoForName dflags
                                              quoter
                                              UseSite
                                              (lookupRdrEnv rdrEnv quoter)
                                              (Just current)
                                              (homeModuleFor dflags linkEnv)

      let quoterInfo = IdInfo{..}
      let idInfo' = (span', SpanQQ quoterInfo) : idInfo
      writeIORef qqRef idInfo'
      return qq

    runHscPlugin :: forall m. MonadIO m => DynFlags -> TcGblEnv -> m TcGblEnv
    runHscPlugin dynFlags env = do
      let processedModule = tcg_mod env
          processedName   = Text.pack $ moduleNameString $ GHC.moduleName processedModule

      qqs <- liftIO $ do
        qqs <- readIORef qqRef
        writeIORef qqRef [] -- Reset for the next module
        return qqs

      pluginResult <- execExtractIdsT dynFlags env qqs processedModule $ do
#if DEBUG
        pretty_mod     <- pretty False processedModule
        pretty_rdr_env <- pretty False (tcg_rdr_env env)
        liftIO $ writeFile "/tmp/ghc.readerenv" pretty_rdr_env
#endif

        -- Information provided by the renamer
        -- See http://www.haskell.org/pipermail/ghc-devs/2013-February/000540.html
        -- It is important we do this *first*, because this creates the initial
        -- cache with the IdInfo objects, which we can then update by processing
        -- the typed AST and the global type environment.
#if DEBUG
        liftIO $ appendFile "/tmp/ghc.log" $ "<<PROCESSING RENAMED AST " ++ pretty_mod ++ ">>\n"
#endif
        extractIds (tcg_rn_decls env)

        -- Information provided by the type checker
#if DEBUG
        liftIO $ appendFile "/tmp/ghc.log" $ "<<PROCESSING TYPED AST " ++ pretty_mod ++ ">>\n"
#endif
        extractIds (tcg_binds env)

        -- Type environment constructed for this module
#if DEBUG
        liftIO $ appendFile "/tmp/ghc.log" $ "<<PROCESSING TYPE ENV FOR " ++ pretty_mod ++ ">>\n"
#endif
        extractTypesFromTypeEnv (tcg_type_env env)

#if DEBUG
    --  liftIO $ writeFile "/tmp/ghc.idmap" (show identMap)
    -- liftIO $ do
    --    cache <- readIORef idPropCacheRef
    --    appendFile "/tmp/ghc.log" $ "Cache == " ++ show cache
#endif
      liftIO $ modifyIORef symbolRef (Map.insert processedName pluginResult)
      return env

extractTypesFromTypeEnv :: TypeEnv -> ExtractIdsM ()
extractTypesFromTypeEnv = mapM_ go . nameEnvUniqueElts
  where
    go :: (Unique, TyThing) -> ExtractIdsM ()
    go (uniq, ADataCon dataCon) =
      recordType uniq (dataConRepType dataCon)
    go _ =
      return ()

{------------------------------------------------------------------------------
  ExtractIdsT adds state and en environment to whatever monad that GHC
  puts us in (this is why it needs to be a monad transformer).

  Note that MonadIO is GHC's MonadIO, not the standard one, and hence we need
  our own instance.
------------------------------------------------------------------------------}

data ExtractIdsEnv = ExtractIdsEnv {
    eIdsDynFlags :: !DynFlags
  , eIdsRdrEnv   :: !RdrName.GlobalRdrEnv
  , eIdsPprStyle :: !PprStyle
  , eIdsCurrent  :: !Module.Module
  , eIdsLinkEnv  :: !LinkEnv
#if DEBUG
  , eIdsStackTrace    :: [String]
#endif
  }

data ExtractIdsState = ExtractIdsState {
    eIdsTidyEnv       :: !TidyEnv
  , eIdsIdList        :: !IdList
  , eIdsExpTypes      :: [(SourceSpan, Text)] -- TODO: explicit sharing?
  , eIdsUseSites      :: !UseSites
  , eIdsFilePathCache :: !(StrictPair (HashMap FilePath Int) Int)
  , eIdsIdPropCache   :: !(Strict IntMap IdProp)
  }

newtype ExtractIdsM a = ExtractIdsM (
      ReaderT ExtractIdsEnv (StrictState ExtractIdsState) a
    )
  deriving (
      Functor
    , Applicative
    , Monad
    , MonadState ExtractIdsState
    , MonadReader ExtractIdsEnv
    )

instance MonadFilePathCaching ExtractIdsM where
  getFilePathCache = eIdsFilePathCache <$> get
  putFilePathCache = \cache -> modify $ \st -> st { eIdsFilePathCache = cache }

instance MonadIdPropCaching ExtractIdsM where
  getIdPropCache = eIdsIdPropCache <$> get
  putIdPropCache = \cache -> modify $ \st -> st { eIdsIdPropCache = cache }

instance TraceMonad ExtractIdsM where
  trace str = do
    -- We are using a strict-state monad (as opposed to a strict state-monad)
    -- so we can use the state to make sure the event gets evaluated
    st <- get
    put $ Debug.traceEvent str st

instance HasDynFlags ExtractIdsM where
  getDynFlags = asks eIdsDynFlags

#if DEBUG
debugLog :: FilePath -> String -> ExtractIdsM ()
debugLog path str = do
    st <- get
    put $ go st
  where
    go :: a -> a
    go x = unsafePerformIO $ do
             appendFile path (str ++ "\n")
             return x
#endif

execExtractIdsT :: MonadIO m
                => DynFlags
                -> TcGblEnv
                -> IdList
                -> Module.Module
                -> ExtractIdsM ()
                -> m PluginResult
execExtractIdsT dynFlags env idList current (ExtractIdsM m) = do
  -- Construct LinkEnv for finding home modules
  -- The order of the package dependencies is important! (See comment for
  -- linkEnvFor.) We assume that ghc gives us the package dependencies in the
  -- right order.
  let pkgDeps = (imp_dep_pkgs (tcg_imports env))
  linkEnv <- liftIO $ linkEnvForDeps dynFlags pkgDeps

  filePathCache <- liftIO $ getFilePathCache
  idPropCache   <- liftIO $ getIdPropCache

  let rdrEnv  = tcg_rdr_env env
      qual    = mkPrintUnqualified dynFlags rdrEnv
      eIdsEnv = ExtractIdsEnv {
                    eIdsDynFlags   = dynFlags
                  , eIdsRdrEnv     = rdrEnv
                  , eIdsPprStyle   = mkUserStyle qual AllTheWay
                  , eIdsCurrent    = current
                  , eIdsLinkEnv    = linkEnv
#if DEBUG
                  , eIdsStackTrace = []
#endif
                  }
      eIdsSt  = ExtractIdsState {
                    eIdsTidyEnv       = emptyTidyEnv
                  , eIdsIdList        = idList
                  , eIdsExpTypes      = []
                  , eIdsUseSites      = Map.empty
                  , eIdsFilePathCache = filePathCache
                  , eIdsIdPropCache   = idPropCache
                  }

  eIdsSt' <- liftIO $ evaluate $ execState (runReaderT m eIdsEnv) eIdsSt

  liftIO $ putFilePathCache (eIdsFilePathCache eIdsSt')
  liftIO $ putIdPropCache   (eIdsIdPropCache   eIdsSt')

  return PluginResult {
      pluginIdList   = eIdsIdList   eIdsSt'
    , pluginExpTypes = eIdsExpTypes eIdsSt'
    , pluginPkgDeps  = force $ map (importPackageId dynFlags) pkgDeps
    , pluginUseSites = eIdsUseSites eIdsSt'
    }

extendIdMap :: SourceSpan -> SpanInfo -> ExtractIdsM ()
extendIdMap span info = modify $ \st -> st {
    eIdsIdList = (span, info) : eIdsIdList st
  }

recordUseSite :: IdPropPtr -> SourceSpan -> ExtractIdsM ()
recordUseSite ptr span = modify $ \st -> st {
      eIdsUseSites = Map.alter insertSpan ptr (eIdsUseSites st)
    }
  where
    insertSpan Nothing   = Just [span]
    insertSpan (Just ss) = Just (span : ss)

tidyType :: Type -> ExtractIdsM Type
tidyType typ = do
  st <- get
  let (tidyEnv', typ') = tidyOpenType (eIdsTidyEnv st) typ
  put $ st { eIdsTidyEnv = tidyEnv' }
  return typ'

lookupRdrEnv :: RdrName.GlobalRdrEnv -> Name -> Maybe RdrName.GlobalRdrElt
lookupRdrEnv rdrEnv name =
  case lookupOccEnv rdrEnv (Name.nameOccName name) of
    Nothing   -> Nothing
    Just elts -> case filter ((== name) . RdrName.gre_name) elts of
                   []    -> Nothing
                   [elt] -> Just elt
                   _     -> error "ghc invariant violated"

recordExpType :: SrcSpan -> Maybe Type -> ExtractIdsM (Maybe Type)
recordExpType span (Just typ) = do
  eitherSpan <- extractSourceSpan span
  case eitherSpan of
    ProperSpan properSpan -> do
      prettyTyp <- tidyType typ >>= showTypeForUser
      modify $ \st -> st {
          eIdsExpTypes = (properSpan, prettyTyp) : eIdsExpTypes st
        }
    TextSpan _ ->
      return () -- Ignore
  return (Just typ)
recordExpType _ Nothing = return Nothing

-- We mark every node in the AST with 'ast'. This is necessary so that we can
-- restore the TidyEnv at every node, so that we can reuse type variables. For
-- instance
--
-- > foo (x, y) = x
-- > bar (x, y) = y
--
-- In this case, we can assign (t, t1) -> t and (t, t1) -> t1 as types, reusing
-- 't' and 't1', but we can only do this because they are not nested.
--
-- For debugging purposes, we also take in two arguments describing the AST.
--
-- TODO: This assumes that the structure of the AST follows Haskell scoping
-- rules.  If this is not the case, this will break.
ast :: Maybe SrcSpan -> String -> ExtractIdsM a -> ExtractIdsM a
ast _mspan _info cont = do
#if DEBUG
  local (\env -> env { eIdsStackTrace = _info : eIdsStackTrace env }) $ do
#endif
    -- Restore the tideEnv after cont
    stBefore <- get
    r        <- cont
    stAfter  <- get
    put $ stAfter { eIdsTidyEnv = eIdsTidyEnv stBefore }
    return r

unsupported :: Maybe SrcSpan -> String -> ExtractIdsM (Maybe Type)
#if DEBUG
unsupported mspan c = ast mspan c $ do
  prettySpan <- prettyM defaultUserStyle mspan
  fail $ "extractIds: unsupported " ++ c ++ " at " ++ prettySpan ++ "\n"
#else
unsupported _ _ = return Nothing
#endif

-- | Construct an IdInfo for a 'Name'. We assume the @GlobalRdrElt@ is
-- uniquely determined by the @Name@ and the @DynFlags@ do not change
-- in a bad way.
idInfoForName
  :: forall m. (MonadFilePathCaching m, MonadIdPropCaching m)
  => DynFlags                         -- ^ The usual dynflags
  -> Name                             -- ^ The name in question
  -> IsBinder                         -- ^ Is this a binding occurrence?
  -> Maybe RdrName.GlobalRdrElt       -- ^ GlobalRdrElt for imported names
  -> Maybe Module.Module              -- ^ Current module for local names
  -> (Name -> Strict Maybe ModuleId)  -- ^ Home modules
  -> m (IdPropPtr, Maybe IdScope)     -- ^ Nothing if imported but no GlobalRdrElt
idInfoForName dflags name idIsBinder mElt mCurrent home = do
    scope      <- constructScope
    idDefSpan  <- extractSourceSpan (Name.nameSrcSpan name)

    let mod          = if isLocal scope
                         then fromJust mCurrent
                         else fromMaybe missingModule $
                           Name.nameModule_maybe name
        idPropPtr    = IdPropPtr . getKey . getUnique $ name
        idDefinedIn  = importModuleId dflags mod
        idHomeModule = home name

    extendIdPropCache idPropPtr IdProp{..}
    return (idPropPtr, scope)
  where
      occ     = Name.nameOccName name
      idName  = Text.pack $ Name.occNameString occ
      idSpace = fromGhcNameSpace $ Name.occNameSpace occ
      idType  = Maybe.nothing  -- after renamer but before typechecker

      constructScope :: m (Maybe IdScope)
      constructScope
        | DefSite <- idIsBinder    = return $ Just Binder
        | Name.isWiredInName  name = return $ Just WiredIn
        | Name.isInternalName name = return $ Just Local
        | otherwise = case mElt of
              Just gre -> do scope <- scopeFromProv (RdrName.gre_prov gre)
                             return (Just scope)
              Nothing  -> return Nothing

      -- We need to guess the PackageId here, because this is not stored as
      -- part of the ImpDeclSpec (listed as a TODO in the ghc sources).
      -- For now we pass 'Nothing' as the PackageQualifier. It *might* be
      -- possible to recover the package qualifier using 'impSpan'.
      scopeFromProv :: RdrName.Provenance -> m IdScope
      scopeFromProv RdrName.LocalDef = do
        return Local
      scopeFromProv (RdrName.Imported spec) = do
        (impMod, impSpan, impQual) <- extractImportInfo spec
        return Imported {
            idImportedFrom = importModuleId' dflags Nothing impMod
          , idImportSpan   = impSpan
          , idImportQual   = Text.pack $ impQual
          }

      extractImportInfo :: [RdrName.ImportSpec] -> m (GHC.ModuleName, EitherSpan, String)
      extractImportInfo (RdrName.ImpSpec decl item:_) = do
        span <- case item of
                  RdrName.ImpAll -> extractSourceSpan (RdrName.is_dloc decl)
                  RdrName.ImpSome _explicit loc -> extractSourceSpan loc
        return
          ( RdrName.is_mod decl
          , span
          , if RdrName.is_qual decl
              then moduleNameString (RdrName.is_as decl) ++ "."
              else ""
          )
      extractImportInfo _ = fail "ghc invariant violated"

      isLocal :: Maybe IdScope -> Bool
      isLocal (Just Local)  = True
      isLocal (Just Binder) = True
      isLocal _             = False

      missingModule :: a
      missingModule = error $ "No module for "
                           ++ pretty dflags defaultUserStyle name

recordType :: Unique -> Type -> ExtractIdsM ()
recordType uniq typ = do
  typStr <- tidyType typ >>= showTypeForUser
  recordIdPropType (IdPropPtr $ getKey uniq) typStr

showTypeForUser :: Type -> ExtractIdsM Text
showTypeForUser typ = do
    pprStyle  <- asks eIdsPprStyle
    prettyTyp <- prettyTypeM pprStyle showForalls typ
    return $ Text.pack prettyTyp
  where
    showForalls = False

extractIds :: Fold a => a -> ExtractIdsM (Maybe Type)
extractIds = fold AstAlg {
    astMark        = ast
  , astUnsupported = unsupported
  , astExpType     = recordExpType
  , astName        = recordName
  , astVar         = recordId
  }

recordId :: Located Id -> IsBinder -> ExtractIdsM (Maybe Type)
recordId (L span id) _idIsBinder = do
    recordType (getUnique id) typ
    recordExpType span (Just typ)
  where
    typ = Var.varType id

recordName :: Located Name -> IsBinder -> ExtractIdsM (Maybe Type)
recordName (L span name) idIsBinder = do
  span' <- extractSourceSpan span
  case span' of
    ProperSpan sourceSpan -> do
      dflags  <- getDynFlags
      rdrEnv  <- asks eIdsRdrEnv
      current <- asks eIdsCurrent
      linkEnv <- asks eIdsLinkEnv

      -- The lookup into the rdr env will only be used for imported names.
      info <- idInfoForName dflags
                            name
                            idIsBinder
                            (lookupRdrEnv rdrEnv name)
                            (Just current)
                            (homeModuleFor dflags linkEnv)

      case info of
        (idProp, Just idScope) -> do
          extendIdMap sourceSpan $ SpanId IdInfo{..}
          case idIsBinder of
            UseSite -> recordUseSite idProp sourceSpan
            _       -> return ()
        _ ->
          -- This only happens for some special cases ('assert' being
          -- the prime example; it gets reported as 'assertError' from
          -- 'GHC.IO.Exception' but there is no corresponding entry in the
          -- GlobalRdrEnv.
          return ()
    TextSpan _ ->
      return ()
  return Nothing
