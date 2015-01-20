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
  ( runHscQQ
  , runHscPlugin
  , runRnSplice
  , extractSourceSpan
  , idInfoForName
  , constructExplicitSharingCache
  , PluginResult(..)
  , IsBinder(..)
  , initExtractIdsSuspendedState
  , ExtractIdsSuspendedState -- opaque
  ) where

#define DEBUG 0

import Control.Monad (liftM)
import Control.Monad.Reader (MonadReader, ReaderT, asks, runReaderT)

#if DEBUG
import Control.Monad.Reader (local)
#endif

import Prelude hiding (id, mod, span, writeFile)
import Control.Applicative (Applicative, (<$>))
import Control.Monad.State.Class (MonadState(..))
import Data.Accessor (Accessor, accessor, (.>))
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe, fromJust)
import Data.Text (Text)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Accessor.Monad.MTL.State as AccState
import qualified Data.ByteString.Char8         as BSSC
import qualified Data.HashMap.Strict           as HashMap
import qualified Data.Text                     as Text
import qualified Debug.Trace                   as Debug

import IdeSession.Strict.Container
import IdeSession.Strict.IORef
import IdeSession.Strict.Pair
import IdeSession.Strict.StateT
import IdeSession.Types.Private as Private
import qualified IdeSession.Strict.IntMap as IntMap
import qualified IdeSession.Strict.Map    as Map
import qualified IdeSession.Strict.Maybe  as Maybe

import DynFlags (HasDynFlags(..), getDynFlags)
import GHC hiding (idType, moduleName, ModuleName)
import HscMain (hscParse', tcRnModule', getHscEnv)
import HscTypes (Hsc, TypeEnv, HscEnv(hsc_dflags), mkPrintUnqualified)
import IOEnv (getEnv)
import MonadUtils (MonadIO (..))
import NameEnv (nameEnvUniqueElts)
import OccName
import Outputable hiding (trace)
import TcRnTypes
import Unique (Unique, getUnique, getKey)
import Var hiding (idInfo)
import VarEnv (TidyEnv, emptyTidyEnv)
import qualified GHC
import qualified Module
import qualified Name
import qualified RdrName

import Conv
import FilePathCaching
import GhcShim
import Haddock
import IdPropCaching
import TraceMonad

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

data PluginResult = PluginResult {
    -- TODO: Why aren't we using strict types for the first two fields?
    pluginIdList   :: !IdList
  , pluginExpTypes :: ![(SourceSpan, Text)]
  , pluginPkgDeps  :: !(Strict [] Private.PackageId)
  , pluginUseSites :: !UseSites
  }

runHscQQ :: StrictIORef ExtractIdsSuspendedState
         -> HsQuasiQuote Name -> RnM (HsQuasiQuote Name)
runHscQQ stRef qq@(HsQuasiQuote quoter _span _str) = do
    span <- (tcl_loc . env_lcl) `liftM` getEnv
    extractIdsResumeTc stRef $ go span
    return qq
  where
    go :: SrcSpan -> ExtractIdsM ()
    go span = do
      ProperSpan span'       <- extractSourceSpan span
      dflags                 <- asks eIdsDynFlags
      linkEnv                <- asks eIdsLinkEnv
      rdrEnv                 <- asks eIdsRdrEnv
      current                <- asks eIdsCurrent
      (idProp, Just idScope) <- idInfoForName dflags
                                              quoter
                                              UseSite
                                              (lookupRdrEnv rdrEnv quoter)
                                              (Just current)
                                              (homeModuleFor dflags linkEnv)
      extendIdMap span' $ SpanQQ IdInfo{..}

runRnSplice :: StrictIORef ExtractIdsSuspendedState
            -> LHsExpr Name -> RnM (LHsExpr Name)
runRnSplice stRef expr = do
  extractIdsResumeTc stRef $ extractIds SpanInSplice expr
  return expr

runHscPlugin :: StrictIORef (Strict (Map ModuleName) PluginResult)
             -> StrictIORef ExtractIdsSuspendedState
             -> ModSummary
             -> Hsc TcGblEnv
runHscPlugin symbolRef stRef mod_summary = do
  dynFlags <- getDynFlags
  tcEnv    <- hscFileFrontEnd mod_summary
  eIdsEnv  <- extractIdsEnvFromTc dynFlags tcEnv

  extractIdsResumeIO stRef eIdsEnv $ do
    -- Information provided by the renamer
    -- See http://www.haskell.org/pipermail/ghc-devs/2013-February/000540.html
    -- It is important we do this *first*, because this creates the initial
    -- cache with the IdInfo objects, which we can then update by processing
    -- the typed AST and the global type environment.
    extractIds SpanId (tcg_rn_decls tcEnv)

    -- Information provided by the type checker
    extractIds SpanId (tcg_binds tcEnv)

    -- Type environment constructed for this module
    extractTypesFromTypeEnv (tcg_type_env tcEnv)

  eIdsSt' <- liftIO $ extractIdsReset stRef
  let processedModule = tcg_mod tcEnv
      processedName   = Text.pack $ moduleNameString $ GHC.moduleName processedModule
      pkgDeps         = imp_dep_pkgs (tcg_imports tcEnv)
      pluginResult    = PluginResult {
           pluginIdList   = _eIdsIdList   eIdsSt'
         , pluginExpTypes = _eIdsExpTypes eIdsSt'
         , pluginPkgDeps  = force $ map (importPackageId dynFlags) pkgDeps
         , pluginUseSites = _eIdsUseSites eIdsSt'
         }

  liftIO $ modifyIORef symbolRef (Map.insert processedName pluginResult)
  return tcEnv

extractTypesFromTypeEnv :: TypeEnv -> ExtractIdsM ()
extractTypesFromTypeEnv = mapM_ go . nameEnvUniqueElts
  where
    go :: (Unique, TyThing) -> ExtractIdsM ()
    go (uniq, tyThing) =
      maybe (return ()) (recordType uniq) (typeOfTyThing tyThing)

{------------------------------------------------------------------------------
  Override hscFileFrontEnd so that we pass the renamer result through the
  type checker
------------------------------------------------------------------------------}

hscFileFrontEnd :: ModSummary -> Hsc TcGblEnv
hscFileFrontEnd mod_summary = do
    hpm <- hscParse' mod_summary
    hsc_env <- getHscEnv
    let passRenamerResult = True
    tcg_env <- tcRnModule' hsc_env mod_summary passRenamerResult hpm
    return tcg_env

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
  , eIdsStackTrace :: [String]
#endif
  }

data ExtractIdsSuspendedState = ExtractIdsSuspendedState {
    _eIdsTidyEnv  :: !TidyEnv
  , _eIdsIdList   :: !IdList
  , _eIdsExpTypes :: [(SourceSpan, Text)] -- TODO: explicit sharing?
  , _eIdsUseSites :: !UseSites
  }

data ExtractIdsRunningState = ExtractIdsRunningState {
    _eIdsSuspendedState :: !ExtractIdsSuspendedState
  ,  eIdsFilePathCache  :: !(StrictPair (HashMap FilePath Int) Int)
  ,  eIdsIdPropCache    :: !(Strict IntMap IdProp)
  }

eIdsTidyEnv'  :: Accessor ExtractIdsSuspendedState TidyEnv
eIdsIdList'   :: Accessor ExtractIdsSuspendedState IdList
eIdsExpTypes' :: Accessor ExtractIdsSuspendedState [(SourceSpan, Text)]
eIdsUseSites' :: Accessor ExtractIdsSuspendedState UseSites

eIdsTidyEnv'  = accessor _eIdsTidyEnv  $ \x s -> s { _eIdsTidyEnv  = x }
eIdsIdList'   = accessor _eIdsIdList   $ \x s -> s { _eIdsIdList   = x }
eIdsExpTypes' = accessor _eIdsExpTypes $ \x s -> s { _eIdsExpTypes = x }
eIdsUseSites' = accessor _eIdsUseSites $ \x s -> s { _eIdsUseSites = x }

eIdsSuspendedState :: Accessor ExtractIdsRunningState ExtractIdsSuspendedState
eIdsSuspendedState = accessor _eIdsSuspendedState $ \x s -> s { _eIdsSuspendedState = x }

eIdsTidyEnv  :: Accessor ExtractIdsRunningState TidyEnv
eIdsIdList   :: Accessor ExtractIdsRunningState IdList
eIdsExpTypes :: Accessor ExtractIdsRunningState [(SourceSpan, Text)]
eIdsUseSites :: Accessor ExtractIdsRunningState UseSites

eIdsTidyEnv  = eIdsSuspendedState .> eIdsTidyEnv'
eIdsIdList   = eIdsSuspendedState .> eIdsIdList'
eIdsExpTypes = eIdsSuspendedState .> eIdsExpTypes'
eIdsUseSites = eIdsSuspendedState .> eIdsUseSites'

newtype ExtractIdsM a = ExtractIdsM (
      ReaderT ExtractIdsEnv (StrictState ExtractIdsRunningState) a
    )
  deriving (
      Functor
    , Applicative
    , Monad
    , MonadState ExtractIdsRunningState
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

extractIdsResumeIO :: MonadIO m
                   => StrictIORef ExtractIdsSuspendedState
                   -> ExtractIdsEnv
                   -> ExtractIdsM a
                   -> m a
extractIdsResumeIO stRef env (ExtractIdsM act) = do
  filePathCache <- liftIO $ getFilePathCache
  idPropCache   <- liftIO $ getIdPropCache
  st            <- liftIO $ readIORef stRef

  let (a, st') = runState (runReaderT act env) ExtractIdsRunningState {
                     _eIdsSuspendedState = st
                   ,  eIdsFilePathCache  = filePathCache
                   ,  eIdsIdPropCache    = idPropCache
                   }

  liftIO $ do
    writeIORef stRef (_eIdsSuspendedState st')
    putFilePathCache ( eIdsFilePathCache  st')
    putIdPropCache   ( eIdsIdPropCache    st')

  return a

extractIdsResumeTc :: StrictIORef ExtractIdsSuspendedState
                   -> ExtractIdsM a
                   -> TcRn a
extractIdsResumeTc stRef act = do
  tcEnv   <- getEnv
  eIdsEnv <- extractIdsEnvFromTc (hsc_dflags (env_top tcEnv)) (env_gbl tcEnv)
  liftIO $ extractIdsResumeIO stRef eIdsEnv act

extractIdsEnvFromTc :: MonadIO m => DynFlags -> TcGblEnv -> m ExtractIdsEnv
extractIdsEnvFromTc eIdsDynFlags env = liftIO $ do
    eIdsLinkEnv <- linkEnvForDeps eIdsDynFlags pkgDeps
    return ExtractIdsEnv{..}
  where
    eIdsRdrEnv     = tcg_rdr_env env
    eIdsCurrent    = tcg_mod env
    pkgDeps        = imp_dep_pkgs (tcg_imports env)
    qual           = mkPrintUnqualified eIdsDynFlags eIdsRdrEnv
    eIdsPprStyle   = mkUserStyle qual AllTheWay
#if DEBUG
    eIdsStackTrace = []
#endif

extractIdsReset :: StrictIORef ExtractIdsSuspendedState -> IO ExtractIdsSuspendedState
extractIdsReset stRef = do
  oldState <- readIORef stRef
  writeIORef stRef initExtractIdsSuspendedState
  return oldState

initExtractIdsSuspendedState :: ExtractIdsSuspendedState
initExtractIdsSuspendedState =
  ExtractIdsSuspendedState {
      _eIdsTidyEnv       = emptyTidyEnv
    , _eIdsIdList        = []
    , _eIdsExpTypes      = []
    , _eIdsUseSites      = Map.empty
    }

_debugLog :: FilePath -> String -> ExtractIdsM ()
_debugLog path str = do
    st <- get
    put $ go st
  where
    go :: a -> a
    go x = unsafePerformIO $ do
             appendFile path (str ++ "\n")
             return x

extendIdMap :: SourceSpan -> SpanInfo -> ExtractIdsM ()
extendIdMap span info =
    AccState.modify eIdsIdList ((span, info) :)

recordUseSite :: IdPropPtr -> SourceSpan -> ExtractIdsM ()
recordUseSite ptr span =
    AccState.modify eIdsUseSites (Map.alter insertSpan ptr)
  where
    insertSpan Nothing   = Just [span]
    insertSpan (Just ss) = Just (span : ss)

tidyType :: Type -> ExtractIdsM Type
tidyType typ = do
  tidyEnv <- AccState.get eIdsTidyEnv
  let (tidyEnv', typ') = tidyOpenType tidyEnv typ
  AccState.set eIdsTidyEnv tidyEnv'
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
      AccState.modify eIdsExpTypes ((properSpan, prettyTyp) :)
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
    tidyEnv <- AccState.get eIdsTidyEnv
    result  <- cont
    AccState.set eIdsTidyEnv tidyEnv
    return result

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

extractIds :: Fold a => (IdInfo -> SpanInfo) -> a -> ExtractIdsM (Maybe Type)
extractIds mkSpanInfo = fold AstAlg {
    astMark        = ast
  , astUnsupported = unsupported
  , astExpType     = recordExpType
  , astName        = recordName mkSpanInfo
  , astVar         = recordId
  }

recordId :: Located Id -> IsBinder -> ExtractIdsM (Maybe Type)
recordId (L span id) _idIsBinder = do
    recordType (getUnique id) typ
    recordExpType span (Just typ)
  where
    typ = Var.varType id

recordName :: (IdInfo -> SpanInfo)
           -> Located Name -> IsBinder
           -> ExtractIdsM (Maybe Type)
recordName mkSpanInfo (L span name) idIsBinder = do
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
          extendIdMap sourceSpan $ mkSpanInfo IdInfo{..}
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
      return () -- Name without source span
  return Nothing
