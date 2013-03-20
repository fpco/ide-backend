{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, TemplateHaskell, CPP, DeriveDataTypeable #-}
module GhcHsWalk
  ( IdMap(..)
  , IdInfo(..)
  , IdNameSpace(..)
  , IdScope(..)
  , LoadedModules
  , extractIdsPlugin
  , haddockLink
  , idInfoAtLocation
  , idMapToList
  , idMapFromList
  , idInfoForName
  , extractSourceSpan
  ) where

import Prelude hiding (span, id, mod)
import Control.Arrow (first, second)
import Control.Monad (forM_)
import Control.Monad.State (MonadState, StateT, execStateT, modify, state, get, put)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, asks)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.IORef
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.TH (deriveJSON)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Version
import Data.Generics
import Data.Maybe (fromMaybe)
import Data.List (stripPrefix)

import qualified Common
import Common hiding (ModuleName)

import qualified GHC
import GHC hiding (idType, PackageId, moduleName)
import TcRnTypes
import Outputable
import HscPlugin
import Var hiding (idInfo)
import qualified Name
import qualified Module
import MonadUtils (MonadIO(..))
import Bag
import DataCon (dataConName)
import qualified Packages
import qualified RdrName
import OccName
import PprTyThing (pprTypeForUser)
import TcType (tidyOpenType)
import VarEnv (TidyEnv, emptyTidyEnv)
import FastString ( unpackFS )

#define DEBUG 1

{------------------------------------------------------------------------------
  Environment mapping source locations to info
------------------------------------------------------------------------------}

-- This type is abstract in GHC. One more reason to define our own.
data IdNameSpace =
    VarName    -- ^ Variables, including real data constructors
  | DataName   -- ^ Source data constructors
  | TvName     -- ^ Type variables
  | TcClsName  -- ^ Type constructors and classes
  deriving (Show, Eq, Data, Typeable)

-- | Information about identifiers
data IdInfo = IdInfo
  { -- | The base name of the identifer at this location. Module prefix
    -- is not included.
    idName :: String
    -- | The module prefix of the identifier. Empty, if a local variable.
  , idSpace :: IdNameSpace
    -- | The type
    -- We don't always know this; in particular, we don't know kinds because
    -- the type checker does not give us LSigs for top-level annotations)
  , idType :: Maybe String
    -- | Scope
  , idScope :: IdScope
  }
  deriving (Eq, Data, Typeable)

-- TODO: Ideally, we would have
-- 1. SourceSpan for Local rather than EitherSpan
-- 2. Don't have Maybe String or Maybe Package in the import case
--    (under which circumstances do we not get package information? -- the unit
--    tests give us examples)
-- 3. SourceSpan for idImportSpan
-- 4. Have a idImportedFromPackage, but unfortunately ghc doesn't give us
--    this information (it's marked as a TODO in RdrName.lhs)
data IdScope =
    -- | This is a binding occurrence (@f x = ..@, @\x -> ..@, etc.)
    Binder
    -- | Defined within this module
  | Local {
        idDefSpan :: EitherSpan
      }
    -- | Imported from a different module
  | Imported {
        idDefSpan      :: EitherSpan
      , idDefinedIn    :: ModuleId
      , idImportedFrom :: ModuleId
      , idImportSpan   :: EitherSpan
      }
    -- | Wired into the compiler (@()@, @True@, etc.)
  | WiredIn
  deriving (Eq, Data, Typeable)

data IdMap = IdMap { idMapToMap :: Map SourceSpan IdInfo }
  deriving (Data, Typeable)

type LoadedModules = Map Common.ModuleName IdMap

$(deriveJSON (\x -> x) ''IdNameSpace)
$(deriveJSON (\x -> x) ''IdScope)
$(deriveJSON (\x -> x) ''IdInfo)

instance FromJSON IdMap where
  parseJSON = fmap idMapFromList . parseJSON

instance ToJSON IdMap where
  toJSON = toJSON . idMapToList

idMapToList :: IdMap -> [(SourceSpan, IdInfo)]
idMapToList = Map.toList . idMapToMap

idMapFromList :: [(SourceSpan, IdInfo)] -> IdMap
idMapFromList = IdMap . Map.fromList

instance Show IdMap where
  show = unlines . map show . idMapToList

instance Show IdInfo where
  show (IdInfo {..}) = idName ++ " "
                    ++ "(" ++ show idSpace ++ ") "
                    ++ (case idType of Just typ -> ":: " ++ typ ++ " "; Nothing -> [])
                    ++ "(" ++ show idScope ++ ")"

-- TODO: If these Maybes stay, we should have a prettier Show instance
-- (but hopefully they will go)
instance Show IdScope where
  show Binder          = "binding occurrence"
  show (Local {..})    = "defined at " ++ show idDefSpan
  show WiredIn         = "wired in to the compiler"
  show (Imported {..}) = "defined in "
                      ++ show idDefinedIn ++ " at "
                      ++ show idDefSpan ++ "; imported from "
                      ++ show idImportedFrom ++ " at "
                      ++ show idImportSpan

fromGhcNameSpace :: Name.NameSpace -> IdNameSpace
fromGhcNameSpace ns
  | ns == Name.varName  = VarName
  | ns == Name.dataName = DataName
  | ns == Name.tvName   = TvName
  | ns == Name.tcName   = TcClsName
  | otherwise = error "fromGhcNameSpace"

-- | Show approximately what Haddock adds to documantation URLs.
haddockSpaceMarks :: IdNameSpace -> String
haddockSpaceMarks VarName = "v"
haddockSpaceMarks DataName = "v"
haddockSpaceMarks TvName = "t"
haddockSpaceMarks TcClsName = "t"

-- | Show approximately a haddock link (without haddock root) for an id.
-- This is an illustraction and a test of the id info, but under ideal
-- conditions could perhaps serve to link to documentation without
-- going via Hoogle.
haddockLink :: IdInfo -> String
haddockLink IdInfo{..} =
  case idScope of
    Imported{idImportedFrom} ->
         dashToSlash (modulePackage idImportedFrom)
      ++ "/doc/html/"
      ++ dotToDash (moduleName idImportedFrom) ++ ".html#"
      ++ haddockSpaceMarks idSpace ++ ":"
      ++ idName
    _ -> "<local identifier>"
 where
   dotToDash = map (\c -> if c == '.' then '-' else c)
   dashToSlash p = case packageVersion p of
     Nothing -> packageName p ++ "/latest"
     Just version -> packageName p ++ "/" ++ version

idInfoAtLocation :: Int -> Int -> IdMap -> [(SourceSpan, IdInfo)]
idInfoAtLocation line col = filter inRange . idMapToList
  where
    inRange :: (SourceSpan, a) -> Bool
    inRange (SourceSpan{..}, _) =
      (line   > spanFromLine || (line == spanFromLine && col >= spanFromColumn)) &&
      (line   < spanToLine   || (line == spanToLine   && col <= spanToColumn))

{------------------------------------------------------------------------------
  Extract an IdMap from information returned by the ghc type checker
------------------------------------------------------------------------------}

extractIdsPlugin :: IORef LoadedModules -> HscPlugin
extractIdsPlugin symbolRef = HscPlugin $ \dynFlags env -> do
  let processedModule = tcg_mod env
      processedName = moduleNameString $ GHC.moduleName processedModule
  identMap <- execExtractIdsT dynFlags (tcg_rdr_env env) $ do
#if DEBUG
    pretty_mod     <- pretty False processedModule
    pretty_rdr_env <- pretty False (tcg_rdr_env env)
    liftIO $ writeFile "/tmp/ghc.readerenv" pretty_rdr_env
#endif
    -- Information provided by the renamer
    -- See http://www.haskell.org/pipermail/ghc-devs/2013-February/000540.html
#if DEBUG
    liftIO $ appendFile "/tmp/ghc.log" $ "<<PROCESSING RENAMED AST " ++ pretty_mod ++ ">>\n"
#endif
    extractIds (tcg_rn_decls env)
    -- Information provided by the type checker
#if DEBUG
    liftIO $ appendFile "/tmp/ghc.log" $ "<<PROCESSING TYPED AST " ++ pretty_mod ++ ">>\n"
#endif
    extractIds (tcg_binds env)
#if DEBUG
  liftIO $ writeFile "/tmp/ghc.idmap" (show identMap)
#endif
  liftIO $ modifyIORef symbolRef (Map.insert processedName identMap)
  return env

{------------------------------------------------------------------------------
  ExtractIdsT is just a wrapper around the writer monad for IdMap
  (we wrap it to avoid orphan instances). Note that MonadIO is GHC's
  MonadIO, not the standard one, and hence we need our own instance.
------------------------------------------------------------------------------}

-- Define type synonym to avoid orphan instances
newtype ExtractIdsT m a = ExtractIdsT (
      ReaderT (DynFlags, RdrName.GlobalRdrEnv) (StateT (TidyEnv, IdMap) m) a
    )
  deriving (Functor, Monad, MonadState (TidyEnv, IdMap), MonadReader (DynFlags, RdrName.GlobalRdrEnv))

execExtractIdsT :: Monad m => DynFlags -> RdrName.GlobalRdrEnv -> ExtractIdsT m () -> m IdMap
execExtractIdsT dynFlags rdrEnv (ExtractIdsT m) = do
  (_, idMap) <- execStateT (runReaderT m (dynFlags, rdrEnv)) (emptyTidyEnv, IdMap Map.empty)
  return idMap

instance MonadTrans ExtractIdsT where
  lift = ExtractIdsT . lift . lift

-- This is not the standard MonadIO, but the MonadIO from GHC!
instance MonadIO m => MonadIO (ExtractIdsT m) where
  liftIO = lift . liftIO

getDynFlags :: Monad m => ExtractIdsT m DynFlags
getDynFlags = asks fst

getGlobalRdrEnv :: Monad m => ExtractIdsT m RdrName.GlobalRdrEnv
getGlobalRdrEnv = asks snd

modifyIdMap :: Monad m => (Map SourceSpan IdInfo -> Map SourceSpan IdInfo) -> ExtractIdsT m ()
modifyIdMap f = modify . second $ IdMap . f . idMapToMap

prettyType :: Monad m => Type -> ExtractIdsT m Type
prettyType typ = state $ \(tidyEnv, idMap) ->
  let (tidyEnv', typ') = tidyOpenType tidyEnv typ
  in (typ', (tidyEnv', idMap))

lookupRdrEnv :: RdrName.GlobalRdrEnv -> Name -> Maybe RdrName.GlobalRdrElt
lookupRdrEnv rdrEnv name =
  case lookupOccEnv rdrEnv (Name.nameOccName name) of
    Nothing   -> Nothing
    Just elts -> case filter ((== name) . RdrName.gre_name) elts of
                   []    -> Nothing
                   [elt] -> Just elt
                   _     -> error "ghc invariant violated"

-- In ghc 7.4 showSDoc does not take the dynflags argument; for 7.6 and up
-- it does
pretty :: (Monad m, Outputable a) => Bool -> a -> ExtractIdsT m String
pretty debugShow val = do
  _dynFlags <- getDynFlags
#if __GLASGOW_HASKELL__ >= 706
  return $ (if debugShow then showSDocDebug else showSDoc) _dynFlags (ppr val)
#else
  return $ (if debugShow then showSDocDebug else showSDoc) (ppr val)
#endif

debugPP :: (MonadIO m, Outputable a) => String -> a -> ExtractIdsT m ()
#if DEBUG
debugPP header val = do
  val' <- pretty True val
  liftIO $ appendFile "/tmp/ghc.log" (header ++ ": " ++ val' ++ "\n")
#else
debugPP _ _ = return ()
#endif

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
ast :: Monad m => Maybe SrcSpan -> String -> ExtractIdsT m a -> ExtractIdsT m a
ast _mspan _info cont = do
  (tidyEnv, _) <- get
  r <- cont
  (_, idMap') <- get
  put (tidyEnv, idMap')
  return r

unsupported :: MonadIO m => Maybe SrcSpan -> String -> ExtractIdsT m ()
#if DEBUG
unsupported mspan c = ast mspan c $ do
  prettySpan <- pretty False mspan
  liftIO . appendFile "/tmp/ghc.log" $ "extractIds: unsupported " ++ c ++ " at " ++ prettySpan ++ "\n"
#else
unsupported _ _ = return ()
#endif

{------------------------------------------------------------------------------
  Record
------------------------------------------------------------------------------}

type IsBinder = Bool

class OutputableBndr id => Record id where
  record :: MonadIO m => SrcSpan -> IsBinder -> id -> ExtractIdsT m ()

instance Record Id where
  record span _idIsBinder id = case extractSourceSpan span of
    ProperSpan sourceSpan -> do
      typ <- prettyType (Var.varType id)
      let typDoc = pprTypeForUser True typ
      let addType idInfo = Just $ idInfo { idType = Just (showSDoc typDoc) }
      modifyIdMap $ Map.update addType sourceSpan
    TextSpan _ ->
      return ()

-- | Construct IdInfo for a 'Name'
idInfoForName :: DynFlags                   -- ^ The usual dynflags
              -> Name                       -- ^ The name in question
              -> Bool                       -- ^ Is this a binding occurrence?
              -> Maybe RdrName.GlobalRdrElt -- ^ GlobalRdrElt for imported names
              -> Maybe IdInfo               -- ^ Nothing if imported but no GlobalRdrElt
idInfoForName dflags name idIsBinder mElt =
    case constructScope of
      Just idScope -> Just IdInfo{..}
      Nothing      -> Nothing
  where
      occ     = Name.nameOccName name
      idName  = Name.occNameString occ
      idSpace = fromGhcNameSpace $ Name.occNameSpace occ
      idType  = Nothing -- After renamer but before typechecker

      constructScope :: Maybe IdScope
      constructScope
        | idIsBinder               = Just Binder
        | Name.isWiredInName  name = Just WiredIn
        | Name.isInternalName name = Just Local {
              idDefSpan = extractSourceSpan (Name.nameSrcSpan name)
            }
        | otherwise = case mElt of
              Just gre -> Just $ scopeFromProv (RdrName.gre_prov gre)
              Nothing  -> Nothing

      scopeFromProv :: RdrName.Provenance -> IdScope
      scopeFromProv RdrName.LocalDef = Local {
          idDefSpan = extractSourceSpan (Name.nameSrcSpan name)
        }
      scopeFromProv (RdrName.Imported spec) =
        let mod = fromMaybe
                    (error (concatMap showSDocDebug $ ppr name : map ppr spec))
                    $ Name.nameModule_maybe name
            (impMod, impSpan) = extractImportInfo spec
        in Imported {
          idDefSpan      = extractSourceSpan (Name.nameSrcSpan name)
        , idDefinedIn    = ModuleId
            { moduleName    = moduleNameString $ Module.moduleName mod
            , modulePackage = fillVersion $ Module.modulePackageId mod
            }
        , idImportedFrom = ModuleId
            { moduleName    = moduleNameString impMod
            , modulePackage = modToPkg impMod
            }
        , idImportSpan   = impSpan
        }

      modToPkg :: ModuleName -> PackageId
      modToPkg impMod =
        let pkgAll = Packages.lookupModuleInAllPackages dflags impMod
            pkgExposed = filter (\ (p, b) -> b && Packages.exposed p) pkgAll
        in case pkgExposed of
          [] -> mainPackage  -- we assume otherwise GHC would signal an error
          [p] -> fillVersion $ Packages.packageConfigId $ fst p
          _ -> let pkgIds = map (first (Module.packageIdString
                                        . Packages.packageConfigId)) pkgExposed
               in error $ "modToPkg: " ++ moduleNameString impMod
                          ++ ": " ++ show pkgIds

      fillVersion :: Module.PackageId -> PackageId
      fillVersion p =
        case Packages.lookupPackage (Packages.pkgIdMap (pkgState dflags)) p of
          Nothing -> if p == Module.mainPackageId
                     then mainPackage
                     else error $ "fillVersion:" ++ Module.packageIdString p
          Just pkgCfg ->
            let sourcePkgId = Packages.sourcePackageId pkgCfg
                pkgName = Packages.pkgName sourcePkgId
                prefixPN = "PackageName "
                showPN = show pkgName
                -- A hack to avoid importing Distribution.Package.
                errPN = fromMaybe (error $ "stripPrefixPN "
                                           ++ prefixPN ++ " "
                                           ++ showPN)
                        $ stripPrefix prefixPN showPN
                packageName = init $ tail errPN
                pkgVersion  = Packages.pkgVersion sourcePkgId
                packageVersion = Just $ case showVersion pkgVersion of
                  -- See http://www.haskell.org/ghc/docs/7.4.2//html/libraries/ghc/Module.html#g:3.
                  -- The version of wired-in packages is completely wiped out,
                  -- but we use a leak in the form of a Cabal package id
                  -- for the same package, which still contains a version.
                  "" -> let installedPkgId = Packages.installedPackageId pkgCfg
                            prefixPV = "InstalledPackageId "
                                       ++ "\"" ++ packageName ++ "-"
                            showPV = show installedPkgId
                            -- A hack to avoid cabal dependency, in particular.
                            errPV = fromMaybe (error $ "stripPrefixPV "
                                                       ++ prefixPV ++ " "
                                                       ++ showPV)
                                    $ stripPrefix prefixPV showPV
                        in reverse $ tail $ snd $ break (=='-') $ reverse errPV
                  s  -> s
            in PackageId {..}

      mainPackage :: PackageId
      mainPackage =
        PackageId { packageName = Module.packageIdString Module.mainPackageId
                  , packageVersion = Nothing }  -- the only case of no version

      extractImportInfo :: [RdrName.ImportSpec] -> (ModuleName, EitherSpan)
      extractImportInfo (RdrName.ImpSpec decl item:_) =
        ( RdrName.is_mod decl
        , case item of
            RdrName.ImpAll -> extractSourceSpan (RdrName.is_dloc decl)
            RdrName.ImpSome _explicit loc -> extractSourceSpan loc
        )
      extractImportInfo _ = error "ghc invariant violated"

extractSourceSpan :: SrcSpan -> EitherSpan
extractSourceSpan (RealSrcSpan srcspan) =
  ProperSpan $ SourceSpan
    (unpackFS (srcSpanFile srcspan))
    (srcSpanStartLine srcspan) (srcSpanStartCol srcspan)
    (srcSpanEndLine srcspan)   (srcSpanEndCol   srcspan)
extractSourceSpan (UnhelpfulSpan s) = TextSpan $ unpackFS s

instance Record Name where
  record span idIsBinder name = case extractSourceSpan span of
      ProperSpan sourceSpan -> do
        dflags <- getDynFlags
        rdrEnv <- getGlobalRdrEnv
        -- The lookup into the rdr env will only be used for imported names
        case idInfoForName dflags name idIsBinder (lookupRdrEnv rdrEnv name) of
          Just idInfo ->
            modifyIdMap $ Map.insert sourceSpan idInfo
          Nothing ->
            -- This only happens for some special cases ('assert' being
            -- the prime example; it gets reported as 'assertError' from
            -- 'GHC.IO.Exception' but there is no corresponding entry in the
            -- GlobalRdrEnv.
            return ()
      TextSpan _ ->
        debugPP "Name without source span" name

{------------------------------------------------------------------------------
  ExtractIds
------------------------------------------------------------------------------}

class ExtractIds a where
  extractIds :: MonadIO m => a -> ExtractIdsT m ()

instance ExtractIds a => ExtractIds [a] where
  extractIds = mapM_ extractIds

instance ExtractIds a => ExtractIds (Maybe a) where
  extractIds Nothing  = return ()
  extractIds (Just x) = extractIds x

instance Record id => ExtractIds (HsGroup id) where
  extractIds group = ast Nothing "HsGroup" $ do
    -- TODO: HsGroup has lots of other fields
    extractIds (hs_valds group)
    extractIds (hs_tyclds group)

instance Record id => ExtractIds (HsValBinds id) where
  extractIds (ValBindsIn {}) =
    fail "extractIds: Unexpected ValBindsIn"
  extractIds (ValBindsOut binds sigs) = ast Nothing "ValBindsOut" $ do
    extractIds (map snd binds)
    extractIds sigs

instance Record id => ExtractIds (LSig id) where
  extractIds (L span (TypeSig names tp)) = ast (Just span) "TypeSig" $ do
    forM_ names $ \name -> record (getLoc name) False (unLoc name)
    extractIds tp
  extractIds (L span (GenericSig _ _)) = unsupported (Just span) "GenericSig"
  extractIds (L span (IdSig _))        = unsupported (Just span) "IdSig"
  extractIds (L span (FixSig _))       = unsupported (Just span) "FixSig"
  extractIds (L span (InlineSig _ _))  = unsupported (Just span) "InlineSig"
  extractIds (L span (SpecSig _ _ _))  = unsupported (Just span) "SpecSig"
  extractIds (L span (SpecInstSig _))  = unsupported (Just span) "SpecInstSig"

instance Record id => ExtractIds (LHsType id) where
  extractIds (L span (HsFunTy arg res)) = ast (Just span) "HsFunTy" $
    extractIds [arg, res]
  extractIds (L span (HsTyVar name)) = ast (Just span) "HsTyVar" $
    record span False name
  extractIds (L span (HsForAllTy _explicitFlag tyVars ctxt body)) = ast (Just span) "hsForAllTy" $ do
    extractIds tyVars
    extractIds ctxt
    extractIds body
  extractIds (L span (HsAppTy fun arg)) = ast (Just span) "HsAppTy" $
    extractIds [fun, arg]
  extractIds (L span (HsTupleTy _tupleSort typs)) = ast (Just span) "HsTupleTy" $
    -- tupleSort is unboxed/boxed/etc.
    extractIds typs
  extractIds (L span (HsListTy typ)) = ast (Just span) "HsListTy" $
    extractIds typ
  extractIds (L span (HsPArrTy typ)) = ast (Just span) "HsPArrTy" $
    extractIds typ
  extractIds (L span (HsParTy typ)) = ast (Just span) "HsParTy" $
    extractIds typ
  extractIds (L span (HsEqTy a b)) = ast (Just span) "HsEqTy" $
    extractIds [a, b]

  extractIds (L span (HsOpTy _ _ _))          = unsupported (Just span) "HsOpTy"
  extractIds (L span (HsIParamTy _ _))        = unsupported (Just span) "HsIParamTy"
  extractIds (L span (HsKindSig _ _))         = unsupported (Just span) "HsKindSig"
  extractIds (L span (HsQuasiQuoteTy _))      = unsupported (Just span) "HsQuasiQuoteTy"
  extractIds (L span (HsSpliceTy _ _ _))      = unsupported (Just span) "HsSpliceTy"
  extractIds (L span (HsDocTy _ _))           = unsupported (Just span) "HsDocTy"
  extractIds (L span (HsBangTy _ _))          = unsupported (Just span) "HsBangTy"
  extractIds (L span (HsRecTy _))             = unsupported (Just span) "HsRecTy"
  extractIds (L span (HsCoreTy _))            = unsupported (Just span) "HsCoreTy"
  extractIds (L span (HsExplicitListTy _ _))  = unsupported (Just span) "HsExplicitListTy"
  extractIds (L span (HsExplicitTupleTy _ _)) = unsupported (Just span) "HsExplicitTupleTy"
  extractIds (L span (HsWrapTy _ _))          = unsupported (Just span) "HsWrapTy"

#if __GLASGOW_HASKELL__ >= 706
  extractIds (L span (HsTyLit _))             = unsupported (Just span) "HsTyLit"
#endif

#if __GLASGOW_HASKELL__ >= 706
instance Record id => ExtractIds (LHsTyVarBndrs id) where
  extractIds (HsQTvs _kvs tvs) = ast Nothing "HsQTvs" $ do
    -- We don't have location info for the kind variables
    extractIds tvs
#endif

instance Record id => ExtractIds (LHsTyVarBndr id) where
#if __GLASGOW_HASKELL__ >= 706
  extractIds (L span (UserTyVar name)) = ast (Just span) "UserTyVar" $
#else
  extractIds (L span (UserTyVar name _postTcKind)) = ast (Just span) "UserTyVar" $
#endif
    record span True name

#if __GLASGOW_HASKELL__ >= 706
  extractIds (L span (KindedTyVar name _kind)) = ast (Just span) "KindedTyVar" $
#else
  extractIds (L span (KindedTyVar name _kind _postTcKind)) = ast (Just span) "KindedTyVar" $
#endif
    -- TODO: deal with _kind
    record span True name

instance Record id => ExtractIds (LHsContext id) where
  extractIds (L span typs) = ast (Just span) "LHsContext" $
    extractIds typs

instance Record id => ExtractIds (LHsBinds id) where
  extractIds = extractIds . bagToList

instance Record id => ExtractIds (LHsBind id) where
  extractIds (L span bind@(FunBind {})) = ast (Just span) "FunBind" $ do
    record (getLoc (fun_id bind)) True (unLoc (fun_id bind))
    extractIds (fun_matches bind)
  extractIds (L span bind@(PatBind {})) = ast (Just span) "PatBind" $ do
    extractIds (pat_lhs bind)
    extractIds (pat_rhs bind)
  extractIds (L span _bind@(VarBind {})) = ast (Just span) "VarBind" $
    -- These are only introduced by the type checker, and don't involve user
    -- written code. The ghc comments says "located 'only for consistency'"
    return ()
  extractIds (L span bind@(AbsBinds {})) = ast (Just span) "AbsBinds" $
    extractIds (abs_binds bind)

#if __GLASGOW_HASKELL__ >= 707
instance (ExtractIds body, Record id) => ExtractIds (MatchGroup id body) where
  extractIds (MG matches _argTys _resTy) = ast Nothing "MatchGroup" $
    extractIds matches
#else
instance Record id => ExtractIds (MatchGroup id) where
  -- We ignore the postTcType, as it doesn't have location information
  extractIds (MatchGroup matches _postTcType) = ast Nothing "MatchGroup" $
    extractIds matches
#endif

#if __GLASGOW_HASKELL__ >= 707
instance (ExtractIds body, Record id) => ExtractIds (LMatch id body) where
#else
instance Record id => ExtractIds (LMatch id) where
#endif
  extractIds (L span (Match pats _type rhss)) = ast (Just span) "Match" $ do
    extractIds pats
    extractIds rhss

#if __GLASGOW_HASKELL__ >= 707
instance (ExtractIds body, Record id) => ExtractIds (GRHSs id body) where
#else
instance Record id => ExtractIds (GRHSs id) where
#endif
  extractIds (GRHSs rhss binds) = ast Nothing "GRHSs" $ do
    extractIds rhss
    extractIds binds

#if __GLASGOW_HASKELL__ >= 707
instance (ExtractIds body, Record id) => ExtractIds (LGRHS id body) where
#else
instance Record id => ExtractIds (LGRHS id) where
#endif
  extractIds (L span (GRHS _guards rhs)) = ast (Just span) "GRHS" $
    extractIds rhs

instance Record id => ExtractIds (HsLocalBinds id) where
  extractIds EmptyLocalBinds =
    return ()
  extractIds (HsValBinds (ValBindsIn _ _)) =
    fail "extractIds: Unexpected ValBindsIn (after renamer these should not exist)"
  extractIds (HsValBinds (ValBindsOut binds sigs)) = ast Nothing "HsValBinds" $ do
    extractIds (map snd binds) -- "fst" is 'rec flag'
    extractIds sigs
  extractIds (HsIPBinds _) =
    unsupported Nothing "HsIPBinds"

instance Record id => ExtractIds (LHsExpr id) where
  extractIds (L span (HsPar expr)) = ast (Just span) "HsPar" $
    extractIds expr
  extractIds (L span (ExprWithTySig expr _type)) = ast (Just span) "ExprWithTySig" $
    extractIds expr
  extractIds (L span (ExprWithTySigOut expr _type)) = ast (Just span) "ExprWithTySigOut" $
    extractIds expr
  extractIds (L span (HsOverLit _ )) = ast (Just span) "HsOverLit" $
    return ()
  extractIds (L span (OpApp left op _fix right)) = ast (Just span) "OpApp" $
    extractIds [left, op, right]
  extractIds (L span (HsVar id)) = ast (Just span) "HsVar" $
    record span False id
  extractIds (L span (HsWrap _wrapper expr)) = ast (Just span) "HsWrap" $
    extractIds (L span expr)
  extractIds (L span (HsLet binds expr)) = ast (Just span) "HsLet" $ do
    extractIds binds
    extractIds expr
  extractIds (L span (HsApp fun arg)) = ast (Just span) "HsApp" $
    extractIds [fun, arg]
  extractIds (L _span (HsLit _)) =
    -- Intentional omission of the "ast" debugging call here.
    -- The syntax "assert" is replaced by GHC by "assertError <span>", where
    -- both "assertError" and the "<span>" are assigned the source span of
    -- the original "assert". This means that the <span> (represented as an
    -- HsLit) might override "assertError" in the IdMap.
    return ()
  extractIds (L span (HsLam matches)) = ast (Just span) "HsLam" $
    extractIds matches
  extractIds (L span (HsDo _ctxt stmts _postTcType)) = ast (Just span) "HsDo" $
    -- ctxt indicates what kind of statement it is; AFAICT there is no
    -- useful information in it for us
    -- postTcType is not located
    extractIds stmts
#if __GLASGOW_HASKELL__ >= 707
  -- Middle argument is something to do with OverloadedLists
  extractIds (L span (ExplicitList _postTcType _ exprs)) = ast (Just span) "ExplicitList" $
    extractIds exprs
#else
  extractIds (L span (ExplicitList _postTcType exprs)) = ast (Just span) "ExplicitList" $
    extractIds exprs
#endif
  extractIds (L span (RecordCon con _postTcType recordBinds)) = ast (Just span) "RecordCon" $ do
    record (getLoc con) False (unLoc con)
    extractIds recordBinds
  extractIds (L span (HsCase expr matches)) = ast (Just span) "HsCase" $ do
    extractIds expr
    extractIds matches
  extractIds (L span (ExplicitTuple args _boxity)) = ast (Just span) "ExplicitTuple" $
    extractIds args
  extractIds (L span (HsIf _ cond true false)) = ast (Just span) "HsIf" $
    -- First argument is something to do with rebindable syntax
    extractIds [cond, true, false]
  extractIds (L span (SectionL arg op)) = ast (Just span) "SectionL" $
    extractIds [arg, op]
  extractIds (L span (SectionR op arg)) = ast (Just span) "SectionR" $
    extractIds [op, arg]

  extractIds (L span (HsIPVar _ ))          = unsupported (Just span) "HsIPVar"
  extractIds (L span (NegApp _ _))          = unsupported (Just span) "NegApp"
  extractIds (L span (ExplicitPArr _ _))    = unsupported (Just span) "ExplicitPArr"
  extractIds (L span (RecordUpd _ _ _ _ _)) = unsupported (Just span) "RecordUpd"
  extractIds (L span (PArrSeq _ _))         = unsupported (Just span) "PArrSeq"
  extractIds (L span (HsSCC _ _))           = unsupported (Just span) "HsSCC"
  extractIds (L span (HsCoreAnn _ _))       = unsupported (Just span) "HsCoreAnn"
  extractIds (L span (HsBracket _))         = unsupported (Just span) "HsBracket"
  extractIds (L span (HsBracketOut _ _))    = unsupported (Just span) "HsBracketOut"
  extractIds (L span (HsSpliceE _))         = unsupported (Just span) "HsSpliceE"
  extractIds (L span (HsQuasiQuoteE _ ))    = unsupported (Just span) "HsQuasiQuoteE"
  extractIds (L span (HsProc _ _))          = unsupported (Just span) "HsProc"
  extractIds (L span (HsArrApp _ _ _ _ _))  = unsupported (Just span) "HsArrApp"
  extractIds (L span (HsArrForm _ _ _))     = unsupported (Just span) "HsArrForm"
  extractIds (L span (HsTick _ _))          = unsupported (Just span) "HsTick"
  extractIds (L span (HsBinTick _ _ _))     = unsupported (Just span) "HsBinTick"
  extractIds (L span (HsTickPragma _ _))    = unsupported (Just span) "HsTickPragma"
  extractIds (L span EWildPat)              = unsupported (Just span) "EWildPat"
  extractIds (L span (EAsPat _ _))          = unsupported (Just span) "EAsPat"
  extractIds (L span (EViewPat _ _))        = unsupported (Just span) "EViewPat"
  extractIds (L span (ELazyPat _))          = unsupported (Just span) "ELazyPat"
  extractIds (L span (HsType _ ))           = unsupported (Just span) "HsType"

#if __GLASGOW_HASKELL__ >= 707
  -- Second argument is something to do with OverloadedLists
  extractIds (L span (ArithSeq _ _ _))      = unsupported (Just span) "ArithSeq"
#else
  extractIds (L span (ArithSeq _ _ ))       = unsupported (Just span) "ArithSeq"
#endif

#if __GLASGOW_HASKELL__ >= 706
  extractIds (L span (HsLamCase _ _ ))      = unsupported (Just span) "HsLamCase"
  extractIds (L span (HsMultiIf _ _))       = unsupported (Just span) "HsMultiIf"
#endif

#if __GLASGOW_HASKELL__ >= 707
  extractIds (L span (HsUnboundVar _))      = unsupported (Just span) "HsUnboundVar"
#endif

instance Record id => ExtractIds (HsTupArg id) where
  extractIds (Present arg) =
    extractIds arg
  extractIds (Missing _postTcType) =
    return ()

instance (ExtractIds a, Record id) => ExtractIds (HsRecFields id a) where
  extractIds (HsRecFields rec_flds _rec_dotdot) = ast Nothing "HsRecFields" $
    extractIds rec_flds

instance (ExtractIds a, Record id) => ExtractIds (HsRecField id a) where
  extractIds (HsRecField id arg _pun) = ast Nothing "HsRecField" $ do
    record (getLoc id) False (unLoc id)
    extractIds arg

-- The meaning of the constructors of LStmt isn't so obvious; see various
-- notes in ghc/compiler/hsSyn/HsExpr.lhs
#if __GLASGOW_HASKELL__ >= 707
instance (ExtractIds body, Record id) => ExtractIds (LStmt id body) where
  extractIds (L span (BodyStmt body _seq _guard _postTcType)) = ast (Just span) "BodyStmt" $
    extractIds body
#else
instance Record id => ExtractIds (LStmt id) where
  extractIds (L span (ExprStmt expr _seq _guard _postTcType)) = ast (Just span) "ExprStmt" $
    -- Neither _seq nor _guard are located
    extractIds expr
#endif
  extractIds (L span (BindStmt pat expr _bind _fail)) = ast (Just span) "BindStmt" $ do
    -- Neither _bind or _fail are located
    extractIds pat
    extractIds expr
  extractIds (L span (LetStmt binds)) = ast (Just span) "LetStmt" $
    extractIds binds
  extractIds (L span (LastStmt expr _return)) = ast (Just span) "LastStmt" $
    extractIds expr

  extractIds (L span (TransStmt {}))     = unsupported (Just span) "TransStmt"
  extractIds (L span (RecStmt {}))       = unsupported (Just span) "RecStmt"

#if __GLASGOW_HASKELL__ >= 706
  extractIds (L span (ParStmt _ _ _))    = unsupported (Just span) "ParStmt"
#else
  extractIds (L span (ParStmt _ _ _ _))  = unsupported (Just span) "ParStmt"
#endif

instance Record id => ExtractIds (LPat id) where
  extractIds (L span (WildPat _postTcType)) = ast (Just span) "WildPat" $
    return ()
  extractIds (L span (VarPat id)) = ast (Just span) "VarPat" $
    record span True id
  extractIds (L span (LazyPat pat)) = ast (Just span) "LazyPat" $
    extractIds pat
  extractIds (L span (AsPat id pat)) = ast (Just span) "AsPat" $ do
    record (getLoc id) True (unLoc id)
    extractIds pat
  extractIds (L span (ParPat pat)) = ast (Just span) "ParPat" $
    extractIds pat
  extractIds (L span (BangPat pat)) = ast (Just span) "BangPat" $
    extractIds pat
#if __GLASGOW_HASKELL__ >= 707
  -- Third argument is something to do with rebindable syntax
  extractIds (L span (ListPat pats _postTcType _)) = ast (Just span) "ListPat" $
    extractIds pats
#else
  extractIds (L span (ListPat pats _postTcType)) = ast (Just span) "ListPat" $
    extractIds pats
#endif
  extractIds (L span (TuplePat pats _boxity _postTcType)) = ast (Just span) "TuplePat" $
    extractIds pats
  extractIds (L span (PArrPat pats _postTcType)) = ast (Just span) "PArrPat" $
    extractIds pats
  extractIds (L span (ConPatIn con details)) = ast (Just span) "ConPatIn" $ do
    -- Unlike ValBindsIn and HsValBindsIn, we *do* get ConPatIn
    record (getLoc con) False (unLoc con) -- the constructor name is non-binding
    extractIds details
  extractIds (L span (ConPatOut {pat_con, pat_args})) = ast (Just span) "ConPatOut" $ do
    record (getLoc pat_con) False (dataConName (unLoc pat_con))
    extractIds pat_args
  extractIds (L span (LitPat _)) = ast (Just span) "LitPat" $
    return ()
  extractIds (L span (NPat _ _ _)) = ast (Just span) "NPat" $
    return ()



  -- View patterns
  extractIds (L span (ViewPat _ _ _))     = unsupported (Just span) "ViewPat"
  extractIds (L span (QuasiQuotePat _))   = unsupported (Just span) "QuasiQuotePat"
  extractIds (L span (NPlusKPat _ _ _ _)) = unsupported (Just span) "NPlusKPat"
  extractIds (L span (SigPatIn _ _))      = unsupported (Just span) "SigPatIn"
  extractIds (L span (SigPatOut _ _))     = unsupported (Just span) "SigPatOut"
  extractIds (L span (CoPat _ _ _))       = unsupported (Just span) "CoPat"

instance (ExtractIds arg, ExtractIds rec) => ExtractIds (HsConDetails arg rec) where
  extractIds (PrefixCon args) = ast Nothing "PrefixCon" $
    extractIds args
  extractIds (RecCon rec) = ast Nothing "RecCon" $
    extractIds rec
  extractIds (InfixCon a b) = ast Nothing "InfixCon" $
    extractIds [a, b]

instance Record id => ExtractIds (LTyClDecl id) where
  extractIds (L span decl@(TyData {})) = ast (Just span) "TyData" $ do
    extractIds (tcdCtxt decl)
    record (getLoc (tcdLName decl)) True (unLoc (tcdLName decl))
    extractIds (tcdTyVars decl)
    extractIds (tcdTyPats decl)
    -- TODO: deal with tcdKindSig
    extractIds (tcdCons decl)
    extractIds (tcdDerivs decl)

  extractIds (L span _decl@(ForeignType {})) = unsupported (Just span) "ForeignType"
  extractIds (L span _decl@(ClassDecl {}))   = unsupported (Just span) "ClassDecl"
#if __GLASGOW_HASKELL__ >= 707
  extractIds (L span _decl@(FamDecl {}))     = unsupported (Just span) "TyFamily"
  extractIds (L span _decl@(SynDecl {}))     = unsupported (Just span) "TySynonym"
  extractIds (L span _decl@(DataDecl {}))    = unsupported (Just span) "DataDecl"
#else
  extractIds (L span _decl@(TyFamily {}))    = unsupported (Just span) "TyFamily"
  extractIds (L span _decl@(TySynonym {}))   = unsupported (Just span) "TySynonym"
#endif

instance Record id => ExtractIds (LConDecl id) where
  extractIds (L span decl@(ConDecl {})) = ast (Just span) "ConDecl" $ do
    record (getLoc (con_name decl)) True (unLoc (con_name decl))
    extractIds (con_qvars decl)
    extractIds (con_cxt decl)
    extractIds (con_details decl)
    -- TODO: deal with con_res

instance Record id => ExtractIds (ConDeclField id) where
  extractIds (ConDeclField name typ _doc) = do
    record (getLoc name) True (unLoc name)
    extractIds typ
