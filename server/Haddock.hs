module Haddock (
    -- Package dependencies
    pkgDepsFromModSummary
    -- Interfacing with Haddock
  , haddockInterfaceFor
    -- Link environment
  , LinkEnv
  , homeModuleFor
    -- Link environment cache
  , linkEnvForDeps
  ) where

-- Platform imports
import Prelude hiding (mod)
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (SomeException)
import Control.Applicative ((<$>))
import qualified Control.Exception as Ex
import Data.Maybe (catMaybes)
import Data.Either (rights)
import qualified Data.Map as LazyMap

-- GHC imports; unqualified where no confusion can arise
import GHC (
    DynFlags(pkgState)
  , unLoc
  , RdrName
  , ImportDecl(..)
  , Located
  )
import Outputable (ppr, showSDoc)
import qualified GHC
import qualified Packages as GHC
import qualified Name     as GHC
import MonadUtils (MonadIO(..)) -- ghc's MonadIO

-- Haddock imports
import qualified Documentation.Haddock as Hk

-- ide-backend imports
import IdeSession.Types.Private
import IdeSession.Strict.Container
import IdeSession.Strict.IORef
import qualified IdeSession.Strict.Map   as StrictMap
import qualified IdeSession.Strict.Maybe as StrictMaybe

-- Our imports
import Conv

{------------------------------------------------------------------------------
  Package dependencies
------------------------------------------------------------------------------}

pkgDepsFromModSummary :: DynFlags
                      -> GHC.ModSummary
                      -> [GHC.PackageId]
pkgDepsFromModSummary dflags s =
    catMaybes (map (uncurry (guessModulePackage dflags)) impMods)
  where
    aux :: Located (ImportDecl RdrName)
        -> (PackageQualifier, GHC.ModuleName)
    aux lIdecl = let idecl = unLoc lIdecl
                 in (ideclPkgQual idecl, unLoc (ideclName idecl))

    impMods :: [(PackageQualifier, GHC.ModuleName)]
    impMods = map aux (GHC.ms_srcimps      s)
           ++ map aux (GHC.ms_textual_imps s)

{------------------------------------------------------------------------------
  Interfacing with Haddock
------------------------------------------------------------------------------}

haddockInterfaceFilePath :: DynFlags
                         -> GHC.PackageId
                         -> Either String FilePath
haddockInterfaceFilePath dflags pkg = do
  let pkgIdMap = GHC.pkgIdMap (pkgState dflags)
  case GHC.lookupPackage pkgIdMap pkg of
    Nothing ->
      Left $ "Package configuration for "
          ++ showSDoc (ppr pkg)
          ++ " not found"
    Just cfg | null (GHC.haddockInterfaces cfg) -> do
      Left $ "No haddock interfaces found for package "
          ++ showSDoc (ppr pkg)
    Just cfg | length (GHC.haddockInterfaces cfg) > 1 -> do
      Left $ "Too many haddock interfaces found for package "
          ++ showSDoc (ppr pkg)
    Just cfg ->
      Right . head . GHC.haddockInterfaces $ cfg

haddockInterfaceFor :: DynFlags
                    -> Hk.NameCacheAccessor IO
                    -> GHC.PackageId
                    -> IO (Either String LinkEnv)
haddockInterfaceFor dflags cache pkg = do
    case haddockInterfaceFilePath dflags pkg of
      Left err   -> return (Left err)
      Right path -> do
        iface <- try' $ Hk.readInterfaceFile cache path
        return (mkLinkEnv . Hk.ifLinkEnv <$> iface)
  where
    try' :: MonadIO m => IO (Either String a) -> m (Either String a)
    try' act = do
      mr <- liftIO $ Ex.try act
      case mr of
        Left e -> do
          return . Left $ show (e :: SomeException)
        Right (Left e) -> do
          return . Left $ e
        Right (Right r) -> do
          return . Right $ r

{------------------------------------------------------------------------------
  Haddock's LinkEnv maps Names to Modules, but unfortunately those Names
  have different Uniques than the Uniques that ghc uses and hence we cannot
  use them for lookup. We create our own environment instead.
------------------------------------------------------------------------------}

type LinkEnv = Strict (Map (GHC.Module, GHC.OccName)) GHC.Module

_showLinkEnv :: LinkEnv -> String
_showLinkEnv = unlines . map (showSDoc . ppr) . StrictMap.toList

mkLinkEnv :: Map GHC.Name GHC.Module -> LinkEnv
mkLinkEnv m = foldr (.) (\x -> x) (map aux (LazyMap.toList m)) StrictMap.empty
  where
    aux :: (GHC.Name, GHC.Module) -> LinkEnv -> LinkEnv
    aux (n, home) = case GHC.nameModule_maybe n of
      Just mod -> StrictMap.insert (mod, GHC.nameOccName n) home
      Nothing  -> \x -> x

homeModuleFor :: DynFlags -> LinkEnv -> GHC.Name -> Strict Maybe ModuleId
homeModuleFor dflags linkEnv name =
  case GHC.nameModule_maybe name of
    Nothing -> do
      StrictMaybe.nothing
    Just mod -> do
      case StrictMap.lookup (mod, GHC.nameOccName name) linkEnv of
        Nothing -> do
          StrictMaybe.nothing
        Just m ->
          StrictMaybe.just (importModuleId dflags m)

{------------------------------------------------------------------------------
  Link environment cache

  We keep separate caches for the link env per package, and the link env per
  set of package dependencies. This is important because the order of the
  dependencies matters; for instance, there is an entry for 'True' in both
  ghc-prim and base, but we want the entry in base so that we report Data.Bool
  as the home module rather than GHC.Types.

  TODO: However, we might be able to be a bit smarter about memory usage here?
------------------------------------------------------------------------------}

linkEnvPerPackageCache :: StrictIORef (Strict (Map GHC.PackageId) (Either String LinkEnv))
{-# NOINLINE linkEnvPerPackageCache #-}
linkEnvPerPackageCache = unsafePerformIO $ newIORef StrictMap.empty

linkEnvForPackage :: DynFlags -> GHC.PackageId -> IO (Either String LinkEnv)
linkEnvForPackage dynFlags pkgId = do
  cache <- readIORef linkEnvPerPackageCache
  case StrictMap.lookup pkgId cache of
    Just linkEnv -> return linkEnv
    Nothing -> do
      linkEnv <- haddockInterfaceFor dynFlags Hk.freshNameCache pkgId
      let cache' = StrictMap.insert pkgId linkEnv cache
      writeIORef linkEnvPerPackageCache cache'
      return linkEnv

linkEnvPerDepsCache :: StrictIORef (Strict (Map [GHC.PackageId]) LinkEnv)
{-# NOINLINE linkEnvPerDepsCache #-}
linkEnvPerDepsCache = unsafePerformIO $ newIORef StrictMap.empty

linkEnvForDeps :: DynFlags -> [GHC.PackageId] -> IO LinkEnv
linkEnvForDeps dynFlags deps = do
  cache <- readIORef linkEnvPerDepsCache
  case StrictMap.lookup deps cache of
    Just linkEnv -> return linkEnv
    Nothing -> do
      linkEnvs <- mapM (linkEnvForPackage dynFlags) deps
      -- See comment above about ordering of dependencies for a justification
      -- of 'unions' (which is biased)
      let linkEnv = StrictMap.unions (rights linkEnvs)
          cache'  = StrictMap.insert deps linkEnv cache
      writeIORef linkEnvPerDepsCache cache'
      return linkEnv
