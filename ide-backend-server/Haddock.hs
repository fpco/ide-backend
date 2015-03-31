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
import GHC (DynFlags, unLoc, RdrName, ImportDecl(..), Located)
import MonadUtils (MonadIO(..)) -- ghc's MonadIO
import Outputable (defaultUserStyle)
import qualified GHC
import qualified Name     as GHC
import qualified Packages as GHC

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
import GhcShim

{------------------------------------------------------------------------------
  Package dependencies
------------------------------------------------------------------------------}

pkgDepsFromModSummary :: DynFlags
                      -> GHC.ModSummary
                      -> [PackageKey]
pkgDepsFromModSummary dflags s =
    catMaybes (map (uncurry (findExposedModule dflags)) impMods)
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
                         -> PackageKey
                         -> Either String FilePath
haddockInterfaceFilePath dflags pkg =
  let cfg = lookupPackage dflags pkg in
  case () of
    () | null (GHC.haddockInterfaces cfg) -> do
      Left $ "No haddock interfaces found for package "
          ++ pretty dflags defaultUserStyle pkg
    () | length (GHC.haddockInterfaces cfg) > 1 -> do
      Left $ "Too many haddock interfaces found for package "
          ++ pretty dflags defaultUserStyle pkg
    _otherwise ->
      Right . head . GHC.haddockInterfaces $ cfg

haddockInterfaceFor :: DynFlags
                    -> Hk.NameCacheAccessor IO
                    -> PackageKey
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

linkEnvPerPackageCache :: StrictIORef (Strict (Map PackageKey) (Either String LinkEnv))
{-# NOINLINE linkEnvPerPackageCache #-}
linkEnvPerPackageCache = unsafePerformIO $ newIORef StrictMap.empty

linkEnvForPackage :: DynFlags -> PackageKey -> IO (Either String LinkEnv)
linkEnvForPackage dynFlags pkgId = do
  cache <- readIORef linkEnvPerPackageCache
  case StrictMap.lookup pkgId cache of
    Just linkEnv -> return linkEnv
    Nothing -> do
      linkEnv <- haddockInterfaceFor dynFlags Hk.freshNameCache pkgId
      let cache' = StrictMap.insert pkgId linkEnv cache
      writeIORef linkEnvPerPackageCache cache'
      return linkEnv

linkEnvPerDepsCache :: StrictIORef (Strict (Map [PackageKey]) LinkEnv)
{-# NOINLINE linkEnvPerDepsCache #-}
linkEnvPerDepsCache = unsafePerformIO $ newIORef StrictMap.empty

linkEnvForDeps :: DynFlags -> [PackageKey] -> IO LinkEnv
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
