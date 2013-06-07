module Haddock (
    -- Package dependencies cache
    pkgDepsFor
  , updatePkgDepsFor
    -- Interfacing with Haddock
  , haddockInterfaceFor
    -- Link environment
  , LinkEnv
  , homeModuleFor
  ) where

-- Platform imports
import System.IO.Unsafe (unsafePerformIO)
import Control.Exception (SomeException)
import qualified Control.Exception as Ex
import Data.Maybe (catMaybes)
import Control.Applicative ((<$>))
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
import qualified OccName  as GHC
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

pkgDepCache :: StrictIORef (Strict (Map ModuleName) [GHC.PackageId])
{-# NOINLINE pkgDepCache #-}
pkgDepCache = unsafePerformIO $ newIORef StrictMap.empty

updatePkgDepsFor :: DynFlags -> ModuleName -> GHC.ModSummary -> IO ()
updatePkgDepsFor dflags m s = do
    cache <- readIORef pkgDepCache
    writeIORef pkgDepCache (StrictMap.insert m deps cache)
  where
    aux :: Located (ImportDecl RdrName)
        -> GHC.ModuleName
    aux = unLoc . ideclName . unLoc

    impMods :: [GHC.ModuleName]
    impMods = map aux (GHC.ms_srcimps      s)
           ++ map aux (GHC.ms_textual_imps s)

    deps :: [GHC.PackageId]
    deps = catMaybes (map (moduleToPackageId dflags) impMods)

pkgDepsFor :: ModuleName -> IO [GHC.PackageId]
pkgDepsFor m = do
  cache <- readIORef pkgDepCache
  case StrictMap.lookup m cache of
    Nothing   -> Ex.throwIO (userError "pkgDepsFor: Unknown module")
    Just deps -> return deps

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

showLinkEnv :: LinkEnv -> String
showLinkEnv = unlines . map (showSDoc . ppr) . StrictMap.toList

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
          StrictMaybe.just (moduleToModuleId dflags m)

{------------------------------------------------------------------------------
  Link environment cache
------------------------------------------------------------------------------}

linkEnvCache :: StrictIORef (Strict (Map ModuleName) LinkEnv)
{-# NOINLINE linkEnvCache #-}
linkEnvCache = unsafePerformIO $ newIORef StrictMap.empty

linkEnvFor :: ModuleName -> IO LinkEnv
linkEnvFor m = do
  cache <- readIORef linkEnvCache
  case StrictMap.lookup m cache of
    Just linkEnv -> return linkEnv

