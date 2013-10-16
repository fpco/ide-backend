module Haddock (
    -- Package dependencies
    pkgDepsFromModSummary
    -- Interfacing with Haddock
  , haddockInterfaceFor
    -- Link environment
  , LinkEnv
  , homeModuleFor
    -- Link environment cache
  , linkEnvFor
  , clearLinkEnvCache
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
    catMaybes (map (uncurry (moduleToPackageId dflags)) impMods)
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
          StrictMaybe.just (moduleToModuleId dflags m)

{------------------------------------------------------------------------------
  Link environment cache
------------------------------------------------------------------------------}

linkEnvCache :: StrictIORef (Strict (Map [GHC.PackageId]) LinkEnv)
{-# NOINLINE linkEnvCache #-}
linkEnvCache = unsafePerformIO $ newIORef StrictMap.empty

clearLinkEnvCache :: IO ()
clearLinkEnvCache = writeIORef linkEnvCache StrictMap.empty

-- | Find the cached link environment, or construct a new one, for the
-- given set of dependencies.
--
-- Note that the order of the dependencies is important!  For instance, there
-- is an entry for 'True' in both ghc-prim and base, but we want the entry in
-- base so that we report Data.Bool as the home module rather than GHC.Types.
linkEnvFor :: DynFlags -> [GHC.PackageId] -> IO LinkEnv
linkEnvFor dynFlags deps = do
  cache <- readIORef linkEnvCache
  case StrictMap.lookup deps cache of
    Just linkEnv -> return linkEnv
    Nothing -> do
      linkEnvs <- liftIO $ mapM (haddockInterfaceFor dynFlags Hk.freshNameCache)
                                deps

      let linkEnv = StrictMap.unions (rights linkEnvs)

      writeIORef linkEnvCache (StrictMap.insert deps linkEnv cache)
      return linkEnv
