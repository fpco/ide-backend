-- | Conversions between GHC's types and our types
module Conv (
    moduleToPackageId
  , moduleNameToId
  , fillVersion
  , moduleToModuleId
  ) where

import Control.Applicative ((<$>))
import qualified Data.Text as Text
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Control.Arrow (first)
import Data.Version (showVersion)

import GHC (DynFlags(pkgState))
import qualified GHC
import qualified Packages
import qualified Module

import IdeSession.Types.Private
import IdeSession.Strict.Maybe as Maybe

-- | Returns Nothing for the main package, or Just a package ID otherwise.
-- Throws an exception if the module cannot be found.
moduleToPackageId :: DynFlags -> GHC.ModuleName -> Maybe GHC.PackageId
moduleToPackageId dflags impMod = case pkgExposed of
    []  -> Nothing
    [p] -> Just $ Packages.packageConfigId (fst p)
    _   -> let pkgIds = map (first (Module.packageIdString
                                     . Packages.packageConfigId)) pkgExposed
           in error $ "modToPkg: " ++ Module.moduleNameString impMod
                   ++ ": " ++ show pkgIds
  where
    pkgAll     = Packages.lookupModuleInAllPackages dflags impMod
    pkgExposed = filter (\ (p, b) -> b && Packages.exposed p) pkgAll

moduleNameToId :: DynFlags -> GHC.ModuleName -> ModuleId
moduleNameToId dflags impMod = ModuleId {
    moduleName    = Text.pack $ Module.moduleNameString $ impMod
  , modulePackage = case moduleToPackageId dflags impMod of
                      Just pkg -> fillVersion dflags pkg
                      Nothing  -> mainPackage
  }

-- | Attempt to find out the version of a package
fillVersion :: DynFlags -> Module.PackageId -> PackageId
fillVersion dflags p =
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
          packageVersion = Maybe.just $ case showVersion pkgVersion of
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
      in PackageId {
             packageName    = Text.pack packageName
           , packageVersion = Text.pack <$> packageVersion
           }

-- | PackageId of the main package
mainPackage :: PackageId
mainPackage = PackageId {
    packageName    = Text.pack $ Module.packageIdString Module.mainPackageId
  , packageVersion = Maybe.nothing -- the only case of no version
  }

moduleToModuleId :: DynFlags -> Module.Module -> ModuleId
moduleToModuleId dflags mod = ModuleId {
    moduleName    = Text.pack $ Module.moduleNameString $ Module.moduleName mod
  , modulePackage = fillVersion dflags $ Module.modulePackageId mod
  }

