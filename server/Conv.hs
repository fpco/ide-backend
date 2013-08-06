-- | Conversions between GHC's types and our types
module Conv (
    moduleToPackageId
  , moduleNameToId
  , fillVersion
  , moduleToModuleId
  , PackageQualifier
  ) where

import Prelude hiding (mod)
import Control.Applicative ((<$>))
import qualified Data.Text as Text
import Data.List (stripPrefix, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)

import GHC (DynFlags(pkgState))
import qualified GHC
import qualified Packages
import qualified Module
import FastString (FastString, unpackFS)

import IdeSession.Types.Private
import qualified IdeSession.Strict.Maybe as Maybe

type PackageQualifier = Maybe FastString

-- | Returns Nothing for the main package, or Just a package ID otherwise.
-- Throws an exception if the module cannot be found.
moduleToPackageId :: DynFlags -> PackageQualifier -> GHC.ModuleName -> Maybe GHC.PackageId
moduleToPackageId dflags pkgQual impMod = case pkgMatching of
    []    -> Nothing
    p : _ -> Just p
  where
    pkgAll      = Packages.lookupModuleInAllPackages dflags impMod
    pkgExposed  = flip filter pkgAll $ \(p, b) ->
                       b                  -- Is the module  exposed?
                    && Packages.exposed p -- Is the package exposed?
    pkgIds      = map (Packages.packageConfigId . fst) pkgExposed
    pkgMatching = filter (matchesPkgQual pkgQual) pkgIds

    -- TODO: This is a bit of a hack. I'm not sure what matching ghc does
    -- exactly, but PackageId's are just strings, they don't have any structure
    matchesPkgQual Nothing   _     = True
    matchesPkgQual (Just fs) pkgId =
      unpackFS fs `isPrefixOf` Module.packageIdString pkgId

moduleNameToId :: DynFlags -> PackageQualifier -> GHC.ModuleName -> ModuleId
moduleNameToId dflags pkgQual impMod = ModuleId {
    moduleName    = Text.pack $ Module.moduleNameString impMod
  , modulePackage = case moduleToPackageId dflags pkgQual impMod of
                      Just pkg -> fillVersion dflags pkg
                      Nothing  -> mainPackage
  }

-- | Attempt to find out the version of a package
fillVersion :: DynFlags -> GHC.PackageId -> PackageId
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
