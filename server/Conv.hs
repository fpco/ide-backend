-- | Conversions between GHC's types and our types
module Conv (
    -- * Types
    PackageQualifier
    -- * Conversions from ghc's types to our types
  , importPackageId
  , importMainPackage
  , importModuleId
  , importModuleId'
  , guessModulePackage
    -- * Conversions from our types to ghc's types
  , exportPackageId
  , exportModuleId
  ) where

import Prelude hiding (mod)
import Control.Applicative ((<$>))
import qualified Data.Text as Text
import Data.List (stripPrefix, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)

import GHC (DynFlags(pkgState))
import qualified GHC      as GHC
import qualified Packages as GHC
import qualified Module   as GHC
import FastString (FastString, unpackFS)

import qualified IdeSession.Types.Private as Private
import qualified IdeSession.Types.Public  as Public
import qualified IdeSession.Strict.Maybe  as Maybe

type PackageQualifier = Maybe FastString

{------------------------------------------------------------------------------
  Conversions from ghc's types to our types
------------------------------------------------------------------------------}

-- | PackageId of the main package
importMainPackage :: Private.PackageId
importMainPackage = Private.PackageId {
    Private.packageName    = Text.pack $ GHC.packageIdString GHC.mainPackageId
  , Private.packageVersion = Maybe.nothing -- the only case of no version
  }

-- | Attempt to find out the version of a package
importPackageId :: DynFlags -> GHC.PackageId -> Private.PackageId
importPackageId dflags p =
  case GHC.lookupPackage (GHC.pkgIdMap (pkgState dflags)) p of
    Nothing -> if p == GHC.mainPackageId
               then importMainPackage
               else error $ "importPackageId:" ++ GHC.packageIdString p
    Just pkgCfg ->
      let sourcePkgId = GHC.sourcePackageId pkgCfg
          pkgName     = GHC.pkgName sourcePkgId
          prefixPN    = "PackageName "
          showPN      = show pkgName
          -- A hack to avoid importing Distribution.Package.
          errPN       = fromMaybe (error $ "stripPrefixPN "
                                        ++ prefixPN ++ " "
                                        ++ showPN)
                          $ stripPrefix prefixPN showPN
          packageName = init $ tail errPN
          pkgVersion  = GHC.pkgVersion sourcePkgId
          packageVersion = Maybe.just $ case showVersion pkgVersion of
            -- See http://www.haskell.org/ghc/docs/7.4.2//html/libraries/ghc/Module.html#g:3.
            -- The version of wired-in packages is completely wiped out,
            -- but we use a leak in the form of a Cabal package id
            -- for the same package, which still contains a version.
            "" -> let installedPkgId = GHC.installedPackageId pkgCfg
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
      in Private.PackageId {
             Private.packageName    = Text.pack packageName
           , Private.packageVersion = Text.pack <$> packageVersion
           }

importModuleId :: DynFlags -> GHC.Module -> Private.ModuleId
importModuleId dflags mod = Private.ModuleId {
    Private.moduleName    = Text.pack $ GHC.moduleNameString $ GHC.moduleName mod
  , Private.modulePackage = importPackageId dflags $ GHC.modulePackageId mod
  }

importModuleId' :: DynFlags -> PackageQualifier -> GHC.ModuleName -> Private.ModuleId
importModuleId' dflags pkgQual impMod = Private.ModuleId {
    Private.moduleName    = Text.pack $ GHC.moduleNameString impMod
  , Private.modulePackage = case guessModulePackage dflags pkgQual impMod of
                      Just pkg -> importPackageId dflags pkg
                      Nothing  -> importMainPackage
  }

-- | Returns Nothing for the main package, or Just a package ID otherwise.
-- Throws an exception if the module cannot be found.
guessModulePackage :: DynFlags -> PackageQualifier -> GHC.ModuleName -> Maybe GHC.PackageId
guessModulePackage dflags pkgQual impMod = case pkgMatching of
    []    -> Nothing
    p : _ -> Just p
  where
    pkgAll      = GHC.lookupModuleInAllPackages dflags impMod
    pkgExposed  = flip filter pkgAll $ \(p, b) ->
                       b             -- Is the module  exposed?
                    && GHC.exposed p -- Is the package exposed?
    pkgIds      = map (GHC.packageConfigId . fst) pkgExposed
    pkgMatching = filter (matchesPkgQual pkgQual) pkgIds

    -- TODO: This is a bit of a hack. I'm not sure what matching ghc does
    -- exactly, but PackageId's are just strings, they don't have any structure
    matchesPkgQual Nothing   _     = True
    matchesPkgQual (Just fs) pkgId =
      unpackFS fs `isPrefixOf` GHC.packageIdString pkgId

{------------------------------------------------------------------------------
  Conversions from our types to ghc's types
------------------------------------------------------------------------------}

exportPackageId :: Public.PackageId -> GHC.PackageId
exportPackageId (Public.PackageId{..}) = GHC.stringToPackageId $
     Text.unpack packageName
  ++ case packageVersion of
       Just version -> "-" ++ Text.unpack version
       Nothing      -> ""

exportModuleId :: Public.ModuleId -> GHC.Module
exportModuleId (Public.ModuleId {..}) =
  GHC.mkModule (exportPackageId modulePackage)
               (GHC.mkModuleName . Text.unpack $ moduleName)
