-- | Conversions between GHC's types and our types
module Conv (
    -- * Conversions from ghc's types to our types
    importPackageId
  , importMainPackage
  , importModuleId
  , importModuleId'
  , findExposedModule
    -- * Conversions from our types to ghc's types
  , exportPackageId
  , exportModuleId
  ) where

import Prelude hiding (mod)
import qualified Data.Text as Text

import GHC (DynFlags)
import qualified Module as GHC

import GhcShim
import qualified IdeSession.Types.Private as Private
import qualified IdeSession.Types.Public  as Public
import qualified IdeSession.Strict.Maybe  as Maybe

{------------------------------------------------------------------------------
  Conversions from ghc's types to our types
------------------------------------------------------------------------------}

-- | PackageId of the main package
importMainPackage :: Private.PackageId
importMainPackage = Private.PackageId {
    Private.packageName    = Text.pack "main"
  , Private.packageVersion = Maybe.nothing -- the only case of no version
  , Private.packageKey     = Text.pack $ packageKeyString mainPackageKey
  }

-- | Attempt to find out the version of a package
importPackageId :: DynFlags -> PackageKey -> Private.PackageId
importPackageId dflags p =
  case packageKeyToSourceId dflags p of
    Nothing ->
      if p == mainPackageKey
        then importMainPackage
        else error $ "importPackageId:" ++ packageKeyString p
    Just (pkgName, pkgVersion) ->
      Private.PackageId {
          Private.packageName    = Text.pack pkgName
        , Private.packageVersion = Maybe.just $ Text.pack pkgVersion
        , Private.packageKey     = Text.pack $ packageKeyString p
        }

importModuleId :: DynFlags -> GHC.Module -> Private.ModuleId
importModuleId dflags mod = Private.ModuleId {
    Private.moduleName    = Text.pack $ GHC.moduleNameString $ GHC.moduleName mod
  , Private.modulePackage = importPackageId dflags $ modulePackageKey mod
  }

importModuleId' :: DynFlags -> PackageQualifier -> GHC.ModuleName -> Private.ModuleId
importModuleId' dflags pkgQual impMod = Private.ModuleId {
    Private.moduleName    = Text.pack $ GHC.moduleNameString impMod
  , Private.modulePackage = case findExposedModule dflags pkgQual impMod of
                              Just pkg -> importPackageId dflags pkg
                              Nothing  -> importMainPackage
  }

{------------------------------------------------------------------------------
  Conversions from our types to ghc's types
------------------------------------------------------------------------------}

exportPackageId :: Public.PackageId -> PackageKey
exportPackageId = stringToPackageKey . Text.unpack . Public.packageKey

exportModuleId :: Public.ModuleId -> GHC.Module
exportModuleId (Public.ModuleId {..}) =
  GHC.mkModule (exportPackageId modulePackage)
               (GHC.mkModuleName . Text.unpack $ moduleName)
