module IdeSession.Cabal (
    buildExecutable
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Version (Version (..), parseVersion)
import qualified Data.Text as Text
import Text.ParserCombinators.ReadP (readP_to_S)

import Distribution.License (License (..))
import Distribution.PackageDescription
import qualified Distribution.Package as Package
import qualified Distribution.Simple.Build as Build
import Distribution.Simple.Configure (configure)
import Distribution.Simple.LocalBuildInfo (localPkgDescr)
import Distribution.Simple.PreProcess (PPSuffixHandler)
import qualified Distribution.Simple.Setup as Setup
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Version (anyVersion, thisVersion)

import IdeSession.State
import IdeSession.Strict.Container
import IdeSession.Types.Public
import IdeSession.Types.Translation
import qualified IdeSession.Strict.List   as StrictList
import qualified IdeSession.Strict.Map    as StrictMap

pIdent :: ModuleName -> Package.PackageIdentifier
pIdent m = Package.PackageIdentifier
  { pkgName    = Package.PackageName $ "executable_" ++ Text.unpack m
  , pkgVersion = Version [1, 0] []
  }

packageDesc :: ModuleName -> PackageDescription
packageDesc m = PackageDescription
  { -- the following are required by all packages:
    package        = pIdent m -- for errors reported by GHC
  , license        = AllRightsReserved  -- dummy
  , licenseFile    = ""
  , copyright      = ""
  , maintainer     = ""
  , author         = ""
  , stability      = ""
  , testedWith     = []
  , homepage       = ""
  , pkgUrl         = ""
  , bugReports     = ""
  , sourceRepos    = []
  , synopsis       = ""
  , description    = ""
  , category       = ""
  , customFieldsPD = []
  , buildDepends   = []  -- probably ignored
  , specVersionRaw = Left $ Version [0, 14, 0] []
  , buildType      = Just Simple
    -- components
  , library        = Nothing
  , executables    = []  -- ignored when inside @GenericPackageDescription@
  , testSuites     = []
  , benchmarks     = []
  , dataFiles      = []  -- TODO: should we mention all of managedData?
  , dataDir        = ""  -- TODO: should we put ideDataDir?
  , extraSrcFiles  = []
  , extraTmpFiles  = []
  }

exeDesc :: FilePath -> ModuleName -> FilePath -> Executable
exeDesc ideSourcesDir m modulePath =
  let buildInfo = emptyBuildInfo
        { buildable = True
        , hsSourceDirs = [ideSourcesDir]
        }
  in Executable
       { exeName = Text.unpack m
       , modulePath
       , buildInfo
       }

-- TODO: we could do the parsing early and export parsed Versions via our API,
-- but we'd need to define our own strict internal variant of Version, etc.
parseVersionString :: Monad m => String -> m Version
parseVersionString versionString = do
  let parser = readP_to_S parseVersion
  case [ v | (v, "") <- parser versionString] of
    [v] -> return v
    _ -> fail $ "buildDeps: can't parse package version: "
                ++ versionString

buildDeps :: Monad m => [PackageId] -> m [Package.Dependency]
buildDeps pkgs =
  let depOfName PackageId{packageName, packageVersion = Nothing} =
        return $
          Package.Dependency (Package.PackageName $ Text.unpack $ packageName)
                             anyVersion
      depOfName PackageId{packageName, packageVersion = Just versionText} = do
        let versionString = Text.unpack versionText
        version <- parseVersionString versionString
        return $
          Package.Dependency (Package.PackageName $ Text.unpack $ packageName)
                             (thisVersion version)
  in mapM depOfName pkgs

configureAndBuild :: FilePath -> FilePath -> [PackageId] -> ModuleName -> IO ()
configureAndBuild ideSourcesDir ideDistDir pkgs m = do
  let modulePath =
        map (\c -> if c == '.' then '/' else c) (Text.unpack m)
        ++ ".hs"  -- Cabal requires ".hs" even for preprocessed files
      pDesc = packageDesc m
      executable = exeDesc ideSourcesDir m modulePath
  deps <- buildDeps pkgs
  let gpDesc = GenericPackageDescription
        { packageDescription = pDesc
        , genPackageFlags    = []  -- seem unused
        , condLibrary        = Nothing
        , condExecutables    = [( exeName executable
                                , CondNode executable deps [] )]
        , condTestSuites     = []
        , condBenchmarks     = []
        }
      confFlags = (Setup.defaultConfigFlags defaultProgramConfiguration)
                     { Setup.configDistPref = Setup.Flag ideDistDir
                     , Setup.configUserInstall = Setup.Flag True
--                     , Setup.configVerbosity = Setup.Flag maxBound
                     }
      -- We don't override most build flags, but use configured values.
      buildFlags = Setup.defaultBuildFlags
                     { Setup.buildDistPref = Setup.Flag ideDistDir
--                     , Setup.buildVerbosity = Setup.Flag maxBound
                     }
      preprocessors :: [PPSuffixHandler]
      preprocessors = []
      hookedBuildInfo = (Nothing, [])  -- we don't want to use hooks
--   putStrLn $ "pDesc: " ++ show pDesc
  lbi <- configure (gpDesc, hookedBuildInfo) confFlags
--   putStrLn $ "lbi: " ++ show lbi
  Build.build (localPkgDescr lbi) lbi buildFlags preprocessors

buildExecutable :: FilePath -> FilePath -> Strict Maybe Computed -> ModuleName
                -> IO ()
buildExecutable ideSourcesDir ideDistDir mcomputed m = do
  case toLazyMaybe mcomputed of
    Nothing -> fail "This session state does not admit buidling executables."
    Just Computed{..} -> do
      let mimports = fmap (toLazyList . StrictList.map (removeExplicitSharing
                                                          computedCache)) $
                       StrictMap.lookup m computedImports
      case mimports of
        Nothing -> fail $ "Module '" ++ Text.unpack m ++ "' not found."
        Just imports -> do
          let deps = map (modulePackage . importModule) imports
          liftIO $ configureAndBuild ideSourcesDir ideDistDir deps m
          -- TODO: keep a list of built (and up-to-date?) executables?
