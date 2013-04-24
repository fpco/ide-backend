module IdeSession.Cabal (
    buildExecutable
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Version (Version (..))
import qualified Data.Text as Text
import System.FilePath (takeBaseName)

import Distribution.License (License (..))
import Distribution.PackageDescription
import qualified Distribution.Package as Package
import qualified Distribution.Simple.Build as Build
import Distribution.Simple.Configure (configure)
import Distribution.Simple.LocalBuildInfo (localPkgDescr)
import qualified Distribution.Simple.Setup as Setup
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Version (anyVersion)

import IdeSession.State
import IdeSession.Strict.Container
import IdeSession.Types.Public
import IdeSession.Types.Translation
import qualified IdeSession.Strict.List   as StrictList
import qualified IdeSession.Strict.Map    as StrictMap

pIdent :: Package.PackageIdentifier
pIdent = Package.PackageIdentifier
  { pkgName    = Package.PackageName "main_package"
  , pkgVersion = Version [1, 0] []
  }

buildDeps :: [PackageId] -> [Package.Dependency]
buildDeps pkgs =
  let depOfName PackageId{packageName, packageVersion = Nothing} =
        Package.Dependency (Package.PackageName $ Text.unpack $ packageName)
                          anyVersion
      depOfName PackageId{packageName, packageVersion = Just ver} =
        Package.Dependency (Package.PackageName $ Text.unpack $ packageName)
                          anyVersion  -- TODO
  in map depOfName pkgs

packageDesc :: PackageDescription
packageDesc = PackageDescription
  { -- the following are required by all packages:
    package        = pIdent
  , license        = OtherLicense
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
  , buildDepends   = []
  , specVersionRaw = Left $ Version [0, 14, 0] []
  , buildType      = Just Simple
    -- components
  , library        = Nothing
  , executables    = []  -- ignored when inside @GenericPackageDescription@
  , testSuites     = []
  , benchmarks     = []
  , dataFiles      = []
  , dataDir        = ""
  , extraSrcFiles  = []
  , extraTmpFiles  = []
  }

exeDesc :: FilePath -> FilePath -> Executable
exeDesc ideSourcesDir modulePath =
  let buildInfo = emptyBuildInfo
        { buildable = True
        , hsSourceDirs = [ideSourcesDir]
        }
  in Executable
       { exeName = takeBaseName modulePath
       , modulePath
       , buildInfo
       }

configureAndBuild :: FilePath -> FilePath -> [PackageId] -> FilePath -> IO ()
configureAndBuild ideSourcesDir ideDistDir deps modulePath = do
  let pDesc = packageDesc
      executable = exeDesc ideSourcesDir modulePath
      gpDesc = GenericPackageDescription
        { packageDescription = pDesc
        , genPackageFlags    = []
        , condLibrary        = Nothing
        , condExecutables    = [( exeName executable
                                , CondNode executable (buildDeps deps) [] )]
        , condTestSuites     = []
        , condBenchmarks     = []
        }
      confFlags = (Setup.defaultConfigFlags defaultProgramConfiguration)
                     { Setup.configDistPref = Setup.Flag ideDistDir
                     , Setup.configUserInstall = Setup.Flag True
--                     , Setup.configVerbosity = Setup.Flag maxBound
                     }
      buildFlags = Setup.defaultBuildFlags
--                     { Setup.buildVerbosity = Setup.Flag maxBound }
  -- putStrLn $ "pDesc: " ++ show pDesc
  lbi <- configure (gpDesc, (Nothing, [])) confFlags
  -- putStrLn $ "lbi: " ++ show lbi
  Build.build (localPkgDescr lbi) lbi buildFlags []

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
          let modulePath =
                map (\c -> if c == '.' then '/' else c) (Text.unpack m)
                ++ ".hs"  -- not ".lhs" at this time
          liftIO $ configureAndBuild ideSourcesDir ideDistDir deps modulePath
          -- TODO: keep a list of built (and up-to-date?) executables?
