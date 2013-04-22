module IdeSession.Cabal (
    buildExecutable
  ) where

import Data.Version (Version (..))
import Distribution.License (License (..))
import Distribution.PackageDescription
import Distribution.Package
import qualified Distribution.Simple.Build as Build
import Distribution.Simple.Configure (configure)
import qualified Distribution.Simple.Setup as Setup
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Version (anyVersion)
import System.FilePath ((</>), takeFileName)

pIdent :: PackageIdentifier
pIdent = PackageIdentifier
  { pkgName    = PackageName "main_package"
  , pkgVersion = Version [1, 0] []
  }

buildDeps :: [String] -> [Dependency]
buildDeps deps =
  let depOfName name = Dependency (PackageName name) anyVersion
  in map depOfName deps

packageDesc :: [String] -> Executable -> PackageDescription
packageDesc deps executable = PackageDescription
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
  , buildDepends   = buildDeps deps
  , specVersionRaw = Left $ Version [0, 14, 0] []
  , buildType      = Just Simple
    -- components
  , library        = Nothing
  , executables    = [executable]
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
       { exeName = takeFileName modulePath
       , modulePath
       , buildInfo
       }

buildExecutable :: FilePath -> FilePath -> FilePath -> IO ()
buildExecutable ideSourcesDir ideDistDir m = do
  let deps = []  -- TODO
      modulePath = ideSourcesDir </> m
      executable = exeDesc ideSourcesDir modulePath
      pDesc = packageDesc deps executable
      gpDesc = GenericPackageDescription
        { packageDescription = pDesc
        , genPackageFlags    = []
        , condLibrary        = Nothing
        , condExecutables    = []
        , condTestSuites     = []
        , condBenchmarks     = []
        }
      confFlags = (Setup.defaultConfigFlags defaultProgramConfiguration)
                    {Setup.configDistPref = Setup.Flag ideDistDir}
  lbi <- configure (gpDesc, (Nothing, [])) confFlags
  Build.build pDesc lbi Setup.defaultBuildFlags []
