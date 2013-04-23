module IdeSession.Cabal (
    buildExecutable
  ) where

import Data.Version (Version (..))
import Distribution.License (License (..))
import Distribution.PackageDescription
import Distribution.Package
import qualified Distribution.Simple.Build as Build
import Distribution.Simple.Configure (configure)
import Distribution.Simple.LocalBuildInfo (localPkgDescr)
import qualified Distribution.Simple.Setup as Setup
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Version (anyVersion)
import System.FilePath (takeBaseName)

pIdent :: PackageIdentifier
pIdent = PackageIdentifier
  { pkgName    = PackageName "main_package"
  , pkgVersion = Version [1, 0] []
  }

buildDeps :: [String] -> [Dependency]
buildDeps deps =
  let depOfName name = Dependency (PackageName name) anyVersion
  in map depOfName deps

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

buildExecutable :: FilePath -> FilePath -> [String] -> FilePath -> IO ()
buildExecutable ideSourcesDir ideDistDir deps modulePath = do
  let pDesc = packageDesc
      executable = exeDesc ideSourcesDir modulePath
      gpDesc = GenericPackageDescription
        { packageDescription = pDesc
        , genPackageFlags    = []
        , condLibrary        = Nothing
        , condExecutables    = [( exeName executable
                                , CondNode executable (buildDeps deps) [])]
        , condTestSuites     = []
        , condBenchmarks     = []
        }
      confFlags = (Setup.defaultConfigFlags defaultProgramConfiguration)
                    { Setup.configDistPref = Setup.Flag ideDistDir
                    , Setup.configUserInstall = Setup.Flag True
--                    , Setup.configVerbosity = Setup.Flag maxBound
                    }
  -- putStrLn $ "pDesc: " ++ show pDesc
  lbi <- configure (gpDesc, (Nothing, [])) confFlags
  -- putStrLn $ "lbi: " ++ show lbi
  Build.build (localPkgDescr lbi) lbi Setup.defaultBuildFlags []
