module IdeSession.Cabal (
    buildExecutable
  ) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (catMaybes)
import Data.Version (Version (..), parseVersion)
import qualified Data.Text as Text
import Text.ParserCombinators.ReadP (readP_to_S)
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)

import Distribution.License (License (..))
import qualified Distribution.ModuleName
import Distribution.PackageDescription
import qualified Distribution.Package as Package
import qualified Distribution.Simple.Build as Build
import Distribution.Simple.Compiler (CompilerFlavor (GHC))
import Distribution.Simple.Configure (configure)
import Distribution.Simple.LocalBuildInfo (localPkgDescr)
import Distribution.Simple.PreProcess (PPSuffixHandler)
import qualified Distribution.Simple.Setup as Setup
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Version (anyVersion, thisVersion)
import Language.Haskell.Extension (Language (Haskell2010))

import IdeSession.State
import IdeSession.Strict.Container
import IdeSession.Types.Public
import IdeSession.Types.Translation
import qualified IdeSession.Strict.List as StrictList
import qualified IdeSession.Strict.Map  as StrictMap

pkgName :: Package.PackageName
pkgName = Package.PackageName "main"  -- matches the import default

pkgVersion :: Version
pkgVersion = Version [1, 0] []

pkgIdent :: Package.PackageIdentifier
pkgIdent = Package.PackageIdentifier
  { pkgName    = pkgName
  , pkgVersion = pkgVersion
  }

pkgDesc :: PackageDescription
pkgDesc = PackageDescription
  { -- the following are required by all packages:
    package        = pkgIdent
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
  , specVersionRaw = Left $ Version [1, 14, 0] []
  , buildType      = Just Simple
    -- components
  , library        = Nothing  -- ignored inside @GenericPackageDescription@
  , executables    = []  -- ignored when inside @GenericPackageDescription@
  , testSuites     = []
  , benchmarks     = []
  , dataFiles      = []  -- for now, we don't mention files from managedData?
  , dataDir        = ""  -- for now we don't put ideDataDir?
  , extraSrcFiles  = []
  , extraTmpFiles  = []
  }

bInfo :: FilePath -> Maybe [String] -> BuildInfo
bInfo sourceDir ghcOpts =
  emptyBuildInfo
    { buildable = True
    , hsSourceDirs = [sourceDir]
    , defaultLanguage = Just Haskell2010
    , options = maybe [] (\opts -> [(GHC, opts)]) ghcOpts
    }

exeDesc :: FilePath -> FilePath -> Maybe [String] -> ModuleName
        -> IO Executable
exeDesc ideSourcesDir ideDistDir ghcOpts m = do
  let exeName = Text.unpack m
  if exeName == "Main" then  -- that's what Cabal expects, no wrapper needed
    return $ Executable
      { exeName
      , modulePath = "Main.hs"
      , buildInfo = bInfo ideSourcesDir ghcOpts
      }
  else do
    mDir <- createTempDirectory ideDistDir exeName
    -- Cabal insists on "Main" and on ".hs".
    let modulePath = mDir </> "Main.hs"
        wrapper = "import qualified " ++ exeName ++ "\n"
                  ++ "main = " ++ exeName ++ ".main"
    writeFile modulePath wrapper
    return $ Executable
      { exeName
      , modulePath
      , buildInfo = bInfo mDir ghcOpts
      }

libDesc :: FilePath -> Maybe [String] -> [Distribution.ModuleName.ModuleName]
        -> Library
libDesc ideSourcesDir ghcOpts ms =
  Library
    { exposedModules = ms
    , libExposed = False
    , libBuildInfo = bInfo ideSourcesDir ghcOpts
    }

-- TODO: we could do the parsing early and export parsed Versions via our API,
-- but we'd need to define our own strict internal variant of Version, etc.
parseVersionString :: Monad m => String -> m Version
parseVersionString versionString = do
  let parser = readP_to_S parseVersion
  case [ v | (v, "") <- parser versionString] of
    [v] -> return v
    _ -> fail $ "parseVersionString: can't parse package version: "
                ++ versionString

externalDeps :: Monad m => [PackageId] -> m [Package.Dependency]
externalDeps pkgs =
  let depOfName :: Monad m => PackageId -> m (Maybe Package.Dependency)
      depOfName PackageId{packageName, packageVersion = Nothing} = do
        let packageN = Package.PackageName $ Text.unpack $ packageName
        if packageN == pkgName then return Nothing
        else return $ Just $ Package.Dependency packageN anyVersion
      depOfName PackageId{packageName, packageVersion = Just versionText} = do
        let packageN = Package.PackageName $ Text.unpack $ packageName
            versionString = Text.unpack versionText
        version <- parseVersionString versionString
        return $ Just $ Package.Dependency packageN (thisVersion version)
  in liftM catMaybes $ mapM depOfName pkgs

configureAndBuild :: FilePath -> FilePath -> Maybe [String]
                  -> [PackageId] -> [ModuleName] -> [ModuleName]
                  -> IO ()
configureAndBuild ideSourcesDir ideDistDir ghcOpts pkgs loadedM ms = do
  libDeps <- externalDeps pkgs
  let mainDep = Package.Dependency pkgName (thisVersion pkgVersion)
      exeDeps = mainDep : libDeps
  executables <- mapM (exeDesc ideSourcesDir ideDistDir ghcOpts) ms
  let condExe exe = (exeName exe, CondNode exe exeDeps [])
      condExecutables = map condExe executables
  let allProjectModules =
        map (Distribution.ModuleName.fromString . Text.unpack) loadedM
      library = libDesc ideSourcesDir ghcOpts allProjectModules
      gpDesc = GenericPackageDescription
        { packageDescription = pkgDesc
        , genPackageFlags    = []  -- seem unused
        , condLibrary        = Just $ CondNode library libDeps []
        , condExecutables
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

buildExecutable :: FilePath -> FilePath -> Maybe [String]
                -> Strict Maybe Computed -> [ModuleName]
                -> IO ()
buildExecutable ideSourcesDir ideDistDir ghcOpts mcomputed ms = do
  case toLazyMaybe mcomputed of
    Nothing -> fail "This session state does not admit buidling executables."
    Just Computed{..} -> do
      let loadedM = StrictMap.keys $ computedLoadedModules
          imp m = do
            let mimports =
                  fmap (toLazyList . StrictList.map (removeExplicitSharing
                                                       computedCache)) $
                    StrictMap.lookup m computedImports
            case mimports of
              Nothing -> fail $ "Module '" ++ Text.unpack m ++ "' not loaded."
              Just imports ->
                return $ map (modulePackage . importModule) imports
      imps <- mapM imp loadedM
      let pkgs = concat imps
      liftIO $ configureAndBuild
                 ideSourcesDir ideDistDir ghcOpts pkgs loadedM ms
      -- TODO: keep a list of built (and up-to-date?) executables?
