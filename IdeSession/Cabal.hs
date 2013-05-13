module IdeSession.Cabal (
    buildExecutable
  ) where

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.List (delete)
import Data.Maybe (catMaybes)
import Data.Version (Version (..), parseVersion)
import qualified Data.Text as Text
import Text.ParserCombinators.ReadP (readP_to_S)
import System.FilePath ((</>))
import Data.IORef (newIORef, readIORef, modifyIORef)
import System.Directory (doesFileExist)
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
import IdeSession.Types.Progress
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

bInfo :: FilePath -> [String] -> BuildInfo
bInfo sourceDir ghcOpts =
  emptyBuildInfo
    { buildable = True
    , hsSourceDirs = [sourceDir]
    , defaultLanguage = Just Haskell2010
    , options = [(GHC, ghcOpts)]
    }

exeDesc :: FilePath -> FilePath -> [String] -> (ModuleName, FilePath)
        -> IO Executable
exeDesc ideSourcesDir ideDistDir ghcOpts (m, path) = do
  let exeName = Text.unpack m
  if exeName == "Main" then do  -- that's what Cabal expects, no wrapper needed
    return $ Executable
      { exeName
      , modulePath = path
      , buildInfo = bInfo ideSourcesDir ghcOpts
      }
  else do
    -- TODO: Verify @path@ somehow.
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

libDesc :: FilePath -> [String] -> [Distribution.ModuleName.ModuleName]
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

configureAndBuild :: FilePath -> FilePath -> [String] -> Bool
                  -> [PackageId] -> [ModuleName] -> (Progress -> IO ())
                  -> [(ModuleName, FilePath)]
                  -> IO ()
configureAndBuild ideSourcesDir ideDistDir ghcOpts dynlink
                  pkgs loadedMs callback ms = do
  counter <- newIORef initialProgress
  let markProgress = do
        oldCounter <- readIORef counter
        modifyIORef counter (updateProgress "")
        callback oldCounter
  markProgress
  libDeps <- externalDeps pkgs
  let mainDep = Package.Dependency pkgName (thisVersion pkgVersion)
      exeDeps = mainDep : libDeps
  executables <- mapM (exeDesc ideSourcesDir ideDistDir ghcOpts) ms
  markProgress
  let condExe exe = (exeName exe, CondNode exe exeDeps [])
      condExecutables = map condExe executables
  hsFound  <- doesFileExist $ ideSourcesDir </> "Main.hs"
  lhsFound <- doesFileExist $ ideSourcesDir </> "Main.lhs"
  -- Cabal can't find the code of @Main@ in subdirectories or in @Foo.hs@.
  -- TODO: this should be discussed and fixed when we generate .cabal files,
  -- because if another module depends on such a @Main@, we're in trouble
  -- (but if the @Main@ is only an executable, we are fine).
  -- Michael said in https://github.com/fpco/fpco/issues/1049
  -- "We'll be handling the disambiguation of Main modules ourselves before
  -- passing the files to you, so that shouldn't be an ide-backend concern.",
  -- so perhaps there is a plan for that.
  let soundMs | hsFound || lhsFound = loadedMs
              | otherwise = delete (Text.pack "Main") loadedMs
      projectMs = map (Distribution.ModuleName.fromString . Text.unpack) soundMs
      library = libDesc ideSourcesDir ghcOpts projectMs
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
                     , Setup.configVerbosity = Setup.Flag minBound
                     , Setup.configSharedLib = Setup.Flag dynlink
                     , Setup.configDynExe = Setup.Flag dynlink
                     }
      -- We don't override most build flags, but use configured values.
      buildFlags = Setup.defaultBuildFlags
                     { Setup.buildDistPref = Setup.Flag ideDistDir
                     , Setup.buildVerbosity = Setup.Flag minBound
                     }
      preprocessors :: [PPSuffixHandler]
      preprocessors = []
      hookedBuildInfo = (Nothing, [])  -- we don't want to use hooks
  lbi <- configure (gpDesc, hookedBuildInfo) confFlags
  markProgress
  Build.build (localPkgDescr lbi) lbi buildFlags preprocessors
  markProgress
  -- TODO: add a callback hook to Cabal that is applied to GHC messages
  -- as they are emitted, similarly as log_action in GHC API

buildExecutable :: FilePath -> FilePath -> [String] -> Bool
                -> Strict Maybe Computed -> (Progress -> IO ())
                -> [(ModuleName, FilePath)]
                -> IO ()
buildExecutable ideSourcesDir ideDistDir ghcOpts dynlink
                mcomputed callback ms = do
  case toLazyMaybe mcomputed of
    Nothing -> fail "This session state does not admit buidling executables."
    Just Computed{..} -> do
      let loadedMs = StrictMap.keys $ computedLoadedModules
          imp m = do
            let mimports =
                  fmap (toLazyList . StrictList.map (removeExplicitSharing
                                                       computedCache)) $
                    StrictMap.lookup m computedImports
            case mimports of
              Nothing -> fail $ "Module '" ++ Text.unpack m ++ "' not loaded."
              Just imports ->
                return $ map (modulePackage . importModule) imports
      imps <- mapM imp loadedMs
      let pkgs = concat imps
      liftIO $ configureAndBuild ideSourcesDir ideDistDir ghcOpts dynlink
                                 pkgs loadedMs callback ms
      -- TODO: keep a list of built (and up-to-date?) executables?
