{-# LANGUAGE ScopedTypeVariables #-}
module IdeSession.Cabal (
    buildExecutable, buildHaddock, buildLicenseCatenation
  ) where

import qualified Control.Exception as Ex
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import Data.List (delete, sort, nub)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Version (Version (..), parseVersion)
import qualified Data.Text as Text
import Text.ParserCombinators.ReadP (readP_to_S)
import System.Exit (ExitCode (ExitSuccess))
import System.FilePath ((</>), takeFileName, splitPath, joinPath)
import Data.IORef (newIORef, readIORef, modifyIORef)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.IO.Temp (createTempDirectory)
import System.IO (IOMode(WriteMode), hClose, openFile, hPutStr)

import Distribution.InstalledPackageInfo
  (InstalledPackageInfo_ (InstalledPackageInfo, haddockInterfaces))
import Distribution.License (License (..))
import qualified Distribution.ModuleName
import Distribution.PackageDescription
import qualified Distribution.Package as Package
import Distribution.ParseUtils ( parseFields, simpleField, ParseResult(..)
                               , FieldDescr, parseLicenseQ, parseFilePathQ
                               , showFilePath )
import qualified Distribution.Simple.Build as Build
import qualified Distribution.Simple.Haddock as Haddock
import Distribution.Simple.Compiler ( CompilerFlavor (GHC)
                                    , PackageDB(..), PackageDBStack )
import Distribution.Simple.Configure (configure)
import Distribution.Simple.GHC (getInstalledPackages)
import Distribution.Simple.LocalBuildInfo (localPkgDescr, withPackageDB)
import Distribution.Simple.PackageIndex (lookupSourcePackageId)
import Distribution.Simple.PreProcess (PPSuffixHandler)
import qualified Distribution.Simple.Setup as Setup
import Distribution.Simple.Program (defaultProgramConfiguration)
import Distribution.Simple.Program.Db (configureAllKnownPrograms)
import qualified Distribution.Text
import Distribution.Version (anyVersion, thisVersion)
import Language.Haskell.Extension (Language (Haskell2010))

import IdeSession.State
import IdeSession.Strict.Container
import IdeSession.Types.Progress
import IdeSession.Types.Public
import IdeSession.Types.Translation
import qualified IdeSession.Strict.List as StrictList
import qualified IdeSession.Strict.Map  as StrictMap
import IdeSession.Util

-- TODO: factor out common parts of exe building and haddock generation
-- after Cabal and the code that calls it are improved not to require
-- the configure step, etc.

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
                  -> PackageDBStack -> [PackageId]
                  -> [ModuleName] -> (Progress -> IO ())
                  -> [(ModuleName, FilePath)]
                  -> IO ExitCode
configureAndBuild ideSourcesDir ideDistDir ghcOpts dynlink
                  withPackageDB pkgs loadedMs callback ms = do
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
                     , Setup.buildVerbosity = Setup.Flag $ toEnum 1
                     }
      preprocessors :: [PPSuffixHandler]
      preprocessors = []
      hookedBuildInfo = (Nothing, [])  -- we don't want to use hooks
  createDirectoryIfMissing False $ ideDistDir </> "build"
  let stdoutLog = ideDistDir </> "build/ide-backend-exe.stdout"
      stderrLog = ideDistDir </> "build/ide-backend-exe.stderr"
  exitCode :: Either ExitCode () <- Ex.bracket
    (do stdOutputBackup <- redirectStdOutput stdoutLog
        stdErrorBackup  <- redirectStdError  stderrLog
        return (stdOutputBackup, stdErrorBackup))
    (\(stdOutputBackup, stdErrorBackup) -> do
        restoreStdOutput stdOutputBackup
        restoreStdError  stdErrorBackup)
    (\_ -> Ex.try $ do
        lbiRaw <- configure (gpDesc, hookedBuildInfo) confFlags
        let lbi = lbiRaw {withPackageDB}
        markProgress
        Build.build (localPkgDescr lbi) lbi buildFlags preprocessors
        markProgress)
  return $! either id (const ExitSuccess) exitCode
  -- TODO: add a callback hook to Cabal that is applied to GHC messages
  -- as they are emitted, similarly as log_action in GHC API,
  -- or filter stdout and display progress on each good line.

configureAndHaddock :: FilePath -> FilePath -> [String] -> Bool
                    -> PackageDBStack -> [PackageId]
                    -> [ModuleName] -> (Progress -> IO ())
                    -> IO ExitCode
configureAndHaddock ideSourcesDir ideDistDir ghcOpts dynlink
                    withPackageDB pkgs loadedMs callback = do
  counter <- newIORef initialProgress
  let markProgress = do
        oldCounter <- readIORef counter
        modifyIORef counter (updateProgress "")
        callback oldCounter
  markProgress
  libDeps <- externalDeps pkgs
  markProgress
  let condExecutables = []
  hsFound  <- doesFileExist $ ideSourcesDir </> "Main.hs"
  lhsFound <- doesFileExist $ ideSourcesDir </> "Main.lhs"
  -- Cabal can't find the code of @Main@, except in the main dir. See above.
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
--                     , Setup.configDynExe = Setup.Flag dynlink
                     }
      preprocessors :: [PPSuffixHandler]
      preprocessors = []
      haddockFlags = Setup.defaultHaddockFlags
        { Setup.haddockDistPref = Setup.Flag ideDistDir
        , Setup.haddockVerbosity = Setup.Flag minBound
        }
      hookedBuildInfo = (Nothing, [])  -- we don't want to use hooks
  createDirectoryIfMissing False $ ideDistDir </> "doc"
  let stdoutLog = ideDistDir </> "doc/ide-backend-doc.stdout"
      stderrLog = ideDistDir </> "doc/ide-backend-doc.stderr"
  exitCode :: Either ExitCode () <- Ex.bracket
    (do stdOutputBackup <- redirectStdOutput stdoutLog
        stdErrorBackup  <- redirectStdError  stderrLog
        return (stdOutputBackup, stdErrorBackup))
    (\(stdOutputBackup, stdErrorBackup) -> do
        restoreStdOutput stdOutputBackup
        restoreStdError  stdErrorBackup)
    (\_ -> Ex.try $ do
        lbiRaw <- configure (gpDesc, hookedBuildInfo) confFlags
        let lbi = lbiRaw {withPackageDB}
        markProgress
        Haddock.haddock (localPkgDescr lbi) lbi preprocessors haddockFlags
        markProgress)
  return $! either id (const ExitSuccess) exitCode
  -- TODO: add a callback hook to Cabal that is applied to GHC messages
  -- as they are emitted, similarly as log_action in GHC API,
  -- or filter stdout and display progress on each good line.

buildExecutable :: FilePath -> FilePath -> [String] -> Bool -> Maybe [FilePath]
                -> Strict Maybe Computed -> (Progress -> IO ())
                -> [(ModuleName, FilePath)]
                -> IO ExitCode
buildExecutable ideSourcesDir ideDistDir ghcOpts dynlink extraPackageDB
                mcomputed callback ms = do
  (loadedMs, pkgs) <- buildDeps mcomputed
  let defaultDB = GlobalPackageDB : UserPackageDB : []
      toDB l = fmap SpecificPackageDB l
      withPackageDB = maybe defaultDB toDB extraPackageDB
  configureAndBuild ideSourcesDir ideDistDir ghcOpts dynlink
                    withPackageDB pkgs loadedMs callback ms

buildHaddock :: FilePath -> FilePath -> [String] -> Bool -> Maybe [FilePath]
             -> Strict Maybe Computed -> (Progress -> IO ())
             -> IO ExitCode
buildHaddock ideSourcesDir ideDistDir ghcOpts dynlink extraPackageDB
             mcomputed callback = do
  (loadedMs, pkgs) <- buildDeps mcomputed
  let defaultDB = GlobalPackageDB : UserPackageDB : []
      toDB l = fmap SpecificPackageDB l
      withPackageDB = maybe defaultDB toDB extraPackageDB
  configureAndHaddock ideSourcesDir ideDistDir ghcOpts dynlink
                      withPackageDB pkgs loadedMs callback

licenseFieldDescrs :: [FieldDescr (Maybe License, Maybe FilePath)]
licenseFieldDescrs =
 [ simpleField "license"
           Distribution.Text.disp              parseLicenseQ
           (fromMaybe AllRightsReserved . fst) (\l t -> (Just l, snd t))
 , simpleField "license-file"
           showFilePath                        parseFilePathQ
           (fromMaybe "" . snd)                (\lf t -> (fst t, Just lf))
 ]

buildLicenseCatenation :: FilePath -> FilePath -> Maybe [FilePath]
                       -> Strict Maybe Computed -> (Progress -> IO ())
                       -> IO ExitCode
buildLicenseCatenation cabalsDir ideDistDir extraPackageDB mcomputed
                       callback = do
  let defaultDB = GlobalPackageDB : UserPackageDB : []
      toDB l = fmap SpecificPackageDB l
      withPackageDB = maybe defaultDB toDB extraPackageDB
  counter <- newIORef initialProgress
  let markProgress = do
        oldCounter <- readIORef counter
        modifyIORef counter (updateProgress "")
        callback oldCounter
  licensesFile <- openFile (ideDistDir </> "licenses.txt") WriteMode
  let mainPackageName = Text.pack "main"
      f :: PackageId -> IO ()
      f PackageId{packageName} | packageName == mainPackageName = return ()
      f PackageId{..} = do
        let nameString = Text.unpack packageName
            packageFile = cabalsDir </> nameString ++ ".cabal"
        b <- doesFileExist packageFile
        if b then do
          s <- readFile packageFile
          case parseFields licenseFieldDescrs (Nothing, Nothing) s of
            ParseFailed _ -> return ()  -- TODO
            -- Ignore warnings, assume harmless:
            ParseOk _ (Just _l, Just lf) -> do
              programDB <- configureAllKnownPrograms
                             minBound defaultProgramConfiguration
              pkgIndex <- getInstalledPackages minBound withPackageDB programDB
              let versionString = maybe "" Text.unpack packageVersion
              version <- parseVersionString versionString
              let pkgId = Package.PackageIdentifier
                            { pkgName = Package.PackageName nameString
                            , pkgVersion = version }
                  pkgInfos = lookupSourcePackageId pkgIndex pkgId
              case pkgInfos of
                InstalledPackageInfo{haddockInterfaces = hIn : _} : _ -> do
                  let prefix = joinPath $ init $ init $ splitPath hIn
                  -- The directory of the licence file is ignored
                  -- in installed packages, hence @takeFileName@.
                  bs <- BSL.readFile $ prefix </> takeFileName lf
                  hPutStr licensesFile $
                    "\nLicense for " ++ nameString ++ ":\n"
                  BSL.hPut licensesFile bs
                _ -> error $ "buildLicenseCatenation: Package "
                             ++ nameString ++ " not properly installed."
            ParseOk _ _ -> return ()  -- TODO
        else return ()  -- TODO
        markProgress
  (_, pkgs) <- buildDeps mcomputed  -- TODO: query transitive deps, not direct
  mapM_ f pkgs
  hClose licensesFile
  return ExitSuccess

-- Gives a list of all modules and a list of all package dependencies
-- of the currently loaded project.
buildDeps :: Strict Maybe Computed -> IO ([ModuleName], [PackageId])
buildDeps mcomputed = do
  case toLazyMaybe mcomputed of
    Nothing -> fail "This session state does not admit artifact generation."
    Just Computed{..} -> do
      let loadedMs = toLazyList computedLoadedModules
          imp m = do
            let mimports =
                  fmap (toLazyList . StrictList.map (removeExplicitSharing
                                                       computedCache)) $
                    StrictMap.lookup m computedImports
            case mimports of
              Nothing -> fail $ "Module '" ++ Text.unpack m ++ "' not loaded."
              Just imports ->
                return $! map (modulePackage . importModule) imports
      imps <- mapM imp loadedMs
      return $ (loadedMs, nub $ sort $ concat imps)
