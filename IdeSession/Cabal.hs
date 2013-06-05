{-# LANGUAGE ScopedTypeVariables #-}
module IdeSession.Cabal (
    buildExecutable, buildHaddock, buildLicenseCatenation
  ) where

import qualified Control.Exception as Ex
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import Data.List (delete, sort, nub)
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Time
  ( getCurrentTime, utcToLocalTime, toGregorian, localDay, getCurrentTimeZone )
import Data.Version (Version (..), parseVersion)
import qualified Data.Text as Text
import Text.ParserCombinators.ReadP (readP_to_S)
import System.Exit (ExitCode (ExitSuccess, ExitFailure))
import System.FilePath ((</>), takeFileName, splitPath, joinPath)
import Data.IORef (newIORef, readIORef, modifyIORef)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.IO.Temp (createTempDirectory)
import System.IO (IOMode(WriteMode), hClose, openBinaryFile, hPutStr)

import Distribution.InstalledPackageInfo
  (InstalledPackageInfo_ (InstalledPackageInfo, haddockInterfaces))
import Distribution.License (License (..))
import qualified Distribution.ModuleName
import Distribution.PackageDescription
import qualified Distribution.Package as Package
import Distribution.ParseUtils ( parseFields, simpleField, ParseResult(..)
                               , FieldDescr, parseLicenseQ, parseFilePathQ
                               , parseFreeText, showFilePath, showFreeText
                               , locatedErrorMsg, showPWarning, PWarning )
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

import IdeSession.Licenses
  ( bsd3, gplv2, gplv3, lgpl2, lgpl3, apache20 )
import IdeSession.State
import IdeSession.Strict.Container
import IdeSession.Types.Progress
import IdeSession.Types.Public
import IdeSession.Types.Translation
import qualified IdeSession.Strict.List as StrictList
import qualified IdeSession.Strict.Map  as StrictMap
import IdeSession.Util

import qualified Paths_ide_backend as Self

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

lFieldDescrs :: [FieldDescr (Maybe License, Maybe FilePath, Maybe String)]
lFieldDescrs =
 [ simpleField "license"
     Distribution.Text.disp              parseLicenseQ
     (\(t1, _, _) -> fromMaybe BSD3 t1)  (\l (_, t2, t3) -> (Just l, t2, t3))
 , simpleField "license-file"
     showFilePath                        parseFilePathQ
     (\(_, t2, _) -> fromMaybe "" t2)    (\lf (t1, _, t3) -> (t1, Just lf, t3))
 , simpleField "author"
     showFreeText                        parseFreeText
     (\(_, _, t3) -> fromMaybe "???" t3) (\a (t1, t2, _) -> (t1, t2, Just a))
 ]

buildLicenseCatenation :: FilePath -> FilePath -> Maybe [FilePath] -> [String]
                       -> Strict Maybe Computed -> (Progress -> IO ())
                       -> IO ExitCode
buildLicenseCatenation cabalsDir ideDistDir extraPackageDB configLicenseExc
                       mcomputed callback = do
  let defaultDB = GlobalPackageDB : UserPackageDB : []
      toDB l = fmap SpecificPackageDB l
      withPackageDB = maybe defaultDB toDB extraPackageDB
  counter <- newIORef initialProgress
  let markProgress = do
        oldCounter <- readIORef counter
        modifyIORef counter (updateProgress "")
        callback oldCounter
  (_, pkgs) <- buildDeps mcomputed  -- TODO: query transitive deps, not direct
  let stdoutLog  = ideDistDir </> "licenses.stdout"  -- warnings
      stderrLog  = ideDistDir </> "licenses.stderr"  -- errors
      licensesFN = ideDistDir </> "licenses.txt"     -- result
  licensesFile <- openBinaryFile licensesFN WriteMode
  -- The file containing concatenated licenses for core components.
  -- If not present in @cabalsDir@, taken from the default location.
  let cabalCoreFN = cabalsDir </> "CoreLicenses.txt"
  defaultCoreFN <- Self.getDataFileName "CoreLicenses.txt"
  cabalCoreExists <- doesFileExist cabalCoreFN
  defaultCoreExists <- doesFileExist defaultCoreFN
  let coreFN | cabalCoreExists = cabalCoreFN
             | defaultCoreExists = defaultCoreFN
             | otherwise = "CoreLicenses.txt"  -- in-place, for testing mostly
  bsCore <- BSL.readFile coreFN
  BSL.hPut licensesFile bsCore
  let mainPackageName = Text.pack "main"
      f :: PackageId -> IO ()
      f PackageId{packageName} | packageName == mainPackageName = return ()
      f PackageId{..} = do
        let nameString = Text.unpack packageName
            packageFile = cabalsDir </> nameString ++ ".cabal"
            versionString = maybe "" Text.unpack packageVersion
        version <- parseVersionString versionString
        let _outputWarns :: [PWarning] -> IO ()
            _outputWarns [] = return ()
            _outputWarns warns = do
              let warnMsg = "Parse warnings for " ++ packageFile ++ ":\n"
                            ++ unlines (map (showPWarning packageFile) warns)
                            ++ "\n"
              appendFile stdoutLog warnMsg
        b <- doesFileExist packageFile
        if b then do
          hPutStr licensesFile $ "\nLicense for " ++ nameString ++ ":\n\n"
          pkgS <- readFile packageFile
          -- We can't use @parsePackageDescription@, because it defaults to
          -- AllRightsReserved and we default to BSD3. It's very hard
          -- to use the machinery from the inside of @parsePackageDescription@,
          -- so instead we use the much simpler @ParseUtils.parseFields@.
          -- The downside is that we are much less past- and future-proof
          -- against .cabal format changes. The upside is @parseFields@
          -- is faster and does not care about most parsing errors
          -- the .cabal file may (appear to) have.
          case parseFields lFieldDescrs (Nothing, Nothing, Nothing) pkgS of
            ParseFailed err -> fail $ snd $ locatedErrorMsg err
            ParseOk _warns (_, Just lf, _) -> do
              -- outputWarns warns  -- false positives
              programDB <- configureAllKnownPrograms  -- won't die
                             minBound defaultProgramConfiguration
              pkgIndex <- getInstalledPackages minBound withPackageDB programDB
              let pkgId = Package.PackageIdentifier
                            { pkgName = Package.PackageName nameString
                            , pkgVersion = version }
                  pkgInfos = lookupSourcePackageId pkgIndex pkgId
              case pkgInfos of
                InstalledPackageInfo{haddockInterfaces = hIn : _} : _ -> do
                  let ps = splitPath hIn
                      prefix = joinPath $ take (length ps - 2) ps
                      -- The directory of the licence file is ignored
                      -- in installed packages, hence @takeFileName@.
                      stdLocation = prefix </> takeFileName lf
                  bstd <- doesFileExist stdLocation
                  if bstd then do
                    bs <- BSL.readFile stdLocation
                    BSL.hPut licensesFile bs
                  else do
                    -- Assume the package is not installed, but in a GHC tree.
                    let treePs = splitPath prefix
                        treePrefix = joinPath $ take (length treePs - 3) treePs
                        treeLocation = treePrefix </> takeFileName lf
                    btree <- doesFileExist treeLocation
                    if btree then do
                      bs <- BSL.readFile treeLocation
                      BSL.hPut licensesFile bs
                    else do
                      -- Assume the package is not installed, but in a GHC tree
                      -- with an alternative layout (OSX?).
                      let osxPrefix = joinPath $ take (length ps - 1) ps
                          osxLocation = osxPrefix </> takeFileName lf
                      bosx <- doesFileExist osxLocation
                      if bosx then do
                        bs <- BSL.readFile osxLocation
                        BSL.hPut licensesFile bs
                      else fail $ "Package " ++ nameString
                                  ++ " has no license file in path "
                                  ++ stdLocation
                                  ++ " nor " ++ treeLocation
                                  ++ " nor " ++ osxLocation
                _ -> fail $ "Package " ++ nameString
                             ++ " not properly installed."
            ParseOk _warns (l, Nothing, mauthor) -> do
              -- outputWarns warns  -- false positives
              when (isNothing l) $ do
                let warnMsg =
                      "WARNING: Package " ++ packageFile
                      ++ " has no license nor license file specified.\n"
                appendFile stdoutLog warnMsg
              let license = fromMaybe BSD3 l
                  author = fromMaybe "???" mauthor
              ms <- licenseText license author
              case ms of
                Nothing -> fail $ "No license text can be found for package "
                                  ++ nameString ++ "."
                Just s -> do
                  hPutStr licensesFile s
                  let assumed = if isNothing l
                                then " and the assumed"
                                else ", but"
                      warnMsg =
                        "WARNING: No license file specified for package "
                        ++ packageFile
                        ++ assumed
                        ++ " license is "
                        ++ show license
                        ++ ". Reproducing standard license text.\n"
                  appendFile stdoutLog warnMsg
        else
          unless (nameString `elem` configLicenseExc) $
            fail $ "No .cabal file provided for package "
                   ++ nameString ++ " so no license can be found."
        markProgress
  res <- Ex.try $ mapM_ f pkgs
  hClose licensesFile
  let handler :: Ex.IOException -> IO ExitCode
      handler e = do
        let msg = "Licenses concatenation failed. The exception is:\n"
                  ++ show e
        writeFile stderrLog msg
        return $ ExitFailure 1
  either handler (const $ return ExitSuccess) res

-- Gives a list of all modules and a list of all transitive package
-- dependencies of the currently loaded project.
buildDeps :: Strict Maybe Computed -> IO ([ModuleName], [PackageId])
buildDeps mcomputed = do
  case toLazyMaybe mcomputed of
    Nothing -> fail "This session state does not admit artifact generation."
    Just Computed{..} -> do
      let loadedMs = toLazyList computedLoadedModules
          imp m = do
            let mdeps =
                  fmap (toLazyList . StrictList.map (removeExplicitSharing
                                                       computedCache)) $
                    StrictMap.lookup m computedPkgDeps
                missing = fail $ "Module '" ++ Text.unpack m ++ "' not loaded."
            return $ fromMaybe missing mdeps
      imps <- mapM imp loadedMs
      return $ (loadedMs, nub $ sort $ concat imps)

licenseText :: License -> String -> IO (Maybe String)
licenseText license author = do
  year <- getYear
  return $! case license of
    BSD3 -> Just $ bsd3 author (show year)

    (GPL (Just (Version {versionBranch = [2]})))
      -> Just gplv2

    (GPL (Just (Version {versionBranch = [3]})))
      -> Just gplv3

    (LGPL (Just (Version {versionBranch = [2]})))
      -> Just lgpl2

    (LGPL (Just (Version {versionBranch = [3]})))
      -> Just lgpl3

-- TODO: needs to wait for a newer version of Cabal
--    (Apache (Just (Version {versionBranch = [2, 0]})))
--      -> Just apache20

    _ -> Nothing

getYear :: IO Integer
getYear = do
  u <- getCurrentTime
  z <- getCurrentTimeZone
  let l = utcToLocalTime z u
      (y, _, _) = toGregorian $ localDay l
  return y
