{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module IdeSession.Cabal (
    buildExecutable, buildHaddock, buildLicenseCatenation
  , generateMacros, buildDotCabal, runComponentCc
  , buildLicsFromPkgs  -- for testing only
  ) where

import qualified Control.Exception as Ex
import Control.Monad
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Function (on)
import Data.List (delete, sort, groupBy, nub, intersperse, intercalate)
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Monoid (Monoid(..))
import Data.Time
  ( getCurrentTime, utcToLocalTime, toGregorian, localDay, getCurrentTimeZone )
import Data.Version (Version (..), parseVersion)
import qualified Data.Text as Text
import Text.ParserCombinators.ReadP (readP_to_S)
import System.Exit (ExitCode (ExitSuccess, ExitFailure), exitFailure)
import System.FilePath ( (</>), takeFileName, makeRelative
                       , takeDirectory, replaceExtension )
import System.FilePath.Find (find, always, extension)
import System.Directory (
    doesFileExist
  , createDirectoryIfMissing
  , removeDirectoryRecursive
  )
import System.IO.Temp (createTempDirectory)
import System.IO (IOMode(WriteMode), hClose, openBinaryFile, hPutStr, hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError, isUserError, catchIOError)

import Distribution.InstalledPackageInfo
  (InstalledPackageInfo_ ( InstalledPackageInfo
                         , haddockInterfaces ))
import Distribution.License (License (..))
import qualified Distribution.Compiler as Compiler
import qualified Distribution.ModuleName
import Distribution.PackageDescription
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import qualified Distribution.Package as Package
import Distribution.ParseUtils ( parseFields, simpleField, ParseResult (..)
                               , FieldDescr, parseLicenseQ, parseFilePathQ
                               , parseFreeText, showFilePath, showFreeText
                               , locatedErrorMsg, showPWarning, PWarning )
import qualified Distribution.Simple.Build as Build
import Distribution.Simple.Build.Macros
import qualified Distribution.Simple.Haddock as Haddock
import Distribution.Simple (PackageDBStack)
import qualified Distribution.Simple.Compiler as Simple.Compiler
import Distribution.Simple.Configure (configure)
import Distribution.Simple.GHC (getInstalledPackages, componentCcGhcOptions,
                                ghcDynamic)
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo (..)
                                          , ComponentLocalBuildInfo (..)
                                          , localPkgDescr)
import Distribution.Simple.PackageIndex ( lookupSourcePackageId )
import Distribution.Simple.PreProcess (PPSuffixHandler)
import qualified Distribution.Simple.Setup as Setup
import qualified Distribution.Simple.Program as Cabal.Program
import Distribution.Simple.Program.Builtin (ghcPkgProgram)
import Distribution.Simple.Program.Db (configureAllKnownPrograms, requireProgram)
import Distribution.Simple.Program.GHC (GhcDynLinkMode(..), GhcOptions(..),
                                        runGHC)
import qualified Distribution.Simple.Program.HcPkg as HcPkg
import Distribution.System (buildPlatform)
import qualified Distribution.Text
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose)
import Distribution.Version (anyVersion, thisVersion)
import Distribution.Verbosity (silent)
import Language.Haskell.Extension (Extension(..), KnownExtension(..),
                                   Language (Haskell2010))
import Language.Haskell.TH.Syntax (lift, runIO)

import IdeSession.Config
import IdeSession.GHC.API (cExtensions, cHeaderExtensions)
import IdeSession.Licenses ( bsd3, gplv2, gplv3, lgpl2, lgpl3, apache20 )
import IdeSession.State
import IdeSession.Strict.Container
import IdeSession.Strict.Maybe (just)
import IdeSession.Types.Progress
import IdeSession.Types.Public
import IdeSession.Types.Translation
import qualified IdeSession.Strict.List as StrictList
import qualified IdeSession.Strict.Map  as StrictMap
import IdeSession.Util

-- TODO: factor out common parts of exe building and haddock generation
-- after Cabal and the code that calls it are improved not to require
-- the configure step, etc.

pkgNameMain :: Package.PackageName
pkgNameMain = Package.PackageName "main"  -- matches the import default

pkgVersionMain :: Version
pkgVersionMain = Version [1, 0] []

pkgDescFromName :: String -> Version -> PackageDescription
pkgDescFromName pkgName version = PackageDescription
  { -- the following are required by all packages:
    package        = Package.PackageIdentifier
                       { pkgName    = Package.PackageName pkgName
                       , pkgVersion = version
                       }
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
  , extraDocFiles  = []
  }

pkgDesc :: PackageDescription
pkgDesc = pkgDescFromName "main" pkgVersionMain

bInfo :: [FilePath] -> [String] -> [FilePath] -> [FilePath] -> BuildInfo
bInfo hsSourceDirs ghcOpts cSources installIncludes =
  emptyBuildInfo
    { buildable = True
    , hsSourceDirs
    , defaultLanguage = Just Haskell2010
    , options = [(Simple.Compiler.GHC, ghcOpts)]
    , cSources
    , installIncludes
    }

-- @relative@ is a hack, because when building, we need an absolute path
-- (so that ghc sees the .c files), but .cabal files need a relative path
-- (and that's enough for 'cabal install', since the package dir
-- will have both the .cabal and the C sources, unlike the directories
-- we build exes in).
getCSources :: Bool -> [FilePath] -> IO [FilePath]
getCSources relative [sourceDir] = do
  files <- find always ((`elem` cExtensions) `liftM` extension) sourceDir
  return $ if relative
           then map (makeRelative sourceDir) files
           else files
getCSources _ _sourceDirs =
  fail $ "getCSources: wrong sourceDir: " ++ intercalate ", " _sourceDirs

getCHeaders :: [FilePath] -> IO [FilePath]
getCHeaders [sourceDir] =
  fmap (map takeFileName) $
    find always ((`elem` cHeaderExtensions) `liftM` extension) sourceDir
getCHeaders _sourceDirs =
  fail $ "getCHeaders: wrong sourceDir: " ++ intercalate ", " _sourceDirs

exeDesc :: [FilePath] -> [FilePath] -> FilePath -> [String]
        -> (ModuleName, FilePath)
        -> IO Executable
exeDesc ideSourcesDir ideSourcesDirForC ideDistDir ghcOpts (m, path) = do
  cSources <- getCSources False ideSourcesDirForC
  cHeaders <- getCHeaders ideSourcesDirForC
  let exeName = Text.unpack m
  if exeName == "Main" then do  -- that's what Cabal expects, no wrapper needed
    return $ Executable
      { exeName
      , modulePath = path
      , buildInfo = bInfo ideSourcesDir ghcOpts cSources cHeaders
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
      , buildInfo = bInfo [mDir] ghcOpts cSources cHeaders
      }

libDesc :: Bool -> [FilePath] -> [FilePath] -> [String]
        -> [Distribution.ModuleName.ModuleName]
        -> IO Library
libDesc relative ideSourcesDir ideSourcesDirForC ghcOpts ms = do
  cSources <- getCSources relative ideSourcesDirForC
  cHeaders <- getCHeaders ideSourcesDirForC
  return $ Library
    { exposedModules = ms
    , libExposed = False
    , libBuildInfo = bInfo ideSourcesDir ghcOpts cSources cHeaders
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
        if packageN == pkgNameMain then return Nothing
        else return $ Just $ Package.Dependency packageN anyVersion
      depOfName PackageId{packageName, packageVersion = Just versionText} = do
        let packageN = Package.PackageName $ Text.unpack $ packageName
            versionString = Text.unpack versionText
        version <- parseVersionString versionString
        return $ Just $ Package.Dependency packageN (thisVersion version)
  in liftM catMaybes $ mapM depOfName pkgs

mkConfFlags :: FilePath -> Bool -> PackageDBStack -> [FilePath] -> Setup.ConfigFlags
mkConfFlags ideDistDir dynlink configPackageDBStack progPathExtra =
  (Setup.defaultConfigFlags (defaultProgramConfiguration progPathExtra))
    { Setup.configDistPref = Setup.Flag ideDistDir
    , Setup.configUserInstall = Setup.Flag False
    , Setup.configVerbosity = Setup.Flag minBound
    , Setup.configDynExe = Setup.Flag dynlink
      -- @Nothing@ wipes out default, initial DBs.
    , Setup.configPackageDBs = Nothing : map Just configPackageDBStack
    , Setup.configProgramPathExtra = progPathExtra
    }

configureAndBuild :: SessionConfig -> FilePath -> FilePath -> [String]
                  -> [PackageId] -> [ModuleName] -> (Progress -> IO ())
                  -> [(ModuleName, FilePath)]
                  -> IO ExitCode
configureAndBuild SessionConfig{..} ideSourcesDir ideDistDir ghcNewOpts
                  pkgs loadedMs callback ms = do
  -- TODO: Check if this 1/4 .. 4/4 sequence of progress messages is
  -- meaningful,  and if so, replace the Nothings with Just meaningful messages
  callback $ Progress 1 4 Nothing Nothing
  libDeps <- externalDeps pkgs
  let mainDep = Package.Dependency pkgNameMain (thisVersion pkgVersionMain)
      exeDeps = mainDep : libDeps
      sourcesDirs = map (\path -> ideSourcesDir </> path)
                        configRelativeIncludes
      ghcOpts = "-rtsopts=some" : ghcNewOpts ++ configExtraBuildExeOptions
  executables <-
    mapM (exeDesc sourcesDirs [ideSourcesDir] ideDistDir ghcOpts) ms
  callback $ Progress 2 4 Nothing Nothing
  let condExe exe = (exeName exe, CondNode exe exeDeps [])
      condExecutables = map condExe executables
  hsFound  <- doesFileExist $ ideSourcesDir </> "Main.hs"
  lhsFound <- doesFileExist $ ideSourcesDir </> "Main.lhs"
  -- Cabal can't find the code of @Main@, to be used as the main executable
  -- module, in subdirectories or in @Foo.hs@. We need a @Main@ to build
  -- an executable, so any other @Main@ modules have to be ignored.
  -- So, if another module depends on such a @Main@,
  -- we're in trouble, but if the @Main@ is only an executable, we are fine.
  -- Michael said in https://github.com/fpco/fpco/issues/1049
  -- "We'll be handling the disambiguation of Main modules ourselves before
  -- passing the files to you, so that shouldn't be an ide-backend concern.",
  -- so perhaps there won't be any problems.
  let soundMs | hsFound || lhsFound = loadedMs
              | otherwise = delete (Text.pack "Main") loadedMs
      projectMs = map (Distribution.ModuleName.fromString . Text.unpack) soundMs
  library <- libDesc False sourcesDirs [ideSourcesDir] ghcOpts projectMs
  let gpDesc = GenericPackageDescription
        { packageDescription = pkgDesc
        , genPackageFlags    = []  -- seem unused
        , condLibrary        = Just $ CondNode library libDeps []
        , condExecutables
        , condTestSuites     = []
        , condBenchmarks     = []
        }
      confFlags = mkConfFlags ideDistDir configDynLink configPackageDBStack configExtraPathDirs
      -- We don't override most build flags, but use configured values.
      buildFlags = Setup.defaultBuildFlags
                     { Setup.buildDistPref = Setup.Flag ideDistDir
                     , Setup.buildVerbosity = Setup.Flag $ toEnum 1
                     }
      preprocessors :: [PPSuffixHandler]
      preprocessors = []
      hookedBuildInfo = (Nothing, [])  -- we don't want to use hooks

  -- Delete the build directory completely so that we trigger a full
  -- recompilation. This is a workaround for #119.
  ignoreDoesNotExist $ removeDirectoryRecursive $ ideDistDir </> "build"

  createDirectoryIfMissing False $ ideDistDir </> "build"
  let confAndBuild = do
        lbi <- configure (gpDesc, hookedBuildInfo) confFlags
        -- Setting @withPackageDB@ here is too late, @configure@ would fail
        -- already. Hence we set it in @mkConfFlags@ (can be reverted,
        -- when/if we construct @lbi@ without @configure@).
        callback $ Progress 3 4 Nothing Nothing
        Build.build (localPkgDescr lbi) lbi buildFlags preprocessors
        callback $ Progress 4 4 Nothing Nothing
  -- Handle various exceptions and stderr/stdout printouts.
  let stdoutLog = ideDistDir </> "build/ide-backend-exe.stdout"
      stderrLog = ideDistDir </> "build/ide-backend-exe.stderr"
  exitCode :: Either ExitCode () <- Ex.bracket
    (do stdOutputBackup <- redirectStdOutput stdoutLog
        stdErrorBackup  <- redirectStdError  stderrLog
        return (stdOutputBackup, stdErrorBackup))
    (\(stdOutputBackup, stdErrorBackup) -> do
        restoreStdOutput stdOutputBackup
        restoreStdError  stdErrorBackup)
    (\_ -> Ex.try $ catchIOError confAndBuild
                      (\e -> if isUserError e
                             then do
                               -- In the new cabal code some exceptions
                               -- are handled with 'die', raising a user error,
                               -- while some still do 'exit 1' and print
                               -- to stderr. For uniformity, we redirect
                               -- user errors to stderr, as well.
                               hPutStrLn stderr $ "Exception caught:"
                               hPutStrLn stderr $ show e
                               exitFailure
                             else ioError e))
  return $! either id (const ExitSuccess) exitCode
  -- TODO: add a callback hook to Cabal that is applied to GHC messages
  -- as they are emitted, similarly as log_action in GHC API,
  -- or filter stdout and display progress on each good line.
  where
    ignoreDoesNotExist :: IO () -> IO ()
    ignoreDoesNotExist = Ex.handle $ \e ->
      if isDoesNotExistError e then return ()
                               else Ex.throwIO e

configureAndHaddock :: SessionConfig -> FilePath -> FilePath -> [String]
                    -> [PackageId] -> [ModuleName] -> (Progress -> IO ())
                    -> IO ExitCode
configureAndHaddock SessionConfig{..} ideSourcesDir ideDistDir ghcNewOpts
                    pkgs loadedMs callback = do
  -- TODO: Check if this 1/4 .. 4/4 sequence of progress messages is
  -- meaningful,  and if so, replace the Nothings with Just meaningful messages
  callback $ Progress 1 4 Nothing Nothing
  libDeps <- externalDeps pkgs
  callback $ Progress 2 4 Nothing Nothing
  let condExecutables = []
      sourcesDirs = map (\path -> ideSourcesDir </> path)
                        configRelativeIncludes
  hsFound  <- doesFileExist $ ideSourcesDir </> "Main.hs"
  lhsFound <- doesFileExist $ ideSourcesDir </> "Main.lhs"
  -- Cabal can't find the code of @Main@, except in the main dir. See above.
  let soundMs | hsFound || lhsFound = loadedMs
              | otherwise = delete (Text.pack "Main") loadedMs
      projectMs = map (Distribution.ModuleName.fromString . Text.unpack) soundMs
  library <- libDesc False sourcesDirs [ideSourcesDir] ghcNewOpts projectMs
  let gpDesc = GenericPackageDescription
        { packageDescription = pkgDesc
        , genPackageFlags    = []  -- seem unused
        , condLibrary        = Just $ CondNode library libDeps []
        , condExecutables
        , condTestSuites     = []
        , condBenchmarks     = []
        }
      confFlags =
        mkConfFlags ideDistDir False configPackageDBStack configExtraPathDirs
      preprocessors :: [PPSuffixHandler]
      preprocessors = []
      haddockFlags = Setup.defaultHaddockFlags
        { Setup.haddockDistPref = Setup.Flag ideDistDir
        , Setup.haddockHtml = Setup.Flag True
        , Setup.haddockHoogle = Setup.Flag True
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
        lbi <- configure (gpDesc, hookedBuildInfo) confFlags
        callback $ Progress 3 4 Nothing Nothing
        Haddock.haddock (localPkgDescr lbi) lbi preprocessors haddockFlags
        callback $ Progress 4 4 Nothing Nothing)
  return $! either id (const ExitSuccess) exitCode
  -- TODO: add a callback hook to Cabal that is applied to GHC messages
  -- as they are emitted, similarly as log_action in GHC API,
  -- or filter stdout and display progress on each good line.

buildDotCabal :: FilePath -> [FilePath] -> [String] -> Computed
              -> IO (String -> Version -> BSL.ByteString)
buildDotCabal ideSourcesDir configRelativeIncludes ghcNewOpts computed = do
  (loadedMs, pkgs) <- buildDeps $ just computed
  libDeps <- externalDeps pkgs
  -- We ignore any @Main@ modules (even in subdirectories or in @Foo.hs@)
  -- so that they don't get in the way when we build an executable
  -- using the library. So, if another module depends on such a @Main@,
  -- we're in trouble, but if the @Main@ is only an executable, we are fine.
  -- Michael said in https://github.com/fpco/fpco/issues/1049
  -- "We'll be handling the disambiguation of Main modules ourselves before
  -- passing the files to you, so that shouldn't be an ide-backend concern.",
  -- so perhaps there won't be any problems.
  let soundMs = delete (Text.pack "Main") loadedMs
      projectMs =
        sort $ map (Distribution.ModuleName.fromString . Text.unpack) soundMs
  library <- libDesc True -- relative C files paths
                     (filter (/= "") configRelativeIncludes) [ideSourcesDir]
                     ghcNewOpts projectMs
  let libE = library {libExposed = True}
      gpDesc libName version = GenericPackageDescription
        { packageDescription = pkgDescFromName libName version
        , genPackageFlags    = []  -- seem unused
        , condLibrary        = Just $ CondNode libE libDeps []
        , condExecutables    = []
        , condTestSuites     = []
        , condBenchmarks     = []
        }
  return $ \libName version ->
    BSL8.pack $ showGenericPackageDescription $ gpDesc libName version

buildExecutable :: SessionConfig -> FilePath -> FilePath -> [String]
                -> Strict Maybe Computed -> (Progress -> IO ())
                -> [(ModuleName, FilePath)]
                -> IO ExitCode
buildExecutable ideConfig ideSourcesDir ideDistDir ghcNewOpts
                mcomputed callback ms = do
  (loadedMs, pkgs) <- buildDeps mcomputed
  configureAndBuild ideConfig ideSourcesDir ideDistDir ghcNewOpts
                    pkgs loadedMs callback ms

buildHaddock :: SessionConfig -> FilePath -> FilePath -> [String]
             -> Strict Maybe Computed -> (Progress -> IO ())
             -> IO ExitCode
buildHaddock ideConfig ideSourcesDir ideDistDir ghcNewOpts
             mcomputed callback = do
  (loadedMs, pkgs) <- buildDeps mcomputed
  configureAndHaddock ideConfig ideSourcesDir ideDistDir ghcNewOpts
                      pkgs loadedMs callback

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

-- | Build the concatenation of all licence files. See 'buildLicenses'.
buildLicenseCatenation :: SessionConfig          -- ^ session configuration
                       -> Strict Maybe Computed  -- ^ compilation state
                       -> FilePath               -- ^ the directory with all the .cabal files
                       -> FilePath               -- ^ the working directory; the resulting file is written there
                       -> (Progress -> IO ())    -- ^ progress callback
                       -> IO ExitCode
buildLicenseCatenation ideConfig mcomputed cabalsDir ideDistDir callback = do
  (_, pkgs) <- buildDeps mcomputed
  buildLicsFromPkgs ideConfig pkgs cabalsDir ideDistDir
                    (configLicenseFixed ideConfig) callback

-- | Build the concatenation of all licence files from a given list
-- of packages.
buildLicsFromPkgs :: SessionConfig          -- ^ session configuration
                  -> [PackageId]            -- ^ the list of packages to process
                  -> FilePath               -- ^ the directory with all the .cabal files
                  -> FilePath               -- ^ the working directory; the resulting file is written there
                  -> [( String
                      , (Maybe License, Maybe FilePath, Maybe String)
                      )]                    -- ^ see 'configLicenseFixed'
                  -> (Progress -> IO ())    -- ^ progress callback
                  -> IO ExitCode
buildLicsFromPkgs SessionConfig{ configExtraPathDirs
                               , configPackageDBStack
                               , configLicenseExc }
                  pkgs cabalsDir ideDistDir licenseFixed callback = do
  -- The following computations are very expensive, so should be done once,
  -- instead of at each invocation of @findLicense@ that needs to perform
  -- @lookupSourcePackageId@.
  programDB <- configureAllKnownPrograms  -- won't die
                 minBound (defaultProgramConfiguration configExtraPathDirs)
  pkgIndex <- getInstalledPackages minBound configPackageDBStack programDB
  let stdoutLogFN = ideDistDir </> "licenses.stdout"  -- warnings
      stderrLogFN = ideDistDir </> "licenses.stderr"  -- errors
      licensesFN  = ideDistDir </> "licenses.txt"     -- result
  stdoutLog <- openBinaryFile stdoutLogFN WriteMode
  stderrLog <- openBinaryFile stderrLogFN WriteMode
  licensesFile <- openBinaryFile licensesFN WriteMode
  -- The file containing concatenated licenses for core components.
  let bsCore = BSL8.pack $(runIO (BSL.readFile "CoreLicenses.txt") >>= lift . BSL8.unpack)
  BSL.hPut licensesFile bsCore

  let numSteps        = length pkgs
      mainPackageName = Text.pack "main"

      f :: (PackageId, Int) -> IO ()
      f (PackageId{packageName}, step) | packageName == mainPackageName =
        callback $ Progress step numSteps (Just packageName) (Just packageName)
      f (PackageId{..}, step) = do
        let nameString = Text.unpack packageName
            packageFile = cabalsDir </> nameString ++ ".cabal"
            versionString = maybe "" Text.unpack packageVersion
        version <- parseVersionString versionString
        let _outputWarns :: [PWarning] -> IO ()
            _outputWarns [] = return ()
            _outputWarns warns = do
              let warnMsg = "Parse warnings for " ++ packageFile ++ ":\n"
                            ++ unlines (map (showPWarning packageFile) warns)
              hPutStrLn stdoutLog warnMsg
        cabalFileExists <- doesFileExist packageFile
        if cabalFileExists then do
          hPutStrLn licensesFile $ "\nLicense for " ++ nameString ++ ":\n"
          pkgS <- readFile packageFile
          let parseResult =
                parseFields lFieldDescrs (Nothing, Nothing, Nothing) pkgS
          findLicense parseResult nameString version packageFile
        else case lookup nameString licenseFixed of
          Just fixedLicence -> do
            hPutStrLn licensesFile $ "\nLicense for " ++ nameString ++ ":\n"
            let fakeParseResult = ParseOk undefined fixedLicence
            findLicense fakeParseResult nameString version packageFile
          Nothing ->
            unless (nameString `elem` configLicenseExc) $ do
              let errorMsg = "No .cabal file provided for package "
                             ++ nameString ++ " so no license can be found."
              hPutStrLn licensesFile errorMsg
              hPutStrLn stderrLog errorMsg
        callback $ Progress step numSteps (Just packageName) (Just packageName)

      findLicense :: ParseResult (Maybe License, Maybe FilePath, Maybe String)
                  -> String -> Version -> FilePath
                  -> IO ()
      findLicense parseResult nameString version packageFile =
          -- We can't use @parsePackageDescription@, because it defaults to
          -- AllRightsReserved and we default to BSD3. It's very hard
          -- to use the machinery from the inside of @parsePackageDescription@,
          -- so instead we use the much simpler @ParseUtils.parseFields@.
          -- The downside is that we are much less past- and future-proof
          -- against .cabal format changes. The upside is @parseFields@
          -- is faster and does not care about most parsing errors
          -- the .cabal file may (appear to) have.
          case parseResult of
            ParseFailed err -> do
              hPutStrLn licensesFile $ snd $ locatedErrorMsg err
              hPutStrLn stderrLog $ snd $ locatedErrorMsg err
            ParseOk _warns (_, Just lf, _) -> do
              -- outputWarns warns  -- false positives
              let pkgId = Package.PackageIdentifier
                            { pkgName = Package.PackageName nameString
                            , pkgVersion = version }
                  pkgInfos = lookupSourcePackageId pkgIndex pkgId
              case pkgInfos of
                InstalledPackageInfo{haddockInterfaces = hIn : _} : _ -> do
                  -- Since the licence file path can't be specified
                  -- in InstalledPackageInfo, we can only guess what it is
                  -- and we do that on the basis of the haddock interfaces path.
                  -- TODO: on next rewrite (and re-testing), base it on htmldir
                  let candidatePaths =
                        [ iterate takeDirectory hIn !! 2
                            -- covers cabal default case: htmldir = docdir/html
                        , takeDirectory hIn
                            -- covers case where htmldir = docdir, for in-place
                        , iterate takeDirectory hIn !! 5
                            -- covers another case for in-place packages
                        ]
                      tryPaths (p : ps) = do
                        -- The directory of the licence file is ignored
                        -- in installed packages, hence @takeFileName@.
                        let loc = p </> takeFileName lf
                        exists <- doesFileExist loc
                        if exists then do
                          bs <- BSL.readFile loc
                          BSL.hPut licensesFile bs
                        else tryPaths ps
                      tryPaths [] = do
                        let errorMsg =
                              "Package " ++ nameString
                              ++ " has no license file in path "
                              ++ concat (intersperse " nor " candidatePaths)
                              ++ ". Haddock interfaces path (from, e.g., --haddockdir or --docdir) is "
                              ++ hIn ++ "."
                        hPutStrLn licensesFile errorMsg
                        hPutStrLn stderrLog errorMsg
                  tryPaths candidatePaths
                _ -> do
                  let errorMsg = "Package " ++ nameString
                                 ++ " not properly installed."
                                 ++ "\n" ++ show pkgInfos
                  hPutStrLn licensesFile errorMsg
                  hPutStrLn stderrLog errorMsg
            ParseOk _warns (l, Nothing, mauthor) -> do
              -- outputWarns warns  -- false positives
              when (isNothing l) $ do
                let warnMsg =
                      "WARNING: Package " ++ packageFile
                      ++ " has no license nor license file specified."
                hPutStrLn stdoutLog warnMsg
              let license = fromMaybe BSD3 l
                  author = fromMaybe "???" mauthor
              ms <- licenseText license author
              case ms of
                Nothing -> do
                  let errorMsg = "No license text can be found for package "
                                 ++ nameString ++ "."
                  hPutStrLn licensesFile errorMsg
                  hPutStrLn stderrLog errorMsg
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
                        ++ ". Reproducing standard license text."
                  hPutStrLn stdoutLog warnMsg

  res <- Ex.try $ mapM_ f (zip pkgs [1..])
  hClose stdoutLog
  hClose stderrLog
  hClose licensesFile
  let handler :: Ex.IOException -> IO ExitCode
      handler e = do
        let msg = "Licenses concatenation failed. The exception is:\n"
                  ++ show e
        writeFile stderrLogFN msg
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
      return $ (nub $ sort $ loadedMs, nub $ sort $ concat imps)

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

    (Apache (Just (Version {versionBranch = [2, 0]})))
      -> Just apache20

    _ -> Nothing

getYear :: IO Integer
getYear = do
  u <- getCurrentTime
  z <- getCurrentTimeZone
  let l = utcToLocalTime z u
      (y, _, _) = toGregorian $ localDay l
  return y

generateMacros :: PackageDBStack -> [FilePath] -> IO String
generateMacros configPackageDBStack configExtraPathDirs = do
  let verbosity = silent
  (ghcPkg, _) <- requireProgram verbosity ghcPkgProgram
                                (defaultProgramConfiguration configExtraPathDirs)
  pkgidss <- mapM (HcPkg.list verbosity ghcPkg) configPackageDBStack
  let newestPkgs = map last . groupBy ((==) `on` Package.packageName) . sort . concat $ pkgidss
  return $ generatePackageVersionMacros newestPkgs

defaultProgramConfiguration :: [FilePath] -> Cabal.Program.ProgramConfiguration
defaultProgramConfiguration configExtraPathDirs =
  Cabal.Program.setProgramSearchPath
    ( Cabal.Program.ProgramSearchPathDefault
    : map Cabal.Program.ProgramSearchPathDir configExtraPathDirs )
  Cabal.Program.defaultProgramConfiguration

localBuildInfo :: FilePath -> PackageDBStack -> [FilePath] -> LocalBuildInfo
localBuildInfo buildDir withPackageDB configExtraPathDirs = LocalBuildInfo
  { withPackageDB
  , withOptimization = Simple.Compiler.NormalOptimisation
  , compiler = Simple.Compiler.Compiler
      { compilerId = Compiler.CompilerId Compiler.GHC (Version [7, 4, 2] [])
      , compilerLanguages  = undefined
      , compilerExtensions = undefined
      }
  , configFlags = undefined
  , extraConfigArgs = undefined
  , installDirTemplates = undefined
  , hostPlatform = buildPlatform
  , buildDir
  , scratchDir = undefined
  , componentsConfigs = undefined
  , installedPkgs = undefined
  , pkgDescrFile = undefined
  , localPkgDescr = undefined
  , withPrograms = defaultProgramConfiguration configExtraPathDirs
  , withVanillaLib = undefined
  , withProfLib = False
  , withSharedLib = False
  , withDynExe = undefined
  , withProfExe = undefined
  , withGHCiLib = undefined
  , splitObjs = undefined
  , stripExes = undefined
  , progPrefix = undefined
  , progSuffix = undefined
  }

-- | Run gcc via ghc, with correct parameters.
-- Copied from bits and pieces of @Distribution.Simple.GHC@.
runComponentCc :: PackageDBStack -> [FilePath]
               -> FilePath -> FilePath -> FilePath -> FilePath
               -> IO (ExitCode, String, String)
runComponentCc configPackageDBStack configExtraPathDirs
               ideDistDir absC absObj pref = do
  let verbosity = silent
      -- TODO: create dist.23412/build? see cabalMacrosLocation
      buildDir = ideDistDir
      lbi = localBuildInfo buildDir configPackageDBStack configExtraPathDirs
      libBi = emptyBuildInfo
                -- of these, only includeDirs and ccOptions are used,
                -- but we don't set them for GHC API so far
      clbi = LibComponentLocalBuildInfo [] []
               -- a stub, this would be expensive (lookups in pkgIndex);
               -- TODO: is it needed? e.g., for C calling into Haskell?
      vanillaCcOpts = (componentCcGhcOptions verbosity lbi
                         libBi clbi pref absC)`mappend` mempty {
                        -- ghc ignores -odir for .o files coming from .c files
                        ghcOptExtra = ["-o", absObj]
                      }
      profCcOpts    = vanillaCcOpts `mappend` mempty {
                        ghcOptProfilingMode = Setup.toFlag True,
                        ghcOptObjSuffix     = Setup.toFlag "p_o"
                      }
      sharedCcOpts   = vanillaCcOpts `mappend` mempty {
                         ghcOptFPic        = Setup.toFlag True,
                         ghcOptDynLinkMode = Setup.toFlag GhcDynamicOnly,
                         ghcOptObjSuffix   = Setup.toFlag "dyn_o",
                         ghcOptExtra = ["-o", replaceExtension absObj "dyn_o"]
                       }
      odir          = Setup.fromFlag (ghcOptObjDir vanillaCcOpts)

  let stdoutLog = ideDistDir </> "ide-backend-cc.stdout"
      stderrLog = ideDistDir </> "ide-backend-cc.stderr"
  exitCode :: Either ExitCode () <- Ex.bracket
    (do stdOutputBackup <- redirectStdOutput stdoutLog
        stdErrorBackup  <- redirectStdError  stderrLog
        return (stdOutputBackup, stdErrorBackup))
    (\(stdOutputBackup, stdErrorBackup) -> do
        restoreStdOutput stdOutputBackup
        restoreStdError  stdErrorBackup)
    (\_ -> Ex.try $ do
        createDirectoryIfMissingVerbose verbosity True odir
        (ghcProg, _) <- requireProgram
                          verbosity Cabal.Program.ghcProgram (withPrograms lbi)
        let runGhcProg = runGHC verbosity ghcProg
        runGhcProg vanillaCcOpts

        isGhcDynamic <- ghcDynamic minBound ghcProg
        let doingTH = EnableExtension TemplateHaskell `elem` allExtensions libBi  -- TODO
            forceSharedLib = doingTH && isGhcDynamic
            -- TH always needs default libs, even when building for profiling
            whenProfLib = when (withProfLib lbi)
            whenSharedLib forceShared = when (forceShared || withSharedLib lbi)

        whenSharedLib forceSharedLib (runGhcProg sharedCcOpts)
        whenProfLib (runGhcProg profCcOpts))
  sout <- readFile stdoutLog
  serr <- readFile stderrLog
  let exitC = either id (const ExitSuccess) exitCode
  return (exitC, sout, serr)
