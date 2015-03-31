{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module IdeSession.Cabal (
    buildDeps, externalDeps
  , configureAndBuild, configureAndHaddock
  , generateMacros, buildDotCabal
  , runComponentCc
  , BuildExeArgs(..), RunCcArgs(..), LicenseArgs(..)
  , ExeCabalRequest(..), ExeCabalResponse(..)
  , buildLicsFromPkgs
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Data.Binary
import Data.Function (on)
import Data.List hiding (find)
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Monoid (Monoid(..))
import Data.Time
import Data.Typeable (Typeable)
import Data.Version (Version (..), parseVersion)
import System.Directory (removeFile, doesFileExist)
import System.Exit (ExitCode (ExitSuccess, ExitFailure), exitFailure)
import System.FilePath
import System.FilePath.Find (find, always, extension)
import System.IO
import System.IO.Error (isUserError, catchIOError)
import System.IO.Temp (createTempDirectory)
import Text.ParserCombinators.ReadP (readP_to_S)
import qualified Control.Exception          as Ex
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Map                   as Map
import qualified Data.Text                  as Text
import qualified Language.Haskell.Extension as Haskell
import qualified Language.Haskell.TH.Syntax as TH

import Distribution.License (License (..))
import Distribution.PackageDescription
import Distribution.PackageDescription.PrettyPrint (showGenericPackageDescription)
import Distribution.ParseUtils
import Distribution.Simple (PackageDBStack)
import Distribution.Simple.Build.Macros
import Distribution.Simple.Configure (configure)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo)
import Distribution.Simple.PackageIndex ( lookupSourcePackageId )
import Distribution.Simple.PreProcess (PPSuffixHandler)
import Distribution.Simple.Program.Builtin (ghcPkgProgram)
import Distribution.Simple.Program.Db (configureAllKnownPrograms, requireProgram)
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose)
import Distribution.System (buildPlatform)
import Distribution.Verbosity (silent)
import Distribution.Version (anyVersion, thisVersion)
import qualified Distribution.Compiler              as Compiler
import qualified Distribution.InstalledPackageInfo  as InstInfo
import qualified Distribution.ModuleName
import qualified Distribution.Package               as Package
import qualified Distribution.Simple.Build          as Build
import qualified Distribution.Simple.Compiler       as Simple.Compiler
import qualified Distribution.Simple.GHC            as GHC
import qualified Distribution.Simple.Haddock        as Haddock
import qualified Distribution.Simple.LocalBuildInfo as BuildInfo
import qualified Distribution.Simple.Program        as Cabal.Program
import qualified Distribution.Simple.Program.GHC    as GHC
import qualified Distribution.Simple.Program.HcPkg  as HcPkg
import qualified Distribution.Simple.Setup          as Setup
import qualified Distribution.Text
import qualified Distribution.Utils.NubList         as NubList

import IdeSession.GHC.API (cExtensions, cHeaderExtensions)
import IdeSession.Licenses ( bsd3, gplv2, gplv3, lgpl2, lgpl3, apache20 )
import IdeSession.State
import IdeSession.Strict.Container
import IdeSession.Strict.Maybe (just)
import IdeSession.Types.Progress
import IdeSession.Types.Public
import IdeSession.Types.Translation
import IdeSession.Util
import qualified IdeSession.Strict.List as StrictList
import qualified IdeSession.Strict.Map  as StrictMap

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
  , licenseFiles   = []
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
    { buildable       = True
    , defaultLanguage = Just Haskell.Haskell2010
    , options         = [(Simple.Compiler.GHC, realGhcOptions)]
    , ccOptions       = actuallyCcOptions
    , otherExtensions = [Haskell.EnableExtension Haskell.TemplateHaskell]  -- TODO: specify in SessionConfig?
    , hsSourceDirs
    , cSources
    , installIncludes
    }
  where
    -- Cabal does not pass ghc-options to ghc when compiling C code, so we
    -- must split these options out and explicitly pass them as options for the
    -- C compiler.
    --
    -- See https://github.com/fpco/ide-backend/issues/218 and
    -- https://github.com/haskell/cabal/pull/2043
    actuallyCcOptions, realGhcOptions :: [String]
    (actuallyCcOptions, realGhcOptions) =
      let (cOpts, hsOpts) = partition isCcOpt ghcOpts
      in (map (drop 5) cOpts, hsOpts)

    isCcOpt :: String -> Bool
    isCcOpt = isPrefixOf "-optc"

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

-- TODO: this works OK if @(m, path)@ are simple, @updateRelativeIncludes@
-- includes @""@ and/or the @Main@ module is in @Main.hs@.
-- For more complex cases we'd need extensive exotic tests and then perhaps
-- also more complex code that searches @updateRelativeIncludes@ paths, etc.
-- I'd rather wait until we fix cabal/GHC not to require "Main" in "./Main.hs"
-- and then simplify this and other code instead.
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
    { exposedModules     = ms
    , libExposed         = False
    , libBuildInfo       = bInfo ideSourcesDir ghcOpts cSources cHeaders
      -- These are new fields. TODO: Do we give them the right values?
    , reexportedModules  = []
    , requiredSignatures = []
    , exposedSignatures  = []
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

mkConfFlags :: FilePath -> PackageDBStack -> [FilePath] -> Setup.ConfigFlags
mkConfFlags ideDistDir configPackageDBStack progPathExtra =
  (Setup.defaultConfigFlags (defaultProgramConfiguration progPathExtra))
    { Setup.configDistPref = Setup.Flag ideDistDir
    , Setup.configUserInstall = Setup.Flag False
    , Setup.configVerbosity = Setup.Flag minBound
      -- @Nothing@ wipes out default, initial DBs.
    , Setup.configPackageDBs = Nothing : map Just configPackageDBStack
    , Setup.configProgramPathExtra = NubList.toNubList progPathExtra
    }

configureAndBuild :: BuildExeArgs
                  -> [(ModuleName, FilePath)]
                  -> IO ExitCode
configureAndBuild BuildExeArgs{ bePackageDBStack   = configPackageDBStack
                              , beExtraPathDirs    = configExtraPathDirs
                              , beSourcesDir       = ideSourcesDir
                              , beDistDir          = ideDistDir
                              , beRelativeIncludes = relativeIncludes
                              , beGhcOpts          = ghcOpts
                              , beLibDeps          = libDeps
                              , beLoadedMs         = loadedMs
                              , .. } ms = do
  -- We need to make sure the user never loses any error message.
  -- Therefore we never wipe out such files in the midst of ide-backend
  -- operation, but only at the start of user-triggered commands,
  -- after he had full access to the previous content of the files.
  beStderrLogExists <- doesFileExist beStderrLog
  when beStderrLogExists $ removeFile beStderrLog
  let mainDep = Package.Dependency pkgNameMain anyVersion
      exeDeps = mainDep : libDeps
      sourcesDirs = map (\path -> ideSourcesDir </> path)
                        relativeIncludes
  executables <-
    mapM (exeDesc sourcesDirs [ideSourcesDir] ideDistDir ghcOpts) ms
  let condExe exe = (exeName exe, CondNode exe exeDeps [])
      condExecutables = map condExe executables
      mainFileExistsInDir dir = do
        hsFound  <- doesFileExist $ dir </> "Main.hs"
        lhsFound <- doesFileExist $ dir </> "Main.lhs"
        return $! hsFound || lhsFound
  mainFileChecks <- mapM mainFileExistsInDir sourcesDirs
  let mainFileExists = or mainFileChecks
  -- Cabal can't find the code of @Main@ (to be used as the main executable
  -- module) in subdirectories or in @Foo.hs@. We need a @Main@ to build
  -- an executable, so any other @Main@ modules have to be ignored.
  -- So, if another module depends on such a @Main@,
  -- we're in trouble, but if the @Main@ is only an executable, we are fine.
  -- Michael said in https://github.com/fpco/fpco/issues/1049
  -- "We'll be handling the disambiguation of Main modules ourselves before
  -- passing the files to you, so that shouldn't be an ide-backend concern.",
  -- so perhaps there won't be any problems.
  let soundMs | mainFileExists = loadedMs
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
      confFlags = mkConfFlags ideDistDir configPackageDBStack configExtraPathDirs
      -- We don't override most build flags, but use configured values.
      buildFlags = Setup.defaultBuildFlags
                     { Setup.buildDistPref = Setup.Flag ideDistDir
                     , Setup.buildVerbosity = Setup.Flag $ toEnum 1
                     }
      preprocessors :: [PPSuffixHandler]
      preprocessors = []
      hookedBuildInfo = (Nothing, [])  -- we don't want to use hooks
  let confAndBuild = do
        lbi <- configure (gpDesc, hookedBuildInfo) confFlags
        -- Setting @withPackageDB@ here is too late, @configure@ would fail
        -- already. Hence we set it in @mkConfFlags@ (can be reverted,
        -- when/if we construct @lbi@ without @configure@).
        Build.build (BuildInfo.localPkgDescr lbi) lbi buildFlags preprocessors
  -- Handle various exceptions and stderr printouts.
  exitCode :: Either ExitCode () <- redirectStderr beStderrLog $
    Ex.try $ catchIOError confAndBuild $ \e ->
      if isUserError e
        then do
          -- In the new cabal code some exceptions are handled with 'die',
          -- raising a user error, while some still do 'exit 1' and print to
          -- stderr. For uniformity, we redirect user errors to stderr, as
          -- well.
          hPutStrLn stderr $ "Exception caught:"
          hPutStrLn stderr $ show e
          exitFailure
        else
          ioError e
  return $! either id (const ExitSuccess) exitCode

configureAndHaddock :: BuildExeArgs
                    -> IO ExitCode
configureAndHaddock BuildExeArgs{ bePackageDBStack = configPackageDBStack
                                , beExtraPathDirs = configExtraPathDirs
                                , beSourcesDir = ideSourcesDir
                                , beDistDir = ideDistDir
                                , beRelativeIncludes = relativeIncludes
                                , beGhcOpts = ghcOpts
                                , beLibDeps = libDeps
                                , beLoadedMs = loadedMs
                                , .. } = do
  beStderrLogExists <- doesFileExist beStderrLog
  when beStderrLogExists $ removeFile beStderrLog
  let condExecutables = []
      sourcesDirs = map (\path -> ideSourcesDir </> path)
                        relativeIncludes
      mainFileExistsInDir dir = do
        hsFound  <- doesFileExist $ dir </> "Main.hs"
        lhsFound <- doesFileExist $ dir </> "Main.lhs"
        return $! hsFound || lhsFound
  mainFileChecks <- mapM mainFileExistsInDir sourcesDirs
  let mainFileExists = or mainFileChecks
  -- Cabal can't find the code of @Main@, except in the main dir.
  -- See the comment above.
  let soundMs | mainFileExists = loadedMs
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
      confFlags =
        mkConfFlags ideDistDir configPackageDBStack configExtraPathDirs
      preprocessors :: [PPSuffixHandler]
      preprocessors = []
      haddockFlags = Setup.defaultHaddockFlags
        { Setup.haddockDistPref = Setup.Flag ideDistDir
        , Setup.haddockHtml = Setup.Flag True
        , Setup.haddockHoogle = Setup.Flag True
        , Setup.haddockVerbosity = Setup.Flag minBound
        }
      hookedBuildInfo = (Nothing, [])  -- we don't want to use hooks
  let confAndBuild = do
        lbi <- configure (gpDesc, hookedBuildInfo) confFlags
        Haddock.haddock (BuildInfo.localPkgDescr lbi) lbi preprocessors haddockFlags
  -- Handle various exceptions and stderr printouts.
  exitCode :: Either ExitCode () <- redirectStderr beStderrLog $
    Ex.try $ catchIOError confAndBuild $ \e ->
      if isUserError e
        then do
          -- In the new cabal code some exceptions are handled with 'die',
          -- raising a user error, while some still do 'exit 1' and print to
          -- stderr. For uniformity, we redirect user errors to stderr, as
          -- well.
          hPutStrLn stderr $ "Exception caught:"
          hPutStrLn stderr $ show e
          exitFailure
        else
          ioError e
  return $! either id (const ExitSuccess) exitCode

buildDotCabal :: FilePath -> [FilePath] -> [String] -> Computed
              -> IO (String -> Version -> BSL.ByteString)
buildDotCabal ideSourcesDir relativeIncludes ghcOpts computed = do
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
                     (filter (/= "") relativeIncludes) [ideSourcesDir]
                     ghcOpts projectMs
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

-- | Build the concatenation of all license files from a given list
-- of packages. See 'buildLicenses'.
buildLicsFromPkgs :: Bool -> LicenseArgs
                  -> IO ExitCode
buildLicsFromPkgs logProgress
                  LicenseArgs{ liPackageDBStack = configPackageDBStack
                             , liExtraPathDirs  = configExtraPathDirs
                             , liLicenseExc     = configLicenseExc
                             , liDistDir        = ideDistDir
                             , liStdoutLog      = stdoutLogFN
                             , liStderrLog      = stderrLogFN
                             , licenseFixed
                             , liCabalsDir      = cabalsDir
                             , liPkgs           = pkgs
                             } = do
  -- Note that @liStderrLog@ is removed in @openBinaryFile@ below.
  -- The following computations are very expensive, so should be done once,
  -- instead of at each invocation of @findLicense@ that needs to perform
  -- @lookupSourcePackageId@.
  programDB <- configureAllKnownPrograms  -- won't die
                 minBound (defaultProgramConfiguration configExtraPathDirs)
  pkgIndex <- GHC.getInstalledPackages minBound configPackageDBStack programDB
  let licensesFN  = ideDistDir </> "licenses.txt"     -- result
  stderrLog <- openBinaryFile stderrLogFN WriteMode
  licensesFile <- openBinaryFile licensesFN WriteMode
  -- The file containing concatenated licenses for core components.
  let bsCore = BSL8.pack $(TH.runIO (BSL.readFile "CoreLicenses.txt") >>= TH.lift . BSL8.unpack)
  BSL.hPut licensesFile bsCore

  let numSteps        = length pkgs
      mainPackageName = Text.pack "main"
      printProgress step packageName =
        when logProgress $
          putStrLn $ "[" ++ show step ++ " of " ++ show numSteps
                     ++ "] " ++ Text.unpack packageName

      f :: (PackageId, Int) -> IO ()
      f (PackageId{packageName}, step) | packageName == mainPackageName =
        printProgress step packageName
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
              hPutStrLn stderrLog warnMsg
        cabalFileExists <- doesFileExist packageFile
        if cabalFileExists then do
          hPutStrLn licensesFile $ "\nLicense for " ++ nameString ++ ":\n"
          pkgS <- readFile packageFile
          let parseResult =
                parseFields lFieldDescrs (Nothing, Nothing, Nothing) pkgS
          findLicense parseResult nameString version packageFile
        else case lookup nameString licenseFixed of
          Just fixedLicense -> do
            hPutStrLn licensesFile $ "\nLicense for " ++ nameString ++ ":\n"
            let fakeParseResult = ParseOk undefined fixedLicense
            findLicense fakeParseResult nameString version packageFile
          Nothing ->
            unless (nameString `elem` configLicenseExc) $ do
              let errorMsg = "No .cabal file provided for package "
                             ++ nameString ++ " so no license can be found."
              hPutStrLn licensesFile errorMsg
              hPutStrLn stderrLog errorMsg
        printProgress step packageName

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
                InstInfo.InstalledPackageInfo{InstInfo.haddockInterfaces = hIn : _} : _ -> do
                  -- Since the license file path can't be specified
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
                        -- The directory of the license file is ignored
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
                hPutStrLn stderrLog warnMsg
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
                  hPutStrLn stderrLog warnMsg

  res <- Ex.try $ mapM_ f (zip pkgs [1..])
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
  (_ghcPkg, ghcConf) <- requireProgram verbosity ghcPkgProgram
                                (defaultProgramConfiguration configExtraPathDirs)
  let hcPkgInfo = GHC.hcPkgInfo ghcConf
  pkgidss <- mapM (HcPkg.list hcPkgInfo verbosity) configPackageDBStack
  let newestPkgs = map last . groupBy ((==) `on` Package.packageName) . sort . concat $ pkgidss
  return $ generatePackageVersionMacros newestPkgs

defaultProgramConfiguration :: [FilePath] -> Cabal.Program.ProgramConfiguration
defaultProgramConfiguration configExtraPathDirs =
  Cabal.Program.setProgramSearchPath
    ( Cabal.Program.ProgramSearchPathDefault
    : map Cabal.Program.ProgramSearchPathDir configExtraPathDirs )
  Cabal.Program.defaultProgramConfiguration

localBuildInfo :: FilePath -> PackageDBStack -> [FilePath] -> LocalBuildInfo
localBuildInfo buildDir withPackageDB configExtraPathDirs = BuildInfo.LocalBuildInfo
  { BuildInfo.withPackageDB
  , BuildInfo.withOptimization    = Simple.Compiler.NormalOptimisation
  , BuildInfo.hostPlatform        = buildPlatform
  , BuildInfo.withPrograms        = defaultProgramConfiguration configExtraPathDirs
  , BuildInfo.withProfLib         = False
  , BuildInfo.withSharedLib       = False
  , BuildInfo.compiler            = Simple.Compiler.Compiler
      -- TODO: Why is it okay that we always say 7.4.2 here?
      { compilerId         = Compiler.CompilerId Compiler.GHC (Version [7, 4, 2] [])
      , compilerLanguages  = error "compilerLanguages not defined"
      , compilerExtensions = error "compilerExtensions not defined"
        -- TOOD: new fields
      , compilerAbiTag     = error "compilerAbiTag not defined"
      , compilerCompat     = error "compilerCompat not defined"
      , compilerProperties = Map.empty -- Must be defined
      }
  , BuildInfo.buildDir
  , BuildInfo.configFlags         = error "BuildInfo.configFlags not defined"
  , BuildInfo.extraConfigArgs     = error "BuildInfo.extraConfigArgs not defined"
  , BuildInfo.installDirTemplates = error "BuildInfo.installDirTemplates not defined"
  , BuildInfo.componentsConfigs   = error "BuildInfo.componentsConfigs not defined"
  , BuildInfo.installedPkgs       = error "BuildInfo.installedPkgs not defined"
  , BuildInfo.pkgDescrFile        = error "BuildInfo.pkgDescrFile not defined"
  , BuildInfo.localPkgDescr       = error "BuildInfo.localPkgDescr not defined"
  , BuildInfo.withVanillaLib      = error "BuildInfo.withVanillaLib not defined"
  , BuildInfo.withDynExe          = error "BuildInfo.withDynExe not defined"
  , BuildInfo.withProfExe         = error "BuildInfo.withProfExe not defined"
  , BuildInfo.withGHCiLib         = error "BuildInfo.withGHCiLib not defined"
  , BuildInfo.splitObjs           = error "BuildInfo.splitObjs not defined"
  , BuildInfo.stripExes           = error "BuildInfo.stripExes not defined"
  , BuildInfo.progPrefix          = error "BuildInfo.progPrefix not defined"
  , BuildInfo.progSuffix          = error "BuildInfo.progSuffix not defined"
  -- TODO: New fields
  , BuildInfo.pkgKey              = error "BuildInfo.pkgKey not defined"
  , BuildInfo.instantiatedWith    = error "BuildInfo.instantiatedWith not defined"
  , BuildInfo.withDebugInfo       = Simple.Compiler.NoDebugInfo -- must be defined
  , BuildInfo.stripLibs           = error "BuildInfo.stripLibs not defined"
  , BuildInfo.relocatable         = error "BuildInfo.relocatable not defined"
  }

-- | Run gcc via ghc, with correct parameters.
-- Copied from bits and pieces of @Distribution.Simple.GHC@.
runComponentCc :: RunCcArgs -> IO ExitCode
runComponentCc RunCcArgs{ rcPackageDBStack = configPackageDBStack
                        , rcExtraPathDirs  = configExtraPathDirs
                        , rcDistDir        = ideDistDir
                        , rcAbsC           = absC
                        , rcAbsObj         = absObj
                        , rcPref           = pref
                        , rcIncludeDirs    = includeDirs
                        , .. }             = do
    rcStderrLogExists <- doesFileExist rcStderrLog
    when rcStderrLogExists $ removeFile rcStderrLog

    exitCode <- redirectStderr rcStderrLog $
      Ex.try $ do
        createDirectoryIfMissingVerbose verbosity True odir
        (ghcProg, _) <- requireProgram
                          verbosity Cabal.Program.ghcProgram (BuildInfo.withPrograms lbi)
        let runGhcProg = GHC.runGHC verbosity ghcProg comp
        runGhcProg vanillaCcOpts

        -- TH always needs default libs, even when building for profiling
        -- TODO: Should we detect the use of TH in a different way?
        let doingTH        = Haskell.EnableExtension Haskell.TemplateHaskell
                               `elem` allExtensions libBi
            isGhcDynamic   = GHC.isDynamic comp
            forceSharedLib = doingTH && isGhcDynamic
            whenProfLib    = when (BuildInfo.withProfLib lbi)
            whenSharedLib forceShared = when (forceShared || BuildInfo.withSharedLib lbi)

        whenSharedLib forceSharedLib (runGhcProg sharedCcOpts)
        whenProfLib (runGhcProg profCcOpts)
    return $! either id (\() -> ExitSuccess) exitCode
  where
    verbosity = silent
    buildDir  = ideDistDir -- TODO: create dist.23412/build? see cabalMacrosLocation
    lbi       = localBuildInfo buildDir configPackageDBStack configExtraPathDirs
    comp      = BuildInfo.compiler lbi
    libBi     = emptyBuildInfo{includeDirs} -- TODO: set ccOptions?
    odir      = Setup.fromFlag (GHC.ghcOptObjDir vanillaCcOpts)

    -- a stub, this would be expensive (lookups in pkgIndex);
    -- TODO: is it needed? e.g., for C calling into Haskell?
    clbi      = BuildInfo.LibComponentLocalBuildInfo [] [] Map.empty []

    -- Construct CC options for various kinds of flavours
    vanillaCcOpts = (GHC.componentCcGhcOptions verbosity lbi
                       libBi clbi pref absC) `mappend` mempty {
                      -- ghc ignores -odir for .o files coming from .c files
                      GHC.ghcOptExtra = NubList.toNubListR $ ["-o", absObj] ++ rcOptions,
                      GHC.ghcOptFPic  = Setup.toFlag True
                    }
    profCcOpts    = vanillaCcOpts `mappend` mempty {
                      GHC.ghcOptProfilingMode = Setup.toFlag True,
                      GHC.ghcOptObjSuffix     = Setup.toFlag "p_o"
                    }
    sharedCcOpts  = vanillaCcOpts `mappend` mempty {
                      GHC.ghcOptDynLinkMode = Setup.toFlag GHC.GhcDynamicOnly,
                      GHC.ghcOptObjSuffix   = Setup.toFlag "dyn_o",
                      GHC.ghcOptExtra       = NubList.toNubListR ["-o", replaceExtension absObj "dyn_o"]
                    }

data BuildExeArgs = BuildExeArgs
  { bePackageDBStack   :: PackageDBStack
  , beExtraPathDirs    :: [FilePath]
  , beSourcesDir       :: FilePath
  , beDistDir          :: FilePath
  , beStdoutLog        :: FilePath
  , beStderrLog        :: FilePath
  , beRelativeIncludes :: [FilePath]
  , beGhcOpts          :: [String]
  , beLibDeps          :: [Package.Dependency]
  , beLoadedMs         :: [ModuleName]
  }

data RunCcArgs = RunCcArgs
  { rcPackageDBStack :: PackageDBStack
  , rcExtraPathDirs  :: [FilePath]
  , rcDistDir        :: FilePath
  , rcStdoutLog      :: FilePath
  , rcStderrLog      :: FilePath
  , rcAbsC           :: FilePath
  , rcAbsObj         :: FilePath
  , rcPref           :: FilePath
  , rcIncludeDirs    :: [FilePath]
  , rcOptions        :: [String]
  }

data LicenseArgs = LicenseArgs
  { -- | 3 fields from session configuration
    liPackageDBStack :: PackageDBStack
  , liExtraPathDirs  :: [FilePath]
  , liLicenseExc     :: [String]
    -- | the working directory; the resulting file is written there
  , liDistDir        :: FilePath
  , liStdoutLog      :: FilePath
  , liStderrLog      :: FilePath
    -- | see 'configLicenseFixed'
  , licenseFixed     :: [( String
                         , (Maybe License, Maybe FilePath, Maybe String)
                         )]
    -- | the directory with all the .cabal files
  , liCabalsDir      :: FilePath
    -- | the list of packages to process
  , liPkgs           :: [PackageId]
  }

data ExeCabalRequest =
    ReqExeBuild BuildExeArgs [(ModuleName, FilePath)]
  | ReqExeDoc BuildExeArgs
  | ReqExeCc RunCcArgs
  | ReqExeLic LicenseArgs
  deriving Typeable

data ExeCabalResponse =
    ExeCabalProgress Progress
  | ExeCabalDone ExitCode
  deriving Typeable

instance Binary ExeCabalRequest where
  put (ReqExeBuild buildArgs ms) = putWord8 0 >> put buildArgs >> put ms
  put (ReqExeDoc buildArgs)      = putWord8 1 >> put buildArgs
  put (ReqExeCc ccArgs)          = putWord8 2 >> put ccArgs
  put (ReqExeLic licenseArgs)    = putWord8 3 >> put licenseArgs

  get = do
    header <- getWord8
    case header of
      0 -> ReqExeBuild <$> get <*> get
      1 -> ReqExeDoc   <$> get
      2 -> ReqExeCc    <$> get
      3 -> ReqExeLic   <$> get
      _ -> fail "ExeCabalRequest.get: invalid header"

instance Binary ExeCabalResponse where
  put (ExeCabalProgress progress) = putWord8 0 >> put progress
  put (ExeCabalDone exitCode)     = putWord8 1 >> put exitCode

  get = do
    header <- getWord8
    case header of
      0 -> ExeCabalProgress <$> get
      1 -> ExeCabalDone <$> get
      _ -> fail "ExeCabalResponse.get: invalid header"

instance Binary BuildExeArgs where
  put BuildExeArgs{..} = do
    put bePackageDBStack
    put beExtraPathDirs
    put beSourcesDir
    put beDistDir
    put beStdoutLog
    put beStderrLog
    put beRelativeIncludes
    put beGhcOpts
    put beLibDeps
    put beLoadedMs

  get = BuildExeArgs <$> get <*> get <*> get
                     <*> get <*> get <*> get
                     <*> get <*> get <*> get <*> get

instance Binary RunCcArgs where
  put RunCcArgs{..} = do
    put rcPackageDBStack
    put rcExtraPathDirs
    put rcDistDir
    put rcStdoutLog
    put rcStderrLog
    put rcAbsC
    put rcAbsObj
    put rcPref
    put rcIncludeDirs
    put rcOptions

  get = RunCcArgs <$> get <*> get <*> get
                  <*> get <*> get <*> get
                  <*> get <*> get <*> get
                  <*> get

instance Binary LicenseArgs where
  put LicenseArgs{..} = do
    put liPackageDBStack
    put liExtraPathDirs
    put liLicenseExc
    put liDistDir
    put liStdoutLog
    put liStderrLog
    put licenseFixed
    put liCabalsDir
    put liPkgs

  get = LicenseArgs <$> get <*> get <*> get
                    <*> get <*> get <*> get
                    <*> get <*> get <*> get

instance Binary ExitCode where
  put ExitSuccess = putWord8 0
  put (ExitFailure code) = putWord8 1 >> put code

  get = do
    header <- getWord8
    case header of
      0 -> return ExitSuccess
      1 -> ExitFailure <$> get
      _ -> fail "ExitCode.get: invalid header"
