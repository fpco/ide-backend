{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TypeSynonymInstances, FlexibleInstances, RecordWildCards, OverlappingInstances, OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar
import qualified Control.Exception as Ex
import Control.Monad
import Control.DeepSeq (rnf)
import qualified Data.ByteString            as BSS
import qualified Data.ByteString.Char8      as BSSC
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Lazy.UTF8  as BSL8
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sort, elemIndex)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Monoid (mconcat, mempty, (<>))
import Data.Text (Text)
import Data.Char (isLower, isSpace)
import Data.Either (lefts)
import qualified Data.Text as Text
import Debug.Trace (traceEventIO)
import Data.Version (Version (..))
import Distribution.License (License (..))
import Distribution.Simple.Program.Find ( -- From our patched cabal
    ProgramSearchPath
  , findProgramOnSearchPath
  , ProgramSearchPathEntry(..)
  )
import Prelude hiding (mod, span)
import System.Directory
import qualified System.Environment as System.Environment
import System.Exit (ExitCode (..))
import System.FilePath
import System.FilePath.Find (always, extension, find)
import System.IO as IO
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess, readProcessWithExitCode)
import qualified System.Process as Process
import System.Random (randomRIO)
import System.Timeout (timeout)
import Text.Regex (mkRegex, subRegex)

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertBool, assertEqual, assertFailure, (@?=))
import Test.HUnit.Lang (HUnitFailure(..))

import Debug
import IdeSession hiding (defaultSessionConfig, defaultSessionInitParams)
import qualified IdeSession as IdeSession
import TestTools

-- Tests using various functions of the IdeSession API
-- and a variety of small test Haskell projects.

-- | Update the session with all modules of the given directory.
getModulesFrom :: IdeSession -> FilePath -> IO (IdeSessionUpdate (), [FilePath])
getModulesFrom session originalSourcesDir = do
  sourcesDir <- getSourcesDir session
  debug dVerbosity $ "\nCopying files from: " ++ originalSourcesDir
                     ++ " to: " ++ sourcesDir
  -- Send the source files from 'originalSourcesDir' to 'configSourcesDir'
  -- using the IdeSession's update mechanism.
  originalFiles <- find always
                        ((`elem` sourceExtensions) `liftM` extension)
                        originalSourcesDir
  let originalUpdate = updateCodeGeneration False
                    <> (mconcat $ map updateSourceFileFromFile originalFiles)
  return (originalUpdate, originalFiles)

getModules :: IdeSession -> IO (IdeSessionUpdate (), [FilePath])
getModules session = do
  sourcesDir <- getSourcesDir session
  getModulesFrom session sourcesDir

loadModulesFrom :: IdeSession -> FilePath -> IO ()
loadModulesFrom session originalSourcesDir =
  loadModulesFrom' session originalSourcesDir $ TargetsExclude []

loadModulesFrom' :: IdeSession -> FilePath -> Targets -> IO ()
loadModulesFrom' session originalSourcesDir targets = do
  (originalUpdate, lm) <- getModulesFrom session originalSourcesDir
  updateSessionD session (originalUpdate <> updateTargets targets) (length lm)

ifIdeBackendHaddockTestsEnabled :: SessionSetup -> (IdeSession -> IO ()) -> IO ()
ifIdeBackendHaddockTestsEnabled setup io = do
  wtsystem <- (System.Environment.getEnv "IDE_BACKEND_DISABLE_HADDOCK_TESTS")
               `Ex.catch` (\(_ :: Ex.IOException) -> return "1")
  unless (wtsystem == "0" || wtsystem == "False") $
    withSession setup io

type SessionSetup = ((SessionInitParams, SessionConfig) -> (SessionInitParams, SessionConfig))

-- | Run the specified action with a new IDE session, configured to use a
-- temporary directory
withSession :: SessionSetup -> (IdeSession -> IO a) -> IO a
withSession setup io =
    Ex.bracket (initSession initParams config) tryShutdownSession io
  where
    (initParams, config) = setup (defaultSessionInitParams, defaultSessionConfig)

    tryShutdownSession session = do
      mDidShutdown <- timeout 2000000 $ shutdownSession session
      case mDidShutdown of
        Just () -> return ()
        Nothing -> do putStrLn "WARNING: Failed to shutdown session (timeout)"
                      forceShutdownSession session

defaultSession :: SessionSetup
defaultSession = id

withOpts :: [String] -> SessionSetup
withOpts opts (initParams, config) = (
    initParams
  , config { configStaticOpts = configStaticOpts config ++ opts }
  )

withIncludes :: FilePath -> SessionSetup
withIncludes path (initParams, config) = (
    initParams
  , config { configRelativeIncludes = path : configRelativeIncludes config }
  )

withMacros :: BSL.ByteString -> SessionSetup
withMacros macros (initParams, config) = (
    initParams { sessionInitCabalMacros = Just macros }
  , config
  )

withModInfo :: Bool -> SessionSetup
withModInfo modInfo (initParams, config) = (
    initParams
  , config { configGenerateModInfo = modInfo }
  )

withDBStack :: PackageDBStack -> SessionSetup
withDBStack dbStack (initParams, config) = (
    initParams
  , config { configPackageDBStack = dbStack }
  )

-- Set of api calls and checks to perform on each project.
--
-- TODO: we need much more tests to recover the functionality of the old set,
-- and then we need to much more to test all API functions.
-- E.g., check that the values of Progress do not exceeed the number of files.
-- Also, check ModuleDelete and all the DataFileChange constructors,
-- getSourceModule an getDataFile.
multipleTests :: [(String, IdeSession -> IdeSessionUpdate () -> [String] -> Assertion)]
multipleTests =
  [ ( "Overwrite with error"
    , \session originalUpdate lm -> do
        updateSessionD session originalUpdate (length lm)
        -- No errors in the original test code.
        assertNoErrors session
        -- Overwrite one of the copied files.
        (_, ms) <- getModules session
        let update = loadModule (head ms) "a = unknownX"
        updateSessionD session update (length ms)  -- we don't know how many modules get recompiled
        assertSourceErrors' session ["Not in scope: `unknownX'"]
    )
  , ( "Overwrite with the same module name in all files"
    , \session originalUpdate lm -> do
        let upd m =
              updateSourceFile m (BSLC.pack "module Wrong where\na = 1")
            update = originalUpdate <> mconcat (map upd lm)
        updateSessionD session update 2
        if length lm >= 2
          then assertSourceErrors' session ["module `main:Wrong' is defined in multiple files"]
          else assertNoErrors session
    )
  , ( "Overwrite modules many times"
    , \session originalUpdate lm0 -> do
        let doubleUpdate = mempty <> originalUpdate <> originalUpdate <> mempty
        -- Updates are idempotent, so no errors and no recompilation.
        updateSessionD session doubleUpdate (length lm0)
        assertNoErrors session
        -- Overwrite one of the copied files with an error.
        (_, lm) <- getModules session
        let update1 =
              updateCodeGeneration False
              <> loadModule (head lm) "a = unknownX"
        updateSessionD session update1 (length lm)
        assertSourceErrors' session ["Not in scope: `unknownX'"]
        updateSessionD session mempty 1  -- was an error, so trying again
        assertSourceErrors' session ["Not in scope: `unknownX'"]
        -- Overwrite all files, many times, with correct code eventually.
        let upd m = loadModule m "x = unknownX"
                    <> loadModule m "y = 2"
                    <> updateCodeGeneration True
            update2 = mconcat $ map upd lm
        updateSessionD session update2 (length lm)
        assertNoErrors session
        -- Overwrite again with the error.
        updateSessionD session update1 1  -- drop bytecode, don't recompile
        assertSourceErrors' session ["Not in scope: `unknownX'"]
        assertRaises "runStmt session Main main"
          (== userError "Cannot run before the code is generated.")
          (runStmt session "Main" "main")
      )
    , ( "Run the sample code; succeed or raise an exception"
      , \session originalUpdate lm -> do
        updateSessionD session originalUpdate (length lm)
        let update = updateCodeGeneration True
        updateSessionD session update (length lm)  -- all recompiled
        assertNoErrors session
        mex <- Ex.try $ runStmt session "Main" "main"
        case mex of
          Right runActions -> void $ runWaitAll runActions
          Left ex -> assertEqual "runStmt" (userError "Module \"Main\" not successfully loaded, when trying to run code.") ex
      )
    , ( "Overwrite all with exception-less code and run it"
      , \session originalUpdate lm -> do
        let upd m = loadModule m "x = 1"
            update =
              originalUpdate
              <> updateSourceFile
                   "Central/TotallyMain.hs"
                   (BSLC.pack
                      "module Central.TotallyMain where\nmain = print \"test run\"")
              <> mconcat (map upd lm)
        let update2 = update <> updateCodeGeneration True
        -- Compile from scratch, generating code from the start.
        updateSessionD session update2 (length lm + 1)
        assertNoErrors session
        runActions <- runStmt session "Central.TotallyMain" "main"
        (output, result) <- runWaitAll runActions
        assertEqual "" RunOk result
        assertEqual "" output (BSLC.pack "\"test run\"\n")
      )
    , ( "Make sure deleting modules removes them from the directory"
      , \session originalUpdate lm -> do
        updateSessionD session originalUpdate (length lm)
        let updateDel = mconcat $ map updateSourceFileDelete lm
        updateSessionD session updateDel 0
        assertNoErrors session
        -- The updates cancel each other out.
        updateSessionD session (originalUpdate <> updateDel) 0
        let update2 = updateCodeGeneration True
        updateSessionD session update2 0  -- 0: nothing to generate code from
        assertNoErrors session
      )
    , ( "Make sure restartSession does not lose source files"
      , \session originalUpdate lm -> do
        let update = originalUpdate <> updateCodeGeneration True
        updateSessionD session update (length lm)
        serverBefore <- getGhcServer session
        mex <- Ex.try $ runStmt session "Main" "main"
        case mex of
          Right _runActions -> return ()  -- don't runWaitAll
          Left ex -> assertEqual "runStmt" (userError "Module \"Main\" not successfully loaded, when trying to run code.") ex
        restartSession session Nothing
        updateSessionD session mempty (length lm)  -- all compiled anew
        assertNoErrors session
        mex2 <- Ex.try $ runStmt session "Main" "main"
        case mex2 of
          Right runActions -> void $ runWaitAll runActions  -- now runWaitAll
          Left ex -> assertEqual "runStmt" (userError "Module \"Main\" not successfully loaded, when trying to run code.") ex
        restartSession session Nothing
        let update2 = mconcat $ map updateSourceFileDelete lm
        updateSessionD session update2 0  -- if any file missing, would yell
        assertNoErrors session
        let update3 = updateCodeGeneration True
        updateSessionD session update3 0  -- 0: nothing to generate code from
        exitCodeBefore <- getGhcExitCode serverBefore
        assertEqual "exitCodeBefore" (Just ExitSuccess) exitCodeBefore
      )
  ]

syntheticTests :: [(String, Assertion)]
syntheticTests = [
    ( "getGhcVersion"
    , withSession defaultSession $ \session -> do
        -- This is just a sanity check to make sure we get _a_ reply
        version <- getGhcVersion session
        assertBool ""  $ version == GHC742
                      || version == GHC78
    )
  , ( "Maintain list of compiled modules I"
    , withSession defaultSession $ \session -> do
        updateSessionD session (loadModule "XXX.hs" "a = 5") 1
        assertLoadedModules session "XXX" ["XXX"]
        updateSessionD session (loadModule "A.hs" "a = 5") 1
        assertLoadedModules session "[m1]" ["A", "XXX"]
        updateSessionD session (loadModule "A2.hs" "import A\na2 = A.a") 1
        assertLoadedModules session "[m1, m2]" ["A", "A2", "XXX"]
        updateSessionD session (loadModule "A3.hs" "") 1
        assertLoadedModules session "[m1, m2, m3]" ["A", "A2", "A3", "XXX"]
        updateSessionD session (loadModule "Wrong.hs" "import A4\na2 = A4.a + 1") 1
        assertLoadedModules session "wrong1" ["A", "A2", "A3", "XXX"]
        updateSessionD session (loadModule "Wrong.hs" "import A\na2 = A.a + c") 1
        assertLoadedModules session "wrong2" ["A", "A2", "A3", "XXX"]
        updateSessionD session (loadModule "A.hs" "a = c") 1
        -- Module "A" is compiled before "Wrong", fails, so it's invalidated
        -- and all modules that depend on it are invalidated. Module "Wrong"
        -- is never compiled.
        assertLoadedModules session "wrong3" ["A3", "XXX"]
    )
  , ( "Maintain list of compiled modules II"
    , withSession defaultSession $ \session -> do
        updateSessionD session (loadModule "XXX.hs" "a = 5") 1
        assertLoadedModules session "XXX" ["XXX"]
        updateSessionD session (loadModule "A.hs" "a = 5") 1
        assertLoadedModules session "[m1]" ["A", "XXX"]
        updateSessionD session (loadModule "A2.hs" "import A\na2 = A.a") 1
        assertLoadedModules session "[m1, m2]" ["A", "A2", "XXX"]
        updateSessionD session (loadModule "A3.hs" "") 1
        assertLoadedModules session "[m1, m2, m3]" ["A", "A2", "A3", "XXX"]
        updateSessionD session (loadModule "Wrong.hs" "import A4\na2 = A4.a + 1") 1
        assertLoadedModules session "wrong1" ["A", "A2", "A3", "XXX"]
        -- This has to be disabled to get the different outcome below:
          -- updateSessionD session (loadModule m4 "import A\na2 = A.a + c") 1
          -- assertLoadedModules session "wrong2" [m1, m2, m3, xxx]
        -- We get this differemnt outcome both in original 7.4.2
        -- and after the GHC#7231 fix. It's probably caused by target
        -- Wrong place before or after target "A" depending on what kind
        -- of error Wrong had. This is strange, but not incorrect.
        updateSessionD session (loadModule "A.hs" "a = c") 1
        -- Module "Wrong" is compiled first here, fails, so module "A"
        -- is never comipiled, so it's not invalidated.
        assertLoadedModules session "wrong3" ["A", "A2", "A3", "XXX"]
    )
  , ( "Maintain list of compiled modules III"
    , withSession defaultSession $ \session -> do
        updateSessionD session (loadModule "A.hs" "a = 5") 1
        assertLoadedModules session "1 [A]" ["A"]
        updateSessionD session (loadModule "A.hs" "a = 5 + True") 1
        assertLoadedModules session "1 []" []
        updateSessionD session (loadModule "A.hs" "a = 5") 1
        assertLoadedModules session "2 [A]" ["A"]
        updateSessionD session (loadModule "A.hs" "a = 5 + wrong") 1
        assertLoadedModules session "2 []" []
        updateSessionD session (loadModule "A.hs" "a = 5") 1
        assertLoadedModules session "3 [A]" ["A"]
        updateSessionD session (loadModule "A.hs" "import WRONG\na = 5") 1
        assertLoadedModules session "3 [A]; wrong imports do not unload old modules" ["A"]
        updateSessionD session (loadModule "A.hs" "a = 5 + True") 1
        assertLoadedModules session "3 []" []
    )
  , ( "Duplicate shutdown"
    , withSession defaultSession $ \session ->
        -- withConfiguredSession will shutdown the session as well
        shutdownSession session
    )
  , ( "Permit a session within a session and duplicated shutdownSession"
    , withSession defaultSession $ \session -> do
        loadModulesFrom session "test/ABnoError"

        withSession defaultSession $ \s2 -> do
         withSession defaultSession $ \s3 -> do
          withSession defaultSession $ \_s4 -> do
           let update2 = loadModule "M.hs" "a = unknownX"
           updateSessionD s2 update2 1
           assertOneError s2
           withSession defaultSession $ \s5 -> do
            let update3 = loadModule "M.hs" "a = 3"
            updateSessionD s3 update3 1
            assertNoErrors session
            shutdownSession s5 -- <-- duplicate "nested" shutdown
    )
  , ( "Compile a project: A depends on B, error in A"
    , withSession defaultSession $ \session -> do
        loadModulesFrom session "test/AerrorB"
        assertSourceErrors session [[(Just "A.hs", "No instance for (Num (IO ()))")]]
     )
  , ( "Compile a project: A depends on B, error in B"
    , withSession defaultSession $ \session -> do
        loadModulesFrom session "test/ABerror"
        assertSourceErrors session [[(Just "B.hs", "No instance for (Num (IO ()))")]]
    )
  , ( "Compile and run a project with some .lhs files"
    , withSession defaultSession $ \session -> do
        loadModulesFrom session "test/compiler/utils"
        assertNoErrors session
        let update2 = updateCodeGeneration True
        updateSessionD session update2 4
        assertNoErrors session
        runActions <- runStmt session "Maybes" "main"
        (output, result) <- runWaitAll runActions
        assertEqual "" RunOk result
        assertEqual "" output (BSLC.pack "False\n")
    )
  , ( "Build executable from some .lhs files"
    , withSession (withIncludes "test/compiler/utils") $ \session -> do
        loadModulesFrom session "test/compiler/utils"
        assertNoErrors session
        status0 <- getBuildExeStatus session
        assertEqual "before exe build" Nothing status0
        distDir <- getDistDir session

        let m = "Maybes"
            upd = buildExe [] [(Text.pack m, m <.> "lhs")]
        updateSessionD session upd 4
        assertNoErrors session
        status1 <- getBuildExeStatus session
        assertEqual "after exe build" (Just ExitSuccess) status1
        let m2 = "Exception"
            upd2 = buildExe [] [(Text.pack m2, m2 <.> "hs")]
        updateSessionD session upd2 3
        let m3 = "Main"
            upd3 = buildExe [] [(Text.pack m3, "Subdir" </> m3 <.> "lhs")]
        updateSessionD session upd3 1
        out <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "Maybes exe output"
                    "False\n"
                    out
        runActions1 <- runExe session m
        (outExe1, statusExe1) <- runWaitAll runActions1
        assertEqual "Maybes exe output from runExe 1"
                    "False\n"
                    outExe1
        assertEqual "after runExe 1" ExitSuccess statusExe1

        out2 <- readProcess (distDir </> "build" </> m2 </> m2) [] []
        assertEqual "Exception exe output"
                    ""
                    out2
        runActions2 <- runExe session m2
        (outExe2, statusExe2) <- runWaitAll runActions2
        assertEqual "Maybes exe output from runExe 2"
                    ""
                    outExe2
        assertEqual "after runExe 2" ExitSuccess statusExe2

        out3 <- readProcess (distDir </> "build" </> m3 </> m3) [] []
        assertEqual "Main exe output"
                    ""
                    out3
        runActions3 <- runExe session m3
        (outExe3, statusExe3) <- runWaitAll runActions3
        assertEqual "Maybes exe output from runExe 3"
                    ""
                    outExe3
        assertEqual "after runExe 3" ExitSuccess statusExe3

        status4 <- getBuildExeStatus session
        assertEqual "after all exe builds" (Just ExitSuccess) status4

        dotCabalFromName <- getDotCabal session
        let dotCabal = dotCabalFromName "libName" $ Version [1, 0] []
        assertEqual "dotCabal for .lhs files" (filterIdeBackendTest $ BSLC.pack "name: libName\nversion: 1.0\ncabal-version: 1.14.0\nbuild-type: Simple\nlicense: AllRightsReserved\nlicense-file: \"\"\ndata-dir: \"\"\n \nlibrary\n    build-depends: base ==4.5.1.0, ghc-prim ==0.2.0.0,\n                   integer-gmp ==0.4.0.0\n    exposed-modules: Exception Maybes OrdList\n    exposed: True\n    buildable: True\n    default-language: Haskell2010\n \n ") $ filterIdeBackendTest dotCabal
        let pkgDir = distDir </> "dotCabal.for.lhs"
        createDirectoryIfMissing False pkgDir
        BSLC.writeFile (pkgDir </> "libName.cabal") dotCabal
        checkWarns <- checkPackage pkgDir
        assertCheckWarns checkWarns
    )
  , ( "Build executable from some .lhs files with dynamic include path change"
    , withSession defaultSession $ \session -> do
        loadModulesFrom session "test/compiler/utils"
        assertNoErrors session
        let m = "Maybes"
            upd0 = buildExe [] [(Text.pack m, m <.> "lhs")]
        updateSessionD session upd0 0
        assertNoErrors session
        status0 <- getBuildExeStatus session
        -- Expected failure! The updateRelativeIncludes below is really needed.
        assertEqual "after exe build 1" (Just $ ExitFailure 1) status0
        updateSessionD session
                       (updateRelativeIncludes ["test/compiler/utils"])
                       4
        assertNoErrors session
        distDir <- getDistDir session

        updateSessionD session upd0 4
        status1 <- getBuildExeStatus session
        assertEqual "after exe build 2" (Just ExitSuccess) status1
        out <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "Maybes exe output"
                    "False\n"
                    out
        runActions1 <- runExe session m
        (outExe1, statusExe1) <- runWaitAll runActions1
        assertEqual "Maybes exe output from runExe 1"
                    "False\n"
                    outExe1
        assertEqual "after runExe 1" ExitSuccess statusExe1

        let m2 = "Exception"
            upd2 = buildExe [] [(Text.pack m2, m2 <.> "hs")]
        updateSessionD session upd2 1
        out2 <- readProcess (distDir </> "build" </> m2 </> m2) [] []
        assertEqual "Exception exe output"
                    ""
                    out2
        runActions2 <- runExe session m2
        (outExe2, statusExe2) <- runWaitAll runActions2
        assertEqual "Maybes exe output from runExe 2"
                    ""
                    outExe2
        assertEqual "after runExe 2" ExitSuccess statusExe2

        let m3 = "Main"
            upd3 = buildExe [] [(Text.pack m3, "Subdir" </> m3 <.> "lhs")]
        updateSessionD session upd3 1
        out3 <- readProcess (distDir </> "build" </> m3 </> m3) [] []
        assertEqual "Main exe output"
                    ""
                    out3
        runActions3 <- runExe session m3
        (outExe3, statusExe3) <- runWaitAll runActions3
        assertEqual "Maybes exe output from runExe 3"
                    ""
                    outExe3
        assertEqual "after runExe 3" ExitSuccess statusExe3

        let upd4 = buildExe [] [(Text.pack m, m <.> "lhs")]
        updateSessionD session upd4 2
        status4 <- getBuildExeStatus session
        assertEqual "after all exe builds" (Just ExitSuccess) status4

        dotCabalFromName <- getDotCabal session
        let dotCabal = dotCabalFromName "libName" $ Version [1, 0] []
        assertEqual "dotCabal for .lhs files" (filterIdeBackendTest $ BSLC.pack "name: libName\nversion: 1.0\ncabal-version: 1.14.0\nbuild-type: Simple\nlicense: AllRightsReserved\nlicense-file: \"\"\ndata-dir: \"\"\n \nlibrary\n    build-depends: base ==4.5.1.0, ghc-prim ==0.2.0.0,\n                   integer-gmp ==0.4.0.0\n    exposed-modules: Exception Maybes OrdList\n    exposed: True\n    buildable: True\n    default-language: Haskell2010\n \n ") $ filterIdeBackendTest dotCabal
        let pkgDir = distDir </> "dotCabal.for.lhs"
        createDirectoryIfMissing False pkgDir
        BSLC.writeFile (pkgDir </> "libName.cabal") dotCabal
        checkWarns <- checkPackage pkgDir
        assertCheckWarns checkWarns
    )
  , ( "Build haddocks from some .lhs files"
    , withSession defaultSession $ \session -> do
        status0 <- getBuildDocStatus session
        assertEqual "before module loading" Nothing status0
        setCurrentDirectory "test/compiler/utils"
        loadModulesFrom session "."
        setCurrentDirectory "../../../"
        assertNoErrors session
        let upd = buildDoc
        updateSessionD session upd 1
        status1 <- getBuildDocStatus session
        assertEqual "after doc build" (Just ExitSuccess) status1
        distDir <- getDistDir session
        indexExists <- doesFileExist $ distDir </> "doc/html/main/index.html"
        assertBool ".lhs haddock files" indexExists
        hoogleExists <- doesFileExist $ distDir </> "doc/html/main/main-1.0.txt"
        assertBool ".lhs hoogle files" hoogleExists
    )
  , ( "Build haddocks and fail"
    , withSession defaultSession $ \session -> do
        setCurrentDirectory "test/ABerror"
        loadModulesFrom session "."
        setCurrentDirectory "../.."
        assertOneError session
        let upd = buildDoc
        -- Note that the stderr log file here is empty, but exit code is 1:
        updateSessionD session upd 1
        status1 <- getBuildDocStatus session
        assertEqual "failure after doc build" (Just $ ExitFailure 1) status1
    )
  , ( "Use cabal macro MIN_VERSION for a package we really depend on"
    , withSession (withOpts ["-XCPP"]) $ \session -> do
        macros <- getCabalMacros session
        assertBool "Main with cabal macro exe output" (not $ BSLC.null macros)
        -- assertEqual "Main with cabal macro exe output" (BSLC.pack "") macros
        let update = updateSourceFile "Main.hs" $ BSLC.pack $ unlines
              [ "#if !MIN_VERSION_base(999,0,0)"
              , "main = print 5"
              , "#else"
              , "terrible error"
              , "#endif"
              ]
        updateSessionD session update 1
        assertNoErrors session
        let update2 = updateCodeGeneration True
        updateSessionD session update2 1
        assertNoErrors session
        runActions <- runStmt session "Main" "main"
        (output, _) <- runWaitAll runActions
        assertEqual "result of ifdefed print 5" (BSLC.pack "5\n") output
        let m = "Main"
            upd = buildExe [] [(Text.pack m, "Main.hs")]
        updateSessionD session upd 2
        distDir <- getDistDir session
        mOut <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "Main with cabal macro exe output" "5\n" mOut
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "5\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe
    )
  , ( "Use cabal macro MIN_VERSION for a package we don't really depend on"
    , withSession (withOpts ["-XCPP"]) $ \session -> do
        let update = updateSourceFile "Main.hs" $ BSLC.pack $ unlines
              [ "#if !MIN_VERSION_containers(999,0,0)"
              , "main = print 5"
              , "#else"
              , "terrible error"
              , "#endif"
              ]
        updateSessionD session update 1
        assertNoErrors session
        let update2 = updateCodeGeneration True
        updateSessionD session update2 1
        assertNoErrors session
        runActions <- runStmt session "Main" "main"
        (output, _) <- runWaitAll runActions
        assertEqual "result of ifdefed print 5" (BSLC.pack "5\n") output
{- FIXME
        let m = "Main"
            upd = buildExe [] [(Text.pack m, "Main.hs")]
        updateSessionD session upd 2
        assertNoErrors session
        distDir <- getDistDir session
        mOut <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "Main with cabal macro exe output" "5\n" mOut

        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "5\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe -}
    )
  , ( "Use cabal macro VERSION by checking if defined"
    , withSession (withOpts ["-XCPP"]) $ \session -> do
        macros <- getCabalMacros session
        assertBool "M with cabal macro exe output" (not $ BSLC.null macros)
        let update = updateSourceFile "M.hs" $ BSLC.pack $ unlines
              [ "module M where"
              , "#ifdef VERSION_base"
              , ""
              , "#if defined(VERSION_base)"
              , "main = print 5"
              , "#else"
              , "terrible error"
              , "#endif"
              , ""
              , "#else"
              , "terrible error 2"
              , "#endif"
              ]
        updateSessionD session (update <> updateCodeGeneration True) 1
        assertNoErrors session
        runActions <- runStmt session "M" "main"
        (output, _) <- runWaitAll runActions
        assertEqual "result of ifdefed print 5" (BSLC.pack "5\n") output
        let m = "M"
            upd = buildExe [] [(Text.pack m, "M.hs")]
        updateSessionD session upd 2
        distDir <- getDistDir session
        mOut <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "M with cabal macro exe output" "5\n" mOut
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "5\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe
    )
  , ( "Use cabal macro VERSION by including an external macros file"
    , withSession (withOpts ["-XCPP"]) $ \session -> do
        macros <- getCabalMacros session
        assertBool "M with cabal macro exe output" (not $ BSLC.null macros)
        let update = updateSourceFile "M.hs" ( BSLC.pack $ unlines
              [ "module M where"
              , "#include \"cabal_macros.h\""
              , "main = print $ MY_VERSION_base == \"foo\""
              ])
              <> updateSourceFile "cabal_macros.h" (BSLC.pack $ unlines
              [ "#define MY_VERSION_base \"4.5.1.0\""
              ])
        updateSessionD session (update <> updateCodeGeneration True) 1
        assertNoErrors session
        runActions <- runStmt session "M" "main"
        (output, _) <- runWaitAll runActions
        assertEqual "result of ifdefed print 5" (BSLC.pack "False\n") output
        let m = "M"
            upd = buildExe [] [(Text.pack m, "M.hs")]
        updateSessionD session upd 2
        distDir <- getDistDir session
        mOut <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "M with cabal macro exe output" "False\n" mOut
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "False\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe


        dotCabalFromName <- getDotCabal session
        let dotCabal = dotCabalFromName "libName" $ Version [1, 0] []
        assertEqual "dotCabal" (filterIdeBackendTestH "cabal_macros.h" $ filterIdeBackendTest $ BSLC.pack "name: libName\nversion: 1.0\ncabal-version: 1.14.0\nbuild-type: Simple\nlicense: AllRightsReserved\nlicense-file: \"\"\ndata-dir: \"\"\n \nlibrary\n    build-depends: base ==4.5.1.0, ghc-prim ==0.2.0.0,\n                   integer-gmp ==0.4.0.0\n    exposed-modules: M\n    exposed: True\n    buildable: True\n    default-language: Haskell2010\n    install-includes: cabal_macros.h\n    ghc-options: -XCPP\n \n ") $ filterIdeBackendTestH "cabal_macros.h" $ filterIdeBackendTest dotCabal
        let pkgDir = distDir </> "dotCabal.test"
        createDirectoryIfMissing False pkgDir
        BSLC.writeFile (pkgDir </> "libName.cabal") dotCabal
        checkWarns <- checkPackage pkgDir
        assertEqual "checkWarns for dotCabal for .lhs files" (filterCheckWarns checkWarns) (filterCheckWarns "The following warnings are likely affect your build negatively:\n* Instead of 'ghc-options: -XCPP' use 'extensions: CPP'\n\nThese warnings may cause trouble when distributing the package:\n* No 'category' field.\n\n* No 'maintainer' field.\n\nThe following errors will cause portability problems on other environments:\n* The package is missing a Setup.hs or Setup.lhs script.\n\n* No 'synopsis' or 'description' field.\n\n* The 'license' field is missing or specified as AllRightsReserved.\n\nHackage would reject this package.\n")
    )
  , ( "Caching cabal macros"
    , do macros <- withSession defaultSession getCabalMacros
         withSession (withOpts ["-XCPP"] . withMacros macros) $ \session -> do
           let update = (updateCodeGeneration True)
                     <> (updateSourceFile "Main.hs" $ BSLC.pack $ unlines [
                             "#if !MIN_VERSION_base(999,0,0)"
                           , "main = print 5"
                           , "#else"
                           , "terrible error"
                           , "#endif"
                           ])
           updateSessionD session update 1
           assertNoErrors session
           runActions <- runStmt session "Main" "main"
           (output, _) <- runWaitAll runActions
           assertEqual "result of ifdefed print 5" (BSLC.pack "5\n") output
           let m = "Main"
               updExe = buildExe [] [(Text.pack m, "Main.hs")]
           updateSessionD session updExe 2
           runActionsExe <- runExe session m
           (outExe, statusExe) <- runWaitAll runActionsExe
           assertEqual "Output from runExe"
                       "5\n"
                       outExe
           assertEqual "after runExe" ExitSuccess statusExe
         let customMacros = BSLC.pack "#define HELLO 1"
         withSession (withOpts ["-XCPP"] . withMacros customMacros) $ \session -> do
           let update = (updateCodeGeneration True)
                     <> (updateSourceFile "Main.hs" $ BSLC.pack $ unlines [
                             "#if HELLO"
                           , "main = print 6"
                           , "#else"
                           , "main = print 7"
                           , "#endif"
                           ])
           updateSessionD session update 1
           assertNoErrors session
           runActions <- runStmt session "Main" "main"
           (output, _) <- runWaitAll runActions
           assertEqual "result of ifdefed print 6" (BSLC.pack "6\n") output
           let m = "Main"
               updExe = buildExe [] [(Text.pack m, "Main.hs")]
           updateSessionD session updExe 2
           runActionsExe <- runExe session m
           (outExe, statusExe) <- runWaitAll runActionsExe
           assertEqual "Output from runExe"
                       "7\n"  -- FIXME
                       outExe
           assertEqual "after runExe" ExitSuccess statusExe
    )
  , ( "Reject a program requiring -XNamedFieldPuns, then set the option"
    , withSession (withOpts ["-hide-package monads-tf"]) $ \session -> do
        setCurrentDirectory "test/Puns"
        loadModulesFrom session "."
        assertMoreErrors session
        let punOpts = ["-XNamedFieldPuns", "-XRecordWildCards"]
            update2 = updateDynamicOpts punOpts
        (_, lm) <- getModules session
        updateSessionD session update2 (length lm)
        setCurrentDirectory "../../"
        assertNoErrors session
        let m = "GHC.RTS.Events"
            upd2 = buildExe [] [(Text.pack m, "GHC/RTS/Events.hs")]
        updateSessionD session upd2 4
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "buildStderr empty" "" buildStderr

        dotCabalFromName <- getDotCabal session
        let dotCabal = dotCabalFromName "libName" $ Version [1, 0] []
        assertEqual "dotCabal" (filterIdeBackendTestH "EventLogFormat.h" $ filterIdeBackendTest $ BSLC.pack "name: libName\nversion: 1.0\ncabal-version: 1.14.0\nbuild-type: Simple\nlicense: AllRightsReserved\nlicense-file: \"\"\ndata-dir: \"\"\n \nlibrary\n    build-depends: array ==0.4.0.0, base ==4.5.1.0, binary ==0.5.1.0,\n                   bytestring ==0.9.2.1, containers ==0.4.2.1, deepseq ==1.3.0.0,\n                   ghc-prim ==0.2.0.0, integer-gmp ==0.4.0.0, mtl ==2.1.2,\n                   transformers ==0.3.0.0\n    exposed-modules: GHC.RTS.EventParserUtils GHC.RTS.EventTypes\n                     GHC.RTS.Events\n    exposed: True\n    buildable: True\n    default-language: Haskell2010\n    install-includes: EventLogFormat.h\n    ghc-options: -hide-package monads-tf -XNamedFieldPuns -XRecordWildCards\n \n ") $ filterIdeBackendTestH "EventLogFormat.h" $ filterIdeBackendTest dotCabal
        let pkgDir = distDir </> "dotCabal.test"
        createDirectoryIfMissing False pkgDir
        BSLC.writeFile (pkgDir </> "libName.cabal") dotCabal
        checkWarns <- checkPackage pkgDir
        assertEqual "checkWarns for dotCabal for .lhs files" (filterCheckWarns checkWarns) (filterCheckWarns "The following warnings are likely affect your build negatively:\n* Instead of 'ghc-options: -XNamedFieldPuns -XRecordWildCards' use\n'extensions: NamedFieldPuns RecordWildCards'\n\nThese warnings may cause trouble when distributing the package:\n* No 'category' field.\n\n* No 'maintainer' field.\n\nThe following errors will cause portability problems on other environments:\n* The package is missing a Setup.hs or Setup.lhs script.\n\n* No 'synopsis' or 'description' field.\n\n* The 'license' field is missing or specified as AllRightsReserved.\n\nHackage would reject this package.\n")
    )
  , ( "Build licenses from NamedFieldPuns (with errors)"
    , withSession (withOpts ["-hide-package monads-tf"]) $ \session -> do
        loadModulesFrom session "test/Puns"
        assertMoreErrors session
        let upd = buildLicenses "test/Puns/cabals"
        updateSessionD session upd 99
        assertMoreErrors session
        distDir <- getDistDir session
        errExists <- doesFileExist $ distDir </> "licenses.stderr"
        when errExists $ do
          licensesErr <- readFile $ distDir </> "licenses.stderr"
          assertEqual "license errors" "" licensesErr
        status <- getBuildLicensesStatus session
        assertEqual "after license build" (Just ExitSuccess) status
        licensesWarns <- readFile $ distDir </> "licenses.stdout"
        assertEqual "licensesWarns length" 367 (length licensesWarns)
        licenses <- readFile $ distDir </> "licenses.txt"
        assertBool "licenses length" $ length licenses >= 27142
    )
  , ( "Build licenses with wrong cabal files and fail"
    , withSession (withOpts ["-hide-package monads-tf"]) $ \session -> do
        loadModulesFrom session "test/Puns"
        assertMoreErrors session
        let updL = buildLicenses "test/Puns/cabals/parse_error"
            punOpts = ["-XNamedFieldPuns", "-XRecordWildCards"]
            upd = updL <> updateDynamicOpts punOpts
        updateSessionD session upd 99
        assertNoErrors session
        status <- getBuildLicensesStatus session
        assertEqual "after license parse_error" (Just ExitSuccess) status
        distDir <- getDistDir session
        licensesErr <- readFile $ distDir </> "licenses.stderr"
        assertEqual "licenses parse_error msgs" licensesErr
          "Parse of field 'license' failed.\nNo .cabal file provided for package transformers so no license can be found.\n"
        let upd2 = buildLicenses "test/Puns/cabals/no_text_error"
        updateSessionD session upd2 99
        status2 <- getBuildLicensesStatus session
        assertEqual "after license no_text_error" (Just ExitSuccess) status2
        licensesErr2 <- readFile $ distDir </> "licenses.stderr"
        assertEqual "licenses no_text_error msgs" licensesErr2
          "No license text can be found for package mtl.\nNo .cabal file provided for package transformers so no license can be found.\n"
    )
  , ( "Test CWD by reading a data file"
    , withSession defaultSession $ \session -> do
        let update = updateDataFile "datafile.dat"
                                    (BSLC.pack "test data content")
        updateSessionD session update 0
        let update2 = loadModule "Main.hs"
              "main = readFile \"datafile.dat\" >>= putStrLn"
        updateSessionD session update2 1
        assertNoErrors session
        let update3 = updateCodeGeneration True
        updateSessionD session update3 1
        runActions <- runStmt session "Main" "main"
        (output, _) <- runWaitAll runActions
        assertEqual "compare test data content"
          (BSLC.pack "test data content\n") output
        let m = "Main"
            updExe = buildExe [] [(Text.pack m, "Main.hs")]
        updateSessionD session updExe 2
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "test data content\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe
        let update4 = updateDataFile "datafile.dat"
                                     (BSLC.pack "new content")
                      <> update2
        updateSessionD session update4 1
        runActions2 <- runStmt session "Main" "main"
        (output2, _) <- runWaitAll runActions2
        assertEqual "compare new content"
          (BSLC.pack "new content\n") output2
        let updExe2 = buildExe [] [(Text.pack m, "Main.hs")]
        updateSessionD session updExe2 2
        runActionsExe2 <- runExe session m
        (outExe2, statusExe2) <- runWaitAll runActionsExe2
        assertEqual "Output from runExe"
                    "new content\n"
                    outExe2
        assertEqual "after runExe" ExitSuccess statusExe2
    )
  , ( "Test CWD in executable building"
    , withSession (withOpts []) $ \session -> do
        let update = updateCodeGeneration True
                     <> updateDataFile "test.txt" (BSLC.pack "test data")
        let update2 = updateSourceFile "Main.hs" $ BSLC.pack $ unlines
              [ "{-# LANGUAGE TemplateHaskell #-}"
              , "module Main where"
              , "import Language.Haskell.TH.Syntax"
              , "main = putStrLn $(qRunIO (readFile \"test.txt\") >>= lift)"
              ]
        updateSessionD session (update <> update2) 1
        assertNoErrors session
        runActions <- runStmt session "Main" "main"
        (output, _) <- runWaitAll runActions
        assertEqual "compare test data"
          "test data\n" (BSLC.unpack output)
        let m = "Main"
            upd = buildExe [] [(Text.pack m, "Main.hs")]
        updateSessionD session upd 2
        distDir <- getDistDir session
        out <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "CWD exe output" (BSLC.unpack output) out
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    output
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe
    )
{- Now that we always load the RTS, we're never in this situation
  , ("Reject getSourceErrors without updateSession"
    , withSession defaultSession $ \session ->
        assertRaises "getSourceErrors session"
          (== userError "This session state does not admit queries.")
          (getSourceErrors session)
    )
-}
  , ("Reject updateSession after shutdownSession"
    , withSession defaultSession $ \session -> do
        shutdownSession session
        assertRaises "updateSessionD session mempty"
          (== userError "Session already shut down.")
          (updateSessionD session mempty 0)
    )
  , ("Reject getSourceErrors after shutdownSession"
    , withSession defaultSession $ \session -> do
        shutdownSession session
        assertRaises "getSourceErrors session"
          (== userError "Session already shut down.")
          (getSourceErrors session)
    )
  , ("Reject runStmt after shutdownSession"
    , withSession defaultSession $ \session -> do
        shutdownSession session
        assertRaises "runStmt session Main main"
          (== userError "State not idle")
          (runStmt session "Main" "main")
    )
  , ( "Test recursive modules"
    , withSession (withIncludes "test/bootMods") $ \session -> do
        loadModulesFrom session "test/bootMods"
        assertNoErrors session

        let m = "Main"
            upd = buildExe [] [(Text.pack m, "C" <.> "hs")]
        updateSessionD session upd 7
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "buildStderr empty" "" buildStderr
        status <- getBuildExeStatus session
        assertEqual "after exe build" (Just ExitSuccess) status
        out <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "" "C\n" out
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "C\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe
    )
  , ( "Test recursive modules with dynamic include path change"
    , withSession defaultSession $ \session -> do
        loadModulesFrom session "test/bootMods"
        assertOneError session

        updateSessionD session
                       (updateRelativeIncludes ["test/bootMods"])
                       4
        assertNoErrors session

        let m = "Main"
            upd = buildExe [] [(Text.pack m, "C" <.> "hs")]
        updateSessionD session upd 7
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "buildStderr empty" "" buildStderr
        status <- getBuildExeStatus session
        assertEqual "after exe build" (Just ExitSuccess) status
        out <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "" "C\n" out
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "C\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe
    )
  , ( "Test TH; code generation on"
    , withSession (withOpts ["-XTemplateHaskell"]) $ \session -> do
        setCurrentDirectory "test"
        (originalUpdate, lm) <- getModulesFrom session "TH"
        let update = originalUpdate <> updateCodeGeneration True
        updateSessionD session update (length lm)
        setCurrentDirectory "../"
        assertNoErrors session
        runActions <- runStmt session "TH.TH" "main"
        (output, result) <- runWaitAll runActions
        assertEqual "" RunOk result
        assertEqual "" output (BSLC.pack "(True,43)\n")
    )
  , ( "Build executable from TH"
    , withSession (withOpts ["-XTemplateHaskell"]) $ \session -> do
        setCurrentDirectory "test"
        (originalUpdate, lm) <- getModulesFrom session "TH"
        let update = originalUpdate <> updateCodeGeneration True
        updateSessionD session update (length lm)
        setCurrentDirectory "../"
        assertNoErrors session
        let m = "TH.TH"
            upd = buildExe ["-rtsopts=all", "-O0"] [(Text.pack m, "TH/TH.hs")]
        updateSessionD session upd 3
        distDir <- getDistDir session
        out <- readProcess (distDir </> "build" </> m </> m)
                           ["+RTS", "-K4M", "-RTS"] []
        assertEqual "TH.TH exe output"
                    "(True,43)\n"
                    out
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "(True,43)\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe

        dotCabalFromName <- getDotCabal session
        let dotCabal = dotCabalFromName "libName" $ Version [1, 0] []
        assertEqual "dotCabal from TH" (filterIdeBackendTest $ BSLC.pack "name: libName\nversion: 1.0\ncabal-version: 1.14.0\nbuild-type: Simple\nlicense: AllRightsReserved\nlicense-file: \"\"\ndata-dir: \"\"\n \nlibrary\n    build-depends: array ==0.4.0.0, base ==4.5.1.0,\n                   containers ==0.4.2.1, deepseq ==1.3.0.0, ghc-prim ==0.2.0.0,\n                   integer-gmp ==0.4.0.0, pretty ==1.1.1.0, template-haskell ==2.7.0.0\n    exposed-modules: TH.BlockingOps TH.TH\n    exposed: True\n    buildable: True\n    default-language: Haskell2010\n    ghc-options: -XTemplateHaskell\n \n ") $ filterIdeBackendTest dotCabal
        let pkgDir = distDir </> "dotCabal.for.lhs"
        createDirectoryIfMissing False pkgDir
        BSLC.writeFile (pkgDir </> "libName.cabal") dotCabal
        checkWarns <- checkPackage pkgDir
        assertEqual "checkWarns for dotCabal for .lhs files" (filterCheckWarns checkWarns) (filterCheckWarns "The following warnings are likely affect your build negatively:\n* Instead of 'ghc-options: -XTemplateHaskell' use 'extensions:\nTemplateHaskell'\n\nThese warnings may cause trouble when distributing the package:\n* No 'category' field.\n\n* No 'maintainer' field.\n\nThe following errors will cause portability problems on other environments:\n* The package is missing a Setup.hs or Setup.lhs script.\n\n* No 'synopsis' or 'description' field.\n\n* The 'license' field is missing or specified as AllRightsReserved.\n\nHackage would reject this package.\n")
    )
  , ( "Build haddocks from TH"
    , withSession (withOpts ["-XTemplateHaskell"]) $ \session -> do
        setCurrentDirectory "test"
        (originalUpdate, lm) <- getModulesFrom session "TH"
        let update = originalUpdate <> updateCodeGeneration True
        updateSessionD session update (length lm)
        setCurrentDirectory "../"
        assertNoErrors session
        let upd = buildDoc
        updateSessionD session upd 1
        distDir <- getDistDir session
        indexExists <- doesFileExist $ distDir </> "doc/html/main/index.html"
        assertBool "TH.TH haddock files" indexExists
        hoogleExists <- doesFileExist $ distDir </> "doc/html/main/main-1.0.txt"
        assertBool "TH.TH hoogle files" hoogleExists
    )
  , ( "Test CPP: ifdefed module header"
    , withSession (withOpts ["-XCPP"]) $ \session -> do
        let update = updateSourceFile "Good.hs" $ BSLC.pack $ unlines
              [ "#if __GLASGOW_HASKELL__ < 600"
              , "module Bad where"
              , "import Data.List"
              , "#else"
              , "module Good where"
              , "import Data.Monoid"
              , "#endif"
              , "x = mappend [] []"
              ]
        updateSessionD session update 1
        assertNoErrors session
        assertIdInfo session "Good" (8,1,8,2) "x" VarName "[a]" "main:Good" "Good.hs@8:1-8:2" "" "binding occurrence"
    )
  , ( "Reject a wrong CPP directive"
    , withSession (withOpts ["-XCPP"]) $ \session -> do
        let update = loadModule "M.hs" "#ifdef"
                     <> updateCodeGeneration True
        updateSessionD session update 1
        msgs <- getSourceErrors session
        -- Due to a GHC bug there are now 2 errors. TODO; when it's fixed,
        -- assert a single specific error here.
        assertSomeErrors msgs
        assertRaises "runStmt session Main main"
          (== userError "Module \"Main\" not successfully loaded, when trying to run code.")
          (runStmt session "Main" "main")
    )
  , ( "Reject a module with mangled header"
    , withSession defaultSession $ \session -> do
        let update = updateSourceFile "M.hs"
                                  (BSLC.pack "module very-wrong where")
        updateSessionD session update 1
        assertSourceErrors' session ["parse error on input `very'\n"]
        let update2 = updateSourceFile "M.hs"
                                   (BSLC.pack "module M.1.2.3.8.T where")
        updateSessionD session update2 1
        assertSourceErrors' session ["parse error on input `.'\n"]
    )
  , ( "Interrupt runStmt (after 1 sec)"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import Control.Concurrent (threadDelay)"
                    , "loop :: IO ()"
                    , "loop = threadDelay 100000 >> loop"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "loop"
        threadDelay 1000000
        interrupt runActions
        resOrEx <- runWait runActions
        case resOrEx of
          Right result -> assertBool "" (isAsyncException result)
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
    )
  , ( "Interrupt runStmt (immediately)"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import Control.Concurrent (threadDelay)"
                    , "loop :: IO ()"
                    , "loop = threadDelay 100000 >> loop"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "loop"
        interrupt runActions
        resOrEx <- runWait runActions
        case resOrEx of
          Right result -> assertBool "" (isAsyncException result)
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
    )
  , ( "Interrupt runStmt (black hole; after 1 sec)"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "loop :: IO ()"
                    , "loop = loop"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "loop"
        threadDelay 1000000
        forceCancel runActions -- Black hole cannot (always) be interrupted using an exception
        resOrEx <- runWait runActions
        case resOrEx of
          Right RunForceCancelled -> return ()
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
    )
  , ( "Interrupt runStmt many times, preferably without deadlock :) (#58)"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "Main.hs" . BSLC.pack $
                    "main = putStrLn \"Hi!\" >> getLine >> return ()")
               <> (updateStdoutBufferMode (RunLineBuffering Nothing))
        updateSessionD session upd 1
        assertNoErrors session

        replicateM_ 100 $ do
          runActions <- runStmt session "Main" "main"
          interrupt runActions
          (_output, result) <- runWaitAll runActions
          assertBool ("Expected asynchronous exception; got " ++ show result) (isAsyncException result)

        runActions <- runStmt session "Main" "main"
        supplyStdin runActions "\n"
        (output, result) <- runWaitAll runActions
        assertEqual "" RunOk result
        assertEqual "" (BSLC.pack "Hi!\n") output
    )
  , ( "Interrupt runExe (after 1 sec)"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import Control.Concurrent (threadDelay)"
                    , "main :: IO ()"
                    , "main = threadDelay 100000 >> main"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        let m = "M"
            updExe = buildExe [] [(Text.pack m, "M.hs")]
        updateSessionD session updExe 2
        runActionsExe <- runExe session m
        threadDelay 1000000
        interrupt runActionsExe
        resOrEx <- runWait runActionsExe
        case resOrEx of
          Right result -> assertEqual "after runExe" (ExitFailure 2) result
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
    )
  , ( "Interrupt runExe (immediately)"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import Control.Concurrent (threadDelay)"
                    , "main :: IO ()"
                    , "main = threadDelay 100000 >> main"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        let m = "M"
            updExe = buildExe [] [(Text.pack m, "M.hs")]
        updateSessionD session updExe 2
        runActionsExe <- runExe session m
        interrupt runActionsExe
        resOrEx <- runWait runActionsExe
        case resOrEx of
          Right result -> assertEqual "after runExe" (ExitFailure 2) result
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
    )
  , ( "Interrupt runExe (black hole; after 1 sec)"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "main :: IO ()"
                    , "main = main"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        let m = "M"
            updExe = buildExe [] [(Text.pack m, "M.hs")]
        updateSessionD session updExe 2
        runActionsExe <- runExe session m
        threadDelay 1000000
        resOrExe <- runWait runActionsExe
        -- Here the result differs from runStmt, because the loop is detected
        -- and reported.
        case resOrExe of
          Left result -> assertEqual "after runExe" "M: <<loop>>\n" result
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrExe
    )
  , ( "Interrupt runExe many times, preferably without deadlock :) (#58)"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "Main.hs" . BSLC.pack $
                    "main = putStrLn \"Hi!\" >> getLine")
               <> (updateStdoutBufferMode (RunLineBuffering Nothing))
        updateSessionD session upd 1
        assertNoErrors session

        let m = "Main"
            updExe = buildExe [] [(Text.pack m, "Main.hs")]
        updateSessionD session updExe 2

        replicateM_ 10 $ do
          runActionsExe <- runExe session m
          interrupt runActionsExe
          (_output, result) <- runWaitAll runActionsExe
          assertEqual "" (ExitFailure 2) result

        -- This doesn't work, because the updateStdoutBufferMode above
        -- is void for runExe.
        -- runActions <- runExe session m
        -- result <- runWait runActions
        -- assertEqual "" (Left (BSSC.pack "Hi!\n")) result
        -- interrupt runActions  -- needed, because exe not killed by shutdown
    )
  , ( "Capture stdout (single putStrLn)"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "hello :: IO ()"
                    , "hello = putStrLn \"Hello World\""
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        assertEqual "" RunOk result
        assertEqual "" (BSLC.pack "Hello World\n") output
    )
  , ( "Capture stdout (single putStr)"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "hello :: IO ()"
                    , "hello = putStr \"Hello World\""
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        assertEqual "" RunOk result
        assertEqual "" (BSLC.pack "Hello World") output
    )
  , ( "Capture stdout (single putStr with delay)"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import Control.Concurrent (threadDelay)"
                    , "import System.IO"
                    , "hello :: IO ()"
                    , "hello = hSetBuffering stdout LineBuffering >> putStr \"hello\" >> threadDelay 1000000 >> putStr \"hi\""
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        assertEqual "" RunOk result
        assertEqual "" (BSLC.pack "hellohi") output
    )
  , ( "Capture stdout (multiple putStrLn)"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "hello :: IO ()"
                    , "hello = do putStrLn \"Hello World 1\""
                    , "           putStrLn \"Hello World 2\""
                    , "           putStrLn \"Hello World 3\""
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        assertEqual "" RunOk result
        assertEqual "" (BSLC.pack "Hello World 1\nHello World 2\nHello World 3\n") output
    )
  , ( "Capture stdout (mixed putStr and putStrLn)"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "hello :: IO ()"
                    , "hello = do putStrLn \"Hello World 1\""
                    , "           putStr   \"Hello World 2\""
                    , "           putStrLn \"Hello World 3\""
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        assertEqual "" RunOk result
        assertEqual "" (BSLC.pack "Hello World 1\nHello World 2Hello World 3\n") output
    )
  , ( "Capture stdin (simple echo process)"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "echo :: IO ()"
                    , "echo = getLine >>= putStrLn"
                    , "main :: IO ()"
                    , "main = echo"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "echo"
        supplyStdin runActions (BSSC.pack "ECHO!\n")
        (output, result) <- runWaitAll runActions
        assertEqual "" RunOk result
        assertEqual "" (BSLC.pack "ECHO!\n") output
        let m = "M"
            updExe = buildExe [] [(Text.pack m, "M.hs")]
        updateSessionD session updExe 2
        runActionsExe <- runExe session m
        supplyStdin runActionsExe (BSSC.pack "ECHO!\n")
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "ECHO!\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe
    )
  , ( "Capture stdin (infinite echo process)"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import System.IO"
                    , "import Control.Monad"
                    , "echo :: IO ()"
                    , "echo = do hSetBuffering stdout LineBuffering"
                    , "          forever $ getLine >>= putStrLn"
                    , "main :: IO ()"
                    , "main = echo"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "echo"

        do supplyStdin runActions (BSSC.pack "ECHO 1!\n")
           result <- runWait runActions
           assertEqual "" (Left (BSSC.pack "ECHO 1!\n")) result

        do supplyStdin runActions (BSSC.pack "ECHO 2!\n")
           result <- runWait runActions
           assertEqual "" (Left (BSSC.pack "ECHO 2!\n")) result

        do interrupt runActions
           resOrEx <- runWait runActions
           case resOrEx of
             Right result -> assertBool "" (isAsyncException result)
             _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
    )
  , ( "Capture stdin (infinite echo process) with runStmt and runExe"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import System.IO"
                    , "import Control.Monad"
                    , "echo :: IO ()"
                    , "echo = do hSetBuffering stdout LineBuffering"
                    , "          forever $ getLine >>= putStrLn"
                    , "main :: IO ()"
                    , "main = echo"
                    ])
        updateSessionD session upd 1
        assertNoErrors session

        let m = "M"
            updExe = buildExe [] [(Text.pack m, "M.hs")]
        updateSessionD session updExe 2

        runActions <- runStmt session "M" "echo"
        runActionsExe <- runExe session m

        do supplyStdin runActions (BSSC.pack "ECHO 1!\n")
           result <- runWait runActions
           assertEqual "" (Left (BSSC.pack "ECHO 1!\n")) result

        do supplyStdin runActionsExe (BSSC.pack "ECHO 1!\n")
           result <- runWait runActionsExe
           assertEqual "" (Left (BSSC.pack "ECHO 1!\n")) result

        do supplyStdin runActions (BSSC.pack "ECHO 2!\n")
           result <- runWait runActions
           assertEqual "" (Left (BSSC.pack "ECHO 2!\n")) result

        do supplyStdin runActionsExe (BSSC.pack "ECHO 2!\n")
           result <- runWait runActionsExe
           assertEqual "" (Left (BSSC.pack "ECHO 2!\n")) result

        do interrupt runActions
           resOrEx <- runWait runActions
           case resOrEx of
             Right result -> assertBool "" (isAsyncException result)
             _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx

        do supplyStdin runActionsExe (BSSC.pack "ECHO 3!\n")
           result <- runWait runActionsExe
           assertEqual "" (Left (BSSC.pack "ECHO 3!\n")) result

        do interrupt runActionsExe
           resOrEx <- runWait runActionsExe
           case resOrEx of
             Right result -> assertEqual "after runExe" (ExitFailure 2) result
             _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
    )
  , ( "Two calls to runStmt"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "echo :: IO ()"
                    , "echo = getLine >>= putStrLn"
                    , "echoReverse :: IO ()"
                    , "echoReverse = getLine >>= putStrLn . reverse"
                    ])
        updateSessionD session upd 1
        assertNoErrors session

        do runActions <- runStmt session "M" "echo"
           supplyStdin runActions (BSSC.pack "ECHO!\n")
           (output, result) <- runWaitAll runActions
           assertEqual "" RunOk result
           assertEqual "" (BSLC.pack "ECHO!\n") output

        do runActions <- runStmt session "M" "echoReverse"
           supplyStdin runActions (BSSC.pack "!OHCE\n")
           (output, result) <- runWaitAll runActions
           assertEqual "" RunOk result
           assertEqual "" (BSLC.pack "ECHO!\n") output
    )
  , ( "Two calls to runExe"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "main :: IO ()"
                    , "main = getLine >>= putStrLn . reverse"
                    ])
        updateSessionD session upd 1
        assertNoErrors session

        let m = "M"
            updExe = buildExe [] [(Text.pack m, "M.hs")]
        updateSessionD session updExe 2

        do runActions <- runExe session "M"
           supplyStdin runActions (BSSC.pack "!OHCE\n")
           (output, result) <- runWaitAll runActions
           assertEqual "" result ExitSuccess
           assertEqual "" (BSLC.pack "ECHO!\n") output

        do runActions <- runExe session "M"
           supplyStdin runActions (BSSC.pack "!OHCE\n")
           (output, result) <- runWaitAll runActions
           assertEqual "" result ExitSuccess
           assertEqual "" (BSLC.pack "ECHO!\n") output
    )
  , ( "Make sure we can terminate the IDE session when code is running"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "echo :: IO ()"
                    , "echo = (getLine >>= putStrLn) >> echo"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        _runActions <- runStmt session "M" "echo"
        return ()
     )
  , ( "Make sure we can terminate the IDE session when exe is running"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "main :: IO ()"
                    , "main = (getLine >>= putStrLn) >> main"
                    ])
        updateSessionD session upd 1

        let m = "M"
            updExe = buildExe [] [(Text.pack m, "M.hs")]
        updateSessionD session updExe 2

        assertNoErrors session
        _runActions <- runExe session "M"
        return ()
     )
  , ( "Capture stderr"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import System.IO"
                    , "hello :: IO ()"
                    , "hello = hPutStrLn stderr \"Hello World\""
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        assertEqual "" RunOk result
        assertEqual "" (BSLC.pack "Hello World\n") output
    )
  , ( "Merge stdout and stderr"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import System.IO"
                    , "hello :: IO ()"
                    , "hello = do hPutStrLn stderr \"Hello World 1\""
                    , "           hPutStrLn stdout \"Hello World 2\""
                    , "           hPutStr   stderr \"Hello World 3\""
                    , "           hPutStr   stdout \"Hello World 4\""
                    , "           hPutStrLn stderr \"Hello World 5\""
                    , "           hPutStrLn stdout \"Hello World 6\""
                    , "           hPutStr   stderr \"Hello World 7\""
                    , "           hPutStr   stdout \"Hello World 8\""
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        let expectedOutput = "Hello World 1\n"
                          ++ "Hello World 2\n"
                          ++ "Hello World 3"
                          ++ "Hello World 4"
                          ++ "Hello World 5\n"
                          ++ "Hello World 6\n"
                          ++ "Hello World 7"
                          ++ "Hello World 8"
        assertEqual "" RunOk result
        assertEqual "" (BSLC.pack expectedOutput) output
    )
  , ( "Merge stdout and stderr in runExe"
    , withSession defaultSession $ \session -> do
        -- Note that we have to set buffering here, to match the default
        -- buffering for snippets.
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import System.IO"
                    , "main :: IO ()"
                    , "main  = do hSetBuffering stdout NoBuffering"
                    , "           hPutStrLn stderr \"Hello World 1\""
                    , "           hPutStrLn stdout \"Hello World 2\""
                    , "           hPutStr   stderr \"Hello World 3\""
                    , "           hPutStr   stdout \"Hello World 4\""
                    , "           hPutStrLn stderr \"Hello World 5\""
                    , "           hPutStrLn stdout \"Hello World 6\""
                    , "           hPutStr   stderr \"Hello World 7\""
                    , "           hPutStr   stdout \"Hello World 8\""
                    ])
        updateSessionD session upd 1
        assertNoErrors session

        let m = "M"
            updExe = buildExe [] [(Text.pack m, "M.hs")]
        updateSessionD session updExe 2

        runActions <- runExe session "M"
        (output, result) <- runWaitAll runActions
        let expectedOutput = "Hello World 1\n"
                          ++ "Hello World 2\n"
                          ++ "Hello World 3"
                          ++ "Hello World 4"
                          ++ "Hello World 5\n"
                          ++ "Hello World 6\n"
                          ++ "Hello World 7"
                          ++ "Hello World 8"
        assertEqual "" result ExitSuccess
        assertEqual "" (BSLC.pack expectedOutput) output
    )
  , ( "Set environment variables"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import System.Environment (getEnv)"
                    , "printFoo :: IO ()"
                    , "printFoo = getEnv \"Foo\" >>= putStr"
                    , "printBar :: IO ()"
                    , "printBar = getEnv \"Bar\" >>= putStr"
                    ])
        updateSessionD session upd 1
        assertNoErrors session

        -- At the start, both Foo and Bar are undefined
        do runActions <- runStmt session "M" "printFoo"
           (_, result) <- runWaitAll runActions
           case result of
             RunProgException ex -> assertEqual "" ex "IOException: Foo: getEnv: does not exist (no environment variable)"
             _ -> assertFailure $ "Unexpected result " ++ show result
        do runActions <- runStmt session "M" "printBar"
           (_, result) <- runWaitAll runActions
           case result of
             RunProgException ex -> assertEqual "" ex "IOException: Bar: getEnv: does not exist (no environment variable)"
             _ -> assertFailure $ "Unexpected result " ++ show result

        -- Update Foo, leave Bar undefined
        updateSession session (updateEnv "Foo" (Just "Value1")) (\_ -> return ())
        do runActions <- runStmt session "M" "printFoo"
           (output, result) <- runWaitAll runActions
           assertEqual "" RunOk result
           assertEqual "" (BSLC.pack "Value1") output
        do runActions <- runStmt session "M" "printBar"
           (_, result) <- runWaitAll runActions
           assertEqual "" result (RunProgException "IOException: Bar: getEnv: does not exist (no environment variable)")

        -- Update Bar, leave Foo defined
        updateSession session (updateEnv "Bar" (Just "Value2")) (\_ -> return ())
        do runActions <- runStmt session "M" "printFoo"
           (output, result) <- runWaitAll runActions
           assertEqual "" RunOk result
           assertEqual "" (BSLC.pack "Value1") output
        do runActions <- runStmt session "M" "printBar"
           (output, result) <- runWaitAll runActions
           assertEqual "" RunOk result
           assertEqual "" (BSLC.pack "Value2") output

        -- Unset Foo, leave Bar defined
        updateSession session (updateEnv "Foo" Nothing) (\_ -> return ())
        do runActions <- runStmt session "M" "printFoo"
           (_, result) <- runWaitAll runActions
           assertEqual "" result (RunProgException "IOException: Foo: getEnv: does not exist (no environment variable)")
        do runActions <- runStmt session "M" "printBar"
           (output, result) <- runWaitAll runActions
           assertEqual "" RunOk result
           assertEqual "" (BSLC.pack "Value2") output
    )
  , ( "Set environment variables and use them in runExe"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import System.Environment"
                    , "main :: IO ()"
                    , "main = do"
                    , "  args <- getArgs"
                    , "  case args of"
                    , "    [\"Foo\"] -> getEnv \"Foo\" >>= putStr"
                    , "    [\"Bar\"] -> getEnv \"Bar\" >>= putStr"
                    , "    _ -> fail \"wrong args\""
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        let m = "M"
            updExe = buildExe [] [(Text.pack m, "M.hs")]
        updateSessionD session updExe 2
        assertNoErrors session

        -- At the start, both Foo and Bar are undefined
        do updateSessionD session (updateArgs ["Foo"]) 1
           runActions <- runExe session "M"
           (_, result) <- runWaitAll runActions
           assertEqual "" result (ExitFailure 1)
        do updateSessionD session (updateArgs ["Bar"]) 1
           runActions <- runExe session "M"
           (_, result) <- runWaitAll runActions
           assertEqual "" result (ExitFailure 1)

        -- Update Foo, leave Bar undefined
        updateSession session (updateEnv "Foo" (Just "Value1")) (\_ -> return ())
        do updateSessionD session (updateArgs ["Foo"]) 1
           runActions <- runExe session "M"
           (output, result) <- runWaitAll runActions
           assertEqual "" result ExitSuccess
           assertEqual "" (BSLC.pack "Value1") output
        do updateSessionD session (updateArgs ["Bar"]) 1
           runActions <- runExe session "M"
           (_, result) <- runWaitAll runActions
           assertEqual "" result (ExitFailure 1)

        -- Update Bar, leave Foo defined
        updateSession session (updateEnv "Bar" (Just "Value2")) (\_ -> return ())
        do updateSessionD session (updateArgs ["Foo"]) 1
           runActions <- runExe session "M"
           (output, result) <- runWaitAll runActions
           assertEqual "" result ExitSuccess
           assertEqual "" (BSLC.pack "Value1") output
        do updateSessionD session (updateArgs ["Bar"]) 1
           runActions <- runExe session "M"
           (output, result) <- runWaitAll runActions
           assertEqual "" result ExitSuccess
           assertEqual "" (BSLC.pack "Value2") output

        -- Unset Foo, leave Bar defined
        updateSession session (updateEnv "Foo" Nothing) (\_ -> return ())
        do updateSessionD session (updateArgs ["Foo"]) 1
           runActions <- runExe session "M"
           (_, result) <- runWaitAll runActions
           assertEqual "" result (ExitFailure 1)
        do updateSessionD session (updateArgs ["Bar"]) 1
           runActions <- runExe session "M"
           (output, result) <- runWaitAll runActions
           assertEqual "" result ExitSuccess
           assertEqual "" (BSLC.pack "Value2") output
    )
  , ( "Update during run"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "loop :: IO ()"
                    , "loop = loop"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        _runActions <- runStmt session "M" "loop"
        assertRaises ""
          (== userError "Cannot update session in running mode")
          (updateSessionD session upd 1)
    )
  , ( "getSourceErrors during run"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "{-# OPTIONS_GHC -Wall #-}"
                    , "module M where"
                    , "loop = loop"
                    , "main :: IO ()"
                    , "main = loop"
                   ])
        updateSessionD session upd 1
        assertSourceErrors' session ["Top-level binding with no type signature"]

        let m = "M"
            updExe = buildExe [] [(Text.pack m, "M.hs")]
        updateSessionD session updExe 2

        msgs1 <- getSourceErrors session
        _ract <- runStmt session "M" "loop"
        msgs2 <- getSourceErrors session
        assertEqual "Running code does not affect getSourceErrors" msgs1 msgs2

        _runActionsExe <- runExe session m

        msgs3 <- getSourceErrors session
        assertEqual "Running exes does not affect getSourceErrors" msgs1 msgs3
    )
  , ( "getLoadedModules during run"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "{-# OPTIONS_GHC -Wall #-}"
                    , "module M where"
                    , "loop = loop"
                    ])
        updateSessionD session upd 1
        mods <- getLoadedModules session
        assertEqual "" [Text.pack "M"] mods
        _runActions <- runStmt session "M" "loop"
        mods' <- getLoadedModules session
        assertEqual "Running code does not affect getLoadedModules" mods mods'
    )
  , ( "Interrupt, then capture stdout"
    , withSession defaultSession $ \session -> do
        updateSession session (updateCodeGeneration True) (\_ -> return ())
        let upd1 = updateSourceFile "Main.hs" . BSLC.pack . unlines $
                     [ "import Control.Monad"
                     , "main = forever $ print 1"
                     ]
            upd2 = updateSourceFile "Main.hs" . BSLC.pack . unlines $
                     [ "main = print 1234" ]

        do updateSessionD session upd1 1
           runActions <- runStmt session "Main" "main"
           -- TODO: Not sure why 'interrupt' doesn't work here.
           --interrupt runActions
           forceCancel runActions
           randomRIO (0, 1000000) >>= threadDelay -- Wait between 0 and 1sec
           void $ runWaitAll runActions

        do let m = "Main"
               updExe = buildExe [] [(Text.pack m, "Main.hs")]
           updateSessionD session updExe 2
           runActionsExe <- runExe session m
           interrupt runActionsExe
           randomRIO (0, 1000000) >>= threadDelay -- Wait between 0 and 1sec
           void $ runWaitAll runActionsExe

        do updateSessionD session upd2 1
           runActions <- runStmt session "Main" "main"
           (output, result) <- runWaitAll runActions
           assertEqual "" RunOk result
           assertEqual "" (BSLC.pack "1234\n") output

        do let m = "Main"
               updExe = buildExe [] [(Text.pack m, "Main.hs")]
           updateSessionD session updExe 2
           runActionsExe <- runExe session m
           (outExe, statusExe) <- runWaitAll runActionsExe
           assertEqual "Output from runExe"
                       "1234\n"
                       outExe
           assertEqual "after runExe" ExitSuccess statusExe

    )
  , ( "Restart session (well-behaved snippet; after .1 sec)"
    , restartRun [ "module M where"
                 , "import Control.Concurrent (threadDelay)"
                 , "loop :: IO ()"
                 , "loop = threadDelay 10000 >> loop"
                 ] ExitSuccess
    )
  , ( "Restart session (blackhole, snippet doesn't swallow exceptions; after .1 sec)"
    , restartRun [ "module M where"
                 , "loop :: IO ()"
                 , "loop = loop"
                 ] ExitSuccess
    )
  , ( "Restart session (snippet swallows all exceptions; after .1 sec)"
    , restartRun [ "module M where"
                 , ""
                 , "import qualified Control.Exception as Ex"
                 , "import Control.Concurrent (threadDelay)"
                 , ""
                 , "innerLoop :: IO ()"
                 , "innerLoop = threadDelay 10000 >> innerLoop"
                 , ""
                 , "loop :: IO ()"
                 , "loop = Ex.catch innerLoop $ \\e -> let _ = e :: Ex.SomeException in loop"
                 ] ExitSuccess
    )
  , ( "Restart session (black hole, swallow all exceptions; after .1 sec)"
    , restartRun [ "module M where"
                 , ""
                 , "import qualified Control.Exception as Ex"
                 , "import Control.Concurrent (threadDelay)"
                 , ""
                 , "innerLoop :: IO ()"
                 , "innerLoop = innerLoop"
                 , ""
                 , "loop :: IO ()"
                 , "loop = Ex.catch innerLoop $ \\e -> let _ = e :: Ex.SomeException in loop"
                 ] ExitSuccess
    )
  , ( "Restart session (evil snippet with infinite stack of exception handlers; after .1 sec)"
    , restartRun [ "module M where"
                 , ""
                 , "import qualified Control.Exception as Ex"
                 , ""
                 , "loop :: IO ()"
                 , "loop = Ex.catch loop $ \\e -> let _ = e :: Ex.SomeException in loop"
                 ] ExitSuccess
    )
  , ( "Make sure environment is restored after session restart"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import System.Environment (getEnv)"
                    , "printFoo :: IO ()"
                    , "printFoo = getEnv \"Foo\" >>= putStr"
                    , "main :: IO ()"
                    , "main = printFoo"
                    ])

        -- Set environment
        updateSession session (updateEnv "Foo" (Just "Value1")) (\_ -> return ())

        -- Compile and run the code on the first server
        updateSessionD session upd 1
        assertNoErrors session
        do runActions <- runStmt session "M" "printFoo"
           (output, result) <- runWaitAll runActions
           assertEqual "" RunOk result
           assertEqual "" (BSLC.pack "Value1") output

        do let m = "M"
               updExe = buildExe [] [(Text.pack m, "M.hs")]
           updateSessionD session updExe 2
           runActionsExe <- runExe session m
           (outExe, statusExe) <- runWaitAll runActionsExe
           assertEqual "Output from runExe"
                       "Value1"
                       outExe
           assertEqual "after runExe" ExitSuccess statusExe

        -- Start a new server
        serverBefore <- getGhcServer session
        restartSession session Nothing

        -- Compile the code on the new server
        updateSessionD session upd 1
        assertNoErrors session

        -- Make sure the old server exited
        exitCodeBefore <- getGhcExitCode serverBefore
        assertEqual "exitCodeBefore" (Just ExitSuccess) exitCodeBefore

        -- Make sure the new server is still alive
        serverAfter <- getGhcServer session
        exitCodeAfter <- getGhcExitCode serverAfter
        assertEqual "exitCodeAfter" Nothing exitCodeAfter

        -- Make sure environment is restored
        do runActions <- runStmt session "M" "printFoo"
           (output, result) <- runWaitAll runActions
           assertEqual "" RunOk result
           assertEqual "" (BSLC.pack "Value1") output

        do let m = "M"
               updExe = buildExe [] [(Text.pack m, "M.hs")]
           updateSessionD session updExe 2
           runActionsExe <- runExe session m
           (outExe, statusExe) <- runWaitAll runActionsExe
           assertEqual "Output from runExe"
                       "Value1"
                       outExe
           assertEqual "after runExe" ExitSuccess statusExe
    )
  , ( "Buffer modes: RunNoBuffering"
    , testBufferMode RunNoBuffering
    )
  , ( "Buffer modes: RunLineBuffering, no timeout"
    , testBufferMode (RunLineBuffering Nothing)
    )
  , ( "Buffer modes: RunBlockBuffering, no timeout"
    , testBufferMode (RunBlockBuffering (Just 5) Nothing)
    )
  , ( "Buffer modes: RunLineBuffering, with timeout"
    , testBufferMode (RunLineBuffering (Just 1000000))
    )
  , ( "Buffer modes: RunBlockBuffering, with timeout"
    , testBufferMode (RunBlockBuffering (Just 4) (Just 1000000))
    )
  , ( "Buffer modes: RunBlockBuffering, buffer never fills, with timeout"
    , testBufferMode (RunBlockBuffering (Just 4096) (Just 1000000))
    )
  , ( "Call runWait after termination (normal termination)"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "hello :: IO ()"
                    , "hello = putStrLn \"Hello World\""
                    , "main :: IO ()"
                    , "main = hello"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        assertEqual "" RunOk result
        assertEqual "" (BSLC.pack "Hello World\n") output
        result' <- runWait runActions
        assertEqual "" result' (Right RunOk)

        let m = "M"
            updExe = buildExe [] [(Text.pack m, "M.hs")]
        updateSessionD session updExe 2
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "Hello World\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe

        result2 <- runWait runActionsExe
        assertEqual "" result2 (Right ExitSuccess)
    )
  , ( "Call runWait after termination (interrupted)"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import Control.Concurrent (threadDelay)"
                    , "loop :: IO ()"
                    , "loop = threadDelay 100000 >> loop"
                    , "main :: IO ()"
                    , "main = loop"
                    ])
        updateSessionD session upd 1
        assertNoErrors session

        let m = "M"
            updExe = buildExe [] [(Text.pack m, "M.hs")]
        updateSessionD session updExe 2
        runActionsExe <- runExe session m
        threadDelay 1000000
        interrupt runActionsExe
        resOrEx <- runWait runActionsExe
        case resOrEx of
          Right result -> assertEqual "after runExe" (ExitFailure 2) result
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
        result' <- runWait runActionsExe
        assertEqual "" result' (Right $ ExitFailure 2)

        runActions <- runStmt session "M" "loop"
        threadDelay 1000000
        interrupt runActions
        resOrEx2 <- runWait runActions
        case resOrEx2 of
          Right result -> assertBool "" (isAsyncException result)
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
        resOrEx' <- runWait runActions
        case resOrEx' of
          Right result -> assertBool "" (isAsyncException result)
          _ -> assertFailure $ "Unexpected run result in repeat call: " ++ show resOrEx'
    )
  , ( "Call runWait after termination (restarted session)"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import Control.Concurrent (threadDelay)"
                    , "loop :: IO ()"
                    , "loop = threadDelay 100000 >> loop"
                    , "main :: IO ()"
                    , "main = loop"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "loop"
        threadDelay 1000000
        restartSession session Nothing
        forceCancel runActions
        resOrEx2 <- runWait runActions
        case resOrEx2 of
          Right RunForceCancelled -> return ()
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx2
        resOrEx' <- runWait runActions
        case resOrEx' of
          Right RunForceCancelled -> return ()
          _ -> assertFailure $ "Unexpected run result in repeat call: " ++ show resOrEx'

        updateSessionD session mempty 1  -- needed to load the code again

        let m = "M"
            updExe = buildExe [] [(Text.pack m, "M.hs")]
        updateSessionD session updExe 2
        runActionsExe <- runExe session m
        threadDelay 1000000
        restartSession session Nothing
        -- restartSession would not suffice, since session restart
        -- doesn't stop the exe, so we need to interrupt manually.
        interrupt runActionsExe
        resOrEx <- runWait runActionsExe
        case resOrEx of
          Right result -> assertEqual "after runExe" (ExitFailure 2) result
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
        result' <- runWait runActionsExe
        assertEqual "" result' (Right $ ExitFailure 2)
    )
  , ( "Call runWait after termination (started new snippet in meantime)"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import Control.Concurrent"
                    , "hello :: IO ()"
                    , "hello = putStrLn \"Hello World\""
                    , "slowHello :: IO ()"
                    , "slowHello = threadDelay 2000000 >> putStrLn \"Oh, hello\""
                    ])
        updateSessionD session upd 1
        assertNoErrors session

        -- Start first snippet and wait for it to terminate
        runActions1 <- runStmt session "M" "hello"
        do (output, result) <- runWaitAll runActions1
           assertEqual "" RunOk result
           assertEqual "" (BSLC.pack "Hello World\n") output

        -- Start second snippet
        runActions2 <- runStmt session "M" "slowHello"

        -- While it is running, call runWait again on the old runActions, make
        -- sure it's still the same
        do result <- runWait runActions1
           assertEqual "" result (Right RunOk)

        -- Make sure that a call to 'runStmt' throws an exception
        -- (because we are still in running state)
        assertRaises "runStmt during running code"
          (== userError "State not idle")
          (runStmt session "M" "hello")

        -- Now call runWait on the *new* runActions and make sure we
        -- get the right result
        do (output, result) <- runWaitAll runActions2
           assertEqual "" RunOk result
           assertEqual "" (BSLC.pack "Oh, hello\n") output
    )
  , ( "Don't recompile unnecessarily (single module)"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "a :: IO ()"
                    , "a = print 'a'"
                    ])

        counter <- newCounter

        updateSession session upd (\_ -> incCounter counter)
        assertCounter counter 1

        resetCounter counter
        updateSession session upd (\_ -> incCounter counter)
        assertCounter counter 0
    )
  , ( "Don't recompile unnecessarily (A depends on B)"
    , withSession defaultSession $ \session -> do
        -- 'updA' is defined so that the interface of 'updA n' is different
        -- to the interface of 'updA m' (with n /= m)
        let updA n = updateSourceFile "A.hs" . BSLC.pack . unlines $
                       [ "module A where"
                       , "import B"
                       ]
                      ++
                       [ "a" ++ show i ++ " = b" ++ show i
                       | i <- [0 .. n :: Int]
                       ]
        let updB n = updateSourceFile "B.hs" . BSLC.pack . unlines $
                       [ "module B where"
                       ]
                      ++
                       [ "b" ++ show i ++ " = return () :: IO ()"
                       | i <- [0 .. n :: Int]
                       ]
        let upd = updateCodeGeneration True <> updA 0 <> updB 0

        counter <- newCounter

        -- Initial compilation needs to recompile for A and B
        updateSession session upd (\_ -> incCounter counter)
        assertCounter counter 2

        -- Overwriting B with the same code requires no compilation at all
        resetCounter counter
        updateSession session (updB 0) (\_ -> incCounter counter)
        assertCounter counter 0

        -- Nor does overwriting A with the same code
        resetCounter counter
        updateSession session (updA 0) (\_ -> incCounter counter)
        assertCounter counter 0

        -- Giving B a new interface means both A and B need to be recompiled
        resetCounter counter
        updateSession session (updB 1) (\_ -> incCounter counter)
        assertCounter counter 2

        -- Changing the interface of A only requires recompilation of A
        resetCounter counter
        updateSession session (updA 1) (\_ -> incCounter counter)
        assertCounter counter 1
    )
  , ( "First snippet closes stdin; next snippet unaffected"
    , withSession defaultSession $ \session -> do
        let updates2 = mconcat
                [ updateCodeGeneration True
                , updateSourceFile "Main.hs" (BSLC.pack "import System.IO\nmain = hClose stdin")
                ]
        updateSession session updates2 $ const $ return ()
        ra2 <- runStmt session "Main" "main"
        out2b <- runWait ra2
        assertEqual "" out2b (Right RunOk)

        let updates3 =
              updateSourceFile "Main.hs" (BSLC.pack "main = getLine >>= putStrLn")
        updateSession session updates3 $ const $ return ()
        ra3 <- runStmt session "Main" "main"
        supplyStdin ra3 (BSSC.pack "Michael\n")
        (output, out3b) <- runWaitAll ra3
        assertEqual "" RunOk out3b
        assertEqual "" (BSLC.pack "Michael\n") output
    )
  , ( "First snippet closes stdin (interrupted 'interact'); next snippet unaffected"
    , withSession defaultSession $ \session -> do
        let updates2 = mconcat
                [ updateCodeGeneration True
                , updateSourceFile "Main.hs" (BSLC.pack "main = getContents >>= putStr")
                , updateStdoutBufferMode $ RunLineBuffering Nothing
                ]
        updateSession session updates2 $ const $ return ()
        ra2 <- runStmt session "Main" "main"
        supplyStdin ra2 (BSSC.pack "hello\n")
        out2a <- runWait ra2
        out2a @?= Left (BSSC.pack "hello\n")
        interrupt ra2
        out2b <- runWait ra2
        case out2b of
          Right result -> assertBool "" (isAsyncException result)
          _ -> assertFailure $ "Unexpected run result: " ++ show out2b

        let updates3 = mconcat
                [ updateCodeGeneration True
                , updateSourceFile "Main.hs" (BSLC.pack "main = putStrLn \"Hi!\" >> getLine >> return ()")
                , updateStdoutBufferMode $ RunLineBuffering Nothing
                ]
        updateSession session updates3 $ const $ return ()
        ra3 <- runStmt session "Main" "main"
        out3a <- runWait ra3
        out3a @?= Left (BSSC.pack "Hi!\n")
        supplyStdin ra3 (BSSC.pack "Michael\n")
        out3b <- runWait ra3
        assertEqual "" out3b (Right RunOk)
    )
  , ( "First snippet closes stdout; next snippet unaffected"
    , withSession defaultSession $ \session -> do
        let updates2 = mconcat
                [ updateCodeGeneration True
                , updateSourceFile "Main.hs" (BSLC.pack "import System.IO\nmain = hClose stdout")
                ]
        updateSession session updates2 $ const $ return ()
        ra2 <- runStmt session "Main" "main"
        out2b <- runWait ra2
        assertEqual "ra2" out2b (Right RunOk)

        let updates3 =
              updateSourceFile "Main.hs" (BSLC.pack "main = getLine >>= putStrLn")
        updateSession session updates3 $ const $ return ()
        ra3 <- runStmt session "Main" "main"
        supplyStdin ra3 (BSSC.pack "Michael\n")
        (output, out3b) <- runWaitAll ra3
        assertEqual "ra3" out3b RunOk
        assertEqual "" (BSLC.pack "Michael\n") output
    )
  , ( "First snippet closes stderr; next snippet unaffected"
    , withSession defaultSession $ \session -> do
        let updates2 = mconcat
                [ updateCodeGeneration True
                , updateSourceFile "Main.hs" (BSLC.pack "import System.IO\nmain = hClose stderr")
                ]
        updateSession session updates2 $ const $ return ()
        ra2 <- runStmt session "Main" "main"
        out2b <- runWait ra2
        assertEqual "ra2" out2b (Right RunOk)

        let updates3 =
              updateSourceFile "Main.hs" (BSLC.pack "import System.IO\nmain = getLine >>= hPutStrLn stderr")
        updateSession session updates3 $ const $ return ()
        ra3 <- runStmt session "Main" "main"
        supplyStdin ra3 (BSSC.pack "Michael\n")
        (output, out3b) <- runWaitAll ra3
        assertEqual "ra3" out3b RunOk
        assertEqual "" (BSLC.pack "Michael\n") output
    )
  , ( "Snippet closes stderr, using timeout buffering"
    , withSession defaultSession $ \session -> do
        let upd = mconcat [
                      updateCodeGeneration True
                    , updateStdoutBufferMode $ RunLineBuffering Nothing
                    , updateStderrBufferMode $ RunBlockBuffering (Just 4096) (Just 250000)
                    , updateSourceFile "Main.hs" . BSLC.pack . unlines $ [
                          "import Control.Concurrent"
                        , "import Control.Monad"
                        , "import System.IO"
                        , "main :: IO ()"
                        , "main = do"
                        , "  hClose stderr"
                        , "  forM_ [1 :: Int .. 3] $ \\i -> do"
                        , "    print i"
                        , "    threadDelay 500000"
                        ]
                    ]
        updateSessionD session upd 1
        ra <- runStmt session "Main" "main"
        forM_ [1 :: Int .. 3] $ \i -> do
          result <- runWait ra
          result @?= Left (BSSC.pack $ show i ++ "\n")

        finalResult <- runWait ra
        assertEqual "" finalResult (Right RunOk)
     )
  , ( "Make sure encoding is UTF8"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSL8.fromString . unlines $
                    [ "module M where"
                    , "main :: IO ()"
                    , "main = putStrLn \"你好\""
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "main"
        (output, result) <- runWaitAll runActions
        assertEqual "" RunOk result
        assertEqual "" (BSL8.fromString "你好\n") output

        {- This is probably not fixable, because the code itself would need
        -- to specify IO.utf8, and we don't want to modify it.
        let m = "M"
            updExe = buildExe [] [(Text.pack m, "M.hs")]
        updateSessionD session updExe 2
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                   "你好\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe
        -}
    )
  , ( "Using something from a different package (no \"Loading package\" msg)"
      -- We pick something from the haskell platform but that doesn't come with ghc itself
      -- https://github.com/haskell/haskell-platform/blob/2012.4.0.0/haskell-platform.cabal
    , withSession (withOpts []) $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSL8.fromString . unlines $
                    [ "module M where"
                    , "import Control.Monad.IO.Class" -- From transformers
                    , "hello :: IO ()"
                    , "hello = liftIO $ print 5"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        assertEqual "" RunOk result
        assertEqual "" (BSL8.fromString "5\n") output
    )
  , ( "Using the FFI via GHC API"
    , withSession defaultSession $ \session -> do
        let upd = mconcat [
                updateCodeGeneration True
              , updateSourceFileFromFile "test/FFI/Main.hs"
              , updateSourceFileFromFile "test/FFI/life.c"
              , updateSourceFileFromFile "test/FFI/life.h"
              ]
        updateSessionD session upd 3
        assertNoErrors session
        runActions <- runStmt session "Main" "main"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk -> assertEqual "" (BSL8.fromString "42\n") output
          _     -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Using the FFI via GHC API with restartSession"
    , withSession defaultSession $ \session -> do
        let upd = mconcat [
                updateCodeGeneration True
              , updateSourceFileFromFile "test/FFI/Main.hs"
              , updateSourceFileFromFile "test/FFI/life.c"
              , updateSourceFileFromFile "test/FFI/life.h"
              ]
        updateSessionD session upd 3
        assertNoErrors session

        restartSession session Nothing
        updateSessionD session mempty 3
        assertNoErrors session

        runActions <- runStmt session "Main" "main"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk -> assertEqual "" (BSL8.fromString "42\n") output
          _     -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Using the FFI via GHC API with deleting and re-adding the .c file"
    , withSession defaultSession $ \session -> do
        let upd = mconcat [
                updateCodeGeneration True
              , updateSourceFileFromFile "test/FFI/Main.hs"
              , updateSourceFileFromFile "test/FFI/life.c"
              , updateSourceFileFromFile "test/FFI/life.h"
              ]
        updateSessionD session upd 3
        assertNoErrors session

        updateSessionD session (updateSourceFileDelete "test/FFI/life.c") 0
        assertNoErrors session

        updateSessionD session (updateSourceFileFromFile "test/FFI/life.c") 4
        assertNoErrors session

        updateSessionD session (updateSourceFileDelete "test/FFI/life.c") 0
        assertNoErrors session

        restartSession session Nothing
        updateSessionD session mempty 1
        assertOneError session

        updateSessionD session (updateSourceFileFromFile "test/FFI/life.c") 4
        assertNoErrors session

        runActions <- runStmt session "Main" "main"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk -> assertEqual "" (BSL8.fromString "42\n") output
          _     -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Using the FFI via GHC API with deleting and adding a different .c file"
    , withSession defaultSession $ \session -> do
        let upd = mconcat [
                updateCodeGeneration True
              , updateSourceFileFromFile "test/FFI/Main.hs"
              , updateSourceFileFromFile "test/FFI/life.c"
              , updateSourceFileFromFile "test/FFI/life.h"
              ]
        updateSessionD session upd 3
        assertNoErrors session

        updateSessionD session (updateSourceFileDelete "test/FFI/life.c"
                                <> updateSourceFileDelete "test/FFI/life.h") 0
        assertNoErrors session

{- duplicate definition for symbol...    errorMsg = "Server killed"
        updateSessionD session (updateSourceFileFromFile "test/FFI/ffiles/life.c"
                                <> updateSourceFileFromFile "test/FFI/ffiles/local.h"
                                <> updateSourceFileFromFile "test/FFI/ffiles/life.h") 4
        assertNoErrors session
        runActions <- runStmt session "Main" "main"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk -> assertEqual "" (BSL8.fromString "42\n") output
          _     -> assertFailure $ "Unexpected run result: " ++ show result
-}
    )
  , ( "Using the FFI from a subdir and compiled via buildExe"
    , withSession defaultSession $ \session -> do
        let upd = mconcat [
                updateCodeGeneration True
              , updateSourceFileFromFile "test/FFI/Main2.hs"
              , updateSourceFileFromFile "test/FFI/ffiles/life.c"
              , updateSourceFileFromFile "test/FFI/ffiles/life.h"
              , updateSourceFileFromFile "test/FFI/ffiles/local.h"
              ]
        updateSessionD session upd 3
        assertNoErrors session
        let m = "Main"
            upd2 = buildExe [] [(Text.pack m, "test/FFI/Main2.hs")]
        updateSessionD session upd2 1
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "buildStderr empty" "" buildStderr
        exeOut <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "FFI exe output" "42\n" exeOut
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "42\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe

        dotCabalFromName <- getDotCabal session
        let dotCabal = dotCabalFromName "libName" $ Version [1, 0] []
        assertEqual "dotCabal" (filterIdeBackendTestH "life.h" $ filterIdeBackendTestC "life.c" $ filterIdeBackendTest $ BSLC.pack "name: libName\nversion: 1.0\ncabal-version: 1.14.0\nbuild-type: Simple\nlicense: AllRightsReserved\nlicense-file: \"\"\ndata-dir: \"\"\n \nlibrary\n    build-depends: base ==4.5.1.0, ghc-prim ==0.2.0.0,\n                   integer-gmp ==0.4.0.0\n    exposed: True\n    buildable: True\n    c-sources: life.c\n    default-language: Haskell2010\n    install-includes: life.h local.h\n \n ") $ filterIdeBackendTestH "life.h" $ filterIdeBackendTestC "life.c" $ filterIdeBackendTest dotCabal
        let pkgDir = distDir </> "dotCabal.test"
        createDirectoryIfMissing False pkgDir
        BSLC.writeFile (pkgDir </> "libName.cabal") dotCabal
        checkWarns <- checkPackage pkgDir
        assertCheckWarns checkWarns
    )
  , ( "Using the FFI with TH and MIN_VERSION_base via buildExe"
    , withSession defaultSession $ \session -> do
        let upd = mconcat [
                updateCodeGeneration True
              , updateSourceFileFromFile "Main3.hs"
              , updateSourceFileFromFile "A.hs"
              , updateSourceFileFromFile "ffiles/life.c"
              , updateSourceFileFromFile "ffiles/life.h"
              , updateSourceFileFromFile "ffiles/local.h"
              ]
        setCurrentDirectory "test/FFI"
        updateSessionD session upd 4
        setCurrentDirectory "../../"
        assertNoErrors session
        let m = "Main"
            upd2 = buildExe [] [(Text.pack m, "Main3.hs")]
        updateSessionD session upd2 3
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "buildStderr empty" "" buildStderr
        exeOut <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "FFI exe output" "84\n" exeOut
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "84\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe

        dotCabalFromName <- getDotCabal session
        let dotCabal = dotCabalFromName "libName" $ Version [1, 0] []
        assertEqual "dotCabal" (filterIdeBackendTestH "life.h" $ filterIdeBackendTestC "life.c" $ filterIdeBackendTest $ BSLC.pack "name: libName\nversion: X.Y.Z\ncabal-version: X.Y.Z\nbuild-type: Simple\nlicense: AllRightsReserved\nlicense-file: \"\"\ndata-dir: \"\"\n \nlibrary\n    build-depends: array ==X.Y.Z, base ==X.Y.Z,\n                   containers ==X.Y.Z, deepseq ==X.Y.Z, ghc-prim ==X.Y.Z,\n                   integer-gmp ==X.Y.Z, pretty ==X.Y.Z, template-haskell ==X.Y.Z\n    exposed-modules: A\n    exposed: True\n    buildable: Truelife.c\n    default-language: Haskell2010life.h local.h\n \n ") $ filterIdeBackendTestH "life.h" $ filterIdeBackendTestC "life.c" $ filterIdeBackendTest dotCabal
        let pkgDir = distDir </> "dotCabal.test"
        createDirectoryIfMissing False pkgDir
        BSLC.writeFile (pkgDir </> "libName.cabal") dotCabal
        checkWarns <- checkPackage pkgDir
        assertCheckWarns checkWarns
    )
  , ( "Using the FFI with withIncludes, TH and MIN_VERSION_base via buildExe"
    , withSession (withIncludes "test/FFI") $ \session -> do
        let upd = mconcat [
                updateCodeGeneration True
              , updateSourceFileFromFile "test/FFI/Main3.hs"
              , updateSourceFileFromFile "test/FFI/A.hs"
              , updateSourceFileFromFile "test/FFI/ffiles/life.c"
              , updateSourceFileFromFile "test/FFI/ffiles/life.h"
              , updateSourceFileFromFile "test/FFI/ffiles/local.h"
              ]
        updateSessionD session upd 4
        assertNoErrors session
        let m = "Main"
            upd2 = buildExe [] [(Text.pack m, "Main3.hs")]
        updateSessionD session upd2 3
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "buildStderr empty" "" buildStderr
        exeOut <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "FFI exe output" "84\n" exeOut
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "84\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe

        dotCabalFromName <- getDotCabal session
        let dotCabal = dotCabalFromName "libName" $ Version [1, 0] []
        assertEqual "dotCabal" (filterIdeBackendTestH "life.h" $ filterIdeBackendTestC "life.c" $ filterIdeBackendTest $ BSLC.pack "name: libName\nversion: X.Y.Z\ncabal-version: X.Y.Z\nbuild-type: Simple\nlicense: AllRightsReserved\nlicense-file: \"\"\ndata-dir: \"\"\n \nlibrary\n    build-depends: array ==X.Y.Z, base ==X.Y.Z,\n                   containers ==X.Y.Z, deepseq ==X.Y.Z, ghc-prim ==X.Y.Z,\n                   integer-gmp ==X.Y.Z, pretty ==X.Y.Z, template-haskell ==X.Y.Z\n    exposed-modules: A\n    exposed: True\n    buildable: Truelife.c\n    default-language: Haskell2010life.h local.h\n \n ") $ filterIdeBackendTestH "life.h" $ filterIdeBackendTestC "life.c" $ filterIdeBackendTest dotCabal
        let pkgDir = distDir </> "dotCabal.test"
        createDirectoryIfMissing False pkgDir
        BSLC.writeFile (pkgDir </> "libName.cabal") dotCabal
        checkWarns <- checkPackage pkgDir
        assertCheckWarns checkWarns
    )
  , ( "Using the FFI with withIncludes, TH and MIN_VERSION_base via buildExe and with restartSession"
    , withSession (withIncludes "test/FFI") $ \session -> do
        let upd = mconcat [
                updateCodeGeneration True
              , updateSourceFileFromFile "test/FFI/Main3.hs"
              , updateSourceFileFromFile "test/FFI/A.hs"
              , updateSourceFileFromFile "test/FFI/ffiles/life.c"
              , updateSourceFileFromFile "test/FFI/ffiles/life.h"
              , updateSourceFileFromFile "test/FFI/ffiles/local.h"
              ]
        updateSessionD session upd 4
        assertNoErrors session

        restartSession session Nothing
        updateSessionD session mempty 4
        assertNoErrors session

        updateSessionD session (updateSourceFileDelete "test/FFI/ffiles/life.c"
                                <> updateSourceFileDelete "test/FFI/ffiles/life.h"
                                <> updateSourceFileDelete "test/FFI/ffiles/local.h") 0
        assertNoErrors session

        restartSession session Nothing
        updateSessionD session mempty 4
        assertOneError session

        updateSessionD session (updateSourceFileFromFile "test/FFI/life.h"
                                <> updateSourceFileFromFile "test/FFI/life.c") 4
        assertNoErrors session

        let m = "Main"
            upd2 = buildExe [] [(Text.pack m, "Main3.hs")]
        updateSessionD session upd2 3
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "buildStderr empty" "" buildStderr
        exeOut <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "FFI exe output" "84\n" exeOut
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "84\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe

        dotCabalFromName <- getDotCabal session
        let dotCabal = dotCabalFromName "libName" $ Version [1, 0] []
        assertEqual "dotCabal" (filterIdeBackendTestH "life.h" $ filterIdeBackendTestC "life.c" $ filterIdeBackendTest $ BSLC.pack "name: libName\nversion: X.Y.Z\ncabal-version: X.Y.Z\nbuild-type: Simple\nlicense: AllRightsReserved\nlicense-file: \"\"\ndata-dir: \"\"\n \nlibrary\n    build-depends: array ==X.Y.Z, base ==X.Y.Z,\n                   containers ==X.Y.Z, deepseq ==X.Y.Z, ghc-prim ==X.Y.Z,\n                   integer-gmp ==X.Y.Z, pretty ==X.Y.Z, template-haskell ==X.Y.Z\n    exposed-modules: A\n    exposed: True\n    buildable: Truelife.c\n    default-language: Haskell2010life.h\n \n ") $ filterIdeBackendTestH "life.h" $ filterIdeBackendTestC "life.c" $ filterIdeBackendTest dotCabal
        let pkgDir = distDir </> "dotCabal.test"
        createDirectoryIfMissing False pkgDir
        BSLC.writeFile (pkgDir </> "libName.cabal") dotCabal
        checkWarns <- checkPackage pkgDir
        assertCheckWarns checkWarns
    )
  , ( "Using the FFI with withIncludes and TargetsExclude"
    , withSession (withIncludes "test/FFI") $ \session -> do
        let upd = mconcat [
                updateCodeGeneration True
              , updateSourceFileFromFile "test/FFI/Main.hs"
              , updateSourceFileFromFile "test/FFI/Main2.hs"
              , updateSourceFileFromFile "test/FFI/Main3.hs"
              , updateSourceFileFromFile "test/FFI/A.hs"
              , updateSourceFileFromFile "test/FFI/ffiles/life.c"
              , updateSourceFileFromFile "test/FFI/ffiles/life.h"
              , updateSourceFileFromFile "test/FFI/ffiles/local.h"
              , updateTargets (TargetsExclude ["test/FFI/life.c", "test/FFI/life.h", "life.c", "life.h", "test/FFI/Main.hs", "test/FFI/Main2.hs"])
                            ]
        updateSessionD session upd 4
        assertNoErrors session
        updateSessionD session (updateSourceFileDelete "test/FFI/ffiles/life.c") 0
        assertNoErrors session

        restartSession session Nothing
        updateSessionD session mempty 1
        assertOneError session

        -- Without the restartSession we get
{-
GHCi runtime linker: fatal error: I found a duplicate definition for symbol
   meaningOfLife
whilst processing object file
   /tmp/ide-backend-test.28928/dist.28928/objs/test/FFI/ffiles/life.o
This could be caused by:
   * Loading two different object files which export the same symbol
   * Specifying the same object file twice on the GHCi command line
   * An incorrect `package.conf' entry, causing some object to be
     loaded twice.
GHCi cannot safely continue in this situation.  Exiting now.  Sorry.

  Using the FFI via GHC API with deleting and adding a different .c file: [Failed]
Unexpected errors: SourceError {errorKind = KindServerDied, errorSpan = <<server died>>, errorMsg = "Server killed"}
-}
        updateSessionD session (updateSourceFileFromFile "test/FFI/life.c"
                                <> updateSourceFileFromFile "test/FFI/life.h") 5
        assertNoErrors session

        let m = "Main"
            upd2 = buildExe [] [(Text.pack m, "Main3.hs")]
        updateSessionD session upd2 3
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "buildStderr empty" "" buildStderr
        exeOut <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "FFI exe output" "84\n" exeOut
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "84\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe

        dotCabalFromName <- getDotCabal session
        let dotCabal = dotCabalFromName "libName" $ Version [1, 0] []
        assertEqual "dotCabal" (filterIdeBackendTestH "life.h" $ filterIdeBackendTestC "life.c" $ filterIdeBackendTest $ BSLC.pack "name: libName\nversion: X.Y.Z\ncabal-version: X.Y.Z\nbuild-type: Simple\nlicense: AllRightsReserved\nlicense-file: \"\"\ndata-dir: \"\"\n \nlibrary\n    build-depends: array ==X.Y.Z, base ==X.Y.Z,\n                   containers ==X.Y.Z, deepseq ==X.Y.Z, ghc-prim ==X.Y.Z,\n                   integer-gmp ==X.Y.Z, pretty ==X.Y.Z, template-haskell ==X.Y.Z\n    exposed-modules: A\n    exposed: True\n    buildable: Truelife.c\n    default-language: Haskell2010life.h local.h life.h\n \n ") $ filterIdeBackendTestH "life.h" $ filterIdeBackendTestC "life.c" $ filterIdeBackendTest dotCabal
        let pkgDir = distDir </> "dotCabal.test"
        createDirectoryIfMissing False pkgDir
        BSLC.writeFile (pkgDir </> "libName.cabal") dotCabal
        checkWarns <- checkPackage pkgDir
        assertCheckWarns checkWarns
    )
  , ( "Using the FFI with dynamic include, TH and MIN_VERSION_base via buildExe"
    , withSession defaultSession $ \session -> do
        updateSessionD session
                       (updateRelativeIncludes [])
                       0
        let upd = mconcat [
                updateCodeGeneration True
              , updateSourceFileFromFile "test/FFI/Main3.hs"
              , updateSourceFileFromFile "test/FFI/A.hs"
              , updateSourceFileFromFile "test/FFI/ffiles/life.c"
              , updateSourceFileFromFile "test/FFI/ffiles/life.h"
              , updateSourceFileFromFile "test/FFI/ffiles/local.h"
              ]
        updateSessionD session upd 4
        assertNoErrors session

        updateSessionD session
                       (updateRelativeIncludes ["test/FFI"])
                       4
        assertNoErrors session

        let m = "Main"
            upd2 = buildExe [] [(Text.pack m, "Main3.hs")]
        updateSessionD session upd2 3
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "buildStderr empty" "" buildStderr
        exeOut <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "FFI exe output" "84\n" exeOut
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "84\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe

        dotCabalFromName <- getDotCabal session
        let dotCabal = dotCabalFromName "libName" $ Version [1, 0] []
        assertEqual "dotCabal" (filterIdeBackendTestH "life.h" $ filterIdeBackendTestC "life.c" $ filterIdeBackendTest $ BSLC.pack "name: libName\nversion: X.Y.Z\ncabal-version: X.Y.Z\nbuild-type: Simple\nlicense: AllRightsReserved\nlicense-file: \"\"\ndata-dir: \"\"\n \nlibrary\n    build-depends: array ==X.Y.Z, base ==X.Y.Z,\n                   containers ==X.Y.Z, deepseq ==X.Y.Z, ghc-prim ==X.Y.Z,\n                   integer-gmp ==X.Y.Z, pretty ==X.Y.Z, template-haskell ==X.Y.Z\n    exposed-modules: A\n    exposed: True\n    buildable: Truelife.c\n    default-language: Haskell2010life.h local.h\n \n ") $ filterIdeBackendTestH "life.h" $ filterIdeBackendTestC "life.c" $ filterIdeBackendTest dotCabal
        let pkgDir = distDir </> "dotCabal.test"
        createDirectoryIfMissing False pkgDir
        BSLC.writeFile (pkgDir </> "libName.cabal") dotCabal
        checkWarns <- checkPackage pkgDir
        assertCheckWarns checkWarns
    )
  , ( "Using the FFI with dynamic include and TargetsInclude"
    , withSession defaultSession $ \session -> do
        let upd = mconcat [
                updateCodeGeneration True
              , updateTargets (TargetsInclude ["test/FFI/Main.hs"])
              , updateSourceFileFromFile "test/FFI/Main.hs"
              , updateSourceFileFromFile "test/FFI/Main2.hs"
              , updateSourceFileFromFile "test/FFI/Main3.hs"
              , updateSourceFileFromFile "test/FFI/A.hs"
              , updateSourceFileFromFile "test/FFI/ffiles/life.c"
              , updateSourceFileFromFile "test/FFI/ffiles/life.h"
              , updateSourceFileFromFile "test/FFI/ffiles/local.h"
              ]
        updateSessionD session upd 4
        assertNoErrors session

        updateSessionD session
                       (updateRelativeIncludes ["test/FFI"])
                       4
        assertNoErrors session

        updateSessionD session (updateTargets (TargetsInclude ["test/FFI/Main2.hs"])) 3
        assertNoErrors session

        updateSessionD session (updateTargets (TargetsInclude ["test/FFI/Main3.hs"])) 4
        assertNoErrors session

        let m = "Main"
            upd2 = buildExe [] [(Text.pack m, "Main3.hs")]
        updateSessionD session upd2 3
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "buildStderr empty" "" buildStderr
        exeOut <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "FFI exe output" "84\n" exeOut
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "84\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe

        dotCabalFromName <- getDotCabal session
        let dotCabal = dotCabalFromName "libName" $ Version [1, 0] []
        assertEqual "dotCabal" (filterIdeBackendTestH "life.h" $ filterIdeBackendTestC "life.c" $ filterIdeBackendTest $ BSLC.pack "name: libName\nversion: X.Y.Z\ncabal-version: X.Y.Z\nbuild-type: Simple\nlicense: AllRightsReserved\nlicense-file: \"\"\ndata-dir: \"\"\n \nlibrary\n    build-depends: array ==X.Y.Z, base ==X.Y.Z,\n                   containers ==X.Y.Z, deepseq ==X.Y.Z, ghc-prim ==X.Y.Z,\n                   integer-gmp ==X.Y.Z, pretty ==X.Y.Z, template-haskell ==X.Y.Z\n    exposed-modules: A\n    exposed: True\n    buildable: Truelife.c\n    default-language: Haskell2010life.h local.h\n \n ") $ filterIdeBackendTestH "life.h" $ filterIdeBackendTestC "life.c" $ filterIdeBackendTest dotCabal
        let pkgDir = distDir </> "dotCabal.test"
        createDirectoryIfMissing False pkgDir
        BSLC.writeFile (pkgDir </> "libName.cabal") dotCabal
        checkWarns <- checkPackage pkgDir
        assertCheckWarns checkWarns
    )
  , ( "Build executable from 2 TH files"
    , withSession defaultSession $ \session -> do
        let upd = updateCodeGeneration True
               <> (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE TemplateHaskell #-}"
                    , "module A where"
                    , "import Language.Haskell.TH"
                    , "ex1 :: Q Exp"
                    , "ex1 = [| \\x -> x |]"
                    , "ex2 :: Q Type"
                    , "ex2 = [t| String -> String |]"
                    , "ex3 :: Q [Dec]"
                    , "ex3 = [d| foo x = x |]"
                    ])
               <> (updateSourceFile "B.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE TemplateHaskell #-}"
                    , "module Main where"
                    , "import A"
                      -- Types and expressions
                    , "ex5 :: $ex2"
                    , "ex5 = $ex1"
                      -- Just to test slightly larger expressions
                    , "ex6 :: $(return =<< ex2)"
                    , "ex6 = $(ex1 >>= return)"
                      -- Declarations
                    , "$ex3"
                      -- Outcome
                    , "main :: IO ()"
                    , "main = print $ $ex1 42"
                    ])
        updateSessionD session upd 2
        assertNoErrors session
        let m = "Main"
            upd2 = buildExe [] [(Text.pack m, "B.hs")]
        updateSessionD session upd2 3
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "buildStderr empty" "" buildStderr
    )
  , ( "Build executable from Main"
    , withSession (withOpts []) $ \session -> do
        setCurrentDirectory "test/MainModule"
        loadModulesFrom session "."
        assertNoErrors session
        setCurrentDirectory "../../"
        let m = "Main"
            upd = buildExe [] [(Text.pack m, "ParFib.hs")]
        updateSessionD session upd 3
        distDir <- getDistDir session
        fibOut <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "ParFib exe output"
                    "running 'A single file with a code to run in parallel' from test/MainModule, which says fib 24 = 75025\n"
                    fibOut
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "running 'A single file with a code to run in parallel' from test/MainModule, which says fib 24 = 75025\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe

        dotCabalFromName <- getDotCabal session
        let dotCabal = dotCabalFromName "main" $ Version [1, 0] []
        assertEqual "dotCabal from Main" (filterIdeBackendTest $ BSLC.pack "name: main\nversion: 1.0\ncabal-version: 1.14.0\nbuild-type: Simple\nlicense: AllRightsReserved\nlicense-file: \"\"\ndata-dir: \"\"\n \nlibrary\n    build-depends: base ==4.5.1.0, ghc-prim ==0.2.0.0,\n                   integer-gmp ==0.4.0.0, old-locale ==1.0.0.4, old-time ==1.1.0.0,\n                   parallel ==3.2.0.4\n    exposed-modules: ParFib.Main\n    exposed: True\n    buildable: True\n    default-language: Haskell2010\n \n ") $ filterIdeBackendTest dotCabal
        let pkgDir = distDir </> "dotCabal.for.lhs"
        createDirectoryIfMissing False pkgDir
        BSLC.writeFile (pkgDir </> "main.cabal") dotCabal
        checkWarns <- checkPackage pkgDir
        assertCheckWarns checkWarns
    )
  , ( "Build executable from Main with explicit -package"
    , let packageOpts = [ "-hide-all-packages"
                        , "-package base"
                        , "-package parallel"
                        , "-package old-time"
                        ]
      in withSession (withOpts packageOpts) $ \session -> do
        setCurrentDirectory "test/MainModule"
        loadModulesFrom session "."
        assertNoErrors session
        setCurrentDirectory "../../"
        let m = "Main"
            upd = buildExe [] [(Text.pack m, "ParFib.hs")]
        updateSessionD session upd 3
        distDir <- getDistDir session
        fibOut <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "ParFib exe output"
                    "running 'A single file with a code to run in parallel' from test/MainModule, which says fib 24 = 75025\n"
                    fibOut
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "running 'A single file with a code to run in parallel' from test/MainModule, which says fib 24 = 75025\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe

        dotCabalFromName <- getDotCabal session
        let dotCabal = dotCabalFromName "libName" $ Version [1, 0] []
        assertEqual "dotCabal from Main with explicit -package" (filterIdeBackendTest $ BSLC.pack "name: libName\nversion: 1.0\ncabal-version: 1.14.0\nbuild-type: Simple\nlicense: AllRightsReserved\nlicense-file: \"\"\ndata-dir: \"\"\n \nlibrary\n    build-depends: base ==4.5.1.0, ghc-prim ==0.2.0.0,\n                   integer-gmp ==0.4.0.0, old-locale ==1.0.0.4, old-time ==1.1.0.0,\n                   parallel ==3.2.0.4\n    exposed-modules: ParFib.Main\n    exposed: True\n    buildable: True\n    default-language: Haskell2010\n    ghc-options: -hide-all-packages -package base -package parallel -package old-time\n \n ") $ filterIdeBackendTest dotCabal
        let pkgDir = distDir </> "dotCabal.for.lhs"
        createDirectoryIfMissing False pkgDir
        BSLC.writeFile (pkgDir </> "libName.cabal") dotCabal
        checkWarns <- checkPackage pkgDir
        assertCheckWarns checkWarns
    )
  , ( "Build executable from ParFib.Main"
    , withSession (withOpts []) $ \session -> do
        setCurrentDirectory "test/MainModule"
        loadModulesFrom session "."
        assertNoErrors session
        setCurrentDirectory "../../"
        let m = "ParFib.Main"
            upd = buildExe [] [ (Text.pack m, "ParFib.Main.hs")
                              , (Text.pack "Main", "ParFib.hs") ]
        updateSessionD session upd 4
        assertNoErrors session
        let upd2 = buildExe [] [(Text.pack "Main", "ParFib.hs")]
        updateSessionD session upd2 0
        distDir <- getDistDir session
        fibOut <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "ParFib exe output"
                    "running 'A single file with a code to run in parallel' from test/MainModule, which says fib 24 = 75025\n"
                    fibOut
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "running 'A single file with a code to run in parallel' from test/MainModule, which says fib 24 = 75025\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe

        dotCabalFromName <- getDotCabal session
        let dotCabal = dotCabalFromName "libName" $ Version [1, 7] []
        assertEqual "dotCabal from ParFib.Main" (filterIdeBackendTest $ BSLC.pack "name: libName\nversion: 1.7\ncabal-version: 1.14.0\nbuild-type: Simple\nlicense: AllRightsReserved\nlicense-file: \"\"\ndata-dir: \"\"\n \nlibrary\n    build-depends: base ==4.5.1.0, ghc-prim ==0.2.0.0,\n                   integer-gmp ==0.4.0.0, old-locale ==1.0.0.4, old-time ==1.1.0.0,\n                   parallel ==3.2.0.4\n    exposed-modules: ParFib.Main\n    exposed: True\n    buildable: True\n    default-language: Haskell2010\n \n ") $ filterIdeBackendTest dotCabal
        let pkgDir = distDir </> "dotCabal.for.lhs"
        createDirectoryIfMissing False pkgDir
        BSLC.writeFile (pkgDir </> "libName.cabal") dotCabal
        checkWarns <- checkPackage pkgDir
        assertCheckWarns checkWarns
    )
  , ( "Build executable with a wrong filename and fail"
    , withSession (withOpts []) $ \session -> do
        setCurrentDirectory "test/MainModule"
        loadModulesFrom session "."
        assertNoErrors session
        setCurrentDirectory "../../"
        let m = "Main"
            upd = buildExe [] [(Text.pack m, "foooooooooooooooo.hs")]
        status0 <- getBuildExeStatus session
        assertEqual "before exe build" Nothing status0
        updateSessionD session upd 1
        assertNoErrors session
        status1 <- getBuildExeStatus session
        assertEqual "failure after exe build" (Just $ ExitFailure 1) status1
    )
  , ( "Build .cabal from TH with a wrong libname and don't fail"
    , withSession (withOpts ["-XTemplateHaskell"]) $ \session -> do
        setCurrentDirectory "test"
        (originalUpdate, lm) <- getModulesFrom session "TH"
        let update = originalUpdate <> updateCodeGeneration True
        updateSessionD session update (length lm)
        setCurrentDirectory "../"
        assertNoErrors session
        dotCabalFromName <- getDotCabal session
        let dotCabal = dotCabalFromName "--///fo/name" $ Version [-1, -9] []
        assertBool ".cabal not empty" $ not $ BSLC.null dotCabal
    )
  , ( "Build licenses from TH with a wrong cabals dir and don't fail"
    , withSession (withOpts ["-XTemplateHaskell"]) $ \session -> do
        setCurrentDirectory "test"
        (originalUpdate, lm) <- getModulesFrom session "TH"
        let update = originalUpdate <> updateCodeGeneration True
        updateSessionD session update (length lm)
        setCurrentDirectory "../"
        assertNoErrors session
        let upd = buildLicenses "--/fooo /fooo/foo"
        updateSessionD session upd 99
        distDir <- getDistDir session
        licensesErrs <- readFile $ distDir </> "licenses.stderr"
        assertBool "licensesErrs length" $ length licensesErrs > 0
        status <- getBuildLicensesStatus session
        assertEqual "after license build" (Just ExitSuccess) status
    )
  , ( "Build haddocks from ParFib"
    , withSession (withOpts []) $ \session -> do
        setCurrentDirectory "test/MainModule"
        loadModulesFrom session "."
        assertNoErrors session
        setCurrentDirectory "../../"
        let upd = buildDoc
        updateSessionD session upd 1
        distDir <- getDistDir session
        indexExists <- doesFileExist $ distDir </> "doc/html/main/index.html"
        assertBool "ParFib haddock files" indexExists
        hoogleExists <- doesFileExist $ distDir </> "doc/html/main/main-1.0.txt"
        assertBool "ParFib hoogle files" hoogleExists
    )
  , ( "Fail on empty package DB"
    , assertRaises ""
        (\e -> e == userError "Invalid package DB stack: []")
        (withSession (withDBStack []) $ \_ -> return ())
    )
  , ( "Build licenses from ParFib"
    , withSession (withOpts []) $ \session -> do
        setCurrentDirectory "test/MainModule"
        loadModulesFrom session "."
        assertNoErrors session
        setCurrentDirectory "../../"
        let upd = buildLicenses "test/MainModule/cabals"
        updateSessionD session upd 6
        distDir <- getDistDir session
        licensesErrs <- readFile $ distDir </> "licenses.stderr"
        assertEqual "licensesErrs length" 0 (length licensesErrs)
        status <- getBuildLicensesStatus session
        assertEqual "after license build" (Just ExitSuccess) status
        licensesWarns <- readFile $ distDir </> "licenses.stdout"
        assertEqual "licensesWarns length" 0 (length licensesWarns)
        licenses <- readFile $ distDir </> "licenses.txt"
        assertBool "licenses length" $ length licenses >= 21409
    )
  , ( "Build licenses from Cabal"
    , withSession (withOpts []) $ \session -> do
        setCurrentDirectory "test/Cabal"
        loadModulesFrom session "."
        assertNoErrors session
        setCurrentDirectory "../../"
        let upd = buildLicenses "test/Puns/cabals"  -- 7 packages missing
        updateSessionD session upd 99
        distDir <- getDistDir session
        licensesErrs <- readFile $ distDir </> "licenses.stderr"
        assertBool "licensesErrs length" $ length licensesErrs <= 576
        status <- getBuildLicensesStatus session
        assertEqual "after license build" (Just ExitSuccess) status
        licensesWarns <- readFile $ distDir </> "licenses.stdout"
        assertEqual "licensesWarns length" 138 (length licensesWarns)
        licenses <- readFile $ distDir </> "licenses.txt"
        assertBool "licenses length" $ length licenses >= 21423
    )
  , ( "Build licenses from 1000 packages fixed in config with no license file"
    , withSession (withOpts []) $ \session -> do
        distDir <- getDistDir session
        let licenseFixedConfig
              :: Int -> [( String
                         , (Maybe License, Maybe FilePath, Maybe String)
                         )]
            licenseFixedConfig 0 = []
            licenseFixedConfig n =
              ("p" ++ show n, (Just BSD3, Nothing, Nothing))
              : licenseFixedConfig (n - 1)
            lics = licenseFixedConfig 1000
            pkgs = map (\(name, _) ->
                         PackageId{ packageName = Text.pack name
                                  , packageVersion = Just $ Text.pack "1.0" }
                       ) lics
        status <- buildLicsFromPkgs defaultSessionConfig
                    pkgs "test" distDir lics (\_ -> return ())
        assertEqual "after license build" ExitSuccess status
        licensesErrs <- readFile $ distDir </> "licenses.stderr"
        assertEqual "licensesErrs length" 0 (length licensesErrs)
        licenses <- readFile $ distDir </> "licenses.txt"
        assertBool "licenses length" $ length licenses >= 1527726
    )
  , ( "Build licenses from 1000 packages fixed in config with no useful info"
    , withSession (withOpts []) $ \session -> do
        distDir <- getDistDir session
        let licenseFixedConfig
              :: Int -> [( String
                         , (Maybe License, Maybe FilePath, Maybe String)
                         )]
            licenseFixedConfig 0 = []
            licenseFixedConfig n =
              ("p" ++ show n, (Just BSD3, Just "test/BSD_TEST", Nothing))
              : licenseFixedConfig (n - 1)
            lics = licenseFixedConfig 1000
            pkgs = map (\(name, _) ->
                         PackageId{ packageName = Text.pack name
                                  , packageVersion = Just $ Text.pack "1.0" }
                       ) lics
        status <- buildLicsFromPkgs defaultSessionConfig
                    pkgs "test" distDir lics (\_ -> return ())
        assertEqual "after license build" ExitSuccess status
        licensesWarns <- readFile $ distDir </> "licenses.stdout"
        assertEqual "licensesWarns length" 0 (length licensesWarns)
        licenses <- readFile $ distDir </> "licenses.txt"
        assertBool "licenses length" $ length licenses >= 63619
    )
  , ( "Type information 1: Local identifiers and Prelude"
    , ifIdeBackendHaddockTestsEnabled defaultSession $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "a = (5 :: Int)"
                    , "b = a + 6"
                    , "c = True"
                    , "d = foldr"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        assertIdInfo session "A" (2,1,2,2) "a" VarName "Int" "main:A" "A.hs@2:1-2:2" "" "binding occurrence"
        assertIdInfo session "A" (3,1,3,2) "b" VarName "Int" "main:A" "A.hs@3:1-3:2" "" "binding occurrence"
        assertIdInfo session "A" (3,5,3,6) "a" VarName "Int" "main:A" "A.hs@2:1-2:2" "" "defined locally"
        assertIdInfo session "A" (3,7,3,8) "+" VarName "Num a => a -> a -> a" "base-4.5.1.0:GHC.Num" "<no location info>" "base-4.5.1.0:Prelude" "imported from base-4.5.1.0:Prelude at A.hs@1:8-1:9"
        assertIdInfo session "A" (4,1,4,2) "c" VarName "Bool" "main:A" "A.hs@4:1-4:2" "" "binding occurrence"
        assertIdInfo session "A" (4,5,4,9) "True" DataName "" "ghc-prim-0.2.0.0:GHC.Types" "<wired into compiler>" "base-4.5.1.0:Data.Bool" "wired in to the compiler"
        assertIdInfo session "A" (5,1,5,2) "d" VarName "(a -> b -> b) -> b -> [a] -> b" "main:A" "A.hs@5:1-5:2" "" "binding occurrence"
        assertIdInfo session "A" (5,5,5,10) "foldr" VarName "(a1 -> b1 -> b1) -> b1 -> [a1] -> b1" "base-4.5.1.0:GHC.Base" "<no location info>" "base-4.5.1.0:Data.List" "imported from base-4.5.1.0:Prelude at A.hs@1:8-1:9"
        {- TODO: reenable
        assertEqual "Haddock link for A.b should be correct"
                    "main/latest/doc/html/A.html#v:b" $
                    haddockLink (idMapToMap idMapB Map.! SourceSpan "B.hs" 5 8 5 9)
        -}
    )
  , ( "Type information 2: Simple ADTs"
    , withSession defaultSession $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "data T = MkT"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        assertIdInfo' session "A" (2,6,2,7) (2,6,2,7) "T" TcClsName [] "main:A" [(GHC742, "A.hs@2:6-2:7"), (GHC78, "A.hs@2:1-2:13")] "" (allVersions "binding occurrence")
        assertIdInfo session "A" (2,10,2,13) "MkT" DataName "T" "main:A" "A.hs@2:10-2:13" "" "binding occurrence"
    )
  , ( "Type information 3: Polymorphism"
    , ifIdeBackendHaddockTestsEnabled defaultSession $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "data TMaybe a = TNothing | TJust a"
                    , ""
                    , "f1 x = x"
                    , "f2 = \\x -> x"
                    , ""
                    , "g1 x y = x"
                    , "g2 = \\x y -> x"
                    , ""
                    , "h1 = h1go True False"
                    , "  where"
                    , "    h1go x y = x"
                    , ""
                    , "h2 = h2go True False"
                    , "  where"
                    , "    h2go = \\x y -> x"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        assertIdInfo' session "A" (2,6,2,12) (2,6,2,12) "TMaybe" TcClsName [] "main:A" [(GHC742, "A.hs@2:6-2:12"), (GHC78, "A.hs@2:1-2:35")] "" (allVersions "binding occurrence")
        assertIdInfo session "A" (2,13,2,14) "a" TvName "" "main:A" "A.hs@2:13-2:14" "" "binding occurrence"
        assertIdInfo session "A" (2,17,2,25) "TNothing" DataName "TMaybe a" "main:A" "A.hs@2:17-2:25" "" "binding occurrence"
        assertIdInfo' session "A" (2,28,2,33) (2,28,2,33) "TJust" DataName (allVersions "a -> TMaybe a") "main:A" [(GHC742, "A.hs@2:28-2:33"), (GHC78, "A.hs@2:28-2:35")] "" (allVersions "binding occurrence")
        assertIdInfo session "A" (2,34,2,35) "a" TvName "" "main:A" "A.hs@2:13-2:14" "" "defined locally"
        assertIdInfo session "A" (4,1,4,3) "f1" VarName "t -> t" "main:A" "A.hs@4:1-4:3" "" "binding occurrence"
        assertIdInfo session "A" (4,4,4,5) "x" VarName "t" "main:A" "A.hs@4:4-4:5" "" "binding occurrence"
        assertIdInfo session "A" (4,8,4,9) "x" VarName "t" "main:A" "A.hs@4:4-4:5" "" "defined locally"
        assertIdInfo session "A" (5,1,5,3) "f2" VarName "t -> t" "main:A" "A.hs@5:1-5:3" "" "binding occurrence"
        assertIdInfo session "A" (5,7,5,8) "x" VarName "t" "main:A" "A.hs@5:7-5:8" "" "binding occurrence"
        assertIdInfo session "A" (5,12,5,13) "x" VarName "t" "main:A" "A.hs@5:7-5:8" "" "defined locally"
        assertIdInfo session "A" (7,1,7,3) "g1" VarName "t -> t1 -> t" "main:A" "A.hs@7:1-7:3" "" "binding occurrence"
        assertIdInfo session "A" (7,4,7,5) "x" VarName "t" "main:A" "A.hs@7:4-7:5" "" "binding occurrence"
        assertIdInfo session "A" (7,6,7,7) "y" VarName "t1" "main:A" "A.hs@7:6-7:7" "" "binding occurrence"
        assertIdInfo session "A" (7,10,7,11) "x" VarName "t" "main:A" "A.hs@7:4-7:5" "" "defined locally"
        assertIdInfo session "A" (8,1,8,3) "g2" VarName "t -> t1 -> t" "main:A" "A.hs@8:1-8:3" "" "binding occurrence"
        assertIdInfo session "A" (8,7,8,8) "x" VarName "t" "main:A" "A.hs@8:7-8:8" "" "binding occurrence"
        assertIdInfo session "A" (8,9,8,10) "y" VarName "t1" "main:A" "A.hs@8:9-8:10" "" "binding occurrence"
        assertIdInfo session "A" (8,14,8,15) "x" VarName "t" "main:A" "A.hs@8:7-8:8" "" "defined locally"
        assertIdInfo session "A" (10,1,10,3) "h1" VarName "Bool" "main:A" "A.hs@10:1-10:3" "" "binding occurrence"
        assertIdInfo session "A" (10,6,10,10) "h1go" VarName "t -> t1 -> t" "main:A" "A.hs@12:5-12:9" "" "defined locally"
        assertIdInfo session "A" (10,11,10,15) "True" DataName "" "ghc-prim-0.2.0.0:GHC.Types" "<wired into compiler>" "base-4.5.1.0:Data.Bool" "wired in to the compiler"
        assertIdInfo session "A" (10,16,10,21) "False" DataName "" "ghc-prim-0.2.0.0:GHC.Types" "<wired into compiler>" "base-4.5.1.0:Data.Bool" "wired in to the compiler"
        assertIdInfo session "A" (12,5,12,9) "h1go" VarName "t -> t1 -> t" "main:A" "A.hs@12:5-12:9" "" "binding occurrence"
        assertIdInfo session "A" (12,10,12,11) "x" VarName "t" "main:A" "A.hs@12:10-12:11" "" "binding occurrence"
        assertIdInfo session "A" (12,12,12,13) "y" VarName "t1" "main:A" "A.hs@12:12-12:13" "" "binding occurrence"
        assertIdInfo session "A" (12,16,12,17) "x" VarName "t" "main:A" "A.hs@12:10-12:11" "" "defined locally"
        assertIdInfo session "A" (14,1,14,3) "h2" VarName "Bool" "main:A" "A.hs@14:1-14:3" "" "binding occurrence"
        assertIdInfo session "A" (14,6,14,10) "h2go" VarName "t -> t1 -> t" "main:A" "A.hs@16:5-16:9" "" "defined locally"
        assertIdInfo session "A" (14,11,14,15) "True" DataName "" "ghc-prim-0.2.0.0:GHC.Types" "<wired into compiler>" "base-4.5.1.0:Data.Bool" "wired in to the compiler"
        assertIdInfo session "A" (14,16,14,21) "False" (DataName) "" "ghc-prim-0.2.0.0:GHC.Types" "<wired into compiler>" "base-4.5.1.0:Data.Bool" "wired in to the compiler"
        assertIdInfo session "A" (16,5,16,9) "h2go" VarName "t -> t1 -> t" "main:A" "A.hs@16:5-16:9" "" "binding occurrence"
        assertIdInfo session "A" (16,13,16,14) "x" VarName "t" "main:A" "A.hs@16:13-16:14" "" "binding occurrence"
        assertIdInfo session "A" (16,15,16,16) "y" VarName "t1" "main:A" "A.hs@16:15-16:16" "" "binding occurrence"
        assertIdInfo session "A" (16,20,16,21) "x" VarName "t" "main:A" "A.hs@16:13-16:14" "" "defined locally"
    )
  , ( "Type information 4: Multiple modules"
    , withSession defaultSession $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "data T = MkT"
                    ])
                    -- Make sure that an occurrence of MkT in a second module
                    -- doesn't cause us to lose type information we learned
                    -- while processing the first
               <> (updateSourceFile "B.hs" . BSLC.pack . unlines $
                    [ "module B where"
                    , "import A"
                    , "foo = MkT"
                    ])
        updateSessionD session upd 2
        assertNoErrors session
        assertIdInfo' session "A" (2,6,2,7) (2,6,2,7) "T" TcClsName [] "main:A" [(GHC742, "A.hs@2:6-2:7"), (GHC78, "A.hs@2:1-2:13")] "" (allVersions "binding occurrence")
        assertIdInfo session "A" (2,10,2,13) "MkT" DataName "T" "main:A" "A.hs@2:10-2:13" "" "binding occurrence"
        assertIdInfo session "B" (3,1,3,4) "foo" VarName "T" "main:B" "B.hs@3:1-3:4" "" "binding occurrence"
        assertIdInfo session "B" (3,7,3,10) "MkT" DataName "T" "main:A" "A.hs@2:10-2:13" "" "imported from main:A at B.hs@2:1-2:9"
    )
  , ( "Type information 5: External packages, type sigs, scoped type vars, kind sigs"
    , let opts = [ "-XScopedTypeVariables"
                 , "-XKindSignatures"
                 ]
      in ifIdeBackendHaddockTestsEnabled (withOpts opts) $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "module A where"

                    , "import Control.Parallel"

                    , "e = True `pseq` False"

                    , "f :: a -> a"
                    , "f x = x"

                    , "g :: forall a. a -> a"
                    , "g x = x"

                    , "h :: forall a. a -> a"
                    , "h x = y"
                    , "  where"
                    , "    y :: a"
                    , "    y = x"

                    , "i :: forall (t :: * -> *) a. t a -> t a"
                    , "i x = x"
                    ])
        updateSessionD session upd 2
        assertNoErrors session
        assertIdInfo session "A" (3,1,3,2) "e" VarName "Bool" "main:A" "A.hs@3:1-3:2" "" "binding occurrence"
        assertIdInfo session "A" (3,5,3,9) "True" DataName "" "ghc-prim-0.2.0.0:GHC.Types" "<wired into compiler>" "base-4.5.1.0:Data.Bool" "wired in to the compiler"
        assertIdInfo session "A" (3,10,3,16) "pseq" VarName "a -> b -> b" "parallel-3.2.0.3:Control.Parallel" "<no location info>" "parallel-3.2.0.3:Control.Parallel" "imported from parallel-3.2.0.3:Control.Parallel at A.hs@2:1-2:24"
        assertIdInfo session "A" (3,17,3,22) "False" DataName "" "ghc-prim-0.2.0.0:GHC.Types" "<wired into compiler>" "base-4.5.1.0:Data.Bool" "wired in to the compiler"
        assertIdInfo session "A" (4,1,4,2) "f" VarName "a -> a" "main:A" "A.hs@5:1-5:2" "" "defined locally"
        assertIdInfo' session "A" (4,6,4,7) (4,6,4,7) "a" TvName [] "main:A" [(GHC742, "A.hs@4:6-4:7"), (GHC78, "A.hs@4:6-4:12")] "" (allVersions "defined locally")
        assertIdInfo' session "A" (4,11,4,12) (4,11,4,12) "a" TvName [] "main:A" [(GHC742, "A.hs@4:6-4:7"), (GHC78, "A.hs@4:6-4:12")] "" (allVersions "defined locally")
        assertIdInfo session "A" (5,1,5,2) "f" VarName "a -> a" "main:A" "A.hs@5:1-5:2" "" "binding occurrence"
        assertIdInfo session "A" (5,3,5,4) "x" VarName "a" "main:A" "A.hs@5:3-5:4" "" "binding occurrence"
        assertIdInfo session "A" (5,7,5,8) "x" VarName "a" "main:A" "A.hs@5:3-5:4" "" "defined locally"
        assertIdInfo session "A" (6,1,6,2) "g" VarName "a -> a" "main:A" "A.hs@7:1-7:2" "" "defined locally"
        assertIdInfo session "A" (6,13,6,14) "a" TvName "" "main:A" "A.hs@6:13-6:14" "" "binding occurrence"
        assertIdInfo session "A" (6,16,6,17) "a" TvName "" "main:A" "A.hs@6:13-6:14" "" "defined locally"
        assertIdInfo session "A" (6,21,6,22) "a" TvName "" "main:A" "A.hs@6:13-6:14" "" "defined locally"
        assertIdInfo session "A" (7,1,7,2) "g" VarName "a -> a" "main:A" "A.hs@7:1-7:2" "" "binding occurrence"
        assertIdInfo session "A" (7,3,7,4) "x" VarName "a" "main:A" "A.hs@7:3-7:4" "" "binding occurrence"
        assertIdInfo session "A" (7,7,7,8) "x" VarName "a" "main:A" "A.hs@7:3-7:4" "" "defined locally"
        assertIdInfo session "A" (8,1,8,2) "h" VarName "a -> a" "main:A" "A.hs@9:1-9:2" "" "defined locally"
        assertIdInfo session "A" (8,13,8,14) "a" TvName "" "main:A" "A.hs@8:13-8:14" "" "binding occurrence"
        assertIdInfo session "A" (8,16,8,17) "a" TvName "" "main:A" "A.hs@8:13-8:14" "" "defined locally"
        assertIdInfo session "A" (8,21,8,22) "a" TvName "" "main:A" "A.hs@8:13-8:14" "" "defined locally"
        assertIdInfo session "A" (9,1,9,2) "h" VarName "a -> a" "main:A" "A.hs@9:1-9:2" "" "binding occurrence"
        assertIdInfo session "A" (9,3,9,4) "x" VarName "a" "main:A" "A.hs@9:3-9:4" "" "binding occurrence"
        assertIdInfo session "A" (9,7,9,8) "y" VarName "a" "main:A" "A.hs@12:5-12:6" "" "defined locally"
        assertIdInfo session "A" (11,5,11,6) "y" VarName "a" "main:A" "A.hs@12:5-12:6" "" "defined locally"
        assertIdInfo session "A" (11,5,11,6) "y" VarName "a" "main:A" "A.hs@12:5-12:6" "" "defined locally"
        assertIdInfo session "A" (11,10,11,11) "a" TvName "" "main:A" "A.hs@8:13-8:14" "" "defined locally"
        assertIdInfo session "A" (11,10,11,11) "a" TvName "" "main:A" "A.hs@8:13-8:14" "" "defined locally"
        assertIdInfo session "A" (12,5,12,6) "y" VarName "a" "main:A" "A.hs@12:5-12:6" "" "binding occurrence"
        assertIdInfo session "A" (12,9,12,10) "x" VarName "a" "main:A" "A.hs@9:3-9:4" "" "defined locally"
        assertIdInfo session "A" (13,1,13,2) "i" VarName "t a -> t a" "main:A" "A.hs@14:1-14:2" "" "defined locally"
        assertIdInfo session "A" (13,13,13,26) "t" TvName "" "main:A" "A.hs@13:13-13:26" "" "binding occurrence"
        assertIdInfo session "A" (13,27,13,28) "a" TvName "" "main:A" "A.hs@13:27-13:28" "" "binding occurrence"
        assertIdInfo session "A" (13,30,13,31) "t" TvName "" "main:A" "A.hs@13:13-13:26" "" "defined locally"
        assertIdInfo session "A" (13,32,13,33) "a" TvName "" "main:A" "A.hs@13:27-13:28" "" "defined locally"
        assertIdInfo session "A" (13,37,13,38) "t" TvName "" "main:A" "A.hs@13:13-13:26" "" "defined locally"
        assertIdInfo session "A" (13,39,13,40) "a" TvName "" "main:A" "A.hs@13:27-13:28" "" "defined locally"
        assertIdInfo session "A" (14,1,14,2) "i" VarName "t a -> t a" "main:A" "A.hs@14:1-14:2" "" "binding occurrence"
        assertIdInfo session "A" (14,3,14,4) "x" VarName "t a" "main:A" "A.hs@14:3-14:4" "" "binding occurrence"
        assertIdInfo session "A" (14,7,14,8) "x" VarName "t a" "main:A" "A.hs@14:3-14:4" "" "defined locally"
    )
  , ( "Type information 6: Reusing type variables"
    , withSession (withOpts ["-XScopedTypeVariables"]) $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "module A where"

                    , "f1 (x, y) = x"
                    , "f2 (x, y) = x"

                    , "f3 (x, y) = f4 (x, y)"
                    , "  where"
                    , "    f4 (x, y) = x"

                    , "f5 :: forall t t1. (t, t1) -> t"
                    , "f5 (x, y) = f6 (x, y)"
                    , "  where"
                    , "    f6 :: forall t2. (t, t2) -> t"
                    , "    f6 (x, y) = x"
                    ])
        updateSessionD session upd 2
        assertNoErrors session
        assertIdInfo session "A" (2,1,2,3) "f1" VarName "(t, t1) -> t" "main:A" "A.hs@2:1-2:3" "" "binding occurrence"
        assertIdInfo session "A" (2,5,2,6) "x" VarName "t" "main:A" "A.hs@2:5-2:6" "" "binding occurrence"
        assertIdInfo session "A" (2,8,2,9) "y" VarName "t1" "main:A" "A.hs@2:8-2:9" "" "binding occurrence"
        assertIdInfo session "A" (2,13,2,14) "x" VarName "t" "main:A" "A.hs@2:5-2:6" "" "defined locally"
        assertIdInfo session "A" (3,1,3,3) "f2" VarName "(t, t1) -> t" "main:A" "A.hs@3:1-3:3" "" "binding occurrence"
        assertIdInfo session "A" (3,5,3,6) "x" VarName "t" "main:A" "A.hs@3:5-3:6" "" "binding occurrence"
        assertIdInfo session "A" (3,8,3,9) "y" VarName "t1" "main:A" "A.hs@3:8-3:9" "" "binding occurrence"
        assertIdInfo session "A" (3,13,3,14) "x" VarName "t" "main:A" "A.hs@3:5-3:6" "" "defined locally"
        assertIdInfo session "A" (4,1,4,3) "f3" VarName "(t, t1) -> t" "main:A" "A.hs@4:1-4:3" "" "binding occurrence"
        assertIdInfo session "A" (4,5,4,6) "x" VarName "t" "main:A" "A.hs@4:5-4:6" "" "binding occurrence"
        assertIdInfo session "A" (4,8,4,9) "y" VarName "t1" "main:A" "A.hs@4:8-4:9" "" "binding occurrence"
        assertIdInfo session "A" (4,13,4,15) "f4" VarName "(t2, t3) -> t2" "main:A" "A.hs@6:5-6:7" "" "defined locally"
        assertIdInfo session "A" (4,17,4,18) "x" VarName "t" "main:A" "A.hs@4:5-4:6" "" "defined locally"
        assertIdInfo session "A" (4,20,4,21) "y" VarName "t1" "main:A" "A.hs@4:8-4:9" "" "defined locally"
        assertIdInfo session "A" (6,5,6,7) "f4" VarName "(t2, t3) -> t2" "main:A" "A.hs@6:5-6:7" "" "binding occurrence"
        assertIdInfo session "A" (6,9,6,10) "x" VarName "t2" "main:A" "A.hs@6:9-6:10" "" "binding occurrence"
        assertIdInfo session "A" (6,12,6,13) "y" VarName "t3" "main:A" "A.hs@6:12-6:13" "" "binding occurrence"
        assertIdInfo session "A" (6,17,6,18) "x" VarName "t2" "main:A" "A.hs@6:9-6:10" "" "defined locally"
        assertIdInfo session "A" (7,1,7,3) "f5" VarName "(t, t1) -> t" "main:A" "A.hs@8:1-8:3" "" "defined locally"
        assertIdInfo session "A" (7,14,7,15) "t" TvName "" "main:A" "A.hs@7:14-7:15" "" "binding occurrence"
        assertIdInfo session "A" (7,16,7,18) "t1" TvName "" "main:A" "A.hs@7:16-7:18" "" "binding occurrence"
        assertIdInfo session "A" (7,21,7,22) "t" TvName "" "main:A" "A.hs@7:14-7:15" "" "defined locally"
        assertIdInfo session "A" (7,24,7,26) "t1" TvName "" "main:A" "A.hs@7:16-7:18" "" "defined locally"
        assertIdInfo session "A" (7,31,7,32) "t" TvName "" "main:A" "A.hs@7:14-7:15" "" "defined locally"
        assertIdInfo session "A" (8,1,8,3) "f5" VarName "(t, t1) -> t" "main:A" "A.hs@8:1-8:3" "" "binding occurrence"
        assertIdInfo session "A" (8,5,8,6) "x" VarName "t" "main:A" "A.hs@8:5-8:6" "" "binding occurrence"
        assertIdInfo session "A" (8,8,8,9) "y" VarName "t1" "main:A" "A.hs@8:8-8:9" "" "binding occurrence"
        assertIdInfo session "A" (8,13,8,15) "f6" VarName "(t, t2) -> t" "main:A" "A.hs@11:5-11:7" "" "defined locally"
        assertIdInfo session "A" (8,17,8,18) "x" VarName "t" "main:A" "A.hs@8:5-8:6" "" "defined locally"
        assertIdInfo session "A" (8,20,8,21) "y" VarName "t1" "main:A" "A.hs@8:8-8:9" "" "defined locally"
        assertIdInfo session "A" (10,5,10,7) "f6" VarName "(t, t2) -> t" "main:A" "A.hs@11:5-11:7" "" "defined locally"
        assertIdInfo session "A" (10,5,10,7) "f6" VarName "(t, t2) -> t" "main:A" "A.hs@11:5-11:7" "" "defined locally"
        assertIdInfo session "A" (10,18,10,20) "t2" TvName "" "main:A" "A.hs@10:18-10:20" "" "binding occurrence"
        assertIdInfo session "A" (10,18,10,20) "t2" TvName "" "main:A" "A.hs@10:18-10:20" "" "binding occurrence"
        assertIdInfo session "A" (10,23,10,24) "t" TvName "" "main:A" "A.hs@7:14-7:15" "" "defined locally"
        assertIdInfo session "A" (10,23,10,24) "t" TvName "" "main:A" "A.hs@7:14-7:15" "" "defined locally"
        assertIdInfo session "A" (10,26,10,28) "t2" TvName "" "main:A" "A.hs@10:18-10:20" "" "defined locally"
        assertIdInfo session "A" (10,26,10,28) "t2" TvName "" "main:A" "A.hs@10:18-10:20" "" "defined locally"
        assertIdInfo session "A" (10,33,10,34) "t" TvName "" "main:A" "A.hs@7:14-7:15" "" "defined locally"
        assertIdInfo session "A" (10,33,10,34) "t" TvName "" "main:A" "A.hs@7:14-7:15" "" "defined locally"
        assertIdInfo session "A" (11,5,11,7) "f6" VarName "(t, t2) -> t" "main:A" "A.hs@11:5-11:7" "" "binding occurrence"
        assertIdInfo session "A" (11,9,11,10) "x" VarName "t" "main:A" "A.hs@11:9-11:10" "" "binding occurrence"
        assertIdInfo session "A" (11,12,11,13) "y" VarName "t2" "main:A" "A.hs@11:12-11:13" "" "binding occurrence"
        assertIdInfo session "A" (11,17,11,18) "x" VarName "t" "main:A" "A.hs@11:9-11:10" "" "defined locally"
    )
  , ( "Type information 7: Qualified imports"
    , ifIdeBackendHaddockTestsEnabled defaultSession $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "import Data.Maybe"
                    , "import qualified Data.List"
                    , "import qualified Data.Function as F"
                    , "foo = (fromJust, Data.List.and, F.on)"
                    ])
        updateSessionD session upd 2
        assertNoErrors session
        assertIdInfo session "A" (5,1,5,4) "foo" VarName "(Maybe a -> a, [Bool] -> Bool, (b -> b -> c) -> (a1 -> b) -> a1 -> a1 -> c)" "main:A" "A.hs@5:1-5:4" "" "binding occurrence"
        assertIdInfo session "A" (5,8,5,16) "fromJust" VarName "Maybe a2 -> a2" "base-4.5.1.0:Data.Maybe" "<no location info>" "base-4.5.1.0:Data.Maybe" "imported from base-4.5.1.0:Data.Maybe at A.hs@2:1-2:18"
        assertIdInfo session "A" (5,18,5,31) "and" VarName "[Bool] -> Bool" "base-4.5.1.0:GHC.List" "<no location info>" "base-4.5.1.0:Data.List" "imported from base-4.5.1.0:Data.List as 'Data.List.' at A.hs@3:1-3:27"
        assertIdInfo session "A" (5,33,5,37) "on" VarName "(b1 -> b1 -> c1) -> (a2 -> b1) -> a2 -> a2 -> c1" "base-4.5.1.0:Data.Function" "<no location info>" "base-4.5.1.0:Data.Function" "imported from base-4.5.1.0:Data.Function as 'F.' at A.hs@4:1-4:36"
    )
  , ( "Type information 8: Imprecise source spans"
    , ifIdeBackendHaddockTestsEnabled defaultSession $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "main = print True"
                    ])
        updateSessionD session upd 1
        assertNoErrors session

        let checkPrint span = assertIdInfo' session "A" span (2, 8, 2, 13) "print" VarName (allVersions "Show a => a -> IO ()") "base-4.5.1.0:System.IO" (allVersions "<no location info>") "base-4.5.1.0:System.IO" (allVersions "imported from base-4.5.1.0:Prelude at A.hs@1:8-1:9")

        checkPrint (2,8,2,13)
        checkPrint (2,8,2,8)
        checkPrint (2,8,2,9)
        checkPrint (2,9,2,9)
        checkPrint (2,9,2,10)
        checkPrint (2,9,2,13)
    )
  , ( "Type information 9a: Quasi-quotation (QQ in own package)"
    , withSession defaultSession $ \session -> do
        let upd = updateCodeGeneration True
               <> (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE TemplateHaskell #-}"
                    , "module A where"
                    , "import Language.Haskell.TH.Quote"
                    , "qq = QuasiQuoter {"
                    , "         quoteExp  = \\str -> case str of"
                    , "                                \"a\" -> [| True |]"
                    , "                                \"b\" -> [| id True |]"
                    , "                                \"c\" -> [| True || False |]"
                    , "                                \"d\" -> [| False |]"
                    , "       , quotePat  = undefined"
                    , "       , quoteType = undefined"
                    , "       , quoteDec  = undefined"
                    , "       }"
                    ])
               <> (updateSourceFile "B.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE QuasiQuotes #-}"
                    , "module B where"
                    , "import A"
                    -- 1234567890123
                    , "ex1 = [qq|a|]"
                    , "ex2 = [qq|b|]"
                    , "ex3 = [qq|c|]"
                    , "ex4 = [qq|d|]"
                    ])
        updateSessionD session upd 2
        assertNoErrors session
        {-
        let span l c = SourceSpan { spanFilePath   = "B.hs"
                                  , spanFromLine   = l
                                  , spanFromColumn = c
                                  , spanToLine     = l
                                  , spanToColumn   = c
                                  }
        print (idInfo (Text.pack "B") (span 4 11))
        print (idInfo (Text.pack "B") (span 5 11))
        print (idInfo (Text.pack "B") (span 6 11))
        print (idInfo (Text.pack "B") (span 7 11))
        -}
        assertIdInfo session "B" (4,7,4,14) "qq" VarName "QuasiQuoter" "main:A" "A.hs@4:1-4:3" "" "imported from main:A at B.hs@3:1-3:9"
        assertIdInfo session "B" (5,7,5,14) "qq" VarName "QuasiQuoter" "main:A" "A.hs@4:1-4:3" "" "imported from main:A at B.hs@3:1-3:9"
        assertIdInfo session "B" (6,7,6,14) "qq" VarName "QuasiQuoter" "main:A" "A.hs@4:1-4:3" "" "imported from main:A at B.hs@3:1-3:9"
        assertIdInfo session "B" (7,7,7,14) "qq" VarName "QuasiQuoter" "main:A" "A.hs@4:1-4:3" "" "imported from main:A at B.hs@3:1-3:9"
    )
  , ( "Type information 9b: Quasi-quotation (QQ in separate package, check home module info)"
    , ifIdeBackendHaddockTestsEnabled defaultSession $ \session -> do
        let upd = updateCodeGeneration True
               <> (updateSourceFile "Main.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,"
                    , "             TemplateHaskell, OverloadedStrings #-}"
                    , "import Yesod"

                    , "data Piggies = Piggies"

                    , "instance Yesod Piggies"

                    , "mkYesod \"Piggies\" [parseRoutes|"
                    , "  / HomeR GET"
                    , "|]"

                    , "getHomeR = defaultLayout [whamlet|"
                    , "  Welcome to the Pigsty!"
                    , "  |]"

                    , "main = warpEnv Piggies"
                    ])
        updateSessionD session upd 2

        errs <- getSourceErrors session
        case errs of
          [] -> do
            -- TODO: why don't we get type information here?
            assertIdInfo session "Main" (6,19,8,3) "parseRoutes" VarName "" "yesod-routes-1.2.0.1:Yesod.Routes.Parse" "<no location info>" "yesod-core-1.2.2:Yesod.Core.Dispatch" "imported from yesod-1.2.1:Yesod at Main.hs@3:1-3:13"
            assertIdInfo session "Main" (9,26,11,5) "whamlet" VarName "" "yesod-core-1.2.2:Yesod.Core.Widget" "<no location info>" "yesod-core-1.2.2:Yesod.Core.Widget" "imported from yesod-1.2.1:Yesod at Main.hs@3:1-3:13"
          _ ->
            putStrLn "WARNING: Skipping due to errors (probably yesod package not installed)"
    )
  , ( "Type information 10: Template Haskell"
    , ifIdeBackendHaddockTestsEnabled defaultSession $ \session -> do
        let upd = updateCodeGeneration True
               <> (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE TemplateHaskell #-}"
                    , "module A where"
                    , "import Language.Haskell.TH"
                    , "ex1 :: Q Exp"
                    , "ex1 = [| \\x -> x |]"
                    , "ex2 :: Q Type"
                    , "ex2 = [t| String -> String |]"
                    , "ex3 :: Q [Dec]"
                    , "ex3 = [d| foo x = x |]"
                    ])
               <> (updateSourceFile "B.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE TemplateHaskell #-}"
                    , "module B where"
                    , "import A"
                      -- Types and expressions
                    , "ex5 :: $ex2"
                    , "ex5 = $ex1"
                      -- Just to test slightly larger expressions
                    , "ex6 :: $(return =<< ex2)"
                    , "ex6 = $(ex1 >>= return)"
                      -- Declarations
                    , "$ex3"
                    ])
        updateSessionD session upd 2
        assertNoErrors session
        assertIdInfo session "A" (4,1,4,4) "ex1" VarName "Q Exp" "main:A" "A.hs@5:1-5:4" "" "defined locally"
        assertIdInfo session "A" (4,8,4,9) "Q" TcClsName "" "template-haskell-2.7.0.0:Language.Haskell.TH.Syntax" "<no location info>" "template-haskell-2.7.0.0:Language.Haskell.TH.Syntax" "imported from template-haskell-2.7.0.0:Language.Haskell.TH at A.hs@3:1-3:27"
        assertIdInfo session "A" (4,10,4,13) "Exp" TcClsName "" "template-haskell-2.7.0.0:Language.Haskell.TH.Syntax" "<no location info>" "template-haskell-2.7.0.0:Language.Haskell.TH.Syntax" "imported from template-haskell-2.7.0.0:Language.Haskell.TH at A.hs@3:1-3:27"
        assertIdInfo session "A" (5,1,5,4) "ex1" VarName "Q Exp" "main:A" "A.hs@5:1-5:4" "" "binding occurrence"
        assertIdInfo session "A" (5,11,5,12) "x" VarName "" "main:A" "A.hs@5:11-5:12" "" "binding occurrence"
        assertIdInfo session "A" (5,16,5,17) "x" VarName "" "main:A" "A.hs@5:11-5:12" "" "defined locally"
        assertIdInfo session "A" (6,1,6,4) "ex2" VarName "Q Type" "main:A" "A.hs@7:1-7:4" "" "defined locally"
        assertIdInfo session "A" (6,8,6,9) "Q" TcClsName "" "template-haskell-2.7.0.0:Language.Haskell.TH.Syntax" "<no location info>" "template-haskell-2.7.0.0:Language.Haskell.TH.Syntax" "imported from template-haskell-2.7.0.0:Language.Haskell.TH at A.hs@3:1-3:27"
        assertIdInfo session "A" (6,10,6,14) "Type" TcClsName "" "template-haskell-2.7.0.0:Language.Haskell.TH.Syntax" "<no location info>" "template-haskell-2.7.0.0:Language.Haskell.TH.Syntax" "imported from template-haskell-2.7.0.0:Language.Haskell.TH at A.hs@3:1-3:27"
        assertIdInfo session "A" (7,1,7,4) "ex2" VarName "Q Type" "main:A" "A.hs@7:1-7:4" "" "binding occurrence"
        assertIdInfo session "A" (7,11,7,17) "String" TcClsName "" "base-4.5.1.0:GHC.Base" "<no location info>" "base-4.5.1.0:Data.String" "imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9"
        assertIdInfo session "A" (7,21,7,27) "String" TcClsName "" "base-4.5.1.0:GHC.Base" "<no location info>" "base-4.5.1.0:Data.String" "imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9"
        assertIdInfo session "B" (4,1,4,4) "ex5" VarName "String -> String" "main:B" "B.hs@5:1-5:4" "" "defined locally"
        assertIdInfo session "B" (4,8,4,12) "ex2" VarName "Q Type" "main:A" "A.hs@7:1-7:4" "" "imported from main:A at B.hs@3:1-3:9"
        assertIdInfo session "B" (5,1,5,4) "ex5" VarName "String -> String" "main:B" "B.hs@5:1-5:4" "" "binding occurrence"
        assertIdInfo session "B" (5,7,5,11) "ex1" VarName "Q Exp" "main:A" "A.hs@5:1-5:4" "" "imported from main:A at B.hs@3:1-3:9"

        assertIdInfo session "B" (6,21,6,24) "ex2" VarName "Q Type" "main:A" "A.hs@7:1-7:4" "" "imported from main:A at B.hs@3:1-3:9"
        assertIdInfo session "B" (7,9,7,12) "ex1" VarName "Q Exp" "main:A" "A.hs@5:1-5:4" "" "imported from main:A at B.hs@3:1-3:9"
        assertIdInfo session "B" (8,1,8,5) "ex3" VarName "Q [Dec]" "main:A" "A.hs@9:1-9:4" "" "imported from main:A at B.hs@3:1-3:9"
    )
  , ( "Type information 11: Take advantage of scope (1)"
    , ifIdeBackendHaddockTestsEnabled defaultSession $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "main = print True"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        assertIdInfo session "A" (2,8,2,13) "print" VarName "Show a => a -> IO ()" "base-4.5.1.0:System.IO" "<no location info>" "base-4.5.1.0:System.IO" "imported from base-4.5.1.0:Prelude at A.hs@1:8-1:9"
    )
  , ( "Type information 12: Take advantage of scope (2)"
    , ifIdeBackendHaddockTestsEnabled defaultSession $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "import Data.ByteString (append)"
                    , "foo = append"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        assertIdInfo session "A" (3,7,3,13) "append" VarName "Data.ByteString.Internal.ByteString -> Data.ByteString.Internal.ByteString -> Data.ByteString.Internal.ByteString" "bytestring-0.9.2.1:Data.ByteString" "<no location info>" "bytestring-0.9.2.1:Data.ByteString" "imported from bytestring-0.9.2.1:Data.ByteString at A.hs@2:25-2:31"
    )
  , ( "Type information 13: Take advantage of scope (3)"
    , ifIdeBackendHaddockTestsEnabled defaultSession $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "import Data.ByteString"
                    , "foo = append"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        assertIdInfo session "A" (3,7,3,13) "append" VarName "ByteString -> ByteString -> ByteString" "bytestring-0.9.2.1:Data.ByteString" "<no location info>" "bytestring-0.9.2.1:Data.ByteString" "imported from bytestring-0.9.2.1:Data.ByteString at A.hs@2:1-2:23"
    )
  , ( "Type information 14: Take advantage of scope (4)"
    , ifIdeBackendHaddockTestsEnabled defaultSession $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "import Data.ByteString (append)"
                    , "import qualified Data.ByteString as BS"
                    , "foo = append"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        assertIdInfo session "A" (4,7,4,13) "append" VarName "BS.ByteString -> BS.ByteString -> BS.ByteString" "bytestring-0.9.2.1:Data.ByteString" "<no location info>" "bytestring-0.9.2.1:Data.ByteString" "imported from bytestring-0.9.2.1:Data.ByteString as 'BS.' at A.hs@3:1-3:39"
    )
  , ( "Type information 15: Other constructs"
    , ifIdeBackendHaddockTestsEnabled defaultSession $ \session -> do
        version <- getGhcVersion session
        let langPragma = case version of
              GHC742 -> "{-# LANGUAGE StandaloneDeriving, DoRec #-}"
              GHC78  -> "{-# LANGUAGE StandaloneDeriving, RecursiveDo #-}"
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ {-  1 -} langPragma
                    , {-  2 -} "module A where"

                    , {-  3 -} "data MkT = MkT"

                    , {-  4 -} "instance Eq MkT where"
                    , {-  5 -} "  (==) = const $ const True"

                    , {-  6 -} "deriving instance Show MkT"

                    , {-  7 -} "(+++) = (++)"
                    , {-  8 -} "infixr 5 +++"

                    , {-  9 -} "default (Int)"

                    , {- 10 -} "{-# DEPRECATED f \"This is completely obsolete\" #-}"
                    , {- 11 -} "f :: Int"
                    , {- 12 -} "f = 1"

                    , {- 13 -} "{-# WARNING g \"You really shouldn't be using g!\" #-}"
                    , {- 14 -} "g :: Int"
                    , {- 15 -} "g = 2"

                    , {- 16 -} "h :: Int -> Int -> Int -> ([Int], [Int], [Int], [Int])"
                    , {- 17 -} "h x y z = ([x ..], [x, y..], [x .. z], [x, y .. z])"

                    , {- 18 -} "justOnes = do rec xs <- Just (1 : xs)"
                    , {- 19 -} "              return (map negate xs)"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        assertIdInfo session "A" (4,10,4,12) "Eq" TcClsName "" "ghc-prim-0.2.0.0:GHC.Classes" "<no location info>" "base-4.5.1.0:Data.Eq" "imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9"
        assertIdInfo session "A" (5,18,5,23) "const" VarName "a -> b -> a" "base-4.5.1.0:GHC.Base" "<no location info>" "base-4.5.1.0:Prelude" "imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9"
        assertIdInfo session "A" (6,19,6,23) "Show" TcClsName "" "base-4.5.1.0:GHC.Show" "<no location info>" "base-4.5.1.0:Text.Show" "imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9"
        assertIdInfo' session "A" (6,24,6,27) (6,24,6,27) "MkT" TcClsName [] "main:A" [(GHC742, "A.hs@3:6-3:9"), (GHC78, "A.hs@3:1-3:15")]  "" (allVersions "defined locally")
        assertIdInfo session "A" (8,10,8,13) "+++" VarName "[a] -> [a] -> [a]" "main:A" "A.hs@7:1-7:6" "" "defined locally"
        assertIdInfo session "A" (9,10,9,13) "Int" TcClsName "" "ghc-prim-0.2.0.0:GHC.Types" "<wired into compiler>" "base-4.5.1.0:Data.Int" "wired in to the compiler"
        assertIdInfo session "A" (17,13,17,14) "x" VarName "Int" "main:A" "A.hs@17:3-17:4" "" "defined locally"
        assertIdInfo session "A" (17,21,17,22) "x" VarName "Int" "main:A" "A.hs@17:3-17:4" "" "defined locally"
        assertIdInfo session "A" (17,24,17,25) "y" VarName "Int" "main:A" "A.hs@17:5-17:6" "" "defined locally"
        assertIdInfo session "A" (17,31,17,32) "x" VarName "Int" "main:A" "A.hs@17:3-17:4" "" "defined locally"
        assertIdInfo session "A" (17,36,17,37) "z" VarName "Int" "main:A" "A.hs@17:7-17:8" "" "defined locally"
        assertIdInfo session "A" (17,41,17,42) "x" VarName "Int" "main:A" "A.hs@17:3-17:4" "" "defined locally"
        assertIdInfo session "A" (17,44,17,45) "y" VarName "Int" "main:A" "A.hs@17:5-17:6" "" "defined locally"
        assertIdInfo session "A" (17,49,17,50) "z" VarName "Int" "main:A" "A.hs@17:7-17:8" "" "defined locally"
        assertIdInfo session "A" (18,19,18,21) "xs" VarName "[Int]" "main:A" "A.hs@18:19-18:21" "" "binding occurrence"
        assertIdInfo session "A" (18,25,18,29) "Just" DataName "" "base-4.5.1.0:Data.Maybe" "<no location info>" "base-4.5.1.0:Data.Maybe" "imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9"
        assertIdInfo session "A" (18,35,18,37) "xs" VarName "[Int]" "main:A" "A.hs@18:19-18:21" "" "defined locally"
    )
  , ( "Type information 16: FFI"
    , ifIdeBackendHaddockTestsEnabled defaultSession $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE ForeignFunctionInterface #-}"
                    , "module A where"

                    , "import Prelude hiding (sin)"
                    , "import Foreign.C"

                    , "foreign import ccall \"sin\" c_sin :: CDouble -> CDouble"

                    , "sin :: Double -> Double"
                    , "sin d = realToFrac (c_sin (realToFrac d))"

                    , "andBack :: CDouble -> CDouble"
                    , "andBack = realToFrac . sin . realToFrac"

                    , "foreign export ccall andBack :: CDouble -> CDouble"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        assertIdInfo' session "A" (5,28,5,33) (5,28,5,33) "c_sin" VarName (allVersions "CDouble -> CDouble") "main:A" [(GHC742, "A.hs@5:28-5:33"), (GHC78, "A.hs@5:1-5:55")] "" (allVersions "binding occurrence")
        assertIdInfo session "A" (5,37,5,44) "CDouble" TcClsName "" "base-4.5.1.0:Foreign.C.Types" "<no location info>" "base-4.5.1.0:Foreign.C.Types" "imported from base-4.5.1.0:Foreign.C at A.hs@4:1-4:17"
        assertIdInfo session "A" (10,22,10,29) "andBack" VarName "CDouble -> CDouble" "main:A" "A.hs@9:1-9:8" "" "defined locally"
        assertIdInfo session "A" (10,33,10,40) "CDouble" TcClsName "" "base-4.5.1.0:Foreign.C.Types" "<no location info>" "base-4.5.1.0:Foreign.C.Types" "imported from base-4.5.1.0:Foreign.C at A.hs@4:1-4:17"
    )
  , ( "Type information 17: GADTs"
    , ifIdeBackendHaddockTestsEnabled defaultSession $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE GADTs, KindSignatures, RankNTypes #-}"
                    , "module A where"

                    , "data Expr :: * -> * where"
                    , "  Num  :: Int -> Expr Int"
                    , "  Bool :: Bool -> Expr Bool"
                    , "  Add  :: Expr Int -> Expr Int -> Expr Int"
                    , "  Cond :: forall a. Expr Bool -> Expr a -> Expr a -> Expr a"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        -- TODO: These types should not contain explicit coercions (#68)
        assertIdInfo' session "A" (4,3,4,6) (4,3,4,6) "Num" DataName [(GHC742, "GHC.Prim.~# * ($a) Int -> Int -> Expr ($a)"), (GHC78, "($a GHC.Prim.~# Int) -> Int -> Expr $a")] "main:A" [(GHC742, "A.hs@4:3-4:6"), (GHC78, "A.hs@4:3-4:26")] "" (allVersions "binding occurrence")
        assertIdInfo session "A" (4,23,4,26) "Int" TcClsName "" "ghc-prim-0.2.0.0:GHC.Types" "<wired into compiler>" "base-4.5.1.0:Data.Int" "wired in to the compiler"
        assertIdInfo' session "A" (7,3,7,7) (7,3,7,7) "Cond" DataName (allVersions "Expr Bool -> Expr a -> Expr a -> Expr a") "main:A" [(GHC742, "A.hs@7:3-7:7"), (GHC78, "A.hs@7:3-7:60")] "" (allVersions "binding occurrence")
        assertIdInfo session "A" (7,18,7,19) "a" TvName "" "main:A" "A.hs@7:18-7:19" "" "binding occurrence"
        assertIdInfo' session "A" (7,54,7,58) (7,54,7,58) "Expr" TcClsName [] "main:A" [(GHC742, "A.hs@3:6-3:10"), (GHC78, "A.hs@3:1-7:60")] "" (allVersions "defined locally")
        assertIdInfo session "A" (7,59,7,60) "a" TvName "" "main:A" "A.hs@7:18-7:19" "" "defined locally"
    )
  , ( "Type information 18: Other types"
    , ifIdeBackendHaddockTestsEnabled defaultSession $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ {-  1 -} "{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeFamilies #-}"
                    , {-  2 -} "module A where"

                    , {-  3 -} "class C a where"
                    , {-  4 -} "  f :: Int -> a"

                    , {-  5 -} "class D a b | a -> b where"
                    , {-  6 -} "  g :: a -> b"

                    , {-  7 -} "type Foo = Int"

                    , {-  8 -} "type family Bar a"
                    , {-  9 -} "type instance Bar Int = Bool"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        -- TODO: we don't get location info for the fundeps
        -- (this is missing from GHC's AST)
        assertIdInfo' session "A" (3,7,3,8) (3,7,3,8) "C" TcClsName [] "main:A" [(GHC742, "A.hs@3:7-3:8"), (GHC78, "A.hs@3:1-4:16")] "" (allVersions "binding occurrence")
        assertIdInfo session "A" (3,9,3,10) "a" TvName "" "main:A" "A.hs@3:9-3:10" "" "binding occurrence"
        assertIdInfo' session "A" (4,3,4,4) (4,3,4,4) "f" VarName [] "main:A" [(GHC742, "A.hs@4:3-4:4"), (GHC78, "A.hs@4:3-4:16")] "" (allVersions "defined locally")
        assertIdInfo session "A" (4,8,4,11) "Int" TcClsName "" "ghc-prim-0.2.0.0:GHC.Types" "<wired into compiler>" "base-4.5.1.0:Data.Int" "wired in to the compiler"
        assertIdInfo session "A" (4,15,4,16) "a" TvName "" "main:A" "A.hs@3:9-3:10" "" "defined locally"
        assertIdInfo' session "A" (5,7,5,8) (5,7,5,8) "D" TcClsName [] "main:A" [(GHC742, "A.hs@5:7-5:8"), (GHC78, "A.hs@5:1-6:14")] "" (allVersions "binding occurrence")
        assertIdInfo session "A" (5,9,5,10) "a" TvName "" "main:A" "A.hs@5:9-5:10" "" "binding occurrence"
        assertIdInfo session "A" (5,11,5,12) "b" TvName "" "main:A" "A.hs@5:11-5:12" "" "binding occurrence"
        assertIdInfo' session "A" (6,3,6,4) (6,3,6,4) "g" VarName [] "main:A" [(GHC742, "A.hs@6:3-6:4"), (GHC78, "A.hs@6:3-6:14")] "" (allVersions "defined locally")
        assertIdInfo session "A" (6,8,6,9) "a" TvName "" "main:A" "A.hs@5:9-5:10" "" "defined locally"
        assertIdInfo session "A" (6,13,6,14) "b" TvName "" "main:A" "A.hs@5:11-5:12" "" "defined locally"
        assertIdInfo' session "A" (7,6,7,9) (7,6,7,9) "Foo" TcClsName [] "main:A" [(GHC742, "A.hs@7:6-7:9"), (GHC78, "A.hs@7:1-7:15")] "" (allVersions "binding occurrence")
        assertIdInfo session "A" (7,12,7,15) "Int" TcClsName "" "ghc-prim-0.2.0.0:GHC.Types" "<wired into compiler>" "base-4.5.1.0:Data.Int" "wired in to the compiler"
        assertIdInfo' session "A" (8,13,8,16) (8,13,8,16) "Bar" TcClsName [] "main:A" [(GHC742, "A.hs@8:13-8:16"), (GHC78, "A.hs@8:1-8:18")] "" (allVersions "binding occurrence")
        assertIdInfo session "A" (8,17,8,18) "a" TvName "" "main:A" "A.hs@8:17-8:18" "" "binding occurrence"
        assertIdInfo' session "A" (9,15,9,18) (9,15,9,18) "Bar" TcClsName [] "main:A" [(GHC742, "A.hs@8:13-8:16"), (GHC78, "A.hs@8:1-8:18")] "" (allVersions "defined locally")
        assertIdInfo session "A" (9,19,9,22) "Int" TcClsName "" "ghc-prim-0.2.0.0:GHC.Types" "<wired into compiler>" "base-4.5.1.0:Data.Int" "wired in to the compiler"
        assertIdInfo session "A" (9,25,9,29) "Bool" TcClsName "" "ghc-prim-0.2.0.0:GHC.Types" "<wired into compiler>" "base-4.5.1.0:Data.Bool" "wired in to the compiler"
    )
  , ( "Type information 19: Default methods"
    , ifIdeBackendHaddockTestsEnabled defaultSession $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "class Foo a where"
                    , "  foo :: a -> Int"
                    , "  foo _ = succ 1"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        assertIdInfo session "A" (4,11,4,15) "succ" VarName "Enum a1 => a1 -> a1" "base-4.5.1.0:GHC.Enum" "<no location info>" "base-4.5.1.0:Prelude" "imported from base-4.5.1.0:Prelude at A.hs@1:8-1:9"
    )
  , ( "Type information 20: Updated session (#142)"
    , ifIdeBackendHaddockTestsEnabled defaultSession $ \session -> do
        let upd1 = updateSourceFile "Main.hs" (BSLC.pack "main = print foo\nfoo = 5")
            upd2 = updateSourceFile "Main.hs" (BSLC.pack "main = print foo\n\nfoo = 5")

        updateSessionD session upd1 1
        assertNoErrors session
        assertIdInfo session "Main" (1,14,1,17) "foo" VarName "Integer" "main:Main" "Main.hs@2:1-2:4" "" "defined locally"

        updateSessionD session upd2 1
        assertNoErrors session
        assertIdInfo session "Main" (1,14,1,17) "foo" VarName "Integer" "main:Main" "Main.hs@3:1-3:4" "" "defined locally"
    )
  , ( "Type information 21: spanInfo vs expTypes (#3043)"
    , ifIdeBackendHaddockTestsEnabled defaultSession $ \session -> do
        let upd = updateSourceFile "A.hs" . BSLC.pack . unlines $ [
                "{-# LANGUAGE NoMonomorphismRestriction #-}"
              , "{-# LANGUAGE OverloadedStrings #-}"
              , "module A where"
              , "import Data.ByteString"
              , "import Text.Parsec"
              --                  1           2         3          4          5         6
              --         123456789012345 67 890123456789012345678 90 123456789012345678901
              , {- 6 -} "foo = (getNum (\"x\" :: String),getNum (\"x\" :: ByteString))"
              --                  1         2         3          4
              --         1234567890123456789012345678901234567 890123 4
              , {- 7 -} "  where getNum = runParserT digit () \"x.txt\""
              ]

        updateSessionD session upd 1
        errs <- getSourceErrors session

        case errs of
          [] -> do
            -- Check the polymorphic type of getNum
            assertIdInfo session "A" (6,  8, 6, 14) "getNum" VarName "Stream s m2 Char => s -> m2 (Either ParseError Char)" "main:A" "A.hs@7:9-7:15" "" "defined locally"
            assertIdInfo session "A" (6, 31, 6, 37) "getNum" VarName "Stream s m2 Char => s -> m2 (Either ParseError Char)" "main:A" "A.hs@7:9-7:15" "" "defined locally"
            assertIdInfo session "A" (7,  9, 7, 15) "getNum" VarName "Stream s m2 Char => s -> m2 (Either ParseError Char)" "main:A" "A.hs@7:9-7:15" "" "binding occurrence"

            -- Check the monomorphic (local) type of getNum
            expTypes <- getExpTypes session
            assertExpTypes expTypes "A" (6,  8, 6, 14) [
                (6,  7, 6, 58, "(m (Either ParseError Char), m1 (Either ParseError Char))")
              , (6,  8, 6, 14, "String -> m (Either ParseError Char)")
              , (6,  8, 6, 14, "Stream s m2 Char => s -> m2 (Either ParseError Char)")
              , (6,  8, 6, 30, "m (Either ParseError Char)")
              ]
            assertExpTypes expTypes "A" (6, 31, 6, 37) [
                (6,  7, 6, 58, "(m (Either ParseError Char), m1 (Either ParseError Char))")
              , (6, 31, 6, 37, "ByteString -> m1 (Either ParseError Char)")
              , (6, 31, 6, 37, "Stream s m2 Char => s -> m2 (Either ParseError Char)")
              , (6, 31, 6, 57, "m1 (Either ParseError Char)")
              ]
            assertExpTypes expTypes "A" (7,  9, 7, 15) [
                (7,  9, 7, 15, "s -> m2 (Either ParseError Char)")
              ]

            -- For completeness' sake, check polymorphic type of foo
            assertIdInfo session "A" (6,  1, 6, 4) "foo" VarName "(Monad m1, Monad m) => (m (Either ParseError Char), m1 (Either ParseError Char))" "main:A" "A.hs@6:1-6:4" "" "binding occurrence"
          _ ->
            putStrLn "WARNING: Skipping due to errors (probably parsec package not installed)"
    )
  , ( "Test internal consistency of local id markers"
    , withSession defaultSession $ \session -> do
        let upd = (updateSourceFile "M.hs" . BSLC.pack . unlines $
              [ "module M where"
              , "import qualified Text.PrettyPrint as Disp"
              , "class Text a where"
              , "  disp  :: a -> Disp.Doc"
              , "display :: Text a => a -> String"
              , "display = Disp.renderStyle style . disp"
              , "  where style = Disp.Style {}"
              ])
        updateSessionD session upd 1
        assertOneError session
    )
  , ( "Test internal consistency of imported id markers"
    , withSession defaultSession $ \session -> do
        let upd = (updateSourceFile "M.hs" . BSLC.pack . unlines $
              [ "module M where"
              , "import qualified Text.PrettyPrint as Disp"
              , "class Text a where"
              , "  disp  :: a -> Disp.Doc"
              , "display :: Text a => a -> String"
              , "display = Disp.renderStyle astyle . disp"
              , "  where astyle = Disp.Style {"
              , "          Disp.mode            = Disp.PageMode,"
              , "          Disp.lineLength      = 79,"
              , "          Disp.ribbonsPerLine  = 1.0"
              , "        }"
              ])
        updateSessionD session upd 1
        assertNoErrors session
    )
  , ( "Autocomplete 1: Imports for partial module"
    , withSession (withOpts ["-XPackageImports"]) $ \session -> do
        let upd = (updateSourceFile "M.hs" . BSLC.pack . unlines $
              [ "module M where"
              , "import Control.Monad"
              , "import Control.Category hiding (id)"
              , "import qualified Control.Arrow as A (second)"
              , "import qualified \"base\" Data.List"
              , "import Control.Parallel"
              , "foo ="
              ])
        updateSessionD session upd 1
        assertSourceErrors' session ["parse error"]
        imports <- getImports session

        let base mod = ModuleId {
                moduleName    = Text.pack mod
              , modulePackage = PackageId {
                    packageName    = Text.pack "base"
                  , packageVersion = Just (Text.pack "X.Y.Z")
                  }
              }
            par mod = ModuleId {
                moduleName    = Text.pack mod
              , modulePackage = PackageId {
                    packageName    = Text.pack "parallel"
                  , packageVersion = Just (Text.pack "X.Y.Z")
                  }
              }

        assertSameSet "imports: " (ignoreVersions . fromJust . imports $ Text.pack "M") $ [
            Import {
                importModule    = base "Prelude"
              , importPackage   = Nothing
              , importQualified = False
              , importImplicit  = True
              , importAs        = Nothing
              , importEntities  = ImportAll
              }
          , Import {
                importModule    = base "Control.Monad"
              , importPackage   = Nothing
              , importQualified = False
              , importImplicit  = False
              , importAs        = Nothing
              , importEntities  = ImportAll
              }
          , Import {
                importModule    = base "Control.Category"
              , importPackage   = Nothing
              , importQualified = False
              , importImplicit  = False
              , importAs        = Nothing
              , importEntities  = ImportHiding [Text.pack "id"]
              }
          , Import {
                importModule     = base "Control.Arrow"
              , importPackage    = Nothing
              , importQualified  = True
              , importImplicit   = False
              , importAs         = Just (Text.pack "A")
              , importEntities   = ImportOnly [Text.pack "second"]
              }
          , Import {
                importModule    = base "Data.List"
              , importPackage   = Just (Text.pack "base")
              , importQualified = True
              , importImplicit  = False
              , importAs        = Nothing
              , importEntities  = ImportAll
              }
          , Import {
                importModule    = par "Control.Parallel"
              , importPackage   = Nothing
              , importQualified = False
              , importImplicit  = False
              , importAs        = Nothing
              , importEntities  = ImportAll
              }
          ]
        autocomplete <- getAutocompletion session
        let completeFo = autocomplete (Text.pack "M") "fo"
        assertSameSet "fo: " (map idInfoQN completeFo) [
            "foldM"
          , "foldM_"
          , "forM"
          , "forM_"
          , "forever"
          , "Data.List.foldl'"
          , "Data.List.foldl1"
          , "Data.List.foldl1'"
          , "Data.List.foldr"
          , "Data.List.foldl"
          , "Data.List.foldr1"
          ]
        let completeControlMonadFo = autocomplete (Text.pack "M") "Data.List.fo"
        assertSameSet "Data.List.fo: " (map idInfoQN completeControlMonadFo) [
            "Data.List.foldl'"
          , "Data.List.foldl1"
          , "Data.List.foldl1'"
          , "Data.List.foldr"
          , "Data.List.foldl"
          , "Data.List.foldr1"
          ]
        let completeSec = autocomplete (Text.pack "M") "sec"
        assertSameSet "sec: " (map idInfoQN completeSec) [
            "A.second"
          ]
    )
  , ( "Autocomplete 2: Recompute after recompilation"
    , withSession defaultSession $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "foobar :: Bool -> Bool"
                    , "foobar = id"
                    ])
               <> (updateSourceFile "B.hs" . BSLC.pack . unlines $
                    [ "module B where"
                    , "import A"
                    ])

        updateSessionD session upd 2
        assertNoErrors session

        -- First check, for sanity
        do autocomplete <- getAutocompletion session
           let completeFoob = autocomplete (Text.pack "B") "foob"
           assertEqual "" "[foobar (VarName) :: Bool -> Bool defined in main:A at A.hs@3:1-3:7 (imported from main:A at B.hs@2:1-2:9)]" (show completeFoob)

        -- Change A, but not B. The type reported in the autocompletion for B
        -- should now be changed, too

        let upd' = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                     [ "module A where"
                     , "foobar :: Int -> Int"
                     , "foobar = id"
                     , "foobar' :: () -> ()"
                     , "foobar' = id"
                     ])

        -- This will trigger recompilation of B
        updateSessionD session upd' 2
        assertNoErrors session

        do autocomplete <- getAutocompletion session
           let completeFoob = autocomplete (Text.pack "B") "foob"
           let expected = "[foobar (VarName) :: Int -> Int defined in main:A at A.hs@3:1-3:7 (imported from main:A at B.hs@2:1-2:9),foobar' (VarName) :: () -> () defined in main:A at A.hs@5:1-5:8 (imported from main:A at B.hs@2:1-2:9)]"
           assertEqual "" expected (show completeFoob)
    )
  , ( "Autocomplete 3: Autocompletion entries should have home module info"
    , ifIdeBackendHaddockTestsEnabled defaultSession $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    ])

        updateSessionD session upd 1
        assertNoErrors session

        autocomplete <- getAutocompletion session
        let completeTru = autocomplete (Text.pack "A") "Tru"
        assertEqual "" (ignoreVersions "[True (DataName) defined in ghc-prim-0.2.0.0:GHC.Types at <wired into compiler> (home base-4.5.1.0:Data.Bool) (wired in to the compiler)]") (ignoreVersions $ show completeTru)
    )
  , ( "Autocomplete 4: fpco issue #2518"
    , withSession (withOpts ["-XPackageImports"]) $ \session -> do
        let upd = (updateSourceFile "M.hs" . BSLC.pack . unlines $
              [ "module M where"
              , "import qualified Data.ByteString.Lazy as B"
              , "foo = toC"
              ])
        updateSessionD session upd 1
        assertSourceErrors' session ["Not in scope: `toC'"]
        autocomplete <- getAutocompletion session
        let complete_toC = autocomplete (Text.pack "M") "toC"
        assertSameSet "" (map idInfoQN complete_toC) [
            "B.toChunks"
          ]
    )
    -- TODO: Autocomplete test that checks import errors
    -- - Explicitly importing something that wasn't exported
    -- - Explicitly hiding something that wasn't exported
    -- - Use of PackageImports without the flag
  , ( "GHC crash 1: No delay, no further requests"
    , withSession defaultSession $ \session -> do
        crashGhcServer session Nothing
    )
  , ( "GHC crash 2: No delay, follow up request"
    , withSession defaultSession $ \session -> do
        crashGhcServer session Nothing
        updateSession session (updateEnv "Foo" Nothing) (\_ -> return ())
        actualErrs <- getSourceErrors session
        let expectedErrs = [
                SourceError {
                    errorKind = KindServerDied
                  , errorSpan = TextSpan (Text.pack "<<server died>>")
                  , errorMsg  = Text.pack ("user error (Intentional crash)")
                  }
              ]
        assertEqual "" expectedErrs actualErrs
    )
  , ( "GHC crash 3: Delay, follow up request"
    , withSession defaultSession $ \session -> do
        crashGhcServer session (Just 1000000)
        updateSession session (updateEnv "Foo" Nothing) (\_ -> return ())
        threadDelay 2000000
        updateSession session (updateEnv "Foo" Nothing) (\_ -> return ())
        assertSourceErrors' session ["Intentional crash"]
    )
  , ( "GHC crash 4: Make sure session gets restarted on second update"
    , withSession defaultSession $ \session -> do
        -- Compile some code
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import System.Environment (getEnv)"
                    , "printFoo :: IO ()"
                    , "printFoo = getEnv \"Foo\" >>= putStr"
                    ])

        let compilingProgress = [(1, 1, "Compiling M")]

        updateSessionP session upd compilingProgress
        assertNoErrors session

        -- Now crash the server
        crashGhcServer session Nothing

        -- The next request fails, and we get a source error
        updateSessionP session (updateEnv "Foo" (Just "Value1")) []
        assertSourceErrors' session ["Intentional crash"]

        -- The next request, however, succeeds (and restarts the server,
        -- and recompiles the code)
        updateSessionP session (updateEnv "Foo" (Just "Value2")) compilingProgress

        -- The code should have recompiled and we should be able to execute it
        do runActions <- runStmt session "M" "printFoo"
           (output, result) <- runWaitAll runActions
           assertEqual "" RunOk result
           assertEqual "" (BSLC.pack "Value2") output
    )
  , ( "GHC crash 5: Repeated crashes and restarts"
    , withSession defaultSession $ \session -> do
        -- Compile some code
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import System.Environment (getEnv)"
                    , "printFoo :: IO ()"
                    , "printFoo = getEnv \"Foo\" >>= putStr"
                    ])

        let compilingProgress = [(1, 1, "Compiling M")]

        updateSessionP session upd compilingProgress
        assertNoErrors session

        -- We repeat the test of 'crash 4' a number of times
        replicateM_ 5 $ do
          -- Now crash the server
          crashGhcServer session Nothing

          -- The next request fails, and we get a source error
          updateSessionP session (updateEnv "Foo" (Just "Value1")) []
          assertSourceErrors' session ["Intentional crash"]

          -- The next request, however, succeeds (and restarts the server,
          -- and recompiles the code)
          updateSessionP session (updateEnv "Foo" (Just "Value2")) compilingProgress

          -- The code should have recompiled and we should be able to execute it
          do runActions <- runStmt session "M" "printFoo"
             (output, result) <- runWaitAll runActions
             assertEqual "" RunOk result
             assertEqual "" (BSLC.pack "Value2") output
    )
  , ( "GHC crash 6: Add additional code after update"
    , withSession defaultSession $ \session -> do
        let updA = (updateCodeGeneration True)
                <> (updateSourceFile "A.hs" . BSLC.pack . unlines $
                     [ "module A where"
                     , "printA :: IO ()"
                     , "printA = putStr \"A\""
                     ])
        let updB = (updateCodeGeneration True)
                <> (updateSourceFile "B.hs" . BSLC.pack . unlines $
                     [ "module B where"
                     , "import A"
                     , "printAB :: IO ()"
                     , "printAB = printA >> putStr \"B\""
                     ])

        let compProgA n = [(1, n, "Compiling A")]
            compProgB n = [(2, n, "Compiling B")]

        updateSessionP session updA (compProgA 1)
        assertNoErrors session

        -- Now crash the server
        crashGhcServer session Nothing

        -- The next request fails, and we get a source error
        updateSessionP session updB []
        assertSourceErrors' session ["Intentional crash"]

        -- The next request, however, succeeds (and restarts the server,
        -- and recompiles the code)
        updateSessionP session updB $ (compProgA 2 ++ compProgB 2)

        -- The code should have recompiled and we should be able to execute it
        do runActions <- runStmt session "B" "printAB"
           (output, result) <- runWaitAll runActions
           assertEqual "" RunOk result
           assertEqual "" (BSLC.pack "AB") output
    )
  , ( "GHC crash 7: Update imported module after update"
    , withSession defaultSession $ \session -> do
        let updA = (updateCodeGeneration True)
                <> (updateSourceFile "A.hs" . BSLC.pack . unlines $
                     [ "module A where"
                     , "printA :: IO ()"
                     , "printA = putStr \"A\""
                     ])
        let updB = (updateCodeGeneration True)
                <> (updateSourceFile "B.hs" . BSLC.pack . unlines $
                     [ "module B where"
                     , "import A"
                     , "printAB :: IO ()"
                     , "printAB = printA >> putStr \"B\""
                     ])

        let compProgA n = [(1, n, "Compiling A")]
            compProgB n = [(2, n, "Compiling B")]

        updateSessionP session (mconcat [updA, updB]) (compProgA 2 ++ compProgB 2)
        assertNoErrors session

        -- Now crash the server
        crashGhcServer session Nothing

        -- The next request fails, and we get a source error
        let updA2 = (updateCodeGeneration True)
                 <> (updateSourceFile "A.hs" . BSLC.pack . unlines $
                      [ "module A where"
                      , "printA :: IO ()"
                      , "printA = putStr \"A2\""
                      ])
        updateSessionP session updA2 []
        assertSourceErrors' session ["Intentional crash"]

        -- The next request, however, succeeds (and restarts the server,
        -- and recompiles the code)
        updateSessionP session updA2 $ mconcat [compProgA 2, compProgB 2]

        -- The code should have recompiled and we should be able to execute it
        do runActions <- runStmt session "B" "printAB"
           (output, result) <- runWaitAll runActions
           assertEqual "" RunOk result
           assertEqual "" (BSLC.pack "A2B") output
    )
  , ( "GHC crash 8: Update importing module after update"
    , withSession defaultSession $ \session -> do
        let updA = (updateCodeGeneration True)
                <> (updateSourceFile "A.hs" . BSLC.pack . unlines $
                     [ "module A where"
                     , "printA :: IO ()"
                     , "printA = putStr \"A\""
                     ])
        let updB = (updateCodeGeneration True)
                <> (updateSourceFile "B.hs" . BSLC.pack . unlines $
                     [ "module B where"
                     , "import A"
                     , "printAB :: IO ()"
                     , "printAB = printA >> putStr \"B\""
                     ])

        let compProgA n = [(1, n, "Compiling A")]
            compProgB n = [(2, n, "Compiling B")]

        updateSessionP session (mconcat [updA, updB]) (compProgA 2 ++ compProgB 2)
        assertNoErrors session

        -- Now crash the server
        crashGhcServer session Nothing

        -- The next request fails, and we get a source error
        let updB2 = (updateCodeGeneration True)
                 <> (updateSourceFile "B.hs" . BSLC.pack . unlines $
                      [ "module B where"
                      , "import A"
                      , "printAB :: IO ()"
                      , "printAB = printA >> putStr \"B2\""
                      ])
        updateSessionP session updB2 []
        assertSourceErrors' session ["Intentional crash"]

        -- The next request, however, succeeds (and restarts the server,
        -- and recompiles the code)
        updateSessionP session updB2 $ mconcat [compProgA 2, compProgB 2]

        -- The code should have recompiled and we should be able to execute it
        do runActions <- runStmt session "B" "printAB"
           (output, result) <- runWaitAll runActions
           assertEqual "" RunOk result
           assertEqual "" (BSLC.pack "AB2") output
    )
  , ( "Parse ghc 'Compiling' messages"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "printA :: IO ()"
                    , "printA = putStr \"A\""
                    ])
               <> (updateSourceFile "B.hs" . BSLC.pack . unlines $
                    [ "module B where"
                    , "import A"
                    , "printAB :: IO ()"
                    , "printAB = printA >> putStr \"B\""
                    ])

        progressUpdatesRef <- newIORef []
        updateSession session upd $ \p -> do
          progressUpdates <- readIORef progressUpdatesRef
          writeIORef progressUpdatesRef (progressUpdates ++ [p])
        assertNoErrors session

        let abstract (Progress {..}) = (progressStep, progressNumSteps, Text.unpack `liftM` progressParsedMsg)
        progressUpdates <- readIORef progressUpdatesRef
        assertEqual "" [(1, 2, Just "Compiling A"), (2, 2, Just "Compiling B")]
                       (map abstract progressUpdates)

    )
  , ( "Parse ghc 'Compiling' messages (with TH)"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE TemplateHaskell #-}"
                    , "module A where"
                    , "import Language.Haskell.TH"
                    , "foo :: Q Exp"
                    , "foo = [| True |]"
                    ])
               <> (updateSourceFile "Main.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE TemplateHaskell #-}"
                    , "module Main where"
                    , "import A"
                    , "main :: IO ()"
                    , "main = print $foo"
                    ])

        let abstract (Progress {..}) = (progressStep, progressNumSteps, Text.unpack `liftM` progressParsedMsg)
        progressUpdatesRef <- newIORef []
        updateSession session upd $ \p -> do
          progressUpdates <- readIORef progressUpdatesRef
          writeIORef progressUpdatesRef (progressUpdates ++ [p])
        assertNoErrors session

        do progressUpdates <- readIORef progressUpdatesRef
           assertEqual "" [(1, 2, Just "Compiling A"), (2, 2, Just "Compiling Main")]
                          (map abstract progressUpdates)

        -- Now we touch A, triggering recompilation of both A and B
        -- This will cause ghc to report "[TH]" as part of the progress message
        -- (at least on the command line). It doesn't seem to happen with the
        -- API; but just in case, we check that we still get the right messages
        -- (and not, for instance, '[TH]' as the module name).

        let upd2 = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE TemplateHaskell #-}"
                    , "module A where"
                    , "import Language.Haskell.TH"
                    , "foo :: Q Exp"
                    , "foo = [| False |]"
                    ])
        writeIORef progressUpdatesRef []
        updateSession session upd2 $ \p -> do
          progressUpdates <- readIORef progressUpdatesRef
          writeIORef progressUpdatesRef (progressUpdates ++ [p])
        assertNoErrors session

        do progressUpdates <- readIORef progressUpdatesRef
           assertEqual "" [(1, 2, Just "Compiling A"), (2, 2, Just "Compiling Main")]
                          (map abstract progressUpdates)
    )
  , ( "getLoadedModules while configGenerateModInfo off"
    , withSession (withModInfo False) $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "hello :: IO ()"
                    , "hello = putStrLn \"Hello World\""
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        assertLoadedModules session "" ["M"]
    )
  , ( "Package dependencies"
    , withSession (withOpts ["-hide-package monads-tf"]) $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    ])
               <> (updateSourceFile "B.hs" . BSLC.pack . unlines $
                    [ "module B where"
                    , "import Control.Parallel"
                    ])
               <> (updateSourceFile "C.hs" . BSLC.pack . unlines $
                    [ "module C where"
                    , "import Control.Monad.Cont" -- from mtl
                    ])

        updateSessionD session upd 3
        assertNoErrors session

        deps <- getPkgDeps session
        assertEqual "" (ignoreVersions "Just [base-4.5.1.0,ghc-prim-0.2.0.0,integer-gmp-0.4.0.0]") (ignoreVersions $ show (deps (Text.pack "A")))
        assertEqual "" (ignoreVersions "Just [parallel-3.2.0.4,base-4.5.1.0,ghc-prim-0.2.0.0,integer-gmp-0.4.0.0]") (ignoreVersions $ show (deps (Text.pack "B")))
        assertEqual "" (ignoreVersions "Just [mtl-2.1.2,base-4.5.1.0,ghc-prim-0.2.0.0,integer-gmp-0.4.0.0,transformers-0.3.0.0]") (ignoreVersions $ show (deps (Text.pack "C")))
     )
  , ( "Set command line arguments"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import System.Environment (getArgs)"
                    , "printArgs :: IO ()"
                    , "printArgs = getArgs >>= print"
                    , "main :: IO ()"
                    , "main = printArgs"
                    ])
        updateSessionD session upd 1
        assertNoErrors session

        let m = "M"
            updExe = buildExe [] [(Text.pack m, "M.hs")]
        updateSessionD session updExe 2

        -- Check that default is []
        do runActions <- runStmt session "M" "printArgs"
           (output, result) <- runWaitAll runActions
           assertEqual "" RunOk result
           assertEqual "" (BSLC.pack "[]\n") output

        do runActionsExe <- runExe session m
           (outExe, statusExe) <- runWaitAll runActionsExe
           assertEqual "after runExe" ExitSuccess statusExe
           assertEqual "Output from runExe"
                       "[]\n"
                       outExe

        -- Check that we can set command line arguments
        updateSession session (updateArgs ["A", "B", "C"]) (\_ -> return ())
        do runActions <- runStmt session "M" "printArgs"
           (output, result) <- runWaitAll runActions
           assertEqual "" RunOk result
           assertEqual "" (BSLC.pack "[\"A\",\"B\",\"C\"]\n") output

        do runActionsExe <- runExe session m
           (outExe, statusExe) <- runWaitAll runActionsExe
           assertEqual "after runExe" ExitSuccess statusExe
           assertEqual "Output from runExe"
                       "[\"A\",\"B\",\"C\"]\n"
                       outExe

        -- Check that we can change command line arguments
        updateSession session (updateArgs ["D", "E"]) (\_ -> return ())
        do runActions <- runStmt session "M" "printArgs"
           (output, result) <- runWaitAll runActions
           assertEqual "" RunOk result
           assertEqual "" (BSLC.pack "[\"D\",\"E\"]\n") output

        do runActionsExe <- runExe session m
           (outExe, statusExe) <- runWaitAll runActionsExe
           assertEqual "after runExe" ExitSuccess statusExe
           assertEqual "Output from runExe"
                       "[\"D\",\"E\"]\n"
                       outExe

        -- Check that we can clear command line arguments
        updateSession session (updateArgs []) (\_ -> return ())
        do runActions <- runStmt session "M" "printArgs"
           (output, result) <- runWaitAll runActions
           assertEqual "" RunOk result
           assertEqual "" (BSLC.pack "[]\n") output

        do runActionsExe <- runExe session m
           (outExe, statusExe) <- runWaitAll runActionsExe
           assertEqual "after runExe" ExitSuccess statusExe
           assertEqual "Output from runExe"
                       "[]\n"
                       outExe
    )
  , ( "Check that command line arguments survive restartSession"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import System.Environment (getArgs)"
                    , "printArgs :: IO ()"
                    , "printArgs = getArgs >>= print"
                    , "main :: IO ()"
                    , "main = printArgs"
                    ])
        updateSessionD session upd 1
        assertNoErrors session

        let m = "M"
            updExe = buildExe [] [(Text.pack m, "M.hs")]
        updateSessionD session updExe 2

        -- Sanity check: check before restart session
        updateSession session (updateArgs ["A", "B", "C"]) (\_ -> return ())
        do runActions <- runStmt session "M" "printArgs"
           (output, result) <- runWaitAll runActions
           assertEqual "" RunOk result
           assertEqual "" (BSLC.pack "[\"A\",\"B\",\"C\"]\n") output

        do runActionsExe <- runExe session m
           (outExe, statusExe) <- runWaitAll runActionsExe
           assertEqual "after runExe" ExitSuccess statusExe
           assertEqual "Output from runExe"
                       "[\"A\",\"B\",\"C\"]\n"
                       outExe

        -- Restart and update the session
        restartSession session Nothing
        updateSessionD session upd 1
        assertNoErrors session

        -- Check that arguments are still here
        do runActions <- runStmt session "M" "printArgs"
           (output, result) <- runWaitAll runActions
           assertEqual "" RunOk result
           assertEqual "" (BSLC.pack "[\"A\",\"B\",\"C\"]\n") output

        do runActionsExe <- runExe session m
           (outExe, statusExe) <- runWaitAll runActionsExe
           assertEqual "after runExe" ExitSuccess statusExe
           assertEqual "Output from runExe"
                       "[\"A\",\"B\",\"C\"]\n"
                       outExe
    )
  , ( "Register a package, don't restart session, don't see the package"
    , do deletePackage "test/simple-lib17"
         withSession (withOpts ["-package simple-lib17"]) $ \session -> do
           let upd = updateSourceFile "Main.hs" . BSLC.pack . unlines $
                       [ "module Main where"
                       , "import SimpleLib (simpleLib)"
                       , "main = print simpleLib"
                       ]
           installPackage "test/simple-lib17"
           -- No restartSession yet, hence the exception at session init.
           let expected = "<command line>: cannot satisfy -package simple-lib17"
           updateSessionD session upd 1
           assertSourceErrors' session [expected]
           deletePackage "test/simple-lib17"
    )
  , ( "Register a package, restart session, see the package and check for cabal macros"
    , withSession (withOpts ["-XCPP"]) $ \session -> do
        deletePackage "test/simple-lib17"
        restartSession session (Just defaultSessionInitParams)
        let upd = updateSourceFile "Main.hs" . BSLC.pack . unlines $
                    [ "module Main where"
                    , "import SimpleLib (simpleLib)"
                    , "#if MIN_VERSION_simple_lib17(0,1,0)"
                    , "main = print simpleLib"
                    , "#else"
                    , "terrible error"
                    , "#endif"
                    ]
        installPackage "test/simple-lib17"
        restartSession session (Just defaultSessionInitParams) -- only now the package accessible
        updateSessionD session upd 1
        assertNoErrors session
        let m = "Main"
            upd2 = buildExe [] [(Text.pack m, "Main.hs")]
        updateSessionD session upd2 2
        distDir <- getDistDir session
        out <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "DB exe output"
                    "42\n"
                    out
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "42\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe
        deletePackage "test/simple-lib17"

        dotCabalFromName <- getDotCabal session
        let dotCabal = dotCabalFromName "libName" $ Version [1, 0] []
        assertEqual "dotCabal from simple-lib17" (filterIdeBackendTest $ BSLC.pack "name: libName\nversion: 1.0\ncabal-version: 1.14.0\nbuild-type: Simple\nlicense: AllRightsReserved\nlicense-file: \"\"\ndata-dir: \"\"\n \nlibrary\n    build-depends: base ==4.5.1.0, ghc-prim ==0.2.0.0,\n                   integer-gmp ==0.4.0.0, simple-lib17 ==0.1.0.0\n    exposed: True\n    buildable: True\n    default-language: Haskell2010\n    ghc-options: -XCPP\n \n ") $ filterIdeBackendTest dotCabal
        let pkgDir = distDir </> "dotCabal.for.lhs"
        createDirectoryIfMissing False pkgDir
        BSLC.writeFile (pkgDir </> "libName.cabal") dotCabal
        checkWarns <- checkPackage pkgDir
        assertEqual "checkWarns for dotCabal for .lhs files" (filterCheckWarns checkWarns) (filterCheckWarns "The following warnings are likely affect your build negatively:\n* Instead of 'ghc-options: -XCPP' use 'extensions: CPP'\n\nThese warnings may cause trouble when distributing the package:\n* No 'category' field.\n\n* No 'maintainer' field.\n\nThe following errors will cause portability problems on other environments:\n* The package is missing a Setup.hs or Setup.lhs script.\n\n* No 'synopsis' or 'description' field.\n\n* The 'license' field is missing or specified as AllRightsReserved.\n\nHackage would reject this package.\n")
    )
  , ( "Make sure package DB is passed to ghc (configGenerateModInfo False)"
    , let packageOpts = ["-package parallel"]
          setup       = withModInfo True
                      . withDBStack [GlobalPackageDB]
                      . withOpts packageOpts
      in withSession setup $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "import Control.Parallel"
                    ])
        -- We expect an error because 'ide-backend-rts' and/or 'parallel'
        -- are not (usually?) installed in the global package DB.
        let expected1 = "cannot satisfy -package ide-backend-rts"
            expected2 = "cannot satisfy -package parallel"
        updateSessionD session upd 1
        assertSourceErrors session [[
            (Nothing, expected1)
          , (Nothing, expected2)
          ]]
    )
    {-
     -- DISABLED: We no longer detect the problem immediately because we no
     -- longer send an initial RPC request after starting the GHC server
  , ( "Make sure package DB is passed to ghc (detect problem immediately)"
    , let packageOpts = ["-package parallel"]
          config      = defaultSessionConfig {
                            configGenerateModInfo = False
                          , configPackageDBStack  = [GlobalPackageDB]
                          , configStaticOpts      = packageOpts
                          }
      in withSession config $ \session -> do
        -- We should be able to see these errors without doing an
        -- updateSession first
        let expected1 = "cannot satisfy -package ide-backend-rts"
            expected2 = "cannot satisfy -package parallel"
        assertSourceErrors session [[
            (Nothing, expected1)
          , (Nothing, expected2)
          ]]
    )
    -}
  , ( "Make sure package DB is passed to ghc (configGenerateModInfo True)"
    , let packageOpts = ["-package parallel"]
          setup       = withModInfo True
                      . withDBStack [GlobalPackageDB]
                      . withOpts packageOpts
      in withSession setup $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "import Control.Parallel"
                    ])
        -- We expect an error because 'ide-backend-rts' and/or 'parallel'
        -- are not (usually?) installed in the global package DB.
        let expected1 = "cannot satisfy -package ide-backend-rts"
            expected2 = "<command line>: cannot satisfy -package parallel"
        updateSessionD session upd 1
        assertSourceErrors session [[
            (Nothing, expected1)
          , (Nothing, expected2)
          ]]
    )
  , ( "Make sure package DB is passed to ghc after restartSession"
    , let packageOpts = ["-package parallel"]
          setup       = withModInfo True
                      . withDBStack [GlobalPackageDB]
                      . withOpts packageOpts
      in withSession setup $ \session -> do
        restartSession session Nothing
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "import Control.Parallel"
                    ])
        -- We expect an error because 'ide-backend-rts' and/or 'parallel'
        -- are not (usually?) installed in the global package DB.
        let expected1 = "cannot satisfy -package ide-backend-rts"
            expected2 = "<command line>: cannot satisfy -package parallel"
        updateSessionD session upd 1
        assertSourceErrors session [[
            (Nothing, expected1)
          , (Nothing, expected2)
          ]]
    )
  , ( "Consistency of IdMap/explicit sharing cache through multiple updates (#88)"
    , let packageOpts = [ "-hide-all-packages"
                        , "-package base"
                        ]
      in ifIdeBackendHaddockTestsEnabled (withOpts packageOpts) $ \sess -> do
        let cb = \_ -> return ()
            update = flip (updateSession sess) cb
            updMod = \mod code -> updateSourceFile mod (BSLC.pack code)

        update $ updateCodeGeneration True
        update $ updateStdoutBufferMode (RunLineBuffering Nothing)
        update $ updateStderrBufferMode (RunLineBuffering Nothing)

        update $ updMod "Foo.hs" $ unlines [
            "module Foo where"
          , ""
          , "import Bar"
          , ""
          , "foo = bar >> bar"
          , ""
          , "foobar = putStrLn \"Baz\""
          ]

        update $ updMod "Bar.hs" $ unlines [
            "module Bar where"
          , ""
          , "bar = putStrLn \"Hello, world!\""
          ]
        assertIdInfo sess "Bar" (3, 7, 3, 15) "putStrLn" VarName "String -> IO ()" "base-4.5.1.0:System.IO" "<no location info>" "base-4.5.1.0:System.IO" "imported from base-4.5.1.0:Prelude at Bar.hs@1:8-1:11"

        update $ updMod "Baz.hs" $ unlines [
            "module Baz where"
          , ""
          , "import Foo"
          , "import Bar"
          , ""
          , "baz = foobar"
          ]

        assertNoErrors sess
        assertIdInfo sess "Bar" (3, 7, 3, 15) "putStrLn" VarName "String -> IO ()" "base-4.5.1.0:System.IO" "<no location info>" "base-4.5.1.0:System.IO" "imported from base-4.5.1.0:Prelude at Bar.hs@1:8-1:11"
        assertIdInfo sess "Baz" (6, 7, 6, 13) "foobar" VarName "IO ()" "main:Foo" "Foo.hs@7:1-7:7" "" "imported from main:Foo at Baz.hs@3:1-3:11"

        update $ updMod "Baz.hs" $ unlines [
            "module Baz where"
          , ""
          , "import Foo"
          , "import Bar"
          , ""
          , "baz = foobar >>>> foo >> bar"
          ]

        assertOneError sess
        assertIdInfo sess "Bar" (3, 7, 3, 15) "putStrLn" VarName "String -> IO ()" "base-4.5.1.0:System.IO" "<no location info>" "base-4.5.1.0:System.IO" "imported from base-4.5.1.0:Prelude at Bar.hs@1:8-1:11"
        -- Baz is broken at this point

        update $ updMod "Baz.hs" $ unlines [
            "module Baz where"
          , ""
          , "import Foo"
          , "import Bar"
          , ""
          , "baz = foobar >> foo >> bar"
          ]

        assertNoErrors sess
        assertIdInfo sess "Bar" (3, 7, 3, 15) "putStrLn" VarName "String -> IO ()" "base-4.5.1.0:System.IO" "<no location info>" "base-4.5.1.0:System.IO" "imported from base-4.5.1.0:Prelude at Bar.hs@1:8-1:11"
        assertIdInfo sess "Baz" (6, 7, 6, 13) "foobar" VarName "IO ()" "main:Foo" "Foo.hs@7:1-7:7" "" "imported from main:Foo at Baz.hs@3:1-3:11"
    )
  , ( "Consistency of multiple modules of the same name"
{-
18:45 < mikolaj> from http://www.haskell.org/ghc/docs/7.4.2/html/users_guide/packages.html#package-overlaps
18:45 < mikolaj> It is possible that by using packages you might end up with a program that contains two modules with the same name: perhaps you used a package P that has a hidden module M, and there is also a module M in your program.
                 Or perhaps the dependencies of packages that you used contain some overlapping modules. Perhaps the program even contains multiple versions of a certain package, due to dependencies from other packages.
18:45 < mikolaj> None of these scenarios gives rise to an error on its own[8], but they may have some interesting consequences. For instance, if you have a type M.T from version 1 of package P, then this is not the same as the type M.T
                 from version 2 of package P, and GHC will report an error if you try to use one where the other is expected.
18:46 < mikolaj> so it seems it's unspecified which module will be used --- it just happens that our idInfo code picks a different package than GHC API in this case
-}
    , ifIdeBackendHaddockTestsEnabled defaultSession $ \sess -> do
        let cb = \_ -> return ()
            update = flip (updateSession sess) cb
            updMod = \mod code -> updateSourceFile mod (BSLC.pack code)

        update $ updMod "Control/Parallel.hs" $ unlines [
            "module Control.Parallel where"
          , ""
          , "import Bar"
          , ""
          , "foo = bar >> bar"
          , ""
          , "foobar = putStrLn \"Baz\""
          ]

        update $ updMod "Bar.hs" $ unlines [
            "module Bar where"
          , ""
          , "bar = putStrLn \"Hello, world!\""
          ]

        update $ updMod "Baz.hs" $ unlines [
            "module Baz where"
          , ""
          , "import Control.Parallel"
          , "import Bar"
          , ""
          , "baz = foobar"
          ]

        assertNoErrors sess
        assertIdInfo sess "Bar" (3, 7, 3, 15) "putStrLn" VarName "String -> IO ()" "base-4.5.1.0:System.IO" "<no location info>" "base-4.5.1.0:System.IO" "imported from base-4.5.1.0:Prelude at Bar.hs@1:8-1:11"
-- would fail:            assertIdInfo gif "Baz" (6, 8, 6, 9) "foobar" VarName "IO ()" "main:Control.Parallel" "Control/Parallel.hs@7:1-7:7" "" "imported from main:Control.Parallel at Baz.hs@3:1-3:24"

        update $ updMod "Baz.hs" $ unlines [
            "module Baz where"
          , ""
          , "import Control.Parallel"
          , "import Bar"
          , ""
          , "baz = foobar >>>> foo >> bar"
          ]
        assertOneError sess
        getSpanInfo sess
        assertIdInfo sess "Bar" (3, 7, 3, 15) "putStrLn" VarName "String -> IO ()" "base-4.5.1.0:System.IO" "<no location info>" "base-4.5.1.0:System.IO" "imported from base-4.5.1.0:Prelude at Bar.hs@1:8-1:11"
        -- Baz is broken at this point

        update $ updMod "Baz.hs" $ unlines [
            "module Baz where"
          , ""
          , "import Control.Parallel"
          , "import Bar"
          , ""
          , "baz = foobar >> foo >> bar"
          ]

        assertNoErrors sess
        assertIdInfo sess "Bar" (3, 7, 3, 15) "putStrLn" VarName "String -> IO ()" "base-4.5.1.0:System.IO" "<no location info>" "base-4.5.1.0:System.IO" "imported from base-4.5.1.0:Prelude at Bar.hs@1:8-1:11"
-- would fail:           assertIdInfo gif "Baz" (6, 8, 6, 9) "foobar" VarName "IO ()" "main:Control.Parallel" "Control/Parallel.hs@7:1-7:7" "" "imported from main:Control/Parallel at Baz.hs@3:1-3:24"
    )
  , ( "Consistency of multiple modules of the same name: PackageImports"
    , ifIdeBackendHaddockTestsEnabled defaultSession $ \sess -> do
        let cb = \_ -> return ()
            update = flip (updateSession sess) cb
            updMod = \mod code -> updateSourceFile mod (BSLC.pack code)

        update $ updMod "Control/Parallel.hs" $ unlines [
            "module Control.Parallel where"
          , ""
          , "import Bar"
          , ""
          , "foo = bar >> bar"
          , ""
          , "par = putStrLn \"Baz\""
          ]

        update $ updMod "Bar.hs" $ unlines [
            "module Bar where"
          , ""
          , "bar = putStrLn \"Hello, world!\""
          ]

        update $ updMod "Baz.hs" $ unlines [
            "{-# LANGUAGE PackageImports #-}"
          , "module Baz where"
          , ""
          , "import \"parallel\" Control.Parallel"
          , "import Bar"
          , ""
          , "baz = par"
          ]

        assertNoErrors sess
        assertIdInfo sess "Bar" (3, 7, 3, 15) "putStrLn" VarName "String -> IO ()" "base-4.5.1.0:System.IO" "<no location info>" "base-4.5.1.0:System.IO" "imported from base-4.5.1.0:Prelude at Bar.hs@1:8-1:11"
        assertIdInfo sess "Baz" (7, 7, 7, 10) "par" VarName "a1 -> b1 -> b1" "parallel-X.Y.Z:Control.Parallel" "<no location info>" "parallel-X.Y.Z:Control.Parallel" "imported from parallel-X.Y.Z:Control.Parallel at Baz.hs@4:1-4:35"
    )
  , ( "Quickfix for Updating static files never triggers recompilation"
    , withSession defaultSession $ \session -> do
        let upd = updateDataFile "A.foo" (BSLC.pack "unboundVarName")
               <> (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE TemplateHaskell #-}"
                    , "module A where"
                    , "import Language.Haskell.TH.Syntax"
                    , "goodVarName = 42"
                    , "foo :: Int"
                    , "foo = $(do"
                    , "          qAddDependentFile \"A.foo\""
                    , "          s <- qRunIO (readFile \"A.foo\")"
                    , "          return $ VarE (mkName s))"
                    ])
        updateSessionD session upd 1
        assertOneError session
        let upd2 = updateDataFile "A.foo" (BSLC.pack "goodVarName")
        updateSessionD session upd2 1
        assertNoErrors session
    )
  , ( "Quickfix for Updating static files never triggers --- illegal var name"
    , withSession defaultSession $ \session -> do
        let upd = updateDataFile "A.foo" (BSLC.pack "42 is a wrong var name")
               <> (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE TemplateHaskell #-}"
                    , "module A where"
                    , "import Language.Haskell.TH.Syntax"
                    , "goodVarName = 42"
                    , "foo :: Int"
                    , "foo = $(do"
                    , "          qAddDependentFile \"A.foo\""
                    , "          s <- qRunIO (readFile \"A.foo\")"
                    , "          return $ VarE (mkName s))"
                    ])
        updateSessionD session upd 1
        assertOneError session
        let upd2 = updateDataFile "A.foo" (BSLC.pack "goodVarName")
        updateSessionD session upd2 1
        assertNoErrors session
    )
  , ( "Quickfix for Updating static files never triggers --- missing file"
    , withSession defaultSession $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE TemplateHaskell #-}"
                    , "module A where"
                    , "import Language.Haskell.TH.Syntax"
                    , "foo :: String"
                    , "foo = $(do"
                    , "          qAddDependentFile \"A.foo\""
                    , "          x <- qRunIO (readFile \"A.foo\")"
                    , "          lift x)"
                    ])
        updateSessionD session upd 1
        assertOneError session
        let upd2 = updateDataFile "A.foo" (BSLC.pack "fooString")
        updateSessionD session upd2 1
        assertNoErrors session
    )
  , ( "Module name visible from 2 packages --- picked from monads-tf"
    , let packageOpts = [ "-hide-all-packages"
                        , "-package base"
                        , "-package monads-tf"
                        , "-package mtl"
                        ]
      in ifIdeBackendHaddockTestsEnabled (withOpts packageOpts) $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE PackageImports #-}"
                    , "module A where"
                    , "import \"monads-tf\" Control.Monad.Cont"
                    , "f = runCont"
                    ])
        updateSessionD session upd 1
        assertNoErrors session

        imports <- getImports session
        let base mod = ModuleId {
                moduleName    = Text.pack mod
              , modulePackage = PackageId {
                    packageName    = Text.pack "base"
                  , packageVersion = Just (Text.pack "X.Y.Z")
                  }
              }
            monads_tf mod = ModuleId {
                moduleName    = Text.pack mod
              , modulePackage = PackageId {
                    packageName    = Text.pack "monads-tf"
                  , packageVersion = Just (Text.pack "X.Y.Z")
                  }
              }
        assertSameSet "imports: " (ignoreVersions . fromJust . imports $ Text.pack "A") $ [
            Import {
                importModule    = base "Prelude"
              , importPackage   = Nothing
              , importQualified = False
              , importImplicit  = True
              , importAs        = Nothing
              , importEntities  = ImportAll
              }
          , Import {
                importModule    = monads_tf "Control.Monad.Cont"
              , importPackage   = Just (Text.pack "monads-tf")
              , importQualified = False
              , importImplicit  = False
              , importAs        = Nothing
              , importEntities  = ImportAll
              }
          ]

        -- TODO: We expect the scope "imported from monads-tf-X.Y.Z:Control.Monad.Cont at A.hs@3:1-3:38"
        -- but we cannot guarantee it (#95)
        assertIdInfo' session "A" (4,5,4,12) (4,5,4,12) "runCont" VarName (allVersions "Cont r1 a1 -> (a1 -> r1) -> r1") "transformers-X.Y.Z:Control.Monad.Trans.Cont" (allVersions "<no location info>") "monads-tf-X.Y.Z:Control.Monad.Cont" []
    )
  , ( "Module name visible from 2 packages --- picked from mtl (expected failure)"
    , let packageOpts = [ "-hide-all-packages"
                        , "-package base"
                        , "-package monads-tf"
                        , "-package mtl"
                        ]
      in ifIdeBackendHaddockTestsEnabled (withOpts packageOpts) $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE PackageImports #-}"
                    , "module A where"
                    , "import \"mtl\" Control.Monad.Cont"
                    , "f = runCont"
                    ])
        updateSessionD session upd 1
        assertNoErrors session

        imports <- getImports session
        let base mod = ModuleId {
                moduleName    = Text.pack mod
              , modulePackage = PackageId {
                    packageName    = Text.pack "base"
                  , packageVersion = Just (Text.pack "X.Y.Z")
                  }
              }
            mtl mod = ModuleId {
                moduleName    = Text.pack mod
              , modulePackage = PackageId {
                    packageName    = Text.pack "mtl"
                  , packageVersion = Just (Text.pack "X.Y.Z")
                  }
              }
        assertSameSet "imports: " (ignoreVersions . fromJust . imports $ Text.pack "A") $ [
            Import {
                importModule    = base "Prelude"
              , importPackage   = Nothing
              , importQualified = False
              , importImplicit  = True
              , importAs        = Nothing
              , importEntities  = ImportAll
              }
          , Import {
                importModule    = mtl "Control.Monad.Cont"
              , importPackage   = Just (Text.pack "mtl")
              , importQualified = False
              , importImplicit  = False
              , importAs        = Nothing
              , importEntities  = ImportAll
              }
          ]

        -- TODO: We expect the scope "imported from mtl-X.Y.Z:Control.Monad.Cont at A.hs@3:1-3:38"
        -- but we cannot guarantee it (#95)
        assertIdInfo' session "A" (4,5,4,12) (4,5,4,12) "runCont" VarName (allVersions "Cont r1 a1 -> (a1 -> r1) -> r1") "transformers-X.Y.Z:Control.Monad.Trans.Cont" (allVersions "<no location info>") "mtl-X.Y.Z:Control.Monad.Cont" []
    )
  , ("Issue #119"
    , withSession defaultSession $ \sess -> do
        distDir <- getDistDir sess

        let cb = \_ -> return ()
            update = flip (updateSession sess) cb

        update $ updateCodeGeneration True
        update $ updateStdoutBufferMode (RunLineBuffering Nothing)
        update $ updateStderrBufferMode (RunLineBuffering Nothing)

        -- Compile code and execute

        update $ updateSourceFile "src/Main.hs" . BSLC.pack $
            "main = putStrLn \"Version 1\""
        assertNoErrors sess

        update $ buildExe [] [(Text.pack "Main", "src/Main.hs")]
        out1 <- readProcess (distDir </> "build" </> "Main" </> "Main") [] []
        assertEqual "" "Version 1\n" out1
        runActionsExe <- runExe sess "Main"
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "Version 1\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe

        -- Update the code and execute again

        update $ updateSourceFile "src/Main.hs" . BSLC.pack $
            "main = putStrLn \"Version 2\""
        assertNoErrors sess

        update $ buildExe [] [(Text.pack "Main", "src/Main.hs")]
        out2 <- readProcess (distDir </> "build" </> "Main" </> "Main") [] []
        assertEqual "" "Version 2\n" out2
        runActionsExe2 <- runExe sess "Main"
        (outExe2, statusExe2) <- runWaitAll runActionsExe2
        assertEqual "Output from runExe"
                    "Version 2\n"
                    outExe2
        assertEqual "after runExe" ExitSuccess statusExe2
    )
  , ( "Subexpression types 1: Simple expressions"
    , withSession (withOpts ["-XNoMonomorphismRestriction"]) $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                      -- Single type var inst, boolean literal
                      --        123456789012
                    , {-  2 -} "t2 = id True"
                      -- Double type var inst, double evidence app, overloaded lit
                      --        1234567890123456789012345678901234567
                    , {-  3 -} "t3 = (fromIntegral :: Int -> Float) 1"
                      -- Lambda
                      --        12345 6789012
                    , {-  4 -} "t4 = \\x -> x"
                      -- Let
                      --        1234567890123456789012345678901
                    , {-  5 -} "t5 = let foo x = () in foo True"
                      -- Type variables
                      --        123456789012
                    , {-  6 -} "t6 x y z = x"
                    , {-  7 -} "t7 x y z = y"
                    , {-  8 -} "t8 x y z = z"
                      -- Brackets, tuples, operators
                      --       0         1         2         3         4
                      --        1234567890123456789012345678901234567890123456789
                    , {-  9 -} "t9 f g x y z = (x `f` (y `g` z), (x `f` y) `g` z)"
                      -- Literals
                      --        123456789012 3456 78
                    , {- 10 -} "t10 = ('a', \"foo\")"
                      -- Do expression
                      --       0         1         2         3         4
                      --        12345678901234567890123456789012345678901234567
                    , {- 11 -} "t11 = do line <- getLine ; return (length line)"
                      -- Lists
                      --        1234567890123456789
                    , {- 12 -} "t12 = [True, False]"
                    , {- 13 -} "t13 = [1, 2]"
                      -- Records
                    , "data MyRecord = MyRecordCon {"
                    , "    a :: Bool"
                    , "  , b :: Int"
                    , "  }"
                      --       0         1         2         3         4
                      --        1234567890123456789012345678901234567
                    , {- 18 -} "t18 = MyRecordCon { a = True, b = 5 }"
                      -- Record update
                    , {- 19 -} "t19 = t18 { b = 6 }"
                      -- Polymorphic records
                    , "data MyPolyRecord a = MyPolyRecordCon { c :: a }"
                      --       0         1         2         3
                      --        123456789012345678901234567890123
                    , {- 21 -} "t21 = MyPolyRecordCon { c = 'a' }"
                    , {- 22 -} "t22 = t21 { c = 'b' }"
                      -- Case statements, underscore
                      --       0         1         2         3         4
                      --        1234567890123456789012345678901234567890
                    , {- 23 -} "t23 = case t18 of MyRecordCon a b -> b"
                    , {- 24 -} "t24 = case t21 of MyPolyRecordCon c -> c"
                    , {- 25 -} "t25 x = case x of Left _  -> Nothing"
                    , {- 26 -} "                  Right y -> Just y"
                      -- If statement
                      --       0         1         2         3
                      --        123456789012345678901234567890
                    , {- 27 -} "t27 x y z = if x then y else z"
                      -- Sections, arithmetic sequences
                      --        1234567890123456
                    , {- 28 -} "t28 = ([1..] !!)"
                    , {- 29 -} "t29 = (!! 0)"
                      -- Negation
                      --        12345678901
                    , {- 30 -} "t30 x = - x"
                    ])
        updateSessionD session upd 1
        assertNoErrors session

        -- TODO: the order of these "dominators" seems rather random!
        expTypes <- getExpTypes session
        assertExpTypes expTypes "A" (2, 6, 2, 6) [
            (2,  6, 2,  8, "Bool -> Bool")
          , (2,  6, 2,  8, "a -> a")
          , (2,  6, 2, 13, "Bool")
          ]
        assertExpTypes expTypes "A" (2, 9, 2, 13) [
            (2, 6, 2, 13, "Bool")
          , (2, 9, 2, 13, "Bool")
          ]
        assertExpTypes expTypes "A" (3, 7, 3, 7) [
            (3,  6, 3, 38, "Float")
          , (3,  7, 3, 19, "Int -> Float")
          , (3,  7, 3, 19, "(Integral a, Num b) => a -> b")
          ]
        assertExpTypes expTypes "A" (3, 37, 3, 37) [
            (3,  6, 3, 38, "Float")
          , (3, 37, 3, 38, "Int")
          ]
        assertExpTypes expTypes "A" (4, 7, 4, 7) [
            (4,  6, 4, 13, "t -> t")
          , (4,  7, 4,  8, "t")
          ]
        assertExpTypes expTypes "A" (5, 25, 5, 25) [
            (5,  6, 5, 32, "()")
          , (5, 24, 5, 27, "Bool -> ()")
          , (5, 24, 5, 27, "t -> ()")
          , (5, 24, 5, 32, "()")
          ]
        assertExpTypes expTypes "A" (6, 12, 6, 13) [
            (6, 12, 6, 13, "t")
          ]
        assertExpTypes expTypes "A" (7, 12, 7, 13) [
            (7, 12, 7, 13, "t1")
          ]
        assertExpTypes expTypes "A" (8, 12, 8, 13) [
            (8, 12, 8, 13, "t2")
          ]
        assertExpTypes expTypes "A" (9, 30, 9, 31) [
            (9, 16, 9, 50, "(t, t)")
          , (9, 17, 9, 32, "t")
          , (9, 24, 9, 31, "t")
          , (9, 30, 9, 31, "t2")
          ]
        assertExpTypes expTypes "A" (9, 35, 9, 36) [
            (9, 16, 9, 50, "(t, t)")
          , (9, 34, 9, 49, "t")
          , (9, 35, 9, 36, "t1")
          , (9, 35, 9, 42, "t")
          ]
        assertExpTypes expTypes "A" (10, 8, 10, 11) [
            (10, 7, 10, 19, "(Char, [Char])")
          , (10, 8, 10, 11, "Char")
          ]
        assertExpTypes expTypes "A" (10, 13, 10, 18) [
            (10,  7, 10, 19, "(Char, [Char])")
          , (10, 13, 10, 18, "[Char]")
          ]
        assertExpTypes expTypes "A" (11, 10, 11, 14) [
            (11,  7, 11, 48, "IO Int")
          , (11, 10, 11, 14, "String")
          ]
        assertExpTypes expTypes "A" (11, 36, 11, 42) [
            (11,  7, 11, 48, "IO Int")
          , (11, 28, 11, 48, "IO Int")
          , (11, 36, 11, 42, "[Char] -> Int")
          , (11, 36, 11, 42, "[a] -> Int")
          , (11, 36, 11, 47, "Int")
          ]
        assertExpTypes expTypes "A" (12, 8, 12, 12) [
            (12, 7, 12, 20, "[Bool]")
          , (12, 8, 12, 12, "Bool")
          ]
        assertExpTypes expTypes "A" (13, 8, 13, 9) [
            (13, 7, 13, 13, "[t]")
          , (13, 8, 13,  9, "t")
          ]
        assertExpTypes expTypes "A" (18, 7, 18, 18) [
            (18, 7, 18, 18, "Bool -> Int -> MyRecord")
          , (18, 7, 18, 38, "MyRecord")
          ]
        assertExpTypes expTypes "A" (18, 21, 18, 22) [
            (18,  7, 18, 38, "MyRecord")
          , (18, 21, 18, 22, "Bool")
          ]
        assertExpTypes expTypes "A" (18, 35, 18, 36) [
            (18,  7, 18, 38, "MyRecord")
          , (18, 35, 18, 36, "Int")
          ]
        assertExpTypes expTypes "A" (19, 13, 19, 14) [
            (19,  7, 19, 20, "MyRecord")
          , (19, 13, 19, 14, "Int")
          ]
        assertExpTypes expTypes "A" (21, 29, 21, 32) [
            (21,  7, 21, 34, "MyPolyRecord Char")
          , (21, 29, 21, 32, "Char")
          ]
        assertExpTypes expTypes "A" (21, 7, 21, 22) [
            (21,  7, 21, 22, "Char -> MyPolyRecord Char")
          , (21,  7, 21, 22, "a -> MyPolyRecord a")
          , (21,  7, 21, 34, "MyPolyRecord Char")
          ]
        assertExpTypes expTypes "A" (22, 13, 22, 14) [
            (22,  7, 22, 22, "MyPolyRecord Char")
          , (22, 13, 22, 14, "Char")
          ]
        -- the "MyRecordCon" apparently is not really recorded as such in the
        -- AST, we don't get type information about it
        assertExpTypes expTypes "A" (23, 19, 23, 30) [
            (23, 7, 23, 39, "Int")
          ]
        assertExpTypes expTypes "A" (24, 35, 24, 36) [
            (24,  7, 24, 41, "Char")
          , (24, 35, 24, 36, "Char")
          ]
        assertExpTypes expTypes "A" (25, 24, 25, 25) [
            (25,  9, 26, 36, "Maybe a")
          , (25, 24, 25, 25, "t")
          ]
        assertExpTypes expTypes "A" (26, 25, 26, 26) [
            (25,  9, 26, 36, "Maybe a")
          , (26, 25, 26, 26, "a")
          ]
        assertExpTypes expTypes "A" (27, 23, 27, 24) [
            (27, 13, 27, 31, "t")
          , (27, 23, 27, 24, "t")
          ]
        assertExpTypes expTypes "A" (28, 8, 28, 13) [
            (28, 8, 28, 13, "a -> [a]")
          , (28, 8, 28, 13, "Enum a1 => a1 -> [a1]")
          , (28, 8, 28, 16, "Int -> a")
          ]
        -- [a1] -> Int -> [a1] is the polymorphic type of (!!),
        -- [a] -> Int -> [a] is the "monomorphic" instance (with type vars)
        assertExpTypes expTypes "A" (29, 8, 29, 10) [
            (29, 8, 29, 10, "[a] -> Int -> a")
          , (29, 8, 29, 10, "[a1] -> Int -> a1")
          , (29, 8, 29, 12, "[a] -> a")
          ]
        -- The 'negation' isn't a proper operator and therefore doesn't get its
        -- own type
        assertExpTypes expTypes "A" (30, 9, 30, 10) [
            (30, 9, 30, 12, "a")
          ]
    )
  , ( "Subexpression types 2: TH and QQ"
    , withSession (withOpts ["-XNoMonomorphismRestriction", "-XTemplateHaskell"]) $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ {-  1 -} "module A where"
                    , {-  2 -} "import Language.Haskell.TH.Syntax"
                      -- Quotations (expressions, types)
                      --        1234567890123456789
                    , {-  3 -} "qTrue = [|  True |]"
                    , {-  4 -} "qBool = [t| Bool |]"
                      --        1234567890123456789012345
                    , {-  5 -} "qComp x xs = [| x : xs |]"
                    ])
               <> (updateSourceFile "B.hs" . BSLC.pack . unlines $
                    [ {-  1 -} "module B where"
                    , {-  2 -} "import A"
                      -- Splices (expressions, types)
                      --        123456789012345678901
                    , {-  3 -} "t1 = $qTrue :: $qBool"
                      --        12345678901234567 890 12
                    , {-  4 -} "t2 = $(qComp 'a' \"bc\")"
                    ])
               <> (updateCodeGeneration True)

        updateSessionD session upd 2
        assertNoErrors session

        expTypes <- getExpTypes session
        -- The AST doesn't really give us a means to extract the type of
        -- a bracket expression :( And we get no info about the lifted vars
        assertExpTypes expTypes "A" (3, 9, 3, 20) []
        assertExpTypes expTypes "A" (4, 9, 4, 20) []
        assertExpTypes expTypes "A" (5, 17, 5, 18) []
        -- We don't return types for types, so check the expr splice only
        assertExpTypes expTypes "B" (3, 6, 3, 12) [
            (3, 6, 3, 12, "Bool")
          ]
        -- The typechecked tree contains the expanded splice, but the location
        -- of every term in the splice is set to the location of the entire
        -- splice
        assertExpTypes expTypes "B" (4, 8, 4, 13) [
            (4, 8, 4, 22, "[Char]")
          , (4, 8, 4, 22, "[Char]")
          , (4, 8, 4, 22, "Char -> [Char] -> [Char]")
          , (4, 8, 4, 22, "a -> [a] -> [a]")
          , (4, 8, 4, 22, "Char")
          ]
    )
  , ( "Subexpression types 3: Type families (fpco #2609)"
    , withSession (withOpts ["-XTypeFamilies"]) $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $ [
                       {- 1 -} "module A where"
                     , {- 2 -} "type family TestTypeFamily a"
                     -- Monomorphic instance
                     , {- 3 -} "type instance TestTypeFamily () = Bool"
                     , {- 4 -} "t1 :: TestTypeFamily ()"
                       --       123456789
                     , {- 5 -} "t1 = True"
                     -- Polymorphic instance
                     , {- 6 -} "type instance TestTypeFamily [a] = Maybe a"
                     , {- 7 -} "t2 :: TestTypeFamily [a]"
                       --       123456789012
                     , {- 8 -} "t2 = Nothing"
                     ])

        updateSessionD session upd 1
        assertNoErrors session

        expTypes <- getExpTypes session
        assertExpTypes expTypes "A" (5, 6, 5, 10) [
            (5, 6, 5, 10, "TestTypeFamily ()")
          , (5, 6, 5, 10, "Bool")
          ]
        assertExpTypes expTypes "A" (8, 6, 8, 13) [
            (8, 6, 8, 13, "TestTypeFamily [a]")
          , (8, 6, 8, 13, "Maybe a")
          , (8, 6, 8, 13, "Maybe a1")
          ]
    )
  , ( "Subexpression types 4: Higher rank types (fpco #2635)"
    , withSession (withOpts ["-XRank2Types"]) $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $ [
                       {- 1 -} "module A where"
                     , {- 2 -} "newtype T = MkT (forall a. Ord a => a -> a -> Bool)"
                       --       12345678901234
                     , {- 3 -} "teq = MkT (==)"
                     ])

        -- Note: intentionally using (==) in this test rather than (<=) so that
        -- the "definition type" is different from the "usage type"
        -- (forall a. Eq a => a -> a -> Bool) vs (forall a. Ord a => a -> a -> Bool)

        updateSessionD session upd 1
        assertNoErrors session

        expTypes <- getExpTypes session
        assertExpTypes expTypes "A" (3, 12, 3, 14) [
            (3,  7, 3, 15, "T")
          , (3, 11, 3, 15, "Ord a => a -> a -> Bool")
          , (3, 11, 3, 15, "a -> a -> Bool")
          , (3, 11, 3, 15, "Eq a => a -> a -> Bool")
          ]
    )
  , ( "Subexpression types 5: Sections of functions with 3 or more args"
    , withSession defaultSession $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $ [
                       {- 1 -} "module A where"
                     , {- 2 -} "f :: Int -> Bool -> Char -> ()"
                     , {- 3 -} "f = undefined"
                       --       1234567890123456789
                     , {- 4 -} "test1 = (`f` True)"
                     , {- 5 -} "test2 = (1 `f`)"
                     ])

        updateSessionD session upd 1
        assertNoErrors session

        expTypes <- getExpTypes session
        assertExpTypes expTypes "A" (4, 11, 4, 12) [
            (4, 10, 4, 13, "Int -> Bool -> Char -> ()")
          , (4, 10, 4, 18, "Int -> Char -> ()")
          ]
        assertExpTypes expTypes "A" (5, 13, 5, 14) [
            (5, 10, 5, 15, "Bool -> Char -> ()")
          , (5, 12, 5, 15, "Int -> Bool -> Char -> ()")
          ]
    )
  , ( "Issue #125: Hang when snippet calls createProcess with close_fds set to True"
    , withSession defaultSession $ \sess -> do
        let cb     = \_ -> return ()
            update = flip (updateSession sess) cb

        update $ updateCodeGeneration True
        update $ updateStdoutBufferMode (RunLineBuffering Nothing)
        update $ updateStderrBufferMode (RunLineBuffering Nothing)
        update $ updateDynamicOpts ["-Wall", "-Werror"]

        update $ updateSourceFile "src/Main.hs" $ BSLC.pack $ unlines [
            "module Main where"

          , "import System.Process"
          , "import System.IO"

          , "main :: IO ()"
          , "main = do"
          , "    (_,Just maybeOut,_,pr) <- createProcess $ (shell \"foo\")"
          , "        { cmdspec      = ShellCommand \"echo 123\""
          , "        , cwd          = Nothing"
          , "        , env          = Nothing"
          , "        , std_in       = CreatePipe"
          , "        , std_out      = CreatePipe"
          , "        , std_err      = CreatePipe"
          , "        , close_fds    = True"
          , "        , create_group = False"
          , "        }"
          , "    putStr =<< hGetContents maybeOut"
          , "    terminateProcess pr"
          ]

        assertNoErrors sess
        mRunActions <- timeout 2000000 $ runStmt sess "Main" "main"
        case mRunActions of
          Just runActions -> do
            mRunResult <- timeout 2000000 $ runWaitAll runActions
            case mRunResult of
              Just (output, RunOk) -> assertEqual "" (BSLC.pack "123\n") output
              Nothing -> assertFailure "Timeout in runWaitAll"
              _       -> assertFailure "Unexpected run result"
          Nothing ->
            assertFailure "Timeout in runStmt"
    )
  , ( "Use sites 1: Global values"
    , withSession defaultSession $ \session -> do
        let upd1 = (updateSourceFile "A.hs" . BSLC.pack . unlines $ [
                        {- 1 -} "module A where"
                              -- 123456789012345
                      , {- 2 -} "f :: Int -> Int"
                      , {- 3 -} "f = (+ 1)"
                      , {- 4 -} "g :: Int -> Int"
                      , {- 5 -} "g = f . f"
                      ])
                <> (updateSourceFile "B.hs" . BSLC.pack . unlines $ [
                        {- 1 -} "module B where"
                      , {- 2 -} "import A"
                      , {- 3 -} "h :: Int -> Int"
                      , {- 4 -} "h = (+ 1) . f . g"
                      ])

        updateSessionD session upd1 2
        assertNoErrors session

        let uses_f = [
                "A.hs@5:9-5:10"
              , "A.hs@5:5-5:6"
              , "B.hs@4:13-4:14"
              ]
            uses_add = [ -- "+"
                "A.hs@3:6-3:7"
              , "B.hs@4:6-4:7"
              ]
            uses_g = [
                "B.hs@4:17-4:18"
              ]
            uses_h = [
              ]

        do useSites <- getUseSites session
           assertUseSites useSites "A" (3,  1, 3,  2) "f" uses_f
           assertUseSites useSites "A" (3,  6, 3,  7) "+" uses_add
           assertUseSites useSites "A" (5,  1, 5,  2) "g" uses_g
           assertUseSites useSites "B" (4,  1, 4,  2) "h" uses_h
           assertUseSites useSites "B" (4,  6, 4,  7) "+" uses_add
           assertUseSites useSites "B" (4, 13, 4, 14) "f" uses_f
           assertUseSites useSites "B" (4, 17, 4, 18) "g" uses_g

        -- Update B, swap positions of g and f

        let upd2 = (updateSourceFile "B.hs" . BSLC.pack . unlines $ [
                        {- 1 -} "module B where"
                      , {- 2 -} "import A"
                      , {- 3 -} "h :: Int -> Int"
                      , {- 4 -} "h = (+ 1) . g . f"
                      ])

        updateSessionD session upd2 1
        assertNoErrors session

        let uses_f2 = [
                "A.hs@5:9-5:10"
              , "A.hs@5:5-5:6"
              , "B.hs@4:17-4:18"
              ]
            uses_g2 = [
                "B.hs@4:13-4:14"
              ]

        do useSites <- getUseSites session
           assertUseSites useSites "A" (3,  1, 3,  2) "f" uses_f2
           assertUseSites useSites "A" (3,  6, 3,  7) "+" uses_add
           assertUseSites useSites "A" (5,  1, 5,  2) "g" uses_g2
           assertUseSites useSites "B" (4,  1, 4,  2) "h" uses_h
           assertUseSites useSites "B" (4,  6, 4,  7) "+" uses_add
           assertUseSites useSites "B" (4, 13, 4, 14) "g" uses_g2
           assertUseSites useSites "B" (4, 17, 4, 18) "f" uses_f2

        -- Update A, remove one internal use of f, and shift the other

        let upd3 = (updateSourceFile "A.hs" . BSLC.pack . unlines $ [
                        {- 1 -} "module A where"
                              -- 123456789012345
                      , {- 2 -} "f :: Int -> Int"
                      , {- 3 -} "f = (+ 1)"
                      , {- 4 -} "g :: Int -> Int"
                      , {- 5 -} "g = (+ 1) . f"
                      ])

        updateSessionD session upd3 2
        assertNoErrors session

        let uses_f3 = [
                "A.hs@5:13-5:14"
              , "B.hs@4:17-4:18"
              ]
            uses_add3 = [ -- "+"
                "A.hs@5:6-5:7"
              , "A.hs@3:6-3:7"
              , "B.hs@4:6-4:7"
              ]

        do useSites <- getUseSites session
           assertUseSites useSites "A" (3,  1, 3,  2) "f" uses_f3
           assertUseSites useSites "A" (3,  6, 3,  7) "+" uses_add3
           assertUseSites useSites "A" (5,  1, 5,  2) "g" uses_g2
           assertUseSites useSites "B" (4,  1, 4,  2) "h" uses_h
           assertUseSites useSites "B" (4,  6, 4,  7) "+" uses_add3
           assertUseSites useSites "B" (4, 13, 4, 14) "g" uses_g2
           assertUseSites useSites "B" (4, 17, 4, 18) "f" uses_f3
    )
  , ( "Use sites 2: Types"
    , withSession defaultSession $ \session -> do
        -- This test follows the same structure as "Use sites 1", but
        -- tests types rather than values

        let upd1 = (updateSourceFile "A.hs" . BSLC.pack . unlines $ [
                        {- 1 -} "module A where"
                              -- 1234567890123456
                      , {- 2 -} "data F = MkF Int"
                      , {- 3 -} "data G = MkG F F"
                      ])
                <> (updateSourceFile "B.hs" . BSLC.pack . unlines $ [
                        {- 1 -} "module B where"
                      , {- 2 -} "import A"
                              -- 12345678901234567890
                      , {- 3 -} "data H = MkH Int F G"
                      ])

        updateSessionD session upd1 2
        assertNoErrors session

        let uses_F = [
                "A.hs@3:16-3:17"
              , "A.hs@3:14-3:15"
              , "B.hs@3:18-3:19"
              ]
            uses_Int = [
                "A.hs@2:14-2:17"
              , "B.hs@3:14-3:17"
              ]
            uses_G = [
                "B.hs@3:20-3:21"
              ]
            uses_H = [
              ]

        do useSites <- getUseSites session
           assertUseSites useSites "A" (2,  6, 2,  7) "F"   uses_F
           assertUseSites useSites "A" (2, 14, 2, 17) "Int" uses_Int
           assertUseSites useSites "A" (3,  6, 3,  7) "G"   uses_G
           assertUseSites useSites "B" (3,  6, 3,  7) "H"   uses_H
           assertUseSites useSites "B" (3, 14, 3, 17) "Int" uses_Int
           assertUseSites useSites "B" (3, 18, 3, 19) "F"   uses_F
           assertUseSites useSites "B" (3, 20, 3, 21) "G"   uses_G

        -- Update B, swap positions of g and f

        let upd2 = (updateSourceFile "B.hs" . BSLC.pack . unlines $ [
                        {- 1 -} "module B where"
                      , {- 2 -} "import A"
                              -- 12345678901234567890
                      , {- 3 -} "data H = MkH Int G F"
                      ])

        updateSessionD session upd2 1
        assertNoErrors session

        let uses_F2 = [
                "A.hs@3:16-3:17"
              , "A.hs@3:14-3:15"
              , "B.hs@3:20-3:21"
              ]
            uses_G2 = [
                "B.hs@3:18-3:19"
              ]

        do useSites <- getUseSites session
           assertUseSites useSites "A" (2,  6, 2,  7) "F"   uses_F2
           assertUseSites useSites "A" (2, 14, 2, 17) "Int" uses_Int
           assertUseSites useSites "A" (3,  6, 3,  7) "G"   uses_G2
           assertUseSites useSites "B" (3,  6, 3,  7) "H"   uses_H
           assertUseSites useSites "B" (3, 14, 3, 17) "Int" uses_Int
           assertUseSites useSites "B" (3, 18, 3, 19) "G"   uses_G2
           assertUseSites useSites "B" (3, 20, 3, 21) "F"   uses_F2

        -- Update A, remove one internal use of f, and shift the other

        let upd3 = (updateSourceFile "A.hs" . BSLC.pack . unlines $ [
                        {- 1 -} "module A where"
                              -- 123456789012345678
                      , {- 2 -} "data F = MkF Int"
                      , {- 3 -} "data G = MkG Int F"
                      ])

        updateSessionD session upd3 2
        assertNoErrors session

        let uses_F3 = [
                "A.hs@3:18-3:19"
              , "B.hs@3:20-3:21"
              ]
            uses_Int3 = [
                "A.hs@3:14-3:17"
              , "A.hs@2:14-2:17"
              , "B.hs@3:14-3:17"
              ]

        do useSites <- getUseSites session
           assertUseSites useSites "A" (2,  6, 2,  7) "F"   uses_F3
           assertUseSites useSites "A" (2, 14, 2, 17) "Int" uses_Int3
           assertUseSites useSites "A" (3,  6, 3,  7) "G"   uses_G2
           assertUseSites useSites "B" (3,  6, 3,  7) "H"   uses_H
           assertUseSites useSites "B" (3, 14, 3, 17) "Int" uses_Int3
           assertUseSites useSites "B" (3, 18, 3, 19) "G"   uses_G2
           assertUseSites useSites "B" (3, 20, 3, 21) "F"   uses_F3
    )
  , ( "Use sites 3: Local identifiers"
    , withSession (withOpts ["-XScopedTypeVariables"]) $ \session -> do
        let upd1 = (updateSourceFile "A.hs" . BSLC.pack . unlines $ [
                        {-  1 -} "module A where"
                              -- 123456789012345
                      , {-  2 -} "test = (+ 1) . f . g"
                      , {-  3 -} "  where"
                      , {-  4 -} "     f :: Int -> Int"
                      , {-  5 -} "     f = (+ 1)"
                      , {-  6 -} "     g :: Int -> Int"
                      , {-  7 -} "     g = f . f"
                        -- Function args, lambda bound, let bound, case bound
                               --          1         2         3
                               -- 1234567890 12345678901234567
                      , {-  8 -} "test2 f = \\x -> case f x of"
                               -- 123456789012345678901234567890123456789
                      , {-  9 -} "            (a, b) -> let c = a * b * b"
                      , {- 10 -} "                      in c * c"
                        -- Type arguments
                               -- 1234567890123456789012
                      , {- 11 -} "data T a b = MkT a a b"
                        -- Implicit forall
                               -- 1234567890123456
                      , {- 12 -} "f :: a -> b -> a"
                      , {- 13 -} "f x y = x"
                        -- Explicit forall
                               -- 1234567890123456789012345678
                      , {- 14 -} "g :: forall a b. a -> b -> a"
                      , {- 15 -} "g x y = x"
                      ])

        updateSessionD session upd1 2
        assertNoErrors session
        useSites <- getUseSites session

        -- where-bound
        do let uses_f = [
                   "A.hs@7:14-7:15"
                 , "A.hs@7:10-7:11"
                 , "A.hs@2:16-2:17"
                 ]
               uses_add = [ -- "+"
                   "A.hs@5:11-5:12"
                 , "A.hs@2:9-2:10"
                 ]
               uses_g = [
                   "A.hs@2:20-2:21"
                 ]

           assertUseSites useSites "A" (5,  6, 5,  7) "f" uses_f
           assertUseSites useSites "A" (5, 11, 5, 12) "+" uses_add
           assertUseSites useSites "A" (7,  6, 7,  7) "g" uses_g

        -- function args, lambda bound, let bound, case bound
        do let uses_f = ["A.hs@8:22-8:23"]
               uses_x = ["A.hs@8:24-8:25"]
               uses_a = ["A.hs@9:31-9:32"]
               uses_b = ["A.hs@9:39-9:40","A.hs@9:35-9:36"]
               uses_c = ["A.hs@10:30-10:31","A.hs@10:26-10:27"]

           assertUseSites useSites "A" (8,  7, 8,  8) "f" uses_f
           assertUseSites useSites "A" (8, 12, 8, 13) "x" uses_x
           assertUseSites useSites "A" (9, 14, 9, 15) "a" uses_a
           assertUseSites useSites "A" (9, 35, 9, 36) "b" uses_b -- using def site for variety
           assertUseSites useSites "A" (9, 27, 9, 28) "c" uses_c

        -- type args
        do let uses_a = ["A.hs@11:20-11:21","A.hs@11:18-11:19"]
               uses_b = ["A.hs@11:22-11:23"]

           assertUseSites useSites "A" (11, 18, 11, 19) "a" uses_a
           assertUseSites useSites "A" (11, 10, 11, 11) "b" uses_b

        -- implicit forall
        do let uses_a = ["A.hs@12:16-12:17","A.hs@12:6-12:7"]
               uses_b = ["A.hs@12:11-12:12"]

           assertUseSites useSites "A" (12,  6, 12,  7) "a" uses_a
           assertUseSites useSites "A" (12, 11, 12, 12) "b" uses_b

        -- explicit forall
        do let uses_a = ["A.hs@14:28-14:29","A.hs@14:18-14:19"]
               uses_b = ["A.hs@14:23-14:24"]

           assertUseSites useSites "A" (14, 13, 14, 14) "a" uses_a
           assertUseSites useSites "A" (14, 23, 14, 24) "b" uses_b
    )
{-
  , ( "Debugging 1: Setting and clearing breakpoints"
    , withSession defaultSession $ \session -> do
        updateSessionD session qsort 1
        assertNoErrors session

        expTypes <- getExpTypes session
        let (modName, mouseSpan) = mkSpan "Main" (2, 16, 2, 16)
            fullSpan = fst . last $ expTypes modName mouseSpan
            (_, wrongSpan) = mkSpan "Main" (0, 0, 0, 0)

        r1 <- setBreakpoint session modName fullSpan True
        r2 <- setBreakpoint session modName fullSpan False
        r3 <- setBreakpoint session modName fullSpan False
        r4 <- setBreakpoint session modName wrongSpan True
        assertEqual "original value of breakpoint"  (Just False) r1
        assertEqual "breakpoint successfully set"   (Just True)  r2
        assertEqual "breakpoint successfully unset" (Just False) r3
        assertEqual "invalid breakpoint"            Nothing      r4
    )
  , ( "Debugging 2: Running until breakpoint, then resuming"
    , withSession defaultSession $ \session -> do
        updateSessionD session qsort 1
        assertNoErrors session

        expTypes <- getExpTypes session
        let (modName, mouseSpan) = mkSpan "Main" (2, 16, 2, 16)
            fullSpan = fst . last $ expTypes modName mouseSpan

        Just False <- setBreakpoint session modName fullSpan True

        let inputList :: [Integer]
            inputList = [8, 4, 0, 3, 1, 23, 11, 18]

        outputs <- forM (mark inputList) $ \(i, isFirst, _isLast) -> do
           runActions <- if isFirst then runStmt session "Main" "main"
                                    else resume session
           (output, RunBreak) <- runWaitAll runActions
           assertBreak session "Main"
                               "Main.hs@2:16-2:48"
                               "[a]"
                               [ ("_result" , "[Integer]" , "_")
                               , ("a"       , "Integer"   , show i)
                               , ("left"    , "[Integer]" , "_")
                               , ("right"   , "[Integer]" , "_")
                               ]
           return output

        do runActions <- resume session
           (finalOutput, finalResult) <- runWaitAll runActions
           let output = BSL.concat $ outputs ++ [finalOutput]
           assertEqual "" RunOk finalResult
           assertEqual "" (show (sort inputList) ++ "\n") (BSLC.unpack output)
           mBreakInfo <- getBreakInfo session
           assertEqual "" Nothing mBreakInfo
    )
  , ( "Debugging 3: Printing and forcing"
    , withSession defaultSession $ \session -> do
        updateSessionD session qsort 1
        assertNoErrors session

        expTypes <- getExpTypes session
        let (modName, mouseSpan) = mkSpan "Main" (2, 16, 2, 16)
            fullSpan = fst . last $ expTypes modName mouseSpan

        Just False <- setBreakpoint session modName fullSpan True
        runActions <- runStmt session "Main" "main"
        (_output, RunBreak) <- runWaitAll runActions

        printed <- printVar session (Text.pack "left") True False
        forced  <- printVar session (Text.pack "left") True True
        assertEqual "" [(Text.pack "left", Text.pack "[Integer]", Text.pack "(_t1::[Integer])")] printed
        assertEqual "" [(Text.pack "left", Text.pack "[Integer]", Text.pack "[4, 0, 3, 1]")] forced
    )
-}
  , ( "Using C files 1: Basic functionality, recompiling Haskell modules when necessary"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import Foreign.C"
                    , "foreign import ccall \"foo\" c_f :: CInt"
                    , "hello :: IO ()"
                    , "hello = print c_f"
                    , "main :: IO ()"
                    , "main = hello"
                    ])
               <> (updateSourceFile "MC.c" . BSLC.pack . unlines $
                    [ "int foo() {"
                    , "  return 12345;"
                    , "}"
                    ])
        -- TODO: Ideally, we'd fix this jump in the reported total number of
        -- progress messages
        updateSessionP session upd [
            (1, 2, "Compiling MC.c")
          , (2, 2, "Loading MC.o")
          , (3, 3, "Compiling M")
          ]
        assertNoErrors session
        do runActions <- runStmt session "M" "hello"
           (output, result) <- runWaitAll runActions
           assertEqual "" RunOk result
           assertEqual "" (BSLC.pack "12345\n") output

        do let m = "M"
               updExe = buildExe [] [(Text.pack m, "M.hs")]
           updateSessionD session updExe 2
           runActionsExe <- runExe session m
           (outExe, statusExe) <- runWaitAll runActionsExe
           assertEqual "Output from runExe"
                       "12345\n"
                       outExe
           assertEqual "after runExe" ExitSuccess statusExe

        -- Update the Haskell module without updating the C module
        let upd2 = (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import Foreign.C"
                    , "foreign import ccall \"foo\" c_f :: CInt"
                    , "hello :: IO ()"
                    , "hello = print (c_f + 1)"
                    , "main :: IO ()"
                    , "main = hello"
                    ])
        updateSessionP session upd2 [
            (1, 1, "Compiling M")
          ]
        assertNoErrors session
        do runActions <- runStmt session "M" "hello"
           (output, result) <- runWaitAll runActions
           assertEqual "" RunOk result
           assertEqual "" (BSLC.pack "12346\n") output

        do let m = "M"
               updExe = buildExe [] [(Text.pack m, "M.hs")]
           updateSessionD session updExe 2
           runActionsExe <- runExe session m
           (outExe, statusExe) <- runWaitAll runActionsExe
           assertEqual "Output from runExe"
                       "12346\n"
                       outExe
           assertEqual "after runExe" ExitSuccess statusExe

        -- Update the C code without updating the Haskell module
        let upd3 = (updateSourceFile "MC.c" . BSLC.pack . unlines $
                    [ "int foo() {"
                    , "  return 54321;"
                    , "}"
                    ])
        updateSessionP session upd3 [
            (1 ,3, "Unloading MC.o")
          , (2, 3, "Compiling MC.c")
          , (3, 3, "Loading MC.o")
          , (4, 4, "Compiling M")
          ]
        assertNoErrors session
        do runActions <- runStmt session "M" "hello"
           (output, result) <- runWaitAll runActions
           assertEqual "" RunOk result
           assertEqual "" (BSLC.pack "54322\n") output
        let m = "M"
            updExe = buildExe [] [(Text.pack m, "M.hs")]
        updateSessionD session updExe 3
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "54322\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe
    )
  , ( "Using C files 2: C files in subdirs"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import Foreign.C"
                    , "foreign import ccall \"foo\" c_f :: CInt"
                    , "foreign import ccall \"bar\" c_g :: CInt"
                    , "hello :: IO ()"
                    , "hello = print (c_f + c_g)"
                    , "main :: IO ()"
                    , "main = hello"
                    ])
               <> (updateSourceFile "a/MC.c" . BSLC.pack . unlines $
                    [ "int foo() {"
                    , "  return 56;"
                    , "}"
                    ])
                    -- intentionally same name for the file
               <> (updateSourceFile "b/MC.c" . BSLC.pack . unlines $
                    [ "int bar() {"
                    , "  return 23;"
                    , "}"
                    ])
        updateSessionP session upd [
            (1, 4, "Compiling a/MC.c")
          , (2, 4, "Loading a/MC.o")
          , (3, 4, "Compiling b/MC.c")
          , (4, 4, "Loading b/MC.o")
          , (5, 5, "Compiling M")
          ]
        assertNoErrors session
        do runActions <- runStmt session "M" "hello"
           (output, result) <- runWaitAll runActions
           assertEqual "" RunOk result
           assertEqual "" (BSLC.pack "79\n") output
        let m = "M"
            updExe = buildExe [] [(Text.pack m, "M.hs")]
        updateSessionD session updExe 2
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "79\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe
    )
  , ( "Using C files 3: Errors and warnings in the C code"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import Foreign.C"
                    , "foreign import ccall \"foo\" c_f :: CInt"
                    , "hello :: IO ()"
                    , "hello = print c_f"
                    ])
               <> (updateSourceFile "MC.c" . BSLC.pack . unlines $ [
                      "int f() {"
                    , "  return thisSymbolDoesNotExist;"
                    , "}"
                    , ""
                    , "void g() {"
                    , "  return 1;"
                    , "}"
                    ])
        updateSessionP session upd [
            (1, 2, "Compiling MC.c")
          , (2, 2, "Skipped loading MC.o")
          , (3, 3, "Compiling M")
          ]
        errors <- getSourceErrors session
        case errors of
          [e1, e2] -> do
            -- Currently we generate precisely one gcc error
            assertEqual "" (TextSpan (Text.pack "<gcc error>")) (errorSpan e1)
            -- The ByteCodeLink exception because of the missing symbol
            assertEqual "" (TextSpan (Text.pack "<from GhcException>")) (errorSpan e2)
          _ -> assertFailure $ "Unexpected errors: " ++ show errors
    )
  , ( "Using C files 4: Errors in C file, then update C file (#201)"
    , withSession defaultSession $ \session -> do
        let mainLBS = BSLC.pack . unlines $ [
                "import Foreign"
              , "import Foreign.C"
              , "import Foreign.C.Types"
              , "foreign import ccall safe \"test_c_func\" test_c_func :: CInt"
              , "main = print test_c_func"
              ]
        let cLBS = BSLC.pack . unlines $ [
                "int test_c_func() { return 42; }"
              ]

        let upd = updateCodeGeneration True
               <> updateSourceFile "Main.hs" mainLBS
               <> updateSourceFile "test.c" cLBS

        updateSessionD session upd 3
        assertNoErrors session

        runActions <- runStmt session "Main" "main"
        (output, result) <- runWaitAll runActions
        assertEqual "" result RunOk
        assertEqual "" output (BSLC.pack "42\n")

        updateSessionD session (updateSourceFile "test.c" "invalid") 3
        assertOneError session

        updateSessionD session (updateSourceFile "test.c" cLBS) 3
        assertNoErrors session
    )
  , ( "ghc qAddDependentFile patch (#118)"
    , withSession defaultSession $ \session -> do
        let cb     = \_ -> return ()
            update = flip (updateSession session) cb

        let mainContents = BSLC.pack $ unlines
                [ "{-# LANGUAGE TemplateHaskell #-}"
                , "import Language.Haskell.TH.Syntax"
                , "main = print ($(do"
                , "  qAddDependentFile \"foo.hamlet\""
                , "  s <- qRunIO $ readFile \"foo.hamlet\""
                , "  lift $ (read s :: Int)"
                , "  ) :: Int)"
                ]

        update $ mconcat
            [ updateSourceFile "Main.hs" mainContents
            , updateDataFile "foo.hamlet" (BSLC.pack "invalid")
            ]

        -- Error message expected, invalid data file
        assertOneError session

        update $ updateDataFile "foo.hamlet" (BSLC.pack "42")
        assertNoErrors session

        update $ updateSourceFile "Main.hs" $ mainContents `BSLC.append` (BSLC.pack "\n\n-- Trigger a recompile")
        assertNoErrors session

        update $ updateDataFile "foo.hamlet" (BSLC.pack "invalid")
        assertOneError session
    )
  , ( "updateTargets  1: [{} < A, {A} < B, {A} < C], require [A]"
    , withSession defaultSession $ \session -> do
        updateSessionD session (mconcat [
            modAn "0"
          , modBn "0"
          , modCn "0"
          , updateTargets (TargetsInclude ["A.hs"])
          ]) 1
        assertNoErrors session
        assertLoadedModules session "" ["A"]

        buildExeTargetHsSucceeds session "A"
    )
  , ( "updateTargets  2: [{} < A, {A} < B, {A} < C], require [A], error in B"
    , withSession defaultSession $ \session -> do
        updateSessionD session (mconcat [
            modAn "0"
          , modBn "invalid"
          , modCn "0"
          , updateTargets (TargetsInclude ["A.hs"])
          ]) 1
        assertNoErrors session
        assertLoadedModules session "" ["A"]

        buildExeTargetHsSucceeds session "A"
    )
  , ( "updateTargets  3: [{} < A, {A} < B, {A} < C], require [B]"
    , withSession defaultSession $ \session -> do
        updateSessionD session (mconcat [
            modAn "0"
          , modBn "0"
          , modCn "0"
          , updateTargets (TargetsInclude ["B.hs"])
          ]) 2
        assertNoErrors session
        assertLoadedModules session "" ["A", "B"]

        buildExeTargetHsSucceeds session "B"
    )
  , ( "updateTargets  4: [{} < A, {A} < B, {A} < C], require [B], error in C"
    , withSession defaultSession $ \session -> do
        updateSessionD session (mconcat [
            modAn "0"
          , modBn "0"
          , modCn "invalid"
          , updateTargets (TargetsInclude ["B.hs"])
          ]) 2
        assertNoErrors session
        assertLoadedModules session "" ["A", "B"]

        buildExeTargetHsSucceeds session "B"
        buildExeTargetHsFails session "C"
    )
  , ( "updateTargets  5: [{} < A, {A} < B, {A} < C], require [A], error in A"
    , withSession defaultSession $ \session -> do
        updateSessionD session (mconcat [
            modAn "invalid"
          , modBn "0"
          , modCn "0"
          , updateTargets (TargetsInclude ["A.hs"])
          ]) 1
        assertOneError session
        assertLoadedModules session "" []

        buildExeTargetHsFails session "A"
    )
  , ( "updateTargets  6: [{} < A, {A} < B, {A} < C], require [B], error in A"
    , withSession defaultSession $ \session -> do
        updateSessionD session (mconcat [
            modAn "invalid"
          , modBn "0"
          , modCn "0"
          , updateTargets (TargetsInclude ["B.hs"])
          ]) 2
        assertOneError session
        assertLoadedModules session "" []

        buildExeTargetHsFails session "B"
    )
  , ( "updateTargets  7: [{} < A, {A} < B, {A} < C], require [B], error in B"
    , withSession defaultSession $ \session -> do
        updateSessionD session (mconcat [
            modAn "0"
          , modBn "invalid"
          , modCn "0"
          , updateTargets (TargetsInclude ["B.hs"])
          ]) 2
        assertOneError session
        assertLoadedModules session "" ["A"]

        buildExeTargetHsFails session "B"
        -- Only fails due to
        -- "Source errors encountered. Not attempting to build executables."
        -- buildExeTargetHsSucceeds session "A"
    )
  , ( "updateTargets  8: [{} < A, {A} < B, {A} < C], require [B, C]"
    , withSession defaultSession $ \session -> do
        updateSessionD session (mconcat [
            modAn "0"
          , modBn "0"
          , modCn "0"
          , updateTargets (TargetsInclude ["B.hs", "C.hs"])
          ]) 3
        assertNoErrors session
        assertLoadedModules session "" ["A", "B", "C"]
        autocomplete <- getAutocompletion session
        assertEqual "we have autocompletion info for C" 2 $
          length (autocomplete (Text.pack "C") "sp") -- span, split

        buildExeTargetHsSucceeds session "B"
        buildExeTargetHsSucceeds session "C"
    )
  , ( "updateTargets  9: [{} < A, {A} < B, {A} < C], require [B, C], then [B]"
    , withSession defaultSession $ \session -> do
        updateSessionD session (mconcat [
            modAn "0"
          , modBn "0"
          , modCn "0"
          , updateTargets (TargetsInclude ["B.hs", "C.hs"])
          ]) 3
        assertNoErrors session
        assertLoadedModules session "" ["A", "B", "C"]
        do autocomplete <- getAutocompletion session
           assertEqual "we have autocompletion info for C" 2 $
             length (autocomplete (Text.pack "C") "sp") -- span, split

        buildExeTargetHsSucceeds session "C"

        updateSessionD session (mconcat [
            updateTargets (TargetsInclude ["B.hs"])
          ]) 2
        assertLoadedModules session "" ["A", "B"]
        do autocomplete <- getAutocompletion session
           assertEqual "we no longer have autocompletion info for C" 0 $
             length (autocomplete (Text.pack "C") "sp") -- span, split

        buildExeTargetHsSucceeds session "B"
    )
  , ( "updateTargets 10: [{} < A, {A} < B, {A} < C], require [B, C], then [B] with modified B"
    , withSession defaultSession $ \session -> do
        updateSessionD session (mconcat [
            modAn "0"
          , modBn "0"
          , modCn "0"
          , updateTargets (TargetsInclude ["B.hs", "C.hs"])
          ]) 3
        assertNoErrors session
        assertLoadedModules session "" ["A", "B", "C"]
        do autocomplete <- getAutocompletion session
           assertEqual "we have autocompletion info for C" 2 $
             length (autocomplete (Text.pack "C") "sp") -- span, split

        buildExeTargetHsSucceeds session "C"

        updateSessionD session (mconcat [
            modBn "1"
          , updateTargets (TargetsInclude ["B.hs"])
          ]) 2
        assertLoadedModules session "" ["A", "B"]
        do autocomplete <- getAutocompletion session
           assertEqual "autocompletion info for C cleared" 0 $
             length (autocomplete (Text.pack "C") "sp")

        buildExeTargetHsSucceeds session "B"
    )
  , ( "updateTargets 11: [{} < A, {A} < B, {A} < C], require [B, C], then [B] with modified B and error in C"
    , withSession defaultSession $ \session -> do
        updateSessionD session (mconcat [
            modAn "0"
          , modBn "0"
          , modCn "0"
          , updateTargets (TargetsInclude ["B.hs", "C.hs"])
          ]) 3
        assertNoErrors session
        assertLoadedModules session "" ["A", "B", "C"]
        do autocomplete <- getAutocompletion session
           assertEqual "we have autocompletion info for C" 2 $
             length (autocomplete (Text.pack "C") "sp") -- span, split

        updateSessionD session (mconcat [
            modBn "1"
          , modCn "invalid"
          , updateTargets (TargetsInclude ["B.hs"])
          ]) 2
        assertLoadedModules session "" ["A", "B"]
        do autocomplete <- getAutocompletion session
           assertEqual "autocompletion info for C cleared" 0 $
             length (autocomplete (Text.pack "C") "sp")

        buildExeTargetHsFails session "C"
        buildExeTargetHsSucceeds session "B"
    )
  , ( "updateTargets 12: [{} < A, {A} < B, {A} < C], require [B, C], then [B] with error in C"
    , withSession defaultSession $ \session -> do
        updateSessionD session (mconcat [
            modAn "0"
          , modBn "0"
          , modCn "0"
          , updateTargets (TargetsInclude ["B.hs", "C.hs"])
          ]) 3
        assertNoErrors session
        assertLoadedModules session "" ["A", "B", "C"]
        do autocomplete <- getAutocompletion session
           assertEqual "we have autocompletion info for C" 2 $
             length (autocomplete (Text.pack "C") "sp") -- span, split

        updateSessionD session (mconcat [
            modCn "invalid"
          , updateTargets (TargetsInclude ["B.hs"])
          ]) 2
        assertLoadedModules session "" ["A", "B"]
        do autocomplete <- getAutocompletion session
           assertEqual "autocompletion info for C cleared" 0 $
             length (autocomplete (Text.pack "C") "sp")

        buildExeTargetHsFails session "C"
        buildExeTargetHsSucceeds session "B"
    )
  , ( "Paths in type errors (#32)"
    , withSession defaultSession $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "f x = show . read"
                    ])
        updateSessionD session upd 1
        errs <- getSourceErrors session
        let none p = all (not . p)
        let containsFullPath e =
              "session." `isInfixOf` Text.unpack (errorMsg e)
        _fixme session "#32" $ assertBool "" (none containsFullPath errs)
    )
  , ( "Updating dependent data files multiple times per second (#134)"
    , withSession defaultSession $ \session -> do
        let cb     = \_ -> return ()
            update = flip (updateSession session) cb
            str i  = BSLC.pack $ show (i :: Int) ++ "\n"

        let mainContents = BSLC.pack $ unlines
                [ "{-# LANGUAGE TemplateHaskell #-}"
                , "import Language.Haskell.TH.Syntax"
                , "main = print ($(do"
                , "  qAddDependentFile \"foo.hamlet\""
                , "  s <- qRunIO $ readFile \"foo.hamlet\""
                , "  lift $ (read s :: Int)"
                , "  ) :: Int)"
                ]

        update $ mconcat
            [ updateSourceFile "Main.hs" mainContents
            , updateDataFile "foo.hamlet" (str 1)
            , updateCodeGeneration True
            ]
        assertNoErrors session

        do runActions <- runStmt session "Main" "main"
           (output, result) <- runWaitAll runActions
           assertEqual "" RunOk result
           assertEqual "" (str 1) output

        forM_ [2 .. 99] $ \i -> do
          update $ updateDataFile "foo.hamlet" (str i)
          runActions <- runStmt session "Main" "main"
          (output, result) <- runWaitAll runActions
          assertEqual "" RunOk result
          assertEqual "" (str i) output
    )
  , ( "Support for hs-boot files (#155)"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "A.hs" $ BSLC.pack $ unlines [
                      "module A where"

                    , "import {-# SOURCE #-} B"

                    , "f :: Int -> Int"
                    , "f 0 = 1"
                    , "f 1 = 1"
                    , "f n = g (n - 1) + g (n - 2)"

                    , "main :: IO ()"
                    , "main = print $ map f [0..9]"
                    ])
               <> (updateSourceFile "B.hs" $ BSLC.pack $ unlines [
                      "module B where"

                    , "import A"

                    , "g :: Int -> Int"
                    , "g = f"
                    ])
               <> (updateSourceFile "B.hs-boot" $ BSLC.pack $ unlines [
                      "module B where"

                    , "g :: Int -> Int"
                    ])
        updateSessionD session upd 3
        assertNoErrors session

        let m = "A"
            updE = buildExe ["-rtsopts", "-O1"] [(Text.pack m, m <.> "hs")]
        updateSessionD session updE 4
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "buildStderr empty" "" buildStderr
        status <- getBuildExeStatus session
        assertEqual "after exe build" (Just ExitSuccess) status
        out <- readProcess (distDir </> "build" </> m </> m)
                           ["+RTS", "-C0.005", "-RTS"] []
        assertEqual "" "[1,1,2,3,5,8,13,21,34,55]\n" out
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "[1,1,2,3,5,8,13,21,34,55]\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe
    )
  , ( "Support for lhs-boot files (#155)"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "A.lhs" $ BSLC.pack $ unlines [
                      "> module A where"

                    , "> import {-# SOURCE #-} B"

                    , "> f :: Int -> Int"
                    , "> f 0 = 1"
                    , "> f 1 = 1"
                    , "> f n = g (n - 1) + g (n - 2)"

                    , "> main :: IO ()"
                    , "> main = print $ map f [0..9]"
                    ])
               <> (updateSourceFile "B.lhs" $ BSLC.pack $ unlines [
                      "> module B where"

                    , "> import A"

                    , "> g :: Int -> Int"
                    , "> g = f"
                    ])
               <> (updateSourceFile "B.lhs-boot" $ BSLC.pack $ unlines [
                      "> module B where"

                    , "> g :: Int -> Int"
                    ])
        updateSessionD session upd 3
        assertNoErrors session

        let m = "A"
            updE = buildExe [] [(Text.pack m, m <.> "lhs")]
        updateSessionD session updE 4
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "buildStderr empty" "" buildStderr
        status <- getBuildExeStatus session
        assertEqual "after exe build" (Just ExitSuccess) status
        out <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "" "[1,1,2,3,5,8,13,21,34,55]\n" out
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "[1,1,2,3,5,8,13,21,34,55]\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe
    )
  , ( "Support for hs-boot files from a subdirectory (#177)"
    , withSession (withIncludes "src") $ \session -> do
        let ahs = BSLC.pack $ "module A where\nimport B( TB(..) )\nnewtype TA = MkTA Int\nf :: TB -> TA\nf (MkTB x) = MkTA x"
            ahsboot = BSLC.pack $ "module A where\nnewtype TA = MkTA Int"
            bhs = BSLC.pack $ "module B where\nimport {-# SOURCE #-} A( TA(..) )\ndata TB = MkTB !Int\ng :: TA -> TB\ng (MkTA x) = MkTB x\nmain = print 42"
        let update = updateSourceFile "src/A.hs" ahs
                  <> updateSourceFile "src/A.hs-boot" ahsboot
                  <> updateSourceFile "src/B.hs" bhs
                  <> updateCodeGeneration True
        updateSessionD session update 3
        assertNoErrors session

        let m = "B"
            updE = buildExe [] [(Text.pack m, m <.> "hs")]
        updateSessionD session updE 4
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "buildStderr empty" "" buildStderr
        status <- getBuildExeStatus session
        assertEqual "after exe build" (Just ExitSuccess) status
        out <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "" "42\n" out
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "42\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe
    )
  , ( "Support for hs-boot files from a subdirectory (#177) with dynamic include path change"
    , withSession defaultSession $ \session -> do
        let ahs = BSLC.pack $ "module A where\nimport B( TB(..) )\nnewtype TA = MkTA Int\nf :: TB -> TA\nf (MkTB x) = MkTA x"
            ahsboot = BSLC.pack $ "module A where\nnewtype TA = MkTA Int"
            bhs = BSLC.pack $ "module B where\nimport {-# SOURCE #-} A( TA(..) )\ndata TB = MkTB !Int\ng :: TA -> TB\ng (MkTA x) = MkTB x\nmain = print 42"
        let update = updateSourceFile "src/A.hs" ahs
                  <> updateSourceFile "src/A.hs-boot" ahsboot
                  <> updateSourceFile "src/B.hs" bhs
                  <> updateCodeGeneration True
        updateSessionD session update 3
        assertOneError session

        updateSessionD session
                       (updateRelativeIncludes ["src"])
                       3
        assertNoErrors session

        let m = "B"
            updE = buildExe [] [(Text.pack m, m <.> "hs")]
        updateSessionD session updE 4
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "buildStderr empty" "" buildStderr
        status <- getBuildExeStatus session
        assertEqual "after exe build" (Just ExitSuccess) status
        out <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "" "42\n" out
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "42\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe
    )
  , ( "Relative include paths (#156)"
    , withSession (withIncludes "test/ABnoError") $ \session -> do
        -- Since we set the target explicitly, ghc will need to be able to find
        -- the other module (B) on its own; that means it will need an include
        -- path to <ideSourcesDir>/test/ABnoError
        loadModulesFrom' session "test/ABnoError" (TargetsInclude ["test/ABnoError/A.hs"])
        assertNoErrors session

        let updE = buildExe [] [(Text.pack "Main", "test/ABnoError/A.hs")]
        updateSessionD session updE 3
        status <- getBuildExeStatus session
        assertEqual "after exe build" (Just ExitSuccess) status

        let updE2 = buildExe [] [(Text.pack "Main", "A.hs")]
        updateSessionD session updE2 0
        status2 <- getBuildExeStatus session
        assertEqual "after exe build" (Just ExitSuccess) status2
    )
  , ( "Relative include paths (#156) with dynamic include path change"
    , withSession defaultSession $ \session -> do
        -- Since we set the target explicitly, ghc will need to be able to find
        -- the other module (B) on its own; that means it will need an include
        -- path to <ideSourcesDir>/test/ABnoError
        loadModulesFrom' session "test/ABnoError" (TargetsInclude ["test/ABnoError/A.hs"])
        assertOneError session

        updateSessionD session
                       (updateRelativeIncludes ["test/ABnoError"])
                       2  -- note the recompilation
        assertNoErrors session

        let updE = buildExe [] [(Text.pack "Main", "test/ABnoError/A.hs")]
        updateSessionD session updE 1
        status <- getBuildExeStatus session
        -- Path "" no longer in include paths here!
        assertEqual "after exe build" (Just $ ExitFailure 1) status

        let updE2 = buildExe [] [(Text.pack "Main", "A.hs")]
        updateSessionD session updE2 2
        status2 <- getBuildExeStatus session
        assertEqual "after exe build" (Just ExitSuccess) status2
    )
  , ( "Switch from one to another relative include path for the same module name with TargetsInclude"
    , withSession defaultSession $ \session -> do
        loadModulesFrom' session "test/AnotherA" (TargetsInclude ["test/AnotherA/A.hs"])
        assertOneError session
        updateSessionD session (updateCodeGeneration True) 0
        updateSessionD session
                       (updateSourceFileFromFile "test/ABnoError/B.hs")
                       0
        assertOneError session
        updateSessionD session
                       (updateSourceFileFromFile "test/AnotherB/B.hs")
                       0
        assertOneError session

        updateSessionD session
                       (updateRelativeIncludes ["", "test/AnotherA", "test/ABnoError"])
                       2  -- note the recompilation
        assertNoErrors session

        runActions <- runStmt session "Main" "main"
        (output, _) <- runWaitAll runActions
        assertEqual "output" (BSLC.pack "\"running 'A depends on B, no errors' from test/ABnoError\"\n") output

        distDir <- getDistDir session
        let m = "Main"
            updE = buildExe [] [(Text.pack m, "test/AnotherA/A.hs")]
        updateSessionD session updE 3
        status <- getBuildExeStatus session
        assertEqual "after exe build" (Just ExitSuccess) status
        (stExc, out, _) <-
           readProcessWithExitCode (distDir </> "build" </> m </> m) [] []
        assertEqual "A throws exception" (ExitFailure 1) stExc
        assertEqual "exe output with old include path"
                    "\"running 'A depends on B, no errors' from test/ABnoError\"\n"
                    out
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "\"running 'A depends on B, no errors' from test/ABnoError\"\nMain: A.hs throws exception\n"
                    outExe
        assertEqual "after runExe" (ExitFailure 1) statusExe

        let updE2 = buildExe [] [(Text.pack m, "A.hs")]
        updateSessionD session updE2 0
        status2 <- getBuildExeStatus session
        assertEqual "after exe build2" (Just ExitSuccess) status2
        (stExc2, out2, _) <-
          readProcessWithExitCode (distDir </> "build" </> m </> m) [] []
        assertEqual "A throws exception" (ExitFailure 1) stExc2
        assertEqual "exe output with old include path"
                    "\"running 'A depends on B, no errors' from test/ABnoError\"\n"
                    out2
        runActionsExe2 <- runExe session m
        (outExe2, statusExe2) <- runWaitAll runActionsExe2
        assertEqual "Output from runExe 2"
                    "\"running 'A depends on B, no errors' from test/ABnoError\"\nMain: A.hs throws exception\n"
                    outExe2
        assertEqual "after runExe" (ExitFailure 1) statusExe2

        updateSessionD session
                       (updateRelativeIncludes ["test/AnotherA", "test/AnotherB"])
                       2
        assertNoErrors session

        runActions3 <- runStmt session "Main" "main"
        (output3, _) <- runWaitAll runActions3
        assertEqual "output3" (BSLC.pack "\"running A with another B\"\n") output3

        updateSessionD session
                       (updateSourceFileDelete "test/ABnoError/B.hs")
                       0  -- already recompiled above
        assertNoErrors session

        runActions35 <- runStmt session "Main" "main"
        (output35, _) <- runWaitAll runActions35
        assertEqual "output35" (BSLC.pack "\"running A with another B\"\n") output35

        -- And this one works OK even without updateSourceFileDelete
        -- and even without session restart.
        let updE3 = buildExe [] [(Text.pack m, "test/AnotherA/A.hs")]
        updateSessionD session updE3 1
        status3 <- getBuildExeStatus session
        -- Path "" no longer in include paths here!
        assertEqual "after exe build3" (Just $ ExitFailure 1) status3

        let updE4 = buildExe [] [(Text.pack m, "A.hs")]
        updateSessionD session updE4 2
        status4 <- getBuildExeStatus session
        assertEqual "after exe build4" (Just ExitSuccess) status4
        (stExc4, out4, _) <-
          readProcessWithExitCode (distDir </> "build" </> m </> m) [] []
        assertEqual "A throws exception" (ExitFailure 1) stExc4
        assertEqual "exe output with new include path"
                    "\"running A with another B\"\n"
                    out4
        runActionsExe4 <- runExe session m
        (outExe4, statusExe4) <- runWaitAll runActionsExe4
        assertEqual "Output from runExe 4"
                    "\"running A with another B\"\nMain: A.hs throws exception\n"
                    outExe4
        assertEqual "after runExe" (ExitFailure 1) statusExe4
    )
  , ( "Switch from one to another relative include path for the same module name with TargetsExclude"
    , withSession defaultSession $ \session -> do
        loadModulesFrom' session "test/AnotherA" (TargetsExclude [])
        assertOneError session

        updateSessionD session (updateCodeGeneration True) 0
        updateSessionD session
                       (updateSourceFileFromFile "test/ABnoError/B.hs")
                       2
        assertNoErrors session
        updateSessionD session
                       (updateRelativeIncludes ["", "test/AnotherA", "test/ABnoError"])
                       2  -- with TargetsExclude [], this is superfluous
        assertNoErrors session

        runActions <- runStmt session "Main" "main"
        (output, _) <- runWaitAll runActions
        assertEqual "output" (BSLC.pack "\"running 'A depends on B, no errors' from test/ABnoError\"\n") output

        distDir <- getDistDir session
        let m = "Main"
            updE = buildExe [] [(Text.pack m, "test/AnotherA/A.hs")]
        updateSessionD session updE 3
        status <- getBuildExeStatus session
        assertEqual "after exe build" (Just ExitSuccess) status
        (stExc, out, _) <-
           readProcessWithExitCode (distDir </> "build" </> m </> m) [] []
        assertEqual "A throws exception" (ExitFailure 1) stExc
        assertEqual "exe output with old include path"
                    "\"running 'A depends on B, no errors' from test/ABnoError\"\n"
                    out
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "\"running 'A depends on B, no errors' from test/ABnoError\"\nMain: A.hs throws exception\n"
                    outExe
        assertEqual "after runExe" (ExitFailure 1) statusExe


        let updE2 = buildExe [] [(Text.pack m, "A.hs")]
        updateSessionD session updE2 0
        status2 <- getBuildExeStatus session
        assertEqual "after exe build2" (Just ExitSuccess) status2
        (stExc2, out2, _) <-
          readProcessWithExitCode (distDir </> "build" </> m </> m) [] []
        assertEqual "A throws exception" (ExitFailure 1) stExc2
        assertEqual "exe output with old include path"
                    "\"running 'A depends on B, no errors' from test/ABnoError\"\n"
                    out2
        runActionsExe2 <- runExe session m
        (outExe2, statusExe2) <- runWaitAll runActionsExe2
        assertEqual "Output from runExe 2"
                    "\"running 'A depends on B, no errors' from test/ABnoError\"\nMain: A.hs throws exception\n"
                    outExe2
        assertEqual "after runExe" (ExitFailure 1) statusExe2

        updateSessionD session
                       (updateSourceFileDelete "test/ABnoError/B.hs")
                       0
        assertOneError session
        updateSessionD session
                       (updateSourceFileFromFile "test/AnotherB/B.hs")
                       2
        assertNoErrors session

        updateSessionD session
                       (updateTargets (TargetsInclude ["test/AnotherA/A.hs"]))
                       0
        assertOneError session

        updateSessionD session
                       (updateRelativeIncludes ["test/AnotherA", "test/AnotherB"])
                       2  -- with TargetsExclude, this would be superfluous
        assertNoErrors session  -- fixed the error from above
        updateSessionD session
                       (updateTargets  (TargetsExclude []))
                       2  -- recompilation due to session restart only
        assertNoErrors session

        runActions3 <- runStmt session "Main" "main"
        (output3, _) <- runWaitAll runActions3
        assertEqual "output3" (BSLC.pack "\"running A with another B\"\n") output3

        let updE3 = buildExe [] [(Text.pack m, "test/AnotherA/A.hs")]
        updateSessionD session updE3 1
        status3 <- getBuildExeStatus session
        -- Path "" no longer in include paths here!
        assertEqual "after exe build3" (Just $ ExitFailure 1) status3

        let updE4 = buildExe [] [(Text.pack m, "A.hs")]
        updateSessionD session updE4 2
        status4 <- getBuildExeStatus session
        assertEqual "after exe build4" (Just ExitSuccess) status4
        (stExc4, out4, _) <-
          readProcessWithExitCode (distDir </> "build" </> m </> m) [] []
        assertEqual "A throws exception" (ExitFailure 1) stExc4
        assertEqual "exe output with new include path"
                    "\"running A with another B\"\n"
                    out4
        runActionsExe4 <- runExe session m
        (outExe4, statusExe4) <- runWaitAll runActionsExe4
        assertEqual "Output from runExe 4"
                    "\"running A with another B\"\nMain: A.hs throws exception\n"
                    outExe4
        assertEqual "after runExe" (ExitFailure 1) statusExe4
    )
  , ( "Switch from one to another relative include path with TargetsInclude and the main module not in path"
    , withSession defaultSession $ \session -> do
        -- Since we set the target explicitly, ghc will need to be able to find
        -- the other module (B) on its own; that means it will need an include
        -- path to <ideSourcesDir>/test/ABnoError
        loadModulesFrom' session "test/ABnoError" (TargetsInclude ["test/ABnoError/A.hs"])
        assertOneError session
        updateSessionD session
                       (updateSourceFileFromFile "test/AnotherB/B.hs")
                       0
        assertOneError session

        updateSessionD session (updateCodeGeneration True) 0
        updateSessionD session
                       (updateRelativeIncludes ["test/ABnoError"])
                       2  -- note the recompilation
        assertNoErrors session

        runActions <- runStmt session "Main" "main"
        (output, _) <- runWaitAll runActions
        assertEqual "output" (BSLC.pack "\"running 'A depends on B, no errors' from test/ABnoError\"\n") output

        updateSessionD session
                       (updateRelativeIncludes ["test/AnotherB"])  -- A not in path
                       2
        assertNoErrors session

        runActions3 <- runStmt session "Main" "main"
        (output3, _) <- runWaitAll runActions3
        assertEqual "output3" (BSLC.pack "\"running A with another B\"\n") output3

        updateSessionD session
                       (updateSourceFileDelete "test/ABnoError/B.hs")
                       0  -- already recompiled above
        assertNoErrors session

        runActions35 <- runStmt session "Main" "main"
        (output35, _) <- runWaitAll runActions35
        assertEqual "output35" (BSLC.pack "\"running A with another B\"\n") output35

        distDir <- getDistDir session
        let m = "Main"

        let updE4 = buildExe [] [(Text.pack m, "A.hs")]
        updateSessionD session updE4 1
        status4 <- getBuildExeStatus session
        assertEqual "after exe build4" (Just $ ExitFailure 1) status4
          -- Failure due to no A in path.

        updateSessionD session
                       (updateRelativeIncludes ["", "test/AnotherB"])  -- A still not in path
                       2
        assertNoErrors session

        -- buildExe with full paths works though, if the includes have ""
        let updE41 = buildExe [] [(Text.pack m, "test/ABnoError/A.hs")]
        updateSessionD session updE41 2
        status41 <- getBuildExeStatus session
        assertEqual "after exe build41" (Just ExitSuccess) status41
        (stExc41, out41, _) <-
          readProcessWithExitCode (distDir </> "build" </> m </> m) [] []
        assertEqual "A throws exception" (ExitFailure 1) stExc41
        assertEqual "exe output with new include path"
                    "\"running A with another B\"\n"
                    out41
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe 41"
                    "\"running A with another B\"\nMain: A.hs throws exception\n"
                    outExe
        assertEqual "after runExe" (ExitFailure 1) statusExe

        updateSessionD session
                       (updateRelativeIncludes ["test/AnotherB", "test/ABnoError"])  -- A again in path
                       2
        assertNoErrors session

        runActions4 <- runStmt session "Main" "main"
        (output4, _) <- runWaitAll runActions4
        assertEqual "output4" (BSLC.pack "\"running A with another B\"\n") output4

        -- A again in path, so this time this works
        updateSessionD session updE4 2
        status45 <- getBuildExeStatus session
        assertEqual "after exe build45" (Just ExitSuccess) status45
        (stExc45, out45, _) <-
          readProcessWithExitCode (distDir </> "build" </> m </> m) [] []
        assertEqual "A throws exception" (ExitFailure 1) stExc45
        assertEqual "exe output with new include path"
                    "\"running A with another B\"\n"
                    out45
        runActionsExe4 <- runExe session m
        (outExe4, statusExe4) <- runWaitAll runActionsExe4
        assertEqual "Output from runExe 45"
                    "\"running A with another B\"\nMain: A.hs throws exception\n"
                    outExe4
        assertEqual "after runExe" (ExitFailure 1) statusExe4

        updateSessionD session
                       (updateRelativeIncludes ["test/ABnoError"])
                       0
        assertOneError session  -- correct
    )
  , ( "Dynamically setting/unsetting -Werror (#115)"
    , withSession defaultSession $ \session -> do
        let upd1 = (updateDynamicOpts ["-Wall", "-Werror"])
                <> (updateSourceFile "src/Main.hs" . BSLC.pack $ unlines [
                       "module Main where"
                     , "main = putStrLn \"Hello 1\""
                     ])

        updateSessionD session upd1 1
        errs1 <- getSourceErrors session
        unless (any (== KindError) $ map errorKind errs1) $
          assertFailure $ "Expected some errors in " ++ show3errors errs1

        let upd2 = (updateDynamicOpts ["-Wall"])
                <> (updateSourceFile "src/Main.hs" . BSLC.pack $ unlines [
                       "module Main where"
                     , "main = putStrLn \"Hello 2\""
                     ])

        updateSessionD session upd2 1
        errs2 <- getSourceErrors session
        when (any (== KindError) $ map errorKind errs2) $
          assertFailure $ "Expected only warnings in " ++ show3errors errs2
    )
  , ( "Invalid GHC option and option warnings (#185-1) "
    , withSession defaultSession $ \session -> do
        let upd = updateDynamicOpts ["-fglasgow-exts","-thisOptionDoesNotExist"]
        updateSessionD session upd 0
        errs <- getSourceErrors session
        -- We expect two warnings (one deprecated, one unrecognized)
        assertEqual "" 2 (length errs)
    )
  , ( "Hiding and unhiding a package (#185-2)"
    , withSession defaultSession $ \session -> do

        -- We want to test that:
        --
        -- 1. The package flags work at all
        -- 2. Transitions
        -- 3. Statelessness of updateDynamicOpts

        let runCode = do
              runActions <- runStmt session "A" "test"
              (output, result) <- runWaitAll runActions
              assertEqual "" RunOk result
              assertEqual "" (BSLC.pack "9\n") output

        -- First, check that we can import stuff from the unix package
        do let upd = (updateSourceFile "A.hs" . BSLC.pack $ unlines [
                         "module A (test) where"
                       , "import System.Posix"
                       , "test :: IO ()"
                       , "test = print sigKILL"
                       ])
                  <> (updateCodeGeneration True)
           updateSessionD session upd 1
           assertNoErrors session
           runCode

        -- Hide the package
        do let upd = updateDynamicOpts ["-hide-package unix"]
           updateSessionD session upd 1
           assertOneError session

        -- Reveal the package again with an explicit flag
        do let upd = updateDynamicOpts ["-package unix"]
           updateSessionD session upd 1
           assertNoErrors session
           runCode

        -- Hide once more
        do let upd = updateDynamicOpts ["-hide-package unix"]
           updateSessionD session upd 1
           assertOneError session

        -- Reveal it again by using the default package flags
        -- (statelessness of updateDynamicOpts)
        do let upd = updateDynamicOpts []
           updateSessionD session upd 1
           assertNoErrors session
           runCode
    )
  , ( "Unhiding and hiding a package (#185-3; converse of #185-2)"
    , withSession defaultSession $ \session -> do
        ghcVersion <- getGhcVersion session
        let runCode = do
              runActions <- runStmt session "A" "test"
              (output, result) <- runWaitAll runActions
              assertEqual "" RunOk result
              case ghcVersion of
                GHC742 -> assertEqual "" (BSLC.pack "7.4\n") output
                GHC78  -> assertEqual "" (BSLC.pack "7.8\n") output

        -- First, check that we cannot import from the ghc package
        do let upd = (updateSourceFile "A.hs" . BSLC.pack $ unlines [
                         "module A (test) where"
                       , "import Config"
                       , "test :: IO ()"
                       , "test = putStrLn (take 3 cProjectVersion)"
                       ])
                  <> (updateCodeGeneration True)
           updateSessionD session upd 1
           assertOneError session

        -- Reveal the package with an explicit flag
        do let upd = updateDynamicOpts ["-package ghc"]
           updateSessionD session upd 1
           assertNoErrors session
           runCode

        -- Hide the package
        do let upd = updateDynamicOpts ["-hide-package ghc"]
           updateSessionD session upd 1
           assertOneError session

        -- Reveal once more
        do let upd = updateDynamicOpts ["-package ghc"]
           updateSessionD session upd 1
           assertNoErrors session
           runCode

        -- Hide it again by using the default package flags
        -- (statelessness of updateDynamicOpts)
        do let upd = updateDynamicOpts []
           updateSessionD session upd 1
           assertOneError session
    )
  , ( "Trusting and distrusting packages (#185-4)"
    , withSession (withOpts ["-XSafe", "-fpackage-trust"]) $ \session -> do
        let runCode = do
              runActions <- runStmt session "A" "test"
              (output, result) <- runWaitAll runActions
              assertEqual "" RunOk result
              assertEqual "" (BSLC.pack "Hello\n") output

        -- First, check that base is untrusted
        do let upd = (updateSourceFile "A.hs" . BSLC.pack $ unlines [
                         "module A (test) where"
                       , "test :: IO ()"
                       , "test = putStrLn \"Hello\""
                       ])
                  <> (updateCodeGeneration True)
           updateSessionD session upd 1
           assertOneError session

        -- Trust base
        do let upd = updateDynamicOpts ["-trust base"]
           updateSessionD session upd 1
           assertNoErrors session
           runCode

        -- Untrust it
        do let upd = updateDynamicOpts ["-distrust base"]
           updateSessionD session upd 1
           assertOneError session

        -- Trust it once more
        do let upd = updateDynamicOpts ["-trust base"]
           updateSessionD session upd 1
           assertNoErrors session
           runCode

        -- Untrust it again by using the default package flags
        -- (statelessness of updateDynamicOpts)
        do let upd = updateDynamicOpts []
           updateSessionD session upd 1
           assertOneError session
    )
  , ( "GHC bug #8333 (#145)"
    , withSession defaultSession $ \session -> do
        let upd1 = (updateCodeGeneration True)
                <> (updateDynamicOpts ["-XScopedTypeVariables", "-O"])
                <> (updateSourceFile "Main.hs" . BSLC.pack $
                     "main = let (x :: String) = \"hello\" in putStrLn x")
        updateSessionD session upd1 1
        assertNoErrors session
    )
  , ( "buildExe on code with type errors (#160)"
    , withSession defaultSession $ \session -> do
        let upd1 = (updateCodeGeneration True)
                <> (updateSourceFile "Main.hs" . BSLC.pack $
                     "main = foo")
        updateSessionD session upd1 1
        assertOneError session

        let upd2 = buildExe [] [(Text.pack "Main", "Main.hs")]
        updateSessionD session upd2 1

        distDir <- getDistDir session
        status <- getBuildExeStatus session
        assertEqual "" (Just $ ExitFailure 1) status
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "" "Source errors encountered. Not attempting to build executables." buildStderr
    )
  , ( "Perf: Load testPerfMs modules in one call 1"
    , withSession defaultSession $ \session -> limitPerfTest $ do
        updateSessionD session (updateCodeGeneration True) 1
        let updates = foldr (\n ups -> updKN n n <> ups)
                            mempty
                            [1..testPerfMs]
        updateSessionD session updates testPerfMs
        assertNoErrors session
    )
  , ( "Perf: Load testPerfMs modules in one call 2"
    , withSession defaultSession $ \session -> limitPerfTest $ do
        updateSessionD session (updateCodeGeneration True) 1
        let updates = foldr (\n ups -> updDepKN n n <> ups)
                            mempty
                            [1..testPerfMs]
        updateSessionD session updates testPerfMs
        assertNoErrors session
    )
  , ( "Perf: Load testPerfMs modules in many calls 1"
    , withSession defaultSession $ \session -> limitPerfTest $ do
        updateSessionD session (updateCodeGeneration True) 1
        let updates = map (\n -> updKN n n) [1..testPerfMs]
        mapM (\up -> updateSessionD session up 1) updates
        assertNoErrors session
    )
  , ( "Perf: Load testPerfMs modules in many calls 2"
    , withSession defaultSession $ \session -> limitPerfTest $ do
        updateSessionD session (updateCodeGeneration True) 1
        let updates = map (\n -> updDepKN n n) [1..testPerfMs]
        mapM (\up -> updateSessionD session up 1) updates
        assertNoErrors session
    )
  , ( "Perf: Load 4xtestPerfMs modules, each batch in one call 1"
    , withSession defaultSession $ \session -> limitPerfTest $ do
        updateSessionD session (updateCodeGeneration True) 1
        let updates1 = foldr (\n ups -> updKN n n <> ups)
                            mempty
                            [1..testPerfMs]
        updateSessionD session updates1 testPerfMs
        let updates2 = foldr (\n ups -> updKN 42 n <> ups)
                            mempty
                            [1..testPerfMs]
        updateSessionD session updates2 testPerfMs
        updateSessionD session updates1 testPerfMs
        updateSessionD session updates2 testPerfMs
        assertNoErrors session
    )
  , ( "Perf: Load 4xtestPerfMs modules, each batch in one call 2"
    , withSession defaultSession $ \session -> limitPerfTest $ do
        updateSessionD session (updateCodeGeneration True) 1
        let updates1 = foldr (\n ups -> updDepKN n n <> ups)
                            mempty
                            [1..testPerfMs]
        updateSessionD session updates1 testPerfMs
        let updates2 = foldr (\n ups -> updDepKN 42 n <> ups)
                            mempty
                            [1..testPerfMs]
        updateSessionD session updates2 testPerfMs
        updateSessionD session updates1 testPerfMs
        updateSessionD session updates2 testPerfMs
        assertNoErrors session
    )
  , ( "Perf: Update a module testPerfTimes with no context 1"
    , withSession defaultSession $ \session -> limitPerfTest $ do
        updateSessionD session (updateCodeGeneration True) 1
        let upd k = updKN k 1
        mapM_ (\k -> updateSessionD session (upd k) 1) [1..testPerfTimes]
        assertNoErrors session
    )
  , ( "Perf: Update a module testPerfTimes with no context 2"
    , withSession defaultSession $ \session -> limitPerfTest $ do
        updateSessionD session (updateCodeGeneration True) 1
        let upd k = updDepKN k 1
        mapM_ (\k -> updateSessionD session (upd k) 1) [1..testPerfTimes]
        assertNoErrors session
    )
  , ( "Perf: Update a module testPerfTimes with testPerfMs modules 1"
    , withSession defaultSession $ \session -> limitPerfTest $ do
        updateSessionD session (updateCodeGeneration True) 1
        let updates = foldr (\n ups -> ups <> updKN n n)
                            mempty
                            [1..testPerfMs]
        updateSessionD session updates testPerfMs
        let upd k = updKN k (testPerfMs `div` 2)
        mapM_ (\k -> updateSessionD session (upd k) 1) [1..testPerfTimes]
        assertNoErrors session
    )
  , ( "Perf: Update a module testPerfTimes with testPerfMs modules 2"
    , withSession defaultSession $ \session -> limitPerfTest $ do
        let testPerfMsFixed = 50  -- dependencies force recompilation: slow
        updateSessionD session (updateCodeGeneration True) 1
        let updates = foldr (\n ups -> ups <> updDepKN n n)
                            mempty
                            [1..testPerfMsFixed]
        updateSessionD session updates testPerfMsFixed
        let upd k = updDepKN k (testPerfMsFixed `div` 2)
        mapM_ (\k -> updateSessionD session (upd k) (1 + testPerfMsFixed `div` 2)) [1..testPerfTimes]
        assertNoErrors session
    )
  , ( "Perf: Update and run a module testPerfTimes with testPerfMs modules 1"
    , withSession defaultSession $ \session -> limitPerfTest $ do
        let testPerfMsFixed = testPerfMs * 1 `div` 2  -- running has overheads
        updateSessionD session (updateCodeGeneration True) 1
        let updates = foldr (\n ups -> ups <> updKN n n)
                            mempty
                            [1..testPerfMsFixed]
        updateSessionD session updates testPerfMsFixed
        let upd k = updKN k (testPerfMsFixed `div` 2)
            mdiv2 = "M" ++ show (testPerfMsFixed `div` 2)
        mapM_ (\k -> do
          updateSessionD session (upd k) 1
          runActions <- runStmt session mdiv2 "m"
          void $ runWaitAll runActions
          ) [1..testPerfTimes]
        assertNoErrors session
    )
  , ( "Perf: Update and run a module testPerfTimes with testPerfMs modules 2"
    , withSession defaultSession $ \session -> limitPerfTest $ do
        let testPerfMsFixed = 40  -- dependencies force recompilation: slow
        updateSessionD session (updateCodeGeneration True) 1
        let updates = foldr (\n ups -> ups <> updDepKN n n)
                            mempty
                            [1..testPerfMsFixed]
        updateSessionD session updates testPerfMsFixed
        let upd k = updDepKN k (testPerfMsFixed `div` 2)
            mdiv2 = "M" ++ show (testPerfMsFixed `div` 2)
        mapM_ (\k -> do
          updateSessionD session (upd k) (1 + testPerfMsFixed `div` 2)
          runActions <- runStmt session mdiv2 "m"
          void $ runWaitAll runActions
          ) [1..testPerfTimes]
        assertNoErrors session
    )
  , ( "GHC API expects 'main' to be present in 'Main' (#170)"
    , withSession defaultSession $ \session -> do
        let update = updateDataFile "Data/Monoid.hs" (BSLC.pack "module Data.Monoid where\nfoo = doesnotexist")
                     <> updateSourceFile "Main.hs" (BSLC.pack "module Main where\nimport Data.Monoid\nmain2222 = return ()")
        updateSessionD session update 2
        assertOneError session
    )
  , ( "buildExe doesn't expect 'Main.main' to be present nor to be in IO (#170)"
    , withSession defaultSession $ \session -> do
        let update = updateDataFile "Data/Monoid.hs" (BSLC.pack "module Data.Monoid where\nfoo = doesnotexist")
                     <> updateSourceFile "Main.hs" (BSLC.pack "module Main where\nimport Data.Monoid\nmain = return ()")
        updateSessionD session update 2
        assertNoErrors session
        let updE = buildExe [] [(Text.pack "Main", "Main.hs")]
        updateSessionD session updE 1
        status <- getBuildExeStatus session
        assertEqual "buildExe doesn't know 'main' is in IO"
          (Just $ ExitFailure 1) status
    )
  , ( "Data files should not leak into compilation if referenced (#169)"
    , withSession defaultSession $ \session -> do
        let update = updateDataFile "Data/Monoid.hs" (BSLC.pack "module Data.Monoid where\nfoo = doesnotexist")
                     <> updateSourceFile "Main.hs" (BSLC.pack "module Main where\nimport Data.Monoid\nmain = return ()")
        updateSessionD session update 2
        assertNoErrors session
    )
  , ( "Data files should not leak in exe building (#169)"
    , withSession defaultSession $ \session -> do
        let update = updateDataFile "Data/Monoid.hs" (BSLC.pack "module Data.Monoid where\nfoo = doesnotexist")
                     <> updateSourceFile "Main.hs" (BSLC.pack "module Main where\nimport Data.Monoid\nmain :: IO()\nmain = return ()")
        updateSessionD session update 2
        assertNoErrors session
        let updE = buildExe [] [(Text.pack "Main", "Main.hs")]
        updateSessionD session updE 2
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "" "" buildStderr
        status <- getBuildExeStatus session
        assertEqual "after exe build" (Just ExitSuccess) status

        let updDoc = buildDoc
        updateSessionD session updDoc 1
        statusDoc <- getBuildDocStatus session
        assertEqual "after doc build" (Just ExitSuccess) statusDoc

        dotCabalFromName <- getDotCabal session
        let dotCabal = dotCabalFromName "main" $ Version [1, 0] []
        assertEqual "dotCabal from Main" (filterIdeBackendTest $ BSLC.pack "name: main\nversion: 1.0\ncabal-version: 1.14.0\nbuild-type: Simple\nlicense: AllRightsReserved\nlicense-file: \"\"\ndata-dir: \"\"\n \nlibrary\n    build-depends: base ==4.5.1.0, ghc-prim ==0.2.0.0,\n                   integer-gmp ==0.4.0.0\n    exposed: True\n    buildable: True\n    default-language: Haskell2010\n \n ") $ filterIdeBackendTest dotCabal
        let pkgDir = distDir </> "dotCabal.for.lhs"
        createDirectoryIfMissing False pkgDir
        BSLC.writeFile (pkgDir </> "main.cabal") dotCabal
        checkWarns <- checkPackage pkgDir
        assertCheckWarns checkWarns
    )
  , ( "Data files should not leak in exe building (#169), with a extra modules"
    , withSession defaultSession $ \session -> do
        let update = updateDataFile "Data/Monoid.hs" (BSLC.pack "module Data.Monoid where\nfoo = doesnotexist")
                     <> updateSourceFile "Main.hs" (BSLC.pack "module Main where\nimport Data.Monoid\nmain :: IO()\nmain = return ()")
                     <> updateSourceFile "NonMain.hs" (BSLC.pack "module NonMain where\nimport Data.Monoid\nmain :: IO()\nmain = return ()")
                     <> updateSourceFile "NonMain2.hs" (BSLC.pack "module NonMain2 where\nimport Data.Monoid\nmain :: IO()\nmain = return ()")
        updateSessionD session update 4
        assertNoErrors session
        let updE = buildExe [] [(Text.pack "Main", "Main.hs")]
        updateSessionD session updE 4
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "" "" buildStderr
        status <- getBuildExeStatus session
        assertEqual "after exe build" (Just ExitSuccess) status

        let updDoc = buildDoc
        updateSessionD session updDoc 1
        statusDoc <- getBuildDocStatus session
        assertEqual "after doc build" (Just ExitSuccess) statusDoc

        dotCabalFromName <- getDotCabal session
        let dotCabal = dotCabalFromName "main" $ Version [1, 0] []
        assertEqual "dotCabal from Main" (filterIdeBackendTest $ BSLC.pack "name: main\nversion: 1.0\ncabal-version: 1.14.0\nbuild-type: Simple\nlicense: AllRightsReserved\nlicense-file: \"\"\ndata-dir: \"\"\n \nlibrary\n    build-depends: base ==4.5.1.0, ghc-prim ==0.2.0.0,\n                   integer-gmp ==0.4.0.0\n    exposed-modules: NonMain NonMain2\n    exposed: True\n    buildable: True\n    default-language: Haskell2010\n \n ") $ filterIdeBackendTest dotCabal
        let pkgDir = distDir </> "dotCabal.for.lhs"
        createDirectoryIfMissing False pkgDir
        BSLC.writeFile (pkgDir </> "main.cabal") dotCabal
        checkWarns <- checkPackage pkgDir
        assertCheckWarns checkWarns
    )
  , ( "Data files should not leak in exe building (#169), with a non-'Main' main module"
    , withSession defaultSession $ \session -> do
        let update = updateDataFile "Data/Monoid.hs" (BSLC.pack "module Data.Monoid where\nfoo = doesnotexist")
                     <> updateSourceFile "Main.hs" (BSLC.pack "module Main where\nimport Data.Monoid\nmain :: IO()\nmain = return ()")
                     <> updateSourceFile "NonMain.hs" (BSLC.pack "module NonMain where\nimport Data.Monoid\nmain :: IO()\nmain = return ()")
                     <> updateSourceFile "NonMain2.hs" (BSLC.pack "module NonMain2 where\nimport Data.Monoid\nmain :: IO()\nmain = return ()")
        updateSessionD session update 4
        assertNoErrors session
        let updE = buildExe [] [(Text.pack "NonMain", "NonMain.hs")]
        updateSessionD session updE 4
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "" "" buildStderr
        status <- getBuildExeStatus session
        assertEqual "after exe build" (Just ExitSuccess) status

        let updDoc = buildDoc
        updateSessionD session updDoc 1
        statusDoc <- getBuildDocStatus session
        assertEqual "after doc build" (Just ExitSuccess) statusDoc

        dotCabalFromName <- getDotCabal session
        let dotCabal = dotCabalFromName "main" $ Version [1, 0] []
        assertEqual "dotCabal from Main" (filterIdeBackendTest $ BSLC.pack "name: main\nversion: 1.0\ncabal-version: 1.14.0\nbuild-type: Simple\nlicense: AllRightsReserved\nlicense-file: \"\"\ndata-dir: \"\"\n \nlibrary\n    build-depends: base ==4.5.1.0, ghc-prim ==0.2.0.0,\n                   integer-gmp ==0.4.0.0\n    exposed-modules: NonMain NonMain2\n    exposed: True\n    buildable: True\n    default-language: Haskell2010\n \n ") $ filterIdeBackendTest dotCabal
        let pkgDir = distDir </> "dotCabal.for.lhs"
        createDirectoryIfMissing False pkgDir
        BSLC.writeFile (pkgDir </> "main.cabal") dotCabal
        checkWarns <- checkPackage pkgDir
        assertCheckWarns checkWarns
    )
  , ( "Start server without bracket (need MANUAL check that server will die; #194)"
    , do let (initParams, config) = defaultSession (defaultSessionInitParams, defaultSessionConfig)
         _session <- initSession initParams config
         putStrLn "Server started.. waiting 2sec"
         threadDelay 2000000
         putStrLn "PLEASE VERIFY THAT SERVER HAS TERMINATED"
    )
  , ( "Test NondecreasingIndentation: GHC API should fail without -XNondecreasingIndentation"
    , withSession defaultSession $ \session -> do
        let src = "module Main where\n\
                  \main = do\n\
                  \    let foo = do\n\
                  \        putStrLn \"hello\"\n\
                  \    foo"
            upd = updateSourceFile "src/Main.hs" src
        updateSessionD session upd 1
        -- The error messages are different in 7.4 and 7.8.
        assertOneError session
    )
  , ( "Test NondecreasingIndentation: buildExe should fail without -XNondecreasingIndentation"
    , withSession defaultSession $ \session -> do
        let src = "module Main where\n\
                  \main = do\n\
                  \    let foo = do\n\
                  \        putStrLn \"hello\"\n\
                  \    foo"
            upd = updateSourceFile "src/Main.hs" src
                  <> updateDynamicOpts ["-XHaskell98"]
        updateSessionD session upd 1
        assertNoErrors session

        let m = "Main"
            updE = buildExe ["-XHaskell2010"] [(m, "src/Main.hs")]
        updateSessionD session updE 1
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        -- The error messages are different in 7.4 and 7.8.
        assertEqual "buildStderr empty" False (null buildStderr)
    )
  , ( "Test NondecreasingIndentation: both should pass with -XNondecreasingIndentation"
    , withSession defaultSession $ \session -> do
        let src = "module Main where\n\
                  \main = do\n\
                  \    let foo = do\n\
                  \        putStrLn \"hello\"\n\
                  \    foo"
            upd = updateSourceFile "src/Main.hs" src
                  <> updateDynamicOpts ["-XNondecreasingIndentation"]
        updateSessionD session upd 1
        assertNoErrors session

        let m = "Main"
            updE = buildExe [] [(m, "src/Main.hs")]
        updateSessionD session updE 1
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "buildStderr empty" True (null buildStderr)
    )
  , ( "Test NondecreasingIndentation: both should pass with -XNondecreasingIndentation in SessionConfig"
    , withSession (withOpts ["-XNondecreasingIndentation"]) $ \session -> do
        let src = "module Main where\n\
                  \main = do\n\
                  \    let foo = do\n\
                  \        putStrLn \"hello\"\n\
                  \    foo"
            upd = updateSourceFile "src/Main.hs" src
        updateSessionD session upd 1
        assertNoErrors session

        let m = "Main"
            updE = buildExe [] [(m, "src/Main.hs")]
        updateSessionD session updE 1
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "buildStderr empty" True (null buildStderr)
    )
  , ( "Test NondecreasingIndentation: both should pass with -XHaskell98"
    , withSession defaultSession $ \session -> do
        let src = "module Main where\n\
                  \main = do\n\
                  \    let foo = do\n\
                  \        putStrLn \"hello\"\n\
                  \    foo"
            upd = updateSourceFile "src/Main.hs" src
                  <> updateDynamicOpts ["-XHaskell98"]
        updateSessionD session upd 1
        assertNoErrors session

        let m = "Main"
            updE = buildExe [] [(m, "src/Main.hs")]
        updateSessionD session updE 1
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "buildStderr empty" True (null buildStderr)
    )
  , ( "Test NondecreasingIndentation: both should pass with -XHaskell98 in SessionConfig"
    , withSession (withOpts ["-XHaskell98"]) $ \session -> do
        let src = "module Main where\n\
                  \main = do\n\
                  \    let foo = do\n\
                  \        putStrLn \"hello\"\n\
                  \    foo"
            upd = updateSourceFile "src/Main.hs" src
        updateSessionD session upd 1
        assertNoErrors session

        let m = "Main"
            updE = buildExe [] [(m, "src/Main.hs")]
        updateSessionD session updE 1
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "buildStderr empty" True (null buildStderr)
    )
  , ( "Concurrent snippets 1: Run same snippet multiple times"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateStdoutBufferMode $ RunLineBuffering Nothing)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import Control.Monad"
                    , "echo :: IO ()"
                    , "echo = do str <- getLine"
                    , "          replicateM_ 100 $ putStrLn str"
                    ])
        updateSessionD session upd 1
        assertNoErrors session

        -- Execute all snippets, but leave them waiting for input
        snippets <- forM ["foo", "bar", "baz", "Foo", "Bar", "Baz", "FOO", "BAR", "BAZ"] $ \str -> do
          let expectedResult = BSLC.concat (replicate 100 (BSLC.pack (str ++ "\n")))
          runActions <- runStmt session "M" "echo"
          return (runActions, str, expectedResult)

        -- Start all snippets and collect all their output concurrently
        testResults <- forM snippets $ \(runActions, str, _expectedResult) -> do
          testResult <- newEmptyMVar
          _ <- forkIO $ do
            supplyStdin runActions (BSSC.pack (str ++ "\n"))
            putMVar testResult =<< runWaitAll' runActions
          return testResult

        -- Wait for all test results, and compare against expected results
        forM_ (zip snippets testResults) $ \((_runActions, _str, expectedResult), testResult) -> do
          (output, result) <- takeMVar testResult
          assertEqual "" RunOk result
          assertEqual "" expectedResult output
    )
  , ( "Concurrent snippets 2: Execute different snippets concurrently"
    , withSession defaultSession $ \session -> do
        -- Execute all snippets, but leave them waiting for input
        snippets <- forM ["foo", "bar", "baz", "Foo", "Bar", "Baz", "FOO", "BAR", "BAZ"] $ \str -> do
          let upd = (updateCodeGeneration True)
                 <> (updateStdoutBufferMode $ RunLineBuffering Nothing)
                 <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                      [ "module M where"
                      , "import Control.Monad"
                      , "echo :: IO ()"
                      , "echo = do _waiting <- getLine"
                      , "          replicateM_ 100 $ putStrLn " ++ show str
                      ])
          updateSessionD session upd 1
          assertNoErrors session

          let expectedResult = BSLC.concat (replicate 100 (BSLC.pack (str ++ "\n")))
          runActions <- runStmt session "M" "echo"
          return (runActions, expectedResult)

        -- Start all snippets and collect all their output concurrently
        testResults <- forM snippets $ \(runActions, _expectedResult) -> do
          testResult <- newEmptyMVar
          _ <- forkIO $ do
            supplyStdin runActions (BSSC.pack "\n")
            putMVar testResult =<< runWaitAll' runActions
          return testResult

        -- Wait for all test results, and compare against expected results
        forM_ (zip snippets testResults) $ \((_runActions, expectedResult), testResult) -> do
          (output, result) <- takeMVar testResult
          assertEqual "" RunOk result
          assertEqual "" expectedResult output
    )
  ]

runWaitAll' :: forall a. RunActions a -> IO (BSL.ByteString, a)
runWaitAll' RunActions{runWait} = go []
  where
    go :: [BSS.ByteString] -> IO (BSL.ByteString, a)
    go acc = do
      resp <- runWait
      case resp of
        Left  bs        -> go (bs : acc)
        Right runResult -> return (BSL.fromChunks (reverse acc), runResult)

buildExeTargetHsSucceeds :: IdeSession -> String -> IO ()
buildExeTargetHsSucceeds session m = do
  let updE = buildExe [] [(Text.pack m, m <.> "hs")]
  updateSessionD session updE 4
  distDir <- getDistDir session
  buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
  assertEqual "buildStderr empty" "" buildStderr
  status <- getBuildExeStatus session
  assertEqual "after exe build" (Just ExitSuccess) status

buildExeTargetHsFails :: IdeSession -> String -> IO ()
buildExeTargetHsFails session m = do
  let updE = buildExe [] [(Text.pack m, m <.> "hs")]
  updateSessionD session updE 4
  status <- getBuildExeStatus session
  assertEqual "after exe build" (Just $ ExitFailure 1) status

testPerfMs :: Int
{-# NOINLINE testPerfMs #-}
testPerfMs = read $ unsafePerformIO $
  System.Environment.getEnv "IDE_BACKEND_testPerfMs"
  `Ex.catch` (\(_ :: Ex.IOException) -> return "150")

testPerfTimes :: Int
{-# NOINLINE testPerfTimes #-}
testPerfTimes = read $ unsafePerformIO $
  System.Environment.getEnv "IDE_BACKEND_testPerfTimes"
  `Ex.catch` (\(_ :: Ex.IOException) -> return "150")

testPerfLimit :: Int
{-# NOINLINE testPerfLimit #-}
testPerfLimit = read $ unsafePerformIO $
  System.Environment.getEnv "IDE_BACKEND_testPerfLimit"
  `Ex.catch` (\(_ :: Ex.IOException) -> return "30")

limitPerfTest :: IO () -> IO ()
limitPerfTest t = do
  mu <- timeout (testPerfLimit * 1000000) t
  case mu of
    Nothing -> fail "Performance test did not finish within alotted time"
    Just () -> return ()

updKN :: Int -> Int -> IdeSessionUpdate ()
updKN k n =
  let moduleN = BSLC.pack $ unlines $
              [ "module M" ++ show n ++ " where"
              , "import Control.Concurrent (threadDelay)"
              , "m :: IO ()"
              , "m = threadDelay " ++ show k
              ]
  in updateSourceFile ("M" ++ show n ++ ".hs") moduleN

updDepKN :: Int -> Int -> IdeSessionUpdate ()
updDepKN k n =
  let depN | n <= 1 = ("System.IO", ".hFlush System.IO.stdout")
           | otherwise = ("M" ++ show (n - 1), ".m")
      moduleN = BSLC.pack $ unlines $
              [ "module M" ++ show n ++ " where"
              , "import Control.Concurrent (threadDelay)"
              , "import qualified " ++ fst depN
              , "m :: IO ()"
              , "m = threadDelay " ++ show k ++ " >> "
                ++ fst depN ++ snd depN
              ]
  in updateSourceFile ("M" ++ show n ++ ".hs") moduleN

modAn, modBn, modCn :: String -> IdeSessionUpdate ()
modAn n = updateSourceFile "A.hs" $ BSLC.pack $ unlines [
    "module A (foo, main) where"
  , "foo :: Int"
  , "foo = " ++ n
  , "main :: IO ()"
  , "main = return ()"
  ]
modBn n = updateSourceFile "B.hs" $ BSLC.pack $ unlines [
    "module B (bar, main) where"
  , "import A (foo)"
  , "bar :: Int"
  , "bar = foo + " ++ n
  , "main :: IO ()"
  , "main = return ()"
  ]
modCn n = updateSourceFile "C.hs" $ BSLC.pack $ unlines [
    "module C (baz, main) where"
  , "import A (foo)"
  , "baz :: Int"
  , "baz = foo + " ++ n
  , "main :: IO ()"
  , "main = return ()"
  ]

qsort :: IdeSessionUpdate ()
qsort = (updateSourceFile "Main.hs" . BSLC.pack . unlines $ [
          --          1         2         3         4         5
          -- 12345678901234567890123456789012345678901234567890123456
            "qsort [] = [] "
          , "qsort (a:as) = qsort left ++ [a] ++ qsort right"
          , "  where (left,right) = (filter (<=a) as, filter (>a) as)"
          , ""
          , "main = print (qsort [8, 4, 0, 3, 1, 23, 11, 18])"
          ])
     <> (updateCodeGeneration True)

_ignoreFailure :: IO () -> IO ()
_ignoreFailure io = Ex.catch io $ \(HUnitFailure err) ->
  putStrLn $ "WARNING: " ++ err

findExe :: String -> IO FilePath
findExe name = do
  extraPathDirs <- (System.Environment.getEnv "IDE_BACKEND_EXTRA_PATH_DIRS")
                     `Ex.catch` (\(_ :: Ex.IOException) -> return "")
  let searchPath :: ProgramSearchPath
      searchPath = ProgramSearchPathDefault
                   : map ProgramSearchPathDir (splitSearchPath extraPathDirs)
  mLoc <- findProgramOnSearchPath minBound searchPath name
  case mLoc of
    Nothing -> fail $ "Could not find " ++ name
    Just prog -> return prog

deletePackage :: FilePath -> IO ()
deletePackage pkgDir = do
  ghcPkgExe <- findExe "ghc-pkg"
  packageDb  <- (System.Environment.getEnv "IDE_BACKEND_PACKAGE_DB")
                   `Ex.catch` (\(_ :: Ex.IOException) -> return "")
  let opts = [ "--package-conf=" ++ packageDb, "-v0", "unregister"
             , takeFileName pkgDir ]
  (_,_,_,r2) <- Process.createProcess (Process.proc ghcPkgExe opts)
                  { Process.cwd = Just pkgDir
                  , Process.std_err = Process.CreatePipe }
  void $ Process.waitForProcess r2

installPackage :: FilePath -> IO ()
installPackage pkgDir = do
  cabalExe <- findExe "cabal"
  packageDb  <- (System.Environment.getEnv "IDE_BACKEND_PACKAGE_DB")
                   `Ex.catch` (\(_ :: Ex.IOException) -> return "")
  extraPathDirs <- (System.Environment.getEnv "IDE_BACKEND_EXTRA_PATH_DIRS")
                     `Ex.catch` (\(_ :: Ex.IOException) -> return "")
  oldEnv <- System.Environment.getEnvironment
  let oldEnvMap = Map.fromList oldEnv
      adjustPATH oldPATH = extraPathDirs ++ ":" ++ oldPATH
      newEnvMap = Map.adjust adjustPATH "PATH" oldEnvMap
      newEnv = Map.toList newEnvMap
  forM_ [ ["clean"]
        , ["configure", "--package-db=" ++ packageDb, "--disable-library-profiling"]
        , ["build"]
        , ["copy"]
        , ["register"] ] $ \cmd -> do
    let opts = cmd ++ ["-v0"]
    (_,_,_,r2) <- Process.createProcess (Process.proc cabalExe opts)
                    { Process.cwd = Just pkgDir
                    , Process.env = Just newEnv }
    void $ Process.waitForProcess r2

checkPackage :: FilePath -> IO String
checkPackage pkgDir = do
    cabalExe <- findExe "cabal"
    (_, mlocal_std_out, _, r2)
      <- Process.createProcess (Process.proc cabalExe ["check"])
           { Process.cwd = Just pkgDir
           , Process.std_out = Process.CreatePipe }
    let local_std_out = fromJust mlocal_std_out
    checkWarns <- IO.hGetContents local_std_out
    Ex.evaluate $ rnf checkWarns
    IO.hClose local_std_out
    void $ Process.waitForProcess r2
    return checkWarns

assertCheckWarns :: String -> Assertion
assertCheckWarns checkWarns =
  assertEqual "checkWarns for dotCabal" (filterCheckWarns checkWarns) (filterCheckWarns "These warnings may cause trouble when distributing the package:\n* No 'category' field.\n\n* No 'maintainer' field.\n\nThe following errors will cause portability problems on other environments:\n* The package is missing a Setup.hs or Setup.lhs script.\n\n* No 'synopsis' or 'description' field.\n\n* The 'license' field is missing or specified as AllRightsReserved.\n\nHackage would reject this package.\n")

filterCheckWarns :: String -> BSSC.ByteString
filterCheckWarns s =
  let (bs1, rest1) =
        BSSC.breakSubstring (BSSC.pack "The following warnings are likely affect your build negatively") $ BSSC.pack s
      (_, bs2) = BSSC.breakSubstring (BSSC.pack "These warnings may cause trouble") rest1
  in BSSC.append bs1 bs2

assertSameSet :: (Ord a, Show a) => String -> [a] -> [a] -> Assertion
assertSameSet header xs ys = assertSameList header (sort xs) (sort ys)

assertSameList :: (Ord a, Show a) => String -> [a] -> [a] -> Assertion
assertSameList header xs ys =
  case diff xs ys of
    [] -> return ()
    ds -> assertFailure (header ++ unlines ds)

-- | Compare two lists, both assumed sorted
--
-- @diff expected actual@ returns a list of differences between two lists,
-- or an empty lists if the input lists are identical
diff :: (Ord a, Show a) => [a] -> [a] -> [String]
diff [] [] = []
diff xs [] = map (\x -> "Missing "    ++ show x) xs
diff [] ys = map (\y -> "Unexpected " ++ show y) ys
diff (x:xs) (y:ys)
  | x <  y    = ("Missing "    ++ show x) : diff xs (y:ys)
  | x >  y    = ("Unexpected " ++ show y) : diff (x:xs) ys
  | otherwise = diff xs ys

-- Set of projects and options to use for them.
projects :: [(String, FilePath, [String])]
projects =
  [ ( "A depends on B, throws exception"
    , "test/ABnoError"
    , []
    )
  , ( "Cabal code"
    , "test/Cabal"
    , []
    )
  , ("A single file with a code to run in parallel"
    , "test/MainModule"
    , []
    )
  ]

-- Driver
tests :: [Test]
tests =
  let groupProject genModInfo ((featureName, check), k) =
        testGroup featureName
        $ map (caseFeature genModInfo featureName check k) projects
      caseFeature genModInfo featureName check k
                  (projectName, originalSourcesDir, opts) = do
        let caseName = projectName ++ " (" ++ show k ++ ")"
        testCase caseName $ do
          debug dVerbosity $ featureName ++ " / " ++ caseName ++ ":"
          traceEventIO ("TEST " ++ featureName ++ " / " ++ caseName)
          withSession (withModInfo genModInfo . withOpts opts) $ \session -> do
            (originalUpdate, lm) <- getModulesFrom session originalSourcesDir
            check session originalUpdate lm
  in [ testGroup "Full integration tests on multiple projects"
       $ map (groupProject False) $ zip multipleTests [1 :: Int ..]
     , testGroup "Synthetic integration tests"
       $ map (uncurry testCase) (traceTests syntheticTests)
     ]

traceTests :: [(String, Assertion)] -> [(String, Assertion)]
traceTests = map $ \(label, test) -> (label, do traceEventIO ("TEST " ++ label) ; test)

main :: IO ()
main = defaultMain tests

updateSessionP :: IdeSession -> IdeSessionUpdate () -> [(Int, Int, String)] -> IO ()
updateSessionP session update expectedProgressUpdates = do
  progressRef <- newIORef []

  -- We just collect the progress messages first, and verify them afterwards
  updateSession session update $ \p -> do
    progressUpdates <- readIORef progressRef
    writeIORef progressRef $ progressUpdates ++ [p]

  progressUpdates <- readIORef progressRef
  assertBool ("We expected " ++ show expectedProgressUpdates ++ ", but got " ++ show progressUpdates)
             (length progressUpdates <= length expectedProgressUpdates)

  forM_ (zip progressUpdates expectedProgressUpdates) $ \(actual, expected@(step, numSteps, msg)) ->
    assertBool ("Unexpected progress update " ++ show actual ++ "; expected " ++ show expected)
               (progressStep actual == step &&
                progressNumSteps actual == numSteps &&
                case progressOrigMsg actual of
                  Just actualMsg -> msg `isInfixOf` Text.unpack actualMsg
                  Nothing        -> False)

updateSessionD :: IdeSession -> IdeSessionUpdate () -> Int -> IO ()
updateSessionD session update numProgressUpdates = do
  progressRef <- newIORef []

  -- We just collect the progress messages first, and verify them afterwards
  updateSession session update $ \p -> do
    progressUpdates <- readIORef progressRef
    writeIORef progressRef $ progressUpdates ++ [p]

  -- These progress messages are often something like
  --
  -- [18 of 27] Compiling IdeSession.Types.Private ( IdeSession/Types/Private.hs, dist/build/IdeSession/Types/Private.o )
  -- [19 of 27] Compiling IdeSession.GHC.API ( IdeSession/GHC/API.hs, dist/build/IdeSession/GHC/API.o )
  -- [20 of 27] Compiling IdeSession.GHC.Client ( IdeSession/GHC/Client.hs, dist/build/IdeSession/GHC/Client.p_o )
  -- [21 of 27] Compiling IdeSession.Types.Translation ( IdeSession/Types/Translation.hs, dist/build/IdeSession/Types/Translation.p_o )
  -- [23 of 27] Compiling IdeSession.State ( IdeSession/State.hs, dist/build/IdeSession/State.p_o )
  --
  -- So these numbers don't need to start at 1, may be discontiguous, out of
  -- order, and may not end with [X of X]. The only thing we can check here is
  -- that we get at most the number of progress messages we expect.
  progressUpdates <- readIORef progressRef
  assertBool ("We expected " ++ show numProgressUpdates ++ " progress messages, but got " ++ show progressUpdates)
             (length progressUpdates <= numProgressUpdates)

-- Extra test tools.
--

loadModule :: FilePath -> String -> IdeSessionUpdate ()
loadModule file contents =
    let mod =  "module " ++ mname file ++ " where\n" ++ contents
    in updateSourceFile file (BSLC.pack mod)
  where
    -- This is a hack: construct a module name from a filename
    mname :: FilePath -> String
    mname path = case "test/" `substr` path of
      Just rest -> dotToSlash . dropExtension . dropFirstPathComponent $ rest
      Nothing   -> takeBaseName path

    dropFirstPathComponent :: FilePath -> FilePath
    dropFirstPathComponent = tail . dropWhile (/= '/')

    dotToSlash :: String -> String
    dotToSlash = map $ \c -> if c == '/' then '.' else c

    -- | Specification:
    --
    -- > bs `substr` (as ++ bs ++ cs) == Just cs
    -- > bs `substr` _                == Nothing
    substr :: Eq a => [a] -> [a] -> Maybe [a]
    substr needle haystack
      | needle `isPrefixOf` haystack = Just $ drop (length needle) haystack
      | otherwise = case haystack of
                      []              -> Nothing
                      (_ : haystack') -> substr needle haystack'

assertIdInfo :: IdeSession
             -> String                -- ^ Module
             -> (Int, Int, Int, Int)  -- ^ Location
             -> String                -- ^ Name
             -> IdNameSpace           -- ^ Namespace
             -> String                -- ^ Type
             -> String                -- ^ Defining module
             -> String                -- ^ Defining span
             -> String                -- ^ Home module
             -> String                -- ^ Scope
             -> Assertion
assertIdInfo session
             mod
             (frLine, frCol, toLine, toCol)
             expectedName
             expectedNameSpace
             expectedType
             expectedDefModule
             expectedDefSpan
             expectedHome
             expectedScope =
  assertIdInfo' session
                mod
                (frLine, frCol, toLine, toCol)
                (frLine, frCol, toLine, toCol)
                expectedName
                expectedNameSpace
                (case expectedType of "" -> []
                                      _  -> allVersions expectedType)
                expectedDefModule
                (allVersions expectedDefSpan)
                expectedHome
                (allVersions expectedScope)

-- | If no answer is specified for a given version, it will not be verified
type PerVersion a = [(GhcVersion, a)]

allVersions :: a -> PerVersion a
allVersions x = [(GHC742, x), (GHC78, x)]

assertIdInfo' :: IdeSession
              -> String                -- ^ Module
              -> (Int, Int, Int, Int)  -- ^ Location
              -> (Int, Int, Int, Int)  -- ^ Precise location
              -> String                -- ^ Name
              -> IdNameSpace           -- ^ Namespace
              -> PerVersion String     -- ^ Type
              -> String                -- ^ Defining module
              -> PerVersion String     -- ^ Defining span
              -> String                -- ^ Home module
              -> PerVersion String     -- ^ Scope
              -> Assertion
assertIdInfo' session
              mod
              givenLocation
              expectedLocation
              expectedName
              expectedNameSpace
              expectedTypes
              expectedDefModule
              expectedDefSpans
              expectedHome
              expectedScopes = do
    idInfo  <- getSpanInfo session
    version <- getGhcVersion session
    case idInfo (Text.pack mod) givenSpan of
      (actualSpan, SpanId actualInfo) : _ -> compareIdInfo version actualSpan actualInfo
      (actualSpan, SpanQQ actualInfo) : _ -> compareIdInfo version actualSpan actualInfo
      _ -> assertFailure $ "No id info found for " ++ show expectedName
                        ++ " at " ++ show mod ++ ":" ++ show givenLocation
  where
    givenSpan, expectedSpan :: SourceSpan
    (_givenMod,    givenSpan)    = mkSpan mod givenLocation
    (_expectedMod, expectedSpan) = mkSpan mod expectedLocation

    compareIdInfo :: GhcVersion -> SourceSpan -> IdInfo -> Assertion
    compareIdInfo version actualSpan IdInfo{idProp = IdProp{..}, idScope} =
      collectErrors [
          assertEqual "name"      expectedName      (Text.unpack idName)
        , assertEqual "location"  expectedSpan      actualSpan
        , assertEqual "namespace" expectedNameSpace idSpace

        , case lookup version expectedDefSpans of
            Nothing              -> return ()
            Just expectedDefSpan -> assertEqual "def span" expectedDefSpan
                                                           (show idDefSpan)

        , assertEqual "def module" (ignoreVersions expectedDefModule)
                                   (ignoreVersions (show idDefinedIn))

        , case lookup version expectedScopes of
            Nothing            -> return ()
            Just expectedScope -> assertEqual "scope" (ignoreVersions expectedScope)
                                                      (ignoreVersions (show idScope))

        , case (lookup version expectedTypes, idType) of
            (Just expectedType, Just actualType) ->
              assertAlphaEquiv "type" expectedType (Text.unpack actualType)
            (Just expectedType, Nothing) ->
              assertFailure $ "expected type " ++ expectedType ++ ", but got none"
            (Nothing, _) ->
              -- Not checking
              return ()

        , case idHomeModule of
            Nothing         -> assertEqual "home" expectedHome ""
            Just actualHome -> assertEqual "home" (ignoreVersions expectedHome)
                                                  (ignoreVersions (show actualHome))
        ]

assertExpTypes :: (ModuleName -> SourceSpan -> [(SourceSpan, Text)])
               -> String
               -> (Int, Int, Int, Int)
               -> [(Int, Int, Int, Int, String)]
               -> Assertion
assertExpTypes expTypes mod loc expected =
    assertAlphaEquiv "" expected actual
  where
    actual = flip map (uncurry expTypes $ mkSpan mod loc) $ \(span, typ) ->
      ( spanFromLine   span
      , spanFromColumn span
      , spanToLine     span
      , spanToColumn   span
      , Text.unpack    typ
      )

assertUseSites :: (ModuleName -> SourceSpan -> [SourceSpan])
               -> String
               -> (Int, Int, Int, Int)
               -> String
               -> [String]
               -> Assertion
assertUseSites useSites mod loc symbol expected =
    assertEqual ("Use sites of `" ++ symbol ++ "` in " ++ show mod) expected actual
  where
    actual = map show (uncurry useSites $ mkSpan mod loc)

mkSpan :: String -> (Int, Int, Int, Int) -> (ModuleName, SourceSpan)
mkSpan mod (frLine, frCol, toLine, toCol) = (Text.pack mod, span)
  where
    span = SourceSpan { spanFilePath   = mod ++ ".hs"
                      , spanFromLine   = frLine
                      , spanFromColumn = frCol
                      , spanToLine     = toLine
                      , spanToColumn   = toCol
                      }

assertSourceErrors' :: IdeSession -> [String] -> Assertion
assertSourceErrors' session = assertSourceErrors session . map
  (\err -> [(Nothing, err)])

-- @assertSourceErrors session [[a,b,c],[d,e,f],..] checks that there are
-- exactly as many errors as elements in the outer list, and each of those
-- errors must match one of the errors inside the inner lists
assertSourceErrors :: IdeSession -> [[(Maybe FilePath, String)]] -> Assertion
assertSourceErrors session expected = do
  errs <- getSourceErrors session
  if length errs /= length expected
    then assertFailure $ "Unexpected source errors: " ++ show3errors errs
    else forM_ (zip expected errs) $ \(potentialExpected, actualErr) ->
           assertErrorOneOf actualErr potentialExpected

assertErrorOneOf :: SourceError -> [(Maybe FilePath, String)] -> Assertion
assertErrorOneOf (SourceError _ loc actual) potentialExpected =
    case foldr1 mplus (map matches potentialExpected) of
      Left err -> assertFailure err
      Right () -> return ()
  where
    matches (mFP, expErr) = do
      matchesFilePath mFP
      matchesError expErr

    matchesFilePath Nothing = Right ()
    matchesFilePath (Just expectedPath) =
      case loc of
        ProperSpan (SourceSpan actualPath _ _ _ _) ->
          if expectedPath `isSuffixOf` actualPath
            then Right ()
            else Left "Wrong file"
        _ ->
          Left "Expected location"

    matchesError expectedErr =
      if ignoreQuotes expectedErr `isInfixOf` ignoreQuotes (Text.unpack actual)
        then Right ()
        else Left $ "Unexpected error: " ++ Text.unpack actual

assertNoErrors :: IdeSession -> Assertion
assertNoErrors session = do
  errs <- getSourceErrors session
  assertBool ("Unexpected errors: " ++ show3errors errs) $ null errs

assertSomeErrors :: [SourceError] -> Assertion
assertSomeErrors msgs = do
  assertBool "An error was expected, but not found" $ length msgs >= 1

assertOneError :: IdeSession -> Assertion
assertOneError session = do
  msgs <- getSourceErrors session
  assertSomeErrors msgs
  assertBool ("Too many type errors: " ++ show3errors msgs)
    $ length msgs <= 1

assertMoreErrors :: IdeSession -> Assertion
assertMoreErrors session = do
  msgs <- getSourceErrors session
  assertBool ("Too few type errors: " ++ show3errors msgs)
    $ length msgs >= 2

show3errors :: [SourceError] -> String
show3errors errs =
  let shown = List.intercalate "\n" (map show $ take 3 $ errs)
      more | length errs > 3 = "\n... and more ..."
           | otherwise       = ""
  in shown ++ more

assertLoadedModules :: IdeSession -> String -> [String] -> Assertion
assertLoadedModules session header goodMods = do
  loadedMods <- getLoadedModules session
  assertSameSet header (map Text.pack goodMods) loadedMods

assertBreak :: IdeSession
            -> String                     -- ^ Module
            -> String                     -- ^ Location
            -> String                     -- ^ Result type
            -> [(String, String, String)] -- ^ Var, type, value
            -> Assertion
assertBreak session mod loc resTy vars = do
  Just BreakInfo{..} <- getBreakInfo session
  assertEqual      "module name" mod   (Text.unpack breakInfoModule)
  assertEqual      "location"    loc   (show breakInfoSpan)
  assertAlphaEquiv "result type" resTy (Text.unpack breakInfoResultType)
  assertEqual      "number of local vars" (length vars) (length breakInfoVariableEnv)
  forM_ (zip vars breakInfoVariableEnv) $ \((var, typ, val), (var', typ', val')) -> do
    assertEqual      "var name" var (Text.unpack var')
    assertAlphaEquiv "var type" typ (Text.unpack typ')
    assertEqual      "var val"  val (Text.unpack val')

isAsyncException :: RunResult -> Bool
isAsyncException (RunProgException ex) =
     (ex == "AsyncException: user interrupt")
  || (ex == "SomeAsyncException: user interrupt")
isAsyncException _ = False

restartRun :: [String] -> ExitCode -> Assertion
restartRun code exitCode =
      withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                     code)

        -- Compile and run the code on the first server
        updateSessionD session upd 1
        assertNoErrors session
        runActionsBefore <- runStmt session "M" "loop"

        -- Start a new server
        threadDelay 100000
        serverBefore <- getGhcServer session
        restartSession session Nothing

        -- Compile the code on the new server
        updateSessionD session upd 1
        assertNoErrors session

        -- Make sure the old server exited
        exitCodeBefore <- getGhcExitCode serverBefore
        assertEqual "exitCodeBefore" (Just exitCode) exitCodeBefore

        -- Make sure the new server is still alive
        serverAfter <- getGhcServer session
        exitCodeAfter <- getGhcExitCode serverAfter
        assertEqual "exitCodeAfter" Nothing exitCodeAfter

        -- Force cancel the old snippet
        forceCancel runActionsBefore
        resOrEx <- runWait runActionsBefore
        case resOrEx of
          Right RunForceCancelled -> return ()
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx

testBufferMode :: RunBufferMode -> Assertion
testBufferMode bufferMode =
  withSession defaultSession $ \session -> do
    let upd = (updateCodeGeneration True)
           <> (updateStdoutBufferMode bufferMode)
           <> (updateStderrBufferMode bufferMode)
           <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                [ "module M where"
                , "import Control.Concurrent"
                , "import Control.Monad"
                , "printCs :: IO ()"
                , "printCs = do"
                , "  threadDelay 500000"
                , "  replicateM_ 5 $ do"
                , "    forM_ ['1' .. '9'] $ \\ch -> do"
                , "      threadDelay 100000"
                , "      putChar ch"
                , "    threadDelay 100000"
                , "    putChar '\\n'"
                ])

    updateSessionD session upd 1
    assertNoErrors session

    runActions <- runStmt session "M" "printCs"
    let go acc = do ret <- runWait runActions
                    case ret of
                      Left bs -> do
                        go (BSSC.unpack bs : acc)
                      Right RunOk ->
                        verify bufferMode (reverse acc)
                      Right res ->
                        assertFailure $ "Program terminated abnormally: " ++ show res
    go []
  where
    verify :: RunBufferMode -> [String] -> Assertion
    verify RunNoBuffering outp =
      assertEqual "" (chunk 1 total) outp
    verify (RunLineBuffering Nothing) outp =
      assertEqual "" (chunkOn '\n' total) outp
    verify (RunBlockBuffering (Just blockSize) Nothing) outp =
      assertEqual "" (chunk blockSize total) outp
    verify (RunLineBuffering (Just 1000000)) outp = do
      -- We don't want to be *too* precise, but we should expect 10 chunks,
      -- half of which should end on a linebreak. And of course they should
      -- total to the right thing :)
      let (withBreak, withoutBreak) = List.partition ((== '\n') . last) outp
      assertEqual "" 5 (length withBreak)
      assertEqual "" 5 (length withoutBreak)
      assertEqual "" total (concat outp)
    verify (RunBlockBuffering (Just 4) (Just 1000000)) outp = do
      -- As above, we don't want to be too precise. Certaily no chunks should
      -- be larger than 4, and "some" should be smaller
      assertBool "" (all ((<= 4) . length) outp)
      assertBool "" (any ((< 4)  . length) outp)
      assertEqual "" total (concat outp)
    verify (RunBlockBuffering (Just 4096) (Just 1000000)) outp = do
      assertEqual "" 6 (length outp)
      assertEqual "" total (concat outp)
    verify mode _outp =
      assertFailure $ "Unsupported mode " ++ show mode

    total :: String
    total = concat $ replicate 5 "123456789\n"

    chunk :: Int -> [a] -> [[a]]
    chunk _ [] = []
    chunk n xs = let (firstChunk, rest) = splitAt n xs
                 in firstChunk : chunk n rest

    chunkOn :: Eq a => a -> [a] -> [[a]]
    chunkOn _ [] = []
    chunkOn x xs = let (firstChunk, rest) = List.span (/= x) xs
                   in case rest of
                        (x' : rest') -> (firstChunk ++ [x']) : chunkOn x rest'
                        []           -> [firstChunk]

filterIdeBackendTest :: BSLC.ByteString -> BSSC.ByteString
filterIdeBackendTest bsLazy =
  let toStrict = BSSC.concat . BSLC.toChunks  -- not in our old bytestring pkg
      bs = ignoreVersions $ toStrict bsLazy
      (bs1Raw, rest1) = BSSC.breakSubstring (BSSC.pack "hs-source-dirs:") bs
      (bs1, _) = BSSC.spanEnd isSpace bs1Raw
      (_, bs2) = BSSC.breakSubstring (BSSC.pack "\n") rest1
      bs3 = if BSSC.null rest1 then bs else BSSC.append bs1 bs2
      (bs5Raw, rest5) = BSSC.breakSubstring (BSSC.pack "other-extensions:") bs3
      (bs5, _) = BSSC.spanEnd isSpace bs5Raw
      (_, bs6) = BSSC.breakSubstring (BSSC.pack "\n") rest5
  in if BSSC.null rest5 then bs3 else BSSC.append bs5 bs6

filterIdeBackendTestC :: String -> BSSC.ByteString -> BSSC.ByteString
filterIdeBackendTestC lastFile bs =
  let (bs1Raw, rest1) = BSSC.breakSubstring (BSSC.pack "c-sources:") bs
      (bs1, _) = BSSC.spanEnd isSpace bs1Raw
      (_, bs2) = BSSC.breakSubstring (BSSC.pack lastFile) rest1
  in if BSSC.null rest1 then bs else BSSC.append bs1 bs2

filterIdeBackendTestH :: String -> BSSC.ByteString -> BSSC.ByteString
filterIdeBackendTestH lastFile bs =
  let (bs1Raw, rest1) = BSSC.breakSubstring (BSSC.pack "install-includes:") bs
      (bs1, _) = BSSC.spanEnd isSpace bs1Raw
      (_, bs2) = BSSC.breakSubstring (BSSC.pack lastFile) rest1
  in if BSSC.null rest1 then bs else BSSC.append bs1 bs2

{------------------------------------------------------------------------------
  Default configuration

  This is slightly bold: we use an unsafePerformIO in order to read the
  system environment.
------------------------------------------------------------------------------}

defaultSessionInitParams :: SessionInitParams
{-# NOINLINE defaultSessionInitParams #-}
defaultSessionInitParams = unsafePerformIO $
  return IdeSession.defaultSessionInitParams

defaultSessionConfig :: SessionConfig
{-# NOINLINE defaultSessionConfig #-}
defaultSessionConfig = unsafePerformIO $ do
  packageDb     <- (System.Environment.getEnv "IDE_BACKEND_PACKAGE_DB")
                     `Ex.catch` (\(_ :: Ex.IOException) -> return "")
  extraPathDirs <- (System.Environment.getEnv "IDE_BACKEND_EXTRA_PATH_DIRS")
                     `Ex.catch` (\(_ :: Ex.IOException) -> return "")
  keepTempFiles <- (System.Environment.getEnv "IDE_BACKEND_KEEP_TEMP_FILES")
                     `Ex.catch` (\(_ :: Ex.IOException) -> return "")
  let packageDbStack
        | null packageDb = configPackageDBStack IdeSession.defaultSessionConfig
        | otherwise      = [GlobalPackageDB, SpecificPackageDB packageDb]
  return IdeSession.defaultSessionConfig {
             configPackageDBStack  = packageDbStack
           , configExtraPathDirs   = splitSearchPath extraPathDirs
           , configDeleteTempFiles = null keepTempFiles
           }

{------------------------------------------------------------------------------
  Aux
------------------------------------------------------------------------------}

newtype Counter = Counter (IORef Int)

newCounter :: IO Counter
newCounter = do
  c <- newIORef 0
  return (Counter c)

resetCounter :: Counter -> IO ()
resetCounter (Counter c) = writeIORef c 0

incCounter :: Counter -> IO ()
incCounter (Counter c) = readIORef c >>= writeIORef c . (+ 1)

assertCounter :: Counter -> Int -> Assertion
assertCounter (Counter c) i = do
  j <- readIORef c
  assertEqual "" i j

-- | Mark each element of the list with booleans indicating whether it's the
-- first and/or the last element in the list
mark :: [a] -> [(a, Bool, Bool)]
mark []     = []
mark [x]    = [(x, True, True)]
mark (x:xs) = (x, True, False) : aux xs
  where
    aux []     = error "impossible"
    aux [y]    = [(y, False, True)]
    aux (y:ys) = (y, False, False) : aux ys

-- | Replace everything that looks like a quote by a standard single quote.
ignoreQuotes :: String -> String
ignoreQuotes s = subRegex (mkRegex quoteRegexp) s "'"
  where
    quoteRegexp :: String
    quoteRegexp = "[‘‛’`\"]"

collectErrors :: [Assertion] -> Assertion
collectErrors as = do
    es <- lefts `liftM` (sequence $ map Ex.try as)
    if null es
      then return ()
      else Ex.throwIO (concatFailures es)

concatFailures :: [HUnitFailure] -> HUnitFailure
concatFailures = go []
  where
    go acc []                    = HUnitFailure (unlines . reverse $ acc)
    go acc (HUnitFailure e : es) = go (e : acc) es

{------------------------------------------------------------------------------
  Replace (type variables) with numbered type variables

  i.e., change "b -> [c]" to "a1 -> [a2]"

  useful for comparing types for alpha-equivalence
------------------------------------------------------------------------------}

assertAlphaEquiv :: (IgnoreVarNames a, Eq a, Show a) => String -> a -> a -> Assertion
assertAlphaEquiv label a b =
  if ignoreVarNames a == ignoreVarNames b
    then return ()
    else assertFailure $ label ++ "\n"
                      ++ "expected: " ++ show a
                      ++ " but got: " ++ show b

class IgnoreVarNames a where
  ignoreVarNames :: a -> a

instance IgnoreVarNames a => IgnoreVarNames [a] where
  ignoreVarNames = map ignoreVarNames

instance IgnoreVarNames a => IgnoreVarNames (Int, Int, Int, Int, a) where
  ignoreVarNames (a, b, c, d, e) = (a, b, c, d, ignoreVarNames e)

instance IgnoreVarNames String where
  ignoreVarNames = unwords . go [] [] . tokenize
    where
      go :: [String] -> [String] -> [String] -> [String]
      go _vars acc [] = reverse acc
      go  vars acc (x:xs)
        | isVar x   = case elemIndex x vars of
                        Just n  -> go vars (var n : acc) xs
                        Nothing -> go (vars ++ [x]) acc (x : xs)
        | otherwise = go vars (x : acc) xs

      isVar :: String -> Bool
      isVar []    = False
      isVar (x:_) = isLower x

      var :: Int -> String
      var n = "a" ++ show n

-- | Repeatedly call lex
tokenize :: String -> [String]
tokenize [] = [[]]
tokenize xs = case lex xs of
                [(token, xs')] -> token : tokenize xs'
                _ -> error "tokenize failed"

{------------------------------------------------------------------------------
  Abstract away versions
------------------------------------------------------------------------------}

class IgnoreVersions a where
  ignoreVersions :: a -> a

instance IgnoreVersions String where
  ignoreVersions s = subRegex (mkRegex versionRegexp) s "X.Y.Z"
    where
      versionRegexp :: String
      versionRegexp = "[0-9]+(\\.[0-9]+)+"

instance IgnoreVersions a => IgnoreVersions [a] where
  ignoreVersions = map ignoreVersions

instance IgnoreVersions a => IgnoreVersions (Maybe a) where
  ignoreVersions = fmap ignoreVersions

instance IgnoreVersions BSSC.ByteString where
  ignoreVersions = BSSC.pack . ignoreVersions . BSSC.unpack

instance IgnoreVersions Text where
  ignoreVersions = Text.pack . ignoreVersions . Text.unpack

instance IgnoreVersions Import where
  ignoreVersions Import{..} = Import {
      importModule    = ignoreVersions importModule
    , importPackage   = importPackage
    , importQualified = importQualified
    , importImplicit  = importImplicit
    , importAs        = importAs
    , importEntities  = importEntities
    }

instance IgnoreVersions ModuleId where
  ignoreVersions ModuleId{..} = ModuleId {
      moduleName    = moduleName
    , modulePackage = ignoreVersions modulePackage
    }

instance IgnoreVersions PackageId where
  ignoreVersions PackageId{..} = PackageId {
      packageName    = packageName
    , packageVersion = ignoreVersions packageVersion
    }

{------------------------------------------------------------------------------
  Known problems
------------------------------------------------------------------------------}

-- | Which ghc versions are affected by these problems?
-- ([] if the bug is unrelated to the GHC version)
knownProblems :: [(String, [GhcVersion])]
knownProblems = [
    -- https://github.com/fpco/ide-backend/issues/32
    -- TODO: In 7.8 the error message does not include a filepath at all,
    -- so the error does not crop up. I don't know if this is true for _all_
    -- errors or just for this particular one (I tried a few but didn't see
    -- filepaths in any of them).
    ("#32", [GHC742])
  ]

_fixme :: IdeSession -> String -> IO () -> IO ()
_fixme session bug io = do
  version <- getGhcVersion session
  let mAllAffected = lookup bug knownProblems
      isAffected   = case mAllAffected of
                       Nothing          -> False
                       Just allAffected -> null allAffected
                                        || version `elem` allAffected

  mErr <- Ex.catch (io >> return Nothing) $ \e ->
            case Ex.fromException e of
              Just (HUnitFailure err) -> return (Just err)
              Nothing -> return (Just $ show e)

  case mErr of
    Just err -> unless isAffected $
                  Ex.throwIO . userError $ "Unexpected failure: " ++ err
    Nothing  -> when isAffected $
                  Ex.throwIO . userError $ "Unexpected success"
                                        ++ " (expected " ++ bug ++ ")"
