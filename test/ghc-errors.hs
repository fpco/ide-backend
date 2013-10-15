{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module Main (main) where

import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import qualified Control.Exception as Ex
import Control.Monad
import qualified Data.ByteString.Char8 as BSSC (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BSLC (null, pack, unpack)
import qualified Data.ByteString.Lazy.UTF8 as BSL8 (fromString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sort)
import qualified Data.List as List
import Data.Maybe (fromJust)
import Data.Monoid (mconcat, mempty, (<>))
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude hiding (mod, span)
import System.Directory
import qualified System.Environment as System.Environment
import System.Exit (ExitCode (..))
import System.FilePath
import System.FilePath.Find (always, extension, find)
import System.IO.Temp (withTempDirectory)
import System.Process (readProcess)
import qualified System.Process as Process
import System.Random (randomRIO)
import Text.Regex (mkRegex, subRegex)
import Debug.Trace (traceEventIO)
import System.Timeout (timeout)

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertBool, assertEqual, assertFailure, (@?=))

import Debug
import IdeSession
import TestTools

-- Tests using various functions of the IdeSession API
-- and a variety of small test Haskell projects.

-- | Update the session with all modules of the given directory.
getModulesFrom :: IdeSession -> FilePath -> IO (IdeSessionUpdate, [FilePath])
getModulesFrom session originalSourcesDir = do
  sourcesDir <- getSourcesDir session
  debug dVerbosity $ "\nCopying files from: " ++ originalSourcesDir
                     ++ " to: " ++ sourcesDir
  -- Send the source files from 'originalSourcesDir' to 'configSourcesDir'
  -- using the IdeSession's update mechanism.
  originalFiles <- find always
                        ((`elem` hsExtensions) `liftM` extension)
                        originalSourcesDir
  let originalUpdate = updateCodeGeneration False
                    <> (mconcat $ map updateModuleFromFile originalFiles)
  return (originalUpdate, originalFiles)

getModules :: IdeSession -> IO (IdeSessionUpdate, [FilePath])
getModules session = do
  sourcesDir <- getSourcesDir session
  getModulesFrom session sourcesDir

loadModulesFrom :: IdeSession -> FilePath -> IO ()
loadModulesFrom session originalSourcesDir = do
  (originalUpdate, lm) <- getModulesFrom session originalSourcesDir
  updateSessionD session originalUpdate (length lm)

ifIdeBackendHaddockTestsEnabled :: SessionConfig -> (IdeSession -> IO ()) -> IO ()
ifIdeBackendHaddockTestsEnabled sessionConfig io = do
  wtsystem <- (System.Environment.getEnv "IDE_BACKEND_DISABLE_HADDOCK_TESTS")
               `Ex.catch` (\(_ :: Ex.IOException) -> return "1")
  unless (wtsystem == "0" || wtsystem == "False") $
    withSession sessionConfig io

-- | Run the specified action with a new IDE session, configured to use a
-- temporary directory
withSession :: SessionConfig -> (IdeSession -> IO a) -> IO a
withSession = withSession' defaultSessionInitParams

withSession' :: SessionInitParams -> SessionConfig -> (IdeSession -> IO a) -> IO a
withSession' initParams config io = do
  slashTmp <- getTemporaryDirectory
  withTempDirectory slashTmp "ide-backend-test." $ \tempDir -> do
    -- Only actually use the temp dir if it's not specifically overriden
    let config' = config {
            configDir = if configDir config == configDir defaultSessionConfig
                          then tempDir
                          else configDir config
          }
    Ex.bracket (initSession initParams config') tryShutdownSession io
  where
    tryShutdownSession session = do
      mDidShutdown <- timeout 2000000 $ shutdownSession session
      case mDidShutdown of
        Just () -> return ()
	Nothing -> putStrLn "WARNING: Failed to shutdown session (timeout)"

withOpts :: [String] -> SessionConfig
withOpts opts =
  defaultSessionConfig {
      configStaticOpts = opts
    }

-- Set of api calls and checks to perform on each project.
--
-- TODO: we need much more tests to recover the functionality of the old,
-- undreadable set, and then we need to much more to test all API functions.
-- E.g., check that the values of Progress do not exceeed the number of files.
-- Also, check ModuleDelete and all the DataFileChange constructors,
-- getSourceModule an getDataFile.
multipleTests :: [(String, IdeSession -> IdeSessionUpdate -> [String] -> Assertion)]
multipleTests =
  [ ( "Overwrite with error"
    , \session originalUpdate lm -> do
        updateSessionD session originalUpdate (length lm)
        -- No errors in the original test code.
        assertNoErrors session
        -- Overwrite one of the copied files.
        (_, ms) <- getModules session
        let update = loadModule (head ms) "a = unknownX"
        updateSessionD session update 1  -- at most 1 recompiled
        assertSourceErrors' session ["Not in scope: `unknownX'"]
    )
  , ( "Overwrite with the same module name in all files"
    , \session originalUpdate lm -> do
        let upd m =
              updateModule m (BSLC.pack "module Wrong where\na = 1")
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
        updateSessionD session update1 1
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
              <> updateModule
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
        case result of
          RunOk _ -> assertEqual "" output (BSLC.pack "\"test run\"\n")
          RunProgException _ex ->
            assertFailure "Unexpected exception raised by the running code."
          RunGhcException ex  ->
            assertFailure $ "Manually corrected code not run successfully: "
                            ++ show ex
          RunForceCancelled ->
            assertFailure "Unexpected force-cancel"
      )
    , ( "Make sure deleting modules removes them from the directory"
      , \session originalUpdate lm -> do
        updateSessionD session originalUpdate (length lm)
        let updateDel = mconcat $ map updateModuleDelete lm
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
        let update2 = mconcat $ map updateModuleDelete lm
        updateSessionD session update2 0  -- if any file missing, would yell
        assertNoErrors session
        let update3 = updateCodeGeneration True
        updateSessionD session update3 0  -- 0: nothing to generate code from
        exitCodeBefore <- getGhcExitCode serverBefore
        assertEqual "exitCodeBefore" (Just (ExitFailure 1)) exitCodeBefore
      )
  ]

syntheticTests :: [(String, Assertion)]
syntheticTests =
  [ ( "Maintain list of compiled modules I"
    , withSession defaultSessionConfig $ \session -> do
        let assEq name goodMods =
              assertEqual name (map Text.pack goodMods)
                =<< (sort <$> getLoadedModules session)
        updateSessionD session (loadModule "XXX.hs" "a = 5") 1
        assEq "XXX" ["XXX"]
        updateSessionD session (loadModule "A.hs" "a = 5") 1
        assEq "[m1]" ["A", "XXX"]
        updateSessionD session (loadModule "A2.hs" "import A\na2 = A.a") 1
        assEq "[m1, m2]" ["A", "A2", "XXX"]
        updateSessionD session (loadModule "A3.hs" "") 1
        assEq "[m1, m2, m3]" ["A", "A2", "A3", "XXX"]
        updateSessionD session (loadModule "Wrong.hs" "import A4\na2 = A4.a + 1") 1
        assEq "wrong1" ["A", "A2", "A3", "XXX"]
        updateSessionD session (loadModule "Wrong.hs" "import A\na2 = A.a + c") 1
        assEq "wrong2" ["A", "A2", "A3", "XXX"]
        updateSessionD session (loadModule "A.hs" "a = c") 1
        -- Module "A" is compiled before "Wrong", fails, so it's invalidated
        -- and all modules that depend on it are invalidated. Module "Wrong"
        -- is never compiled.
        assEq "wrong3" ["A3", "XXX"]
    )
  , ( "Maintain list of compiled modules II"
    , withSession defaultSessionConfig $ \session -> do
        let assEq name goodMods =
              assertEqual name (map Text.pack goodMods)
                =<< (sort <$> getLoadedModules session)
        updateSessionD session (loadModule "XXX.hs" "a = 5") 1
        assEq "XXX" ["XXX"]
        updateSessionD session (loadModule "A.hs" "a = 5") 1
        assEq "[m1]" ["A", "XXX"]
        updateSessionD session (loadModule "A2.hs" "import A\na2 = A.a") 1
        assEq "[m1, m2]" ["A", "A2", "XXX"]
        updateSessionD session (loadModule "A3.hs" "") 1
        assEq "[m1, m2, m3]" ["A", "A2", "A3", "XXX"]
        updateSessionD session (loadModule "Wrong.hs" "import A4\na2 = A4.a + 1") 1
        assEq "wrong1" ["A", "A2", "A3", "XXX"]
        -- This has to be disabled to get the different outcome below:
          -- updateSessionD session (loadModule m4 "import A\na2 = A.a + c") 1
          -- assEq "wrong2" [m1, m2, m3, xxx]
        -- We get this differemnt outcome both in original 7.4.2
        -- and after the GHC#7231 fix. It's probably caused by target
        -- Wrong place before or after target "A" depending on what kind
        -- of error Wrong had. This is strange, but not incorrect.
        updateSessionD session (loadModule "A.hs" "a = c") 1
        -- Module "Wrong" is compiled first here, fails, so module "A"
        -- is never comipiled, so it's not invalidated.
        assEq "wrong3" ["A", "A2", "A3", "XXX"]
    )
  , ( "Maintain list of compiled modules III"
    , withSession defaultSessionConfig $ \session -> do
        let assEq name goodMods =
              assertEqual name (sort (map Text.pack goodMods))
                =<< getLoadedModules session
        updateSessionD session (loadModule "A.hs" "a = 5") 1
        assEq "1 [A]" ["A"]
        updateSessionD session (loadModule "A.hs" "a = 5 + True") 1
        assEq "1 []" []
        updateSessionD session (loadModule "A.hs" "a = 5") 1
        assEq "2 [A]" ["A"]
        updateSessionD session (loadModule "A.hs" "a = 5 + wrong") 1
        assEq "2 []" []
        updateSessionD session (loadModule "A.hs" "a = 5") 1
        assEq "3 [A]" ["A"]
        updateSessionD session (loadModule "A.hs" "import WRONG\na = 5") 1
        assEq "3 [A]; wrong imports do not unload old modules" ["A"]
        updateSessionD session (loadModule "A.hs" "a = 5 + True") 1
        assEq "3 []" []
    )
  , ( "Duplicate shutdown"
    , withSession defaultSessionConfig $ \session ->
        -- withConfiguredSession will shutdown the session as well
        shutdownSession session
    )
  , ( "Permit a session within a session and duplicated shutdownSession"
    , withSession defaultSessionConfig $ \session -> do
        loadModulesFrom session "test/ABnoError"
        config <- getSessionConfig session
        let tweakConfig :: Int -> SessionConfig -> IO SessionConfig
            tweakConfig n cfg@SessionConfig{configDir} = do
              let newDir = configDir </> "new" ++ show n
              createDirectory newDir
              return cfg { configDir = newDir }
        config2 <- tweakConfig 2 config
        config3 <- tweakConfig 3 config
        config4 <- tweakConfig 4 config
        config5 <- tweakConfig 5 config
        withSession config2 $ \s2 -> do
         withSession config3 $ \s3 -> do
          withSession config4 $ \_s4 -> do
           let update2 = loadModule "M.hs" "a = unknownX"
           updateSessionD s2 update2 1
           assertOneError s2
           withSession config5 $ \s5 -> do
            let update3 = loadModule "M.hs" "a = 3"
            updateSessionD s3 update3 1
            assertNoErrors session
            shutdownSession s5 -- <-- duplicate "nested" shutdown
    )
  , ( "Compile a project: A depends on B, error in A"
    , withSession defaultSessionConfig $ \session -> do
        loadModulesFrom session "test/AerrorB"
        assertSourceErrors session [[(Just "A.hs", "No instance for (Num (IO ()))")]]
     )
  , ( "Compile a project: A depends on B, error in B"
    , withSession defaultSessionConfig $ \session -> do
        loadModulesFrom session "test/ABerror"
        assertSourceErrors session [[(Just "B.hs", "No instance for (Num (IO ()))")]]
    )
  , ( "Compile and run a project with some .lhs files"
    , withSession defaultSessionConfig $ \session -> do
        loadModulesFrom session "test/compiler/utils"
        assertNoErrors session
        let update2 = updateCodeGeneration True
        updateSessionD session update2 4
        assertNoErrors session
        runActions <- runStmt session "Maybes" "main"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" output (BSLC.pack "False\n")
          _ -> assertFailure "Unexpected snippet run result"
    )
  , ( "Build executable from some .lhs files"
    , withSession defaultSessionConfig $ \session -> do
        setCurrentDirectory "test/compiler/utils"
        loadModulesFrom session "."
        setCurrentDirectory "../../../"
        assertNoErrors session
        status0 <- getBuildExeStatus session
        assertEqual "before exe build" Nothing status0
        distDir <- getDistDir session

        let m = "Maybes"
            upd = buildExe [(Text.pack m, m <.> "lhs")]
        updateSessionD session upd 4
        status1 <- getBuildExeStatus session
        assertEqual "after exe build" (Just ExitSuccess) status1
        out <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "Maybes exe output"
                    "False\n"
                    out

        let m2 = "Exception"
            upd2 = buildExe [(Text.pack m2, m2 <.> "hs")]
        updateSessionD session upd2 4
        out2 <- readProcess (distDir </> "build" </> m2 </> m2) [] []
        assertEqual "Exception exe output"
                    ""
                    out2

        let m3 = "Main"
            upd3 = buildExe [(Text.pack m3, "Subdir" </> m3 <.> "lhs")]
        updateSessionD session upd3 4
        out3 <- readProcess (distDir </> "build" </> m3 </> m3) [] []
        assertEqual "Main exe output"
                    ""
                    out3

        let upd4 = buildExe [(Text.pack m, m <.> "lhs")]
        updateSessionD session upd4 4
        status4 <- getBuildExeStatus session
        assertEqual "after all exe builds" (Just ExitSuccess) status4
    )
  , ( "Build haddocks from some .lhs files"
    , withSession defaultSessionConfig $ \session -> do
        status0 <- getBuildDocStatus session
        assertEqual "before module loading" Nothing status0
        setCurrentDirectory "test/compiler/utils"
        loadModulesFrom session "."
        setCurrentDirectory "../../../"
        assertNoErrors session
        let upd = buildDoc
        updateSessionD session upd 4
        status1 <- getBuildDocStatus session
        assertEqual "after doc build" (Just ExitSuccess) status1
        distDir <- getDistDir session
        indexExists <- doesFileExist $ distDir </> "doc/html/main/index.html"
        assertBool ".lhs haddock files" indexExists
    )
  , ( "Build haddocks and fail"
    , withSession defaultSessionConfig $ \session -> do
        setCurrentDirectory "test/ABerror"
        loadModulesFrom session "."
        setCurrentDirectory "../.."
        assertOneError session
        let upd = buildDoc
        -- Note that the stderr log file here is empty, but exit code is 1:
        updateSessionD session upd 4
        status1 <- getBuildDocStatus session
        assertEqual "failure after doc build" (Just $ ExitFailure 1) status1
    )
  , ( "Use cabal macro MIN_VERSION for a package we really depend on"
    , withSession (withOpts ["-XCPP"]) $ \session -> do
        macros <- getCabalMacros session
        assertBool "Main with cabal macro exe output" (not $ BSLC.null macros)
        -- assertEqual "Main with cabal macro exe output" (BSLC.pack "") macros
        let update = updateModule "Main.hs" $ BSLC.pack $ unlines
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
            upd = buildExe [(Text.pack m, "Main.hs")]
        updateSessionD session upd 4
        distDir <- getDistDir session
        mOut <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "Main with cabal macro exe output" "5\n" mOut
    )
  , ( "Use cabal macro MIN_VERSION for a package we don't really depend on"
    , withSession (withOpts ["-XCPP"]) $ \session -> do
        let update = updateModule "Main.hs" $ BSLC.pack $ unlines
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
        -- TODO:
        -- let m = "Main"
        --     upd = buildExe [(Text.pack m, "Main.hs")]
        -- updateSessionD session upd 4
        -- distDir <- getDistDir session
        -- mOut <- readProcess (distDir </> "build" </> m </> m) [] []
        -- assertEqual "Main with cabal macro exe output" "5\n" mOut
    )
  , ( "Use cabal macro VERSION by checking if defined"
    , withSession (withOpts ["-XCPP"]) $ \session -> do
        macros <- getCabalMacros session
        assertBool "M with cabal macro exe output" (not $ BSLC.null macros)
        let update = updateModule "M.hs" $ BSLC.pack $ unlines
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
            upd = buildExe [(Text.pack m, "M.hs")]
        updateSessionD session upd 4
        distDir <- getDistDir session
        mOut <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "M with cabal macro exe output" "5\n" mOut
    )
  , ( "Use cabal macro VERSION by including the macros file"
    , withSession (withOpts ["-XCPP"]) $ \session -> do
        macros <- getCabalMacros session
        assertBool "M with cabal macro exe output" (not $ BSLC.null macros)
        let update = updateModule "M.hs" $ BSLC.pack $ unlines
              [ "module M where"
              , "#include \"cabal_macros.h\""
              , "main = print $ VERSION_base == \"foo\""
              ]
        updateSessionD session (update <> updateCodeGeneration True) 1
        assertNoErrors session
        runActions <- runStmt session "M" "main"
        (output, _) <- runWaitAll runActions
        assertEqual "result of ifdefed print 5" (BSLC.pack "False\n") output
        let m = "M"
            upd = buildExe [(Text.pack m, "M.hs")]
        updateSessionD session upd 4
        distDir <- getDistDir session
        mOut <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "M with cabal macro exe output" "False\n" mOut
    )
  , ( "Caching cabal macros"
    , do macros <- withSession defaultSessionConfig getCabalMacros
         let initParams = defaultSessionInitParams {
                              sessionInitCabalMacros = Just macros
                            }
         withSession' initParams (withOpts ["-XCPP"]) $ \session -> do
           let update = (updateCodeGeneration True)
                     <> (updateModule "Main.hs" $ BSLC.pack $ unlines [
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
         let customMacros = BSLC.pack "#define HELLO 1"
             initParams'  = initParams {
                                 sessionInitCabalMacros = Just customMacros
                               }
         withSession' initParams' (withOpts ["-XCPP"]) $ \session -> do
           let update = (updateCodeGeneration True)
                     <> (updateModule "Main.hs" $ BSLC.pack $ unlines [
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
    )
  , ( "Reject a program requiring -XNamedFieldPuns, then set the option"
    , withSession (withOpts []) $ \session -> do
        let update = updateDataFileFromFile "EventLogFormat.h"
                                            "test/Puns/EventLogFormat.h"
        updateSessionD session update 0
        loadModulesFrom session "test/Puns"
        -- Exact errors depend on order of loaded modules, etc.
        -- Anyway, right not the test is partially bit-rotted,
        -- because the .h files are no longer available in the source dir
        -- and the data dir can't be set to the source dir. See #10.
        msgs <- getSourceErrors session
        assertSomeErrors msgs
        let punOpts = ["-XNamedFieldPuns", "-XRecordWildCards"]
            update2 = updateGhcOptions (Just punOpts)
        (_, lm) <- getModules session
        updateSessionD session update2 (length lm)
        msgs2 <- getSourceErrors session
        assertSomeErrors msgs2
-- TODO: the hack with supplying .h as a data file no longer works;
-- we need a proper support for .h
--      assertNoErrors msgs2
    )
  , ( "Build licenses from NamedFieldPuns"
    , withSession (withOpts []) $ \session -> do
        loadModulesFrom session "test/Puns"
        let upd = buildLicenses "test/Puns/cabals"
        updateSessionD session upd 99
        msgs <- getSourceErrors session
        assertSomeErrors msgs
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
        assertEqual "licenses length" 27142 (length licenses)
    )
  , ( "Build licenses with wrong cabal files and fail"
    , withSession (withOpts []) $ \session -> do
        loadModulesFrom session "test/Puns"
        let upd = buildLicenses "test/Puns/cabals/parse_error"
        updateSessionD session upd 99
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
    , withSession defaultSessionConfig $ \session -> do
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
        let update4 = updateDataFile "datafile.dat"
                                     (BSLC.pack "new content")
                      <> update2
        updateSessionD session update4 1
        runActions2 <- runStmt session "Main" "main"
        (output2, _) <- runWaitAll runActions2
        assertEqual "compare new content"
          (BSLC.pack "new content\n") output2
    )
  , ( "Test CWD in executable building"
    , withSession (withOpts []) $ \session -> do
        let update = updateCodeGeneration True
                     <> updateDataFile "test.txt" (BSLC.pack "test data")
        let update2 = updateModule "Main.hs" $ BSLC.pack $ unlines
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
            upd = buildExe [(Text.pack m, "Main.hs")]
        updateSessionD session upd 4
        distDir <- getDistDir session
        out <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "CWD exe output" (BSLC.unpack output) out
    )
{- Now that we always load the RTS, we're never in this situation
  , ("Reject getSourceErrors without updateSession"
    , withSession defaultSessionConfig $ \session ->
        assertRaises "getSourceErrors session"
          (== userError "This session state does not admit queries.")
          (getSourceErrors session)
    )
-}
  , ("Reject updateSession after shutdownSession"
    , withSession defaultSessionConfig $ \session -> do
        shutdownSession session
        assertRaises "updateSessionD session mempty"
          (== userError "Session already shut down.")
          (updateSessionD session mempty 0)
    )
  , ("Reject getSourceErrors after shutdownSession"
    , withSession defaultSessionConfig $ \session -> do
        shutdownSession session
        assertRaises "getSourceErrors session"
          (== userError "Session already shut down.")
          (getSourceErrors session)
    )
  , ("Reject runStmt after shutdownSession"
    , withSession defaultSessionConfig $ \session -> do
        shutdownSession session
        assertRaises "runStmt session Main main"
          (== userError "Session already shut down.")
          (runStmt session "Main" "main")
    )
  , ( "Test recursive modules"
    , withSession defaultSessionConfig $ \session -> do
        loadModulesFrom session "test/bootMods"
        -- Fails, because special support is needed, similar to .h files.
        -- Proabably, the .hs-boot files should be copied to the src dir,
        -- but not made GHC.load targets.
        assertOneError session
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
        case result of
          RunOk _ -> assertEqual "" output (BSLC.pack "(True,43)\n")
          _ -> assertFailure "Unexpected snippet run result"
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
            upd = buildExe [(Text.pack m, "TH/TH.hs")]
        updateSessionD session upd 4
        distDir <- getDistDir session
        out <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "TH.TH exe output"
                    "(True,43)\n"
                    out
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
        updateSessionD session upd 4
        distDir <- getDistDir session
        indexExists <- doesFileExist $ distDir </> "doc/html/main/index.html"
        assertBool "TH.TH haddock files" indexExists

    )
  , ( "Test CPP: ifdefed module header"
    , withSession (withOpts ["-XCPP"]) $ \session -> do
        let update = updateModule "Good.hs" $ BSLC.pack $ unlines
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
        idInfo <- getSpanInfo session
        assertIdInfo idInfo "Good" (8,1,8,2) "x (VarName) :: [a] defined in main:Good at Good.hs@8:1-8:2 (binding occurrence)"
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
    , withSession defaultSessionConfig $ \session -> do
        let update = updateModule "M.hs"
                                  (BSLC.pack "module very-wrong where")
        updateSessionD session update 1
        assertSourceErrors' session ["parse error on input `very'\n"]
        let update2 = updateModule "M.hs"
                                   (BSLC.pack "module M.1.2.3.8.T where")
        updateSessionD session update2 1
        assertSourceErrors' session ["parse error on input `.'\n"]
    )
  , ( "Interrupt runStmt (after 1 sec)"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
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
          Right (RunProgException "AsyncException: user interrupt") -> return ()
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
    )
  , ( "Interrupt runStmt (immediately)"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
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
          Right (RunProgException "AsyncException: user interrupt") -> return ()
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
    )
  , ( "Interrupt runStmt (black hole; after 1 sec)"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "loop :: IO ()"
                    , "loop = loop"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "loop"
        threadDelay 1000000
        interrupt runActions
        resOrEx <- runWait runActions
        case resOrEx of
          Right (RunProgException "AsyncException: user interrupt") -> return ()
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
    )
  , ( "Capture stdout (single putStrLn)"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "hello :: IO ()"
                    , "hello = putStrLn \"Hello World\""
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" (BSLC.pack "Hello World\n") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Capture stdout (single putStr)"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "hello :: IO ()"
                    , "hello = putStr \"Hello World\""
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" (BSLC.pack "Hello World") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Capture stdout (single putStr with delay)"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
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
        case result of
          RunOk _ -> assertEqual "" (BSLC.pack "hellohi") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Capture stdout (multiple putStrLn)"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
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
        case result of
          RunOk _ -> assertEqual "" (BSLC.pack "Hello World 1\nHello World 2\nHello World 3\n") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Capture stdout (mixed putStr and putStrLn)"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
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
        case result of
          RunOk _ -> assertEqual "" (BSLC.pack "Hello World 1\nHello World 2Hello World 3\n") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Capture stdin (simple echo process)"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "echo :: IO ()"
                    , "echo = getLine >>= putStrLn"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "echo"
        supplyStdin runActions (BSSC.pack "ECHO!\n")
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" (BSLC.pack "ECHO!\n") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Capture stdin (infinite echo process)"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import System.IO"
                    , "import Control.Monad"
                    , "echo :: IO ()"
                    , "echo = do hSetBuffering stdout LineBuffering"
                    , "          forever $ getLine >>= putStrLn"
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
           result <- runWait runActions
           case result of
             Right (RunProgException "AsyncException: user interrupt") -> return ()
             _ -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Two calls to runStmt"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
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
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "ECHO!\n") output
             _       -> assertFailure $ "Unexpected run result: " ++ show result

        do runActions <- runStmt session "M" "echoReverse"
           supplyStdin runActions (BSSC.pack "!OHCE\n")
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "ECHO!\n") output
             _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Make sure we can terminate the IDE session when code is running"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "echo :: IO ()"
                    , "echo = (getLine >>= putStrLn) >> echo"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        _runActions <- runStmt session "M" "echo"
        return ()
     )
  , ( "Capture stderr"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import System.IO"
                    , "hello :: IO ()"
                    , "hello = hPutStrLn stderr \"Hello World\""
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" (BSLC.pack "Hello World\n") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Merge stdout and stderr"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
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
        case result of
          RunOk _ -> assertEqual "" (BSLC.pack expectedOutput) output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Set environment variables"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
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
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "Value1") output
             _       -> assertFailure $ "Unexpected result " ++ show result
        do runActions <- runStmt session "M" "printBar"
           (_, result) <- runWaitAll runActions
           case result of
             RunProgException ex -> assertEqual "" ex "IOException: Bar: getEnv: does not exist (no environment variable)"
             _ -> assertFailure $ "Unexpected result " ++ show result

        -- Update Bar, leave Foo defined
        updateSession session (updateEnv "Bar" (Just "Value2")) (\_ -> return ())
        do runActions <- runStmt session "M" "printFoo"
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "Value1") output
             _       -> assertFailure $ "Unexpected result " ++ show result
        do runActions <- runStmt session "M" "printBar"
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "Value2") output
             _       -> assertFailure $ "Unexpected result " ++ show result

        -- Unset Foo, leave Bar defined
        updateSession session (updateEnv "Foo" Nothing) (\_ -> return ())
        do runActions <- runStmt session "M" "printFoo"
           (_, result) <- runWaitAll runActions
           case result of
             RunProgException ex -> assertEqual "" ex "IOException: Foo: getEnv: does not exist (no environment variable)"
             _ -> assertFailure $ "Unexpected result " ++ show result
        do runActions <- runStmt session "M" "printBar"
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "Value2") output
             _       -> assertFailure $ "Unexpected result " ++ show result
    )
  , ( "Update during run"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
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
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "{-# OPTIONS_GHC -Wall #-}"
                    , "module M where"
                    , "loop = loop"
                    ])
        updateSessionD session upd 1
        assertSourceErrors' session ["Warning: Top-level binding with no type signature"]

        msgs1 <- getSourceErrors session
        _ract <- runStmt session "M" "loop"
        msgs2 <- getSourceErrors session
        assertEqual "Running code does not affect getSourceErrors" msgs1 msgs2
    )
  , ( "getLoadedModules during run"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
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
    , withSession defaultSessionConfig $ \session -> do
        updateSession session (updateCodeGeneration True) (\_ -> return ())
        let upd1 = updateModule "Main.hs" . BSLC.pack . unlines $
                     [ "import Control.Monad"
                     , "main = forever $ print 1"
                     ]
            upd2 = updateModule "Main.hs" . BSLC.pack . unlines $
                     [ "main = print 1234" ]

        do updateSessionD session upd1 1
           runActions <- runStmt session "Main" "main"
           interrupt runActions
           randomRIO (0, 1000000) >>= threadDelay -- Wait between 0 and 1sec
           void $ runWaitAll runActions

        do updateSessionD session upd2 1
           runActions <- runStmt session "Main" "main"
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "1234\n") output
             _       -> assertFailure $ "Unexpected result: " ++ show result
    )
  , ( "Restart session (snippet doesn't swallow exceptions; after .1 sec)"
    , restartRun [ "module M where"
                 , "loop :: IO ()"
                 , "loop = loop"
                 ] (ExitFailure 1)
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
                 ] (ExitFailure 1)
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
                 ] (ExitFailure 1)
    )
  , ( "Restart session (evil snippet with infinite stack of exception handlers; after .1 sec)"
    , restartRun [ "module M where"
                 , ""
                 , "import qualified Control.Exception as Ex"
                 , ""
                 , "loop :: IO ()"
                 , "loop = Ex.catch loop $ \\e -> let _ = e :: Ex.SomeException in loop"
                 ] (ExitFailure 1)
    )
  , ( "Make sure environment is restored after session restart"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import System.Environment (getEnv)"
                    , "printFoo :: IO ()"
                    , "printFoo = getEnv \"Foo\" >>= putStr"
                    ])

        -- Set environment
        updateSession session (updateEnv "Foo" (Just "Value1")) (\_ -> return ())

        -- Compile and run the code on the first server
        updateSessionD session upd 1
        assertNoErrors session
        do runActions <- runStmt session "M" "printFoo"
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "Value1") output
             _       -> assertFailure $ "Unexpected result " ++ show result

        -- Start a new server
        serverBefore <- getGhcServer session
        restartSession session Nothing

        -- Compile the code on the new server
        updateSessionD session upd 1
        assertNoErrors session

        -- Make sure the old server exited
        exitCodeBefore <- getGhcExitCode serverBefore
        assertEqual "exitCodeBefore" (Just (ExitFailure 1)) exitCodeBefore

        -- Make sure the new server is still alive
        serverAfter <- getGhcServer session
        exitCodeAfter <- getGhcExitCode serverAfter
        assertEqual "exitCodeAfter" Nothing exitCodeAfter

        -- Make sure environment is restored
        do runActions <- runStmt session "M" "printFoo"
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "Value1") output
             _       -> assertFailure $ "Unexpected result " ++ show result
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
  , ( "Use relative path in SessionConfig"
    , do withTempDirectory "." "ide-backend-test." $ \fullPath -> do
           relativePath <- makeRelativeToCurrentDirectory fullPath
           let sessionConfig = defaultSessionConfig {
                                   configDir = relativePath
                                 }
           withSession sessionConfig $ \session -> do
             let upd = (updateCodeGeneration True)
                    <> (updateModule "M.hs" . BSLC.pack . unlines $
                         [ "module M where"
                         , "hello :: IO ()"
                         , "hello = putStr \"Hello World\""
                         ])
             updateSessionD session upd 1
             assertNoErrors session
             runActions <- runStmt session "M" "hello"
             (output, result) <- runWaitAll runActions
             case result of
               RunOk _ -> assertEqual "" (BSLC.pack "Hello World") output
               _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Call runWait after termination (normal termination)"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "hello :: IO ()"
                    , "hello = putStrLn \"Hello World\""
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" (BSLC.pack "Hello World\n") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
        result' <- runWait runActions
        case result' of
          Right (RunOk _) -> return ()
          _ -> assertFailure $ "Unexpected run result in repeat call: " ++ show result'
    )
  , ( "Call runWait after termination (interrupted)"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
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
          Right (RunProgException "AsyncException: user interrupt") -> return ()
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
        resOrEx' <- runWait runActions
        case resOrEx' of
          Right (RunProgException "AsyncException: user interrupt") -> return ()
          _ -> assertFailure $ "Unexpected run result in repeat call: " ++ show resOrEx'
    )
  , ( "Call runWait after termination (restarted session)"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import Control.Concurrent (threadDelay)"
                    , "loop :: IO ()"
                    , "loop = threadDelay 100000 >> loop"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "loop"
        threadDelay 1000000
        restartSession session Nothing
        resOrEx <- runWait runActions
        case resOrEx of
          Right RunForceCancelled -> return ()
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
        resOrEx' <- runWait runActions
        case resOrEx' of
          Right RunForceCancelled -> return ()
          _ -> assertFailure $ "Unexpected run result in repeat call: " ++ show resOrEx'
    )
  , ( "Call runWait after termination (started new snippet in meantime)"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
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
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "Hello World\n") output
             _       -> assertFailure $ "Unexpected run result: " ++ show result

        -- Start second snippet
        runActions2 <- runStmt session "M" "slowHello"

        -- While it is running, call runWait again on the old runActions, make
        -- sure it's still the same
        do result <- runWait runActions1
           case result of
             Right (RunOk _) -> return ()
             _ -> assertFailure $ "Unexpected run result in repeat call: " ++ show result

        -- Make sure that a call to 'runStmt' throws an exception
        -- (because we are still in running state)
        assertRaises "runStmt during running code"
          (== userError "Cannot run code concurrently")
          (runStmt session "M" "hello")

        -- Now call runWait on the *new* runActions and make sure we
        -- get the right result
        do (output, result) <- runWaitAll runActions2
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "Oh, hello\n") output
             _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Don't recompile unnecessarily (single module)"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "A.hs" . BSLC.pack . unlines $
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
    , withSession defaultSessionConfig $ \session -> do
        -- 'updA' is defined so that the interface of 'updA n' is different
        -- to the interface of 'updA m' (with n /= m)
        let updA n = updateModule "A.hs" . BSLC.pack . unlines $
                       [ "module A where"
                       , "import B"
                       ]
                      ++
                       [ "a" ++ show i ++ " = b" ++ show i
                       | i <- [0 .. n :: Int]
                       ]
        let updB n = updateModule "B.hs" . BSLC.pack . unlines $
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
    , withSession defaultSessionConfig $ \session -> do
        let updates2 = mconcat
                [ updateCodeGeneration True
                , updateModule "Main.hs" (BSLC.pack "import System.IO\nmain = hClose stdin")
                ]
        updateSession session updates2 $ const $ return ()
        ra2 <- runStmt session "Main" "main"
        out2b <- runWait ra2
        case out2b of
          Right (RunOk _) -> return ()
          _ -> assertFailure $ "Unexpected result " ++ show out2b

        let updates3 =
              updateModule "Main.hs" (BSLC.pack "main = getLine >>= putStrLn")
        updateSession session updates3 $ const $ return ()
        ra3 <- runStmt session "Main" "main"
        supplyStdin ra3 (BSSC.pack "Michael\n")
        (output, out3b) <- runWaitAll ra3
        case out3b of
          RunOk _ -> assertEqual "" (BSLC.pack "Michael\n") output
          _ -> assertFailure $ "Unexpected result " ++ show out3b
    )
  , ( "First snippet closes stdin (interrupted 'interact'); next snippet unaffected"
    , withSession defaultSessionConfig $ \session -> do
        let updates2 = mconcat
                [ updateCodeGeneration True
                , updateModule "Main.hs" (BSLC.pack "main = getContents >>= putStr")
                , updateStdoutBufferMode $ RunLineBuffering Nothing
                ]
        updateSession session updates2 $ const $ return ()
        ra2 <- runStmt session "Main" "main"
        supplyStdin ra2 (BSSC.pack "hello\n")
        out2a <- runWait ra2
        out2a @?= Left (BSSC.pack "hello\n")
        interrupt ra2
        out2b <- runWait ra2
        out2b @?= Right (RunProgException "AsyncException: user interrupt")

        let updates3 = mconcat
                [ updateCodeGeneration True
                , updateModule "Main.hs" (BSLC.pack "main = putStrLn \"Hi!\" >> getLine >> return ()")
                , updateStdoutBufferMode $ RunLineBuffering Nothing
                ]
        updateSession session updates3 $ const $ return ()
        ra3 <- runStmt session "Main" "main"
        out3a <- runWait ra3
        out3a @?= Left (BSSC.pack "Hi!\n")
        supplyStdin ra3 (BSSC.pack "Michael\n")
        out3b <- runWait ra3
        case out3b of
          Right (RunOk _) -> return ()
          _ -> assertFailure $ "Unexpected result " ++ show out3b
    )
  , ( "First snippet closes stdout; next snippet unaffected"
    , withSession defaultSessionConfig $ \session -> do
        let updates2 = mconcat
                [ updateCodeGeneration True
                , updateModule "Main.hs" (BSLC.pack "import System.IO\nmain = hClose stdout")
                ]
        updateSession session updates2 $ const $ return ()
        ra2 <- runStmt session "Main" "main"
        out2b <- runWait ra2
        case out2b of
          Right (RunOk _) -> return ()
          _ -> assertFailure $ "Unexpected result " ++ show out2b

        let updates3 =
              updateModule "Main.hs" (BSLC.pack "main = getLine >>= putStrLn")
        updateSession session updates3 $ const $ return ()
        ra3 <- runStmt session "Main" "main"
        supplyStdin ra3 (BSSC.pack "Michael\n")
        (output, out3b) <- runWaitAll ra3
        case out3b of
          RunOk _ -> assertEqual "" (BSLC.pack "Michael\n") output
          _ -> assertFailure $ "Unexpected result " ++ show out3b
    )
  , ( "First snippet closes stderr; next snippet unaffected"
    , withSession defaultSessionConfig $ \session -> do
        let updates2 = mconcat
                [ updateCodeGeneration True
                , updateModule "Main.hs" (BSLC.pack "import System.IO\nmain = hClose stderr")
                ]
        updateSession session updates2 $ const $ return ()
        ra2 <- runStmt session "Main" "main"
        out2b <- runWait ra2
        case out2b of
          Right (RunOk _) -> return ()
          _ -> assertFailure $ "Unexpected result " ++ show out2b

        let updates3 =
              updateModule "Main.hs" (BSLC.pack "import System.IO\nmain = getLine >>= hPutStrLn stderr")
        updateSession session updates3 $ const $ return ()
        ra3 <- runStmt session "Main" "main"
        supplyStdin ra3 (BSSC.pack "Michael\n")
        (output, out3b) <- runWaitAll ra3
        case out3b of
          RunOk _ -> assertEqual "" (BSLC.pack "Michael\n") output
          _ -> assertFailure $ "Unexpected result " ++ show out3b
    )
  , ( "Snippet closes stderr, using timeout buffering"
    , withSession defaultSessionConfig $ \session -> do
        let upd = mconcat [
                      updateCodeGeneration True
                    , updateStdoutBufferMode $ RunLineBuffering Nothing
                    , updateStderrBufferMode $ RunBlockBuffering (Just 4096) (Just 250000)
                    , updateModule "Main.hs" . BSLC.pack . unlines $ [
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
        case finalResult of
          Right (RunOk _) -> return ()
          _ -> assertFailure $ "Unexpected result " ++ show finalResult
     )
  , ( "Make sure encoding is UTF8"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSL8.fromString . unlines $
                    [ "module M where"
                    , "hello :: IO ()"
                    , "hello = putStrLn \"你好\""
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" (BSL8.fromString "你好\n") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Using something from a different package (no \"Loading package\" msg)"
      -- We pick something from the haskell platform but that doesn't come with ghc itself
      -- https://github.com/haskell/haskell-platform/blob/2012.4.0.0/haskell-platform.cabal
    , withSession (withOpts []) $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSL8.fromString . unlines $
                    [ "module M where"
                    , "import Control.Monad.IO.Class" -- From transformers
                    , "hello :: IO ()"
                    , "hello = liftIO $ print 5"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" (BSL8.fromString "5\n") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Using the FFI (expected failure)"
    , withSession defaultSessionConfig $ \session -> do
        let upd = mconcat [
                updateCodeGeneration True
              , updateModuleFromFile "test/FFI/Main.hs"
              ]
        updateSessionD session upd 1
        assertOneError session
        {-
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" (BSL8.fromString "5\n") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
        -}
    )
  , ( "Build executable from Main"
    , withSession (withOpts []) $ \session -> do
        setCurrentDirectory "test/MainModule"
        loadModulesFrom session "."
        assertNoErrors session
        setCurrentDirectory "../../"
        let m = "Main"
            upd = buildExe [(Text.pack m, "ParFib.hs")]
        updateSessionD session upd 4
        distDir <- getDistDir session
        fibOut <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "ParFib exe output"
                    "running 'A single file with a code to run in parallel' from test/MainModule, which says fib 24 = 75025\n"
                    fibOut
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
            upd = buildExe [(Text.pack m, "ParFib.hs")]
        updateSessionD session upd 4
        distDir <- getDistDir session
        fibOut <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "ParFib exe output"
                    "running 'A single file with a code to run in parallel' from test/MainModule, which says fib 24 = 75025\n"
                    fibOut
    )
  , ( "Build executable from ParFib.Main"
    , withSession (withOpts []) $ \session -> do
        setCurrentDirectory "test/MainModule"
        loadModulesFrom session "."
        assertNoErrors session
        setCurrentDirectory "../../"
        let m = "ParFib.Main"
            upd = buildExe [ (Text.pack m, "ParFib.Main.hs")
                           , (Text.pack "Main", "ParFib.hs") ]
        updateSessionD session upd 4
        --let upd2 = buildExe [(Text.pack "Main", "ParFib.hs")]
        --updateSessionD session upd2 4
        distDir <- getDistDir session
        fibOut <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "ParFib exe output"
                    "running 'A single file with a code to run in parallel' from test/MainModule, which says fib 24 = 75025\n"
                    fibOut
    )
  , ( "Build executable and fail"
    , withSession (withOpts []) $ \session -> do
        setCurrentDirectory "test/MainModule"
        loadModulesFrom session "."
        assertNoErrors session
        setCurrentDirectory "../../"
        let m = "Main"
            upd = buildExe [(Text.pack m, "foooooooooooooooo")]
        status0 <- getBuildExeStatus session
        assertEqual "before exe build" Nothing status0
        updateSessionD session upd 4
        status1 <- getBuildExeStatus session
        assertEqual "failure after exe build" (Just $ ExitFailure 1) status1
    )
  , ( "Build haddocks from ParFib"
    , withSession (withOpts []) $ \session -> do
        setCurrentDirectory "test/MainModule"
        loadModulesFrom session "."
        assertNoErrors session
        setCurrentDirectory "../../"
        let upd = buildDoc
        updateSessionD session upd 4
        distDir <- getDistDir session
        indexExists <- doesFileExist $ distDir </> "doc/html/main/index.html"
        assertBool "ParFib haddock files" indexExists
    )
  , ( "Fail on empty package DB"
    , let config = defaultSessionConfig { configPackageDBStack  = [] }
      in assertRaises ""
           (\e -> e == userError "Invalid package DB stack: []")
           (withSession config $ \_ -> return ())
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
        assertEqual "licenses length" 21409 (length licenses)
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
        assertEqual "licensesErrs length" 507 (length licensesErrs)
        status <- getBuildLicensesStatus session
        assertEqual "after license build" (Just ExitSuccess) status
        licensesWarns <- readFile $ distDir </> "licenses.stdout"
        assertEqual "licensesWarns length" 138 (length licensesWarns)
        licenses <- readFile $ distDir </> "licenses.txt"
        assertEqual "licenses length" 21423 (length licenses)
    )
  , ( "Type information 1: Local identifiers and Prelude"
    , ifIdeBackendHaddockTestsEnabled defaultSessionConfig $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "a = (5 :: Int)"
                    , "b = a + 6"
                    , "c = True"
                    , "d = foldr"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        idInfo <- getSpanInfo session
        assertIdInfo idInfo "A" (2,1,2,2) "a (VarName) :: Int defined in main:A at A.hs@2:1-2:2 (binding occurrence)"
        assertIdInfo idInfo "A" (3,1,3,2) "b (VarName) :: Int defined in main:A at A.hs@3:1-3:2 (binding occurrence)"
        assertIdInfo idInfo "A" (3,5,3,6) "a (VarName) :: Int defined in main:A at A.hs@2:1-2:2 (defined locally)"
        assertIdInfo idInfo "A" (3,7,3,8) "+ (VarName) :: Num a => a -> a -> a defined in base-4.5.1.0:GHC.Num at <no location info> (home base-4.5.1.0:Prelude) (imported from base-4.5.1.0:Prelude at A.hs@1:8-1:9)"
        assertIdInfo idInfo "A" (4,1,4,2) "c (VarName) :: Bool defined in main:A at A.hs@4:1-4:2 (binding occurrence)"
        assertIdInfo idInfo "A" (4,5,4,9) "True (DataName) defined in ghc-prim-0.2.0.0:GHC.Types at <wired into compiler> (home base-4.5.1.0:Data.Bool) (wired in to the compiler)"
        assertIdInfo idInfo "A" (5,1,5,2) "d (VarName) :: (a -> b -> b) -> b -> [a] -> b defined in main:A at A.hs@5:1-5:2 (binding occurrence)"
        assertIdInfo idInfo "A" (5,5,5,10) "foldr (VarName) :: (a1 -> b1 -> b1) -> b1 -> [a1] -> b1 defined in base-4.5.1.0:GHC.Base at <no location info> (home base-4.5.1.0:Data.List) (imported from base-4.5.1.0:Prelude at A.hs@1:8-1:9)"
        {- TODO: reenable
        assertEqual "Haddock link for A.b should be correct"
                    "main/latest/doc/html/A.html#v:b" $
                    haddockLink (idMapToMap idMapB Map.! SourceSpan "B.hs" 5 8 5 9)
        -}
    )
  , ( "Type information 2: Simple ADTs"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "data T = MkT"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        idInfo <- getSpanInfo session
        assertIdInfo idInfo "A" (2,6,2,7) "T (TcClsName) defined in main:A at A.hs@2:6-2:7 (binding occurrence)"
        assertIdInfo idInfo "A" (2,10,2,13) "MkT (DataName) :: T defined in main:A at A.hs@2:10-2:13 (binding occurrence)"
    )
  , ( "Type information 3: Polymorphism"
    , ifIdeBackendHaddockTestsEnabled defaultSessionConfig $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
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
        idInfo <- getSpanInfo session
        assertIdInfo idInfo "A" (2,6,2,12) "TMaybe (TcClsName) defined in main:A at A.hs@2:6-2:12 (binding occurrence)"
        assertIdInfo idInfo "A" (2,13,2,14) "a (TvName) defined in main:A at A.hs@2:13-2:14 (binding occurrence)"
        assertIdInfo idInfo "A" (2,17,2,25) "TNothing (DataName) :: TMaybe a defined in main:A at A.hs@2:17-2:25 (binding occurrence)"
        assertIdInfo idInfo "A" (2,28,2,33) "TJust (DataName) :: a -> TMaybe a defined in main:A at A.hs@2:28-2:33 (binding occurrence)"
        assertIdInfo idInfo "A" (2,34,2,35) "a (TvName) defined in main:A at A.hs@2:13-2:14 (defined locally)"
        assertIdInfo idInfo "A" (4,1,4,3) "f1 (VarName) :: t -> t defined in main:A at A.hs@4:1-4:3 (binding occurrence)"
        assertIdInfo idInfo "A" (4,4,4,5) "x (VarName) :: t defined in main:A at A.hs@4:4-4:5 (binding occurrence)"
        assertIdInfo idInfo "A" (4,8,4,9) "x (VarName) :: t defined in main:A at A.hs@4:4-4:5 (defined locally)"
        assertIdInfo idInfo "A" (5,1,5,3) "f2 (VarName) :: t -> t defined in main:A at A.hs@5:1-5:3 (binding occurrence)"
        assertIdInfo idInfo "A" (5,7,5,8) "x (VarName) :: t defined in main:A at A.hs@5:7-5:8 (binding occurrence)"
        assertIdInfo idInfo "A" (5,12,5,13) "x (VarName) :: t defined in main:A at A.hs@5:7-5:8 (defined locally)"
        assertIdInfo idInfo "A" (7,1,7,3) "g1 (VarName) :: t -> t1 -> t defined in main:A at A.hs@7:1-7:3 (binding occurrence)"
        assertIdInfo idInfo "A" (7,4,7,5) "x (VarName) :: t defined in main:A at A.hs@7:4-7:5 (binding occurrence)"
        assertIdInfo idInfo "A" (7,6,7,7) "y (VarName) :: t1 defined in main:A at A.hs@7:6-7:7 (binding occurrence)"
        assertIdInfo idInfo "A" (7,10,7,11) "x (VarName) :: t defined in main:A at A.hs@7:4-7:5 (defined locally)"
        assertIdInfo idInfo "A" (8,1,8,3) "g2 (VarName) :: t -> t1 -> t defined in main:A at A.hs@8:1-8:3 (binding occurrence)"
        assertIdInfo idInfo "A" (8,7,8,8) "x (VarName) :: t defined in main:A at A.hs@8:7-8:8 (binding occurrence)"
        assertIdInfo idInfo "A" (8,9,8,10) "y (VarName) :: t1 defined in main:A at A.hs@8:9-8:10 (binding occurrence)"
        assertIdInfo idInfo "A" (8,14,8,15) "x (VarName) :: t defined in main:A at A.hs@8:7-8:8 (defined locally)"
        assertIdInfo idInfo "A" (10,1,10,3) "h1 (VarName) :: Bool defined in main:A at A.hs@10:1-10:3 (binding occurrence)"
        assertIdInfo idInfo "A" (10,6,10,10) "h1go (VarName) :: t -> t1 -> t defined in main:A at A.hs@12:5-12:9 (defined locally)"
        assertIdInfo idInfo "A" (10,11,10,15) "True (DataName) defined in ghc-prim-0.2.0.0:GHC.Types at <wired into compiler> (home base-4.5.1.0:Data.Bool) (wired in to the compiler)"
        assertIdInfo idInfo "A" (10,16,10,21) "False (DataName) defined in ghc-prim-0.2.0.0:GHC.Types at <wired into compiler> (home base-4.5.1.0:Data.Bool) (wired in to the compiler)"
        assertIdInfo idInfo "A" (12,5,12,9) "h1go (VarName) :: t -> t1 -> t defined in main:A at A.hs@12:5-12:9 (binding occurrence)"
        assertIdInfo idInfo "A" (12,10,12,11) "x (VarName) :: t defined in main:A at A.hs@12:10-12:11 (binding occurrence)"
        assertIdInfo idInfo "A" (12,12,12,13) "y (VarName) :: t1 defined in main:A at A.hs@12:12-12:13 (binding occurrence)"
        assertIdInfo idInfo "A" (12,16,12,17) "x (VarName) :: t defined in main:A at A.hs@12:10-12:11 (defined locally)"
        assertIdInfo idInfo "A" (14,1,14,3) "h2 (VarName) :: Bool defined in main:A at A.hs@14:1-14:3 (binding occurrence)"
        assertIdInfo idInfo "A" (14,6,14,10) "h2go (VarName) :: t -> t1 -> t defined in main:A at A.hs@16:5-16:9 (defined locally)"
        assertIdInfo idInfo "A" (14,11,14,15) "True (DataName) defined in ghc-prim-0.2.0.0:GHC.Types at <wired into compiler> (home base-4.5.1.0:Data.Bool) (wired in to the compiler)"
        assertIdInfo idInfo "A" (14,16,14,21) "False (DataName) defined in ghc-prim-0.2.0.0:GHC.Types at <wired into compiler> (home base-4.5.1.0:Data.Bool) (wired in to the compiler)"
        assertIdInfo idInfo "A" (16,5,16,9) "h2go (VarName) :: t -> t1 -> t defined in main:A at A.hs@16:5-16:9 (binding occurrence)"
        assertIdInfo idInfo "A" (16,13,16,14) "x (VarName) :: t defined in main:A at A.hs@16:13-16:14 (binding occurrence)"
        assertIdInfo idInfo "A" (16,15,16,16) "y (VarName) :: t1 defined in main:A at A.hs@16:15-16:16 (binding occurrence)"
        assertIdInfo idInfo "A" (16,20,16,21) "x (VarName) :: t defined in main:A at A.hs@16:13-16:14 (defined locally)"
    )
  , ( "Type information 4: Multiple modules"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "data T = MkT"
                    ])
                    -- Make sure that an occurrence of MkT in a second module
                    -- doesn't cause us to lose type information we learned
                    -- while processing the first
               <> (updateModule "B.hs" . BSLC.pack . unlines $
                    [ "module B where"
                    , "import A"
                    , "foo = MkT"
                    ])
        updateSessionD session upd 2
        assertNoErrors session
        idInfo <- getSpanInfo session
        assertIdInfo idInfo "A" (2,6,2,7) "T (TcClsName) defined in main:A at A.hs@2:6-2:7 (binding occurrence)"
        assertIdInfo idInfo "A" (2,10,2,13) "MkT (DataName) :: T defined in main:A at A.hs@2:10-2:13 (binding occurrence)"
        assertIdInfo idInfo "B" (3,1,3,4) "foo (VarName) :: T defined in main:B at B.hs@3:1-3:4 (binding occurrence)"
        assertIdInfo idInfo "B" (3,7,3,10) "MkT (DataName) :: T defined in main:A at A.hs@2:10-2:13 (imported from main:A at B.hs@2:1-2:9)"
    )
  , ( "Type information 5: External packages, type sigs, scoped type vars, kind sigs"
    , let opts = [ "-XScopedTypeVariables"
                 , "-XKindSignatures"
                 ]
      in ifIdeBackendHaddockTestsEnabled (withOpts opts) $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
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
        idInfo <- getSpanInfo session
        assertIdInfo idInfo "A" (3,1,3,2) "e (VarName) :: Bool defined in main:A at A.hs@3:1-3:2 (binding occurrence)"
        assertIdInfo idInfo "A" (3,5,3,9) "True (DataName) defined in ghc-prim-0.2.0.0:GHC.Types at <wired into compiler> (home base-4.5.1.0:Data.Bool) (wired in to the compiler)"
        assertIdInfo idInfo "A" (3,10,3,16) "pseq (VarName) :: a -> b -> b defined in parallel-3.2.0.3:Control.Parallel at <no location info> (home parallel-3.2.0.3:Control.Parallel) (imported from parallel-3.2.0.3:Control.Parallel at A.hs@2:1-2:24)"
        assertIdInfo idInfo "A" (3,17,3,22) "False (DataName) defined in ghc-prim-0.2.0.0:GHC.Types at <wired into compiler> (home base-4.5.1.0:Data.Bool) (wired in to the compiler)"
        assertIdInfo idInfo "A" (4,1,4,2) "f (VarName) :: a -> a defined in main:A at A.hs@5:1-5:2 (defined locally)"
        assertIdInfo idInfo "A" (4,6,4,7) "a (TvName) defined in main:A at A.hs@4:6-4:7 (defined locally)"
        assertIdInfo idInfo "A" (4,11,4,12) "a (TvName) defined in main:A at A.hs@4:6-4:7 (defined locally)"
        assertIdInfo idInfo "A" (5,1,5,2) "f (VarName) :: a -> a defined in main:A at A.hs@5:1-5:2 (binding occurrence)"
        assertIdInfo idInfo "A" (5,3,5,4) "x (VarName) :: a defined in main:A at A.hs@5:3-5:4 (binding occurrence)"
        assertIdInfo idInfo "A" (5,7,5,8) "x (VarName) :: a defined in main:A at A.hs@5:3-5:4 (defined locally)"
        assertIdInfo idInfo "A" (6,1,6,2) "g (VarName) :: a -> a defined in main:A at A.hs@7:1-7:2 (defined locally)"
        assertIdInfo idInfo "A" (6,13,6,14) "a (TvName) defined in main:A at A.hs@6:13-6:14 (binding occurrence)"
        assertIdInfo idInfo "A" (6,16,6,17) "a (TvName) defined in main:A at A.hs@6:13-6:14 (defined locally)"
        assertIdInfo idInfo "A" (6,21,6,22) "a (TvName) defined in main:A at A.hs@6:13-6:14 (defined locally)"
        assertIdInfo idInfo "A" (7,1,7,2) "g (VarName) :: a -> a defined in main:A at A.hs@7:1-7:2 (binding occurrence)"
        assertIdInfo idInfo "A" (7,3,7,4) "x (VarName) :: a defined in main:A at A.hs@7:3-7:4 (binding occurrence)"
        assertIdInfo idInfo "A" (7,7,7,8) "x (VarName) :: a defined in main:A at A.hs@7:3-7:4 (defined locally)"
        assertIdInfo idInfo "A" (8,1,8,2) "h (VarName) :: a -> a defined in main:A at A.hs@9:1-9:2 (defined locally)"
        assertIdInfo idInfo "A" (8,13,8,14) "a (TvName) defined in main:A at A.hs@8:13-8:14 (binding occurrence)"
        assertIdInfo idInfo "A" (8,16,8,17) "a (TvName) defined in main:A at A.hs@8:13-8:14 (defined locally)"
        assertIdInfo idInfo "A" (8,21,8,22) "a (TvName) defined in main:A at A.hs@8:13-8:14 (defined locally)"
        assertIdInfo idInfo "A" (9,1,9,2) "h (VarName) :: a -> a defined in main:A at A.hs@9:1-9:2 (binding occurrence)"
        assertIdInfo idInfo "A" (9,3,9,4) "x (VarName) :: a defined in main:A at A.hs@9:3-9:4 (binding occurrence)"
        assertIdInfo idInfo "A" (9,7,9,8) "y (VarName) :: a defined in main:A at A.hs@12:5-12:6 (defined locally)"
        assertIdInfo idInfo "A" (11,5,11,6) "y (VarName) :: a defined in main:A at A.hs@12:5-12:6 (defined locally)"
        assertIdInfo idInfo "A" (11,5,11,6) "y (VarName) :: a defined in main:A at A.hs@12:5-12:6 (defined locally)"
        assertIdInfo idInfo "A" (11,10,11,11) "a (TvName) defined in main:A at A.hs@8:13-8:14 (defined locally)"
        assertIdInfo idInfo "A" (11,10,11,11) "a (TvName) defined in main:A at A.hs@8:13-8:14 (defined locally)"
        assertIdInfo idInfo "A" (12,5,12,6) "y (VarName) :: a defined in main:A at A.hs@12:5-12:6 (binding occurrence)"
        assertIdInfo idInfo "A" (12,9,12,10) "x (VarName) :: a defined in main:A at A.hs@9:3-9:4 (defined locally)"
        assertIdInfo idInfo "A" (13,1,13,2) "i (VarName) :: t a -> t a defined in main:A at A.hs@14:1-14:2 (defined locally)"
        assertIdInfo idInfo "A" (13,13,13,26) "t (TvName) defined in main:A at A.hs@13:13-13:26 (binding occurrence)"
        assertIdInfo idInfo "A" (13,27,13,28) "a (TvName) defined in main:A at A.hs@13:27-13:28 (binding occurrence)"
        assertIdInfo idInfo "A" (13,30,13,31) "t (TvName) defined in main:A at A.hs@13:13-13:26 (defined locally)"
        assertIdInfo idInfo "A" (13,32,13,33) "a (TvName) defined in main:A at A.hs@13:27-13:28 (defined locally)"
        assertIdInfo idInfo "A" (13,37,13,38) "t (TvName) defined in main:A at A.hs@13:13-13:26 (defined locally)"
        assertIdInfo idInfo "A" (13,39,13,40) "a (TvName) defined in main:A at A.hs@13:27-13:28 (defined locally)"
        assertIdInfo idInfo "A" (14,1,14,2) "i (VarName) :: t a -> t a defined in main:A at A.hs@14:1-14:2 (binding occurrence)"
        assertIdInfo idInfo "A" (14,3,14,4) "x (VarName) :: t a defined in main:A at A.hs@14:3-14:4 (binding occurrence)"
        assertIdInfo idInfo "A" (14,7,14,8) "x (VarName) :: t a defined in main:A at A.hs@14:3-14:4 (defined locally)"
    )
  , ( "Type information 6: Reusing type variables"
    , withSession (withOpts ["-XScopedTypeVariables"]) $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
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
        idInfo <- getSpanInfo session
        assertIdInfo idInfo "A" (2,1,2,3) "f1 (VarName) :: (t, t1) -> t defined in main:A at A.hs@2:1-2:3 (binding occurrence)"
        assertIdInfo idInfo "A" (2,5,2,6) "x (VarName) :: t defined in main:A at A.hs@2:5-2:6 (binding occurrence)"
        assertIdInfo idInfo "A" (2,8,2,9) "y (VarName) :: t1 defined in main:A at A.hs@2:8-2:9 (binding occurrence)"
        assertIdInfo idInfo "A" (2,13,2,14) "x (VarName) :: t defined in main:A at A.hs@2:5-2:6 (defined locally)"
        assertIdInfo idInfo "A" (3,1,3,3) "f2 (VarName) :: (t, t1) -> t defined in main:A at A.hs@3:1-3:3 (binding occurrence)"
        assertIdInfo idInfo "A" (3,5,3,6) "x (VarName) :: t defined in main:A at A.hs@3:5-3:6 (binding occurrence)"
        assertIdInfo idInfo "A" (3,8,3,9) "y (VarName) :: t1 defined in main:A at A.hs@3:8-3:9 (binding occurrence)"
        assertIdInfo idInfo "A" (3,13,3,14) "x (VarName) :: t defined in main:A at A.hs@3:5-3:6 (defined locally)"
        assertIdInfo idInfo "A" (4,1,4,3) "f3 (VarName) :: (t, t1) -> t defined in main:A at A.hs@4:1-4:3 (binding occurrence)"
        assertIdInfo idInfo "A" (4,5,4,6) "x (VarName) :: t defined in main:A at A.hs@4:5-4:6 (binding occurrence)"
        assertIdInfo idInfo "A" (4,8,4,9) "y (VarName) :: t1 defined in main:A at A.hs@4:8-4:9 (binding occurrence)"
        assertIdInfo idInfo "A" (4,13,4,15) "f4 (VarName) :: (t2, t3) -> t2 defined in main:A at A.hs@6:5-6:7 (defined locally)"
        assertIdInfo idInfo "A" (4,17,4,18) "x (VarName) :: t defined in main:A at A.hs@4:5-4:6 (defined locally)"
        assertIdInfo idInfo "A" (4,20,4,21) "y (VarName) :: t1 defined in main:A at A.hs@4:8-4:9 (defined locally)"
        assertIdInfo idInfo "A" (6,5,6,7) "f4 (VarName) :: (t2, t3) -> t2 defined in main:A at A.hs@6:5-6:7 (binding occurrence)"
        assertIdInfo idInfo "A" (6,9,6,10) "x (VarName) :: t2 defined in main:A at A.hs@6:9-6:10 (binding occurrence)"
        assertIdInfo idInfo "A" (6,12,6,13) "y (VarName) :: t3 defined in main:A at A.hs@6:12-6:13 (binding occurrence)"
        assertIdInfo idInfo "A" (6,17,6,18) "x (VarName) :: t2 defined in main:A at A.hs@6:9-6:10 (defined locally)"
        assertIdInfo idInfo "A" (7,1,7,3) "f5 (VarName) :: (t, t1) -> t defined in main:A at A.hs@8:1-8:3 (defined locally)"
        assertIdInfo idInfo "A" (7,14,7,15) "t (TvName) defined in main:A at A.hs@7:14-7:15 (binding occurrence)"
        assertIdInfo idInfo "A" (7,16,7,18) "t1 (TvName) defined in main:A at A.hs@7:16-7:18 (binding occurrence)"
        assertIdInfo idInfo "A" (7,21,7,22) "t (TvName) defined in main:A at A.hs@7:14-7:15 (defined locally)"
        assertIdInfo idInfo "A" (7,24,7,26) "t1 (TvName) defined in main:A at A.hs@7:16-7:18 (defined locally)"
        assertIdInfo idInfo "A" (7,31,7,32) "t (TvName) defined in main:A at A.hs@7:14-7:15 (defined locally)"
        assertIdInfo idInfo "A" (8,1,8,3) "f5 (VarName) :: (t, t1) -> t defined in main:A at A.hs@8:1-8:3 (binding occurrence)"
        assertIdInfo idInfo "A" (8,5,8,6) "x (VarName) :: t defined in main:A at A.hs@8:5-8:6 (binding occurrence)"
        assertIdInfo idInfo "A" (8,8,8,9) "y (VarName) :: t1 defined in main:A at A.hs@8:8-8:9 (binding occurrence)"
        assertIdInfo idInfo "A" (8,13,8,15) "f6 (VarName) :: (t, t2) -> t defined in main:A at A.hs@11:5-11:7 (defined locally)"
        assertIdInfo idInfo "A" (8,17,8,18) "x (VarName) :: t defined in main:A at A.hs@8:5-8:6 (defined locally)"
        assertIdInfo idInfo "A" (8,20,8,21) "y (VarName) :: t1 defined in main:A at A.hs@8:8-8:9 (defined locally)"
        assertIdInfo idInfo "A" (10,5,10,7) "f6 (VarName) :: (t, t2) -> t defined in main:A at A.hs@11:5-11:7 (defined locally)"
        assertIdInfo idInfo "A" (10,5,10,7) "f6 (VarName) :: (t, t2) -> t defined in main:A at A.hs@11:5-11:7 (defined locally)"
        assertIdInfo idInfo "A" (10,18,10,20) "t2 (TvName) defined in main:A at A.hs@10:18-10:20 (binding occurrence)"
        assertIdInfo idInfo "A" (10,18,10,20) "t2 (TvName) defined in main:A at A.hs@10:18-10:20 (binding occurrence)"
        assertIdInfo idInfo "A" (10,23,10,24) "t (TvName) defined in main:A at A.hs@7:14-7:15 (defined locally)"
        assertIdInfo idInfo "A" (10,23,10,24) "t (TvName) defined in main:A at A.hs@7:14-7:15 (defined locally)"
        assertIdInfo idInfo "A" (10,26,10,28) "t2 (TvName) defined in main:A at A.hs@10:18-10:20 (defined locally)"
        assertIdInfo idInfo "A" (10,26,10,28) "t2 (TvName) defined in main:A at A.hs@10:18-10:20 (defined locally)"
        assertIdInfo idInfo "A" (10,33,10,34) "t (TvName) defined in main:A at A.hs@7:14-7:15 (defined locally)"
        assertIdInfo idInfo "A" (10,33,10,34) "t (TvName) defined in main:A at A.hs@7:14-7:15 (defined locally)"
        assertIdInfo idInfo "A" (11,5,11,7) "f6 (VarName) :: (t, t2) -> t defined in main:A at A.hs@11:5-11:7 (binding occurrence)"
        assertIdInfo idInfo "A" (11,9,11,10) "x (VarName) :: t defined in main:A at A.hs@11:9-11:10 (binding occurrence)"
        assertIdInfo idInfo "A" (11,12,11,13) "y (VarName) :: t2 defined in main:A at A.hs@11:12-11:13 (binding occurrence)"
        assertIdInfo idInfo "A" (11,17,11,18) "x (VarName) :: t defined in main:A at A.hs@11:9-11:10 (defined locally)"
    )
  , ( "Type information 7: Qualified imports"
    , ifIdeBackendHaddockTestsEnabled defaultSessionConfig $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "import Data.Maybe"
                    , "import qualified Data.List"
                    , "import qualified Data.Function as F"
                    , "foo = (fromJust, Data.List.and, F.on)"
                    ])
        updateSessionD session upd 2
        assertNoErrors session
        idInfo <- getSpanInfo session
        assertIdInfo idInfo "A" (5,1,5,4) "foo (VarName) :: (Maybe a -> a, [Bool] -> Bool, (b -> b -> c) -> (a1 -> b) -> a1 -> a1 -> c) defined in main:A at A.hs@5:1-5:4 (binding occurrence)"
        assertIdInfo idInfo "A" (5,8,5,16) "fromJust (VarName) :: Maybe a2 -> a2 defined in base-4.5.1.0:Data.Maybe at <no location info> (home base-4.5.1.0:Data.Maybe) (imported from base-4.5.1.0:Data.Maybe at A.hs@2:1-2:18)"
        assertIdInfo idInfo "A" (5,18,5,31) "and (VarName) :: [Bool] -> Bool defined in base-4.5.1.0:GHC.List at <no location info> (home base-4.5.1.0:Data.List) (imported from base-4.5.1.0:Data.List as 'Data.List.' at A.hs@3:1-3:27)"
        assertIdInfo idInfo "A" (5,33,5,37) "on (VarName) :: (b1 -> b1 -> c1) -> (a2 -> b1) -> a2 -> a2 -> c1 defined in base-4.5.1.0:Data.Function at <no location info> (home base-4.5.1.0:Data.Function) (imported from base-4.5.1.0:Data.Function as 'F.' at A.hs@4:1-4:36)"
    )
  , ( "Type information 8: Imprecise source spans"
    , ifIdeBackendHaddockTestsEnabled defaultSessionConfig $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "main = print True"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        idInfo <- getSpanInfo session
        let infoPrint = "print (VarName) :: Show a => a -> IO () defined in base-4.5.1.0:System.IO at <no location info> (home base-4.5.1.0:System.IO) (imported from base-4.5.1.0:Prelude at A.hs@1:8-1:9)"

        assertIdInfo idInfo "A" (2,8,2,13) infoPrint
        assertIdInfo idInfo "A" (2,8,2,8)  infoPrint
        assertIdInfo idInfo "A" (2,8,2,9)  infoPrint
        assertIdInfo idInfo "A" (2,9,2,9)  infoPrint
        assertIdInfo idInfo "A" (2,9,2,10) infoPrint
        assertIdInfo idInfo "A" (2,9,2,13) infoPrint
    )
  , ( "Type information 9a: Quasi-quotation (QQ in own package)"
    , withSession defaultSessionConfig $ \session -> do
        let upd = updateCodeGeneration True
               <> (updateModule "A.hs" . BSLC.pack . unlines $
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
               <> (updateModule "B.hs" . BSLC.pack . unlines $
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
        idInfo <- getSpanInfo session
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
        assertIdInfo idInfo "B" (4,7,4,14) "quasi-quote with quoter qq (VarName) :: QuasiQuoter defined in main:A at A.hs@4:1-4:3 (imported from main:A at B.hs@3:1-3:9)"
        assertIdInfo idInfo "B" (5,7,5,14) "quasi-quote with quoter qq (VarName) :: QuasiQuoter defined in main:A at A.hs@4:1-4:3 (imported from main:A at B.hs@3:1-3:9)"
        assertIdInfo idInfo "B" (6,7,6,14) "quasi-quote with quoter qq (VarName) :: QuasiQuoter defined in main:A at A.hs@4:1-4:3 (imported from main:A at B.hs@3:1-3:9)"
        assertIdInfo idInfo "B" (7,7,7,14) "quasi-quote with quoter qq (VarName) :: QuasiQuoter defined in main:A at A.hs@4:1-4:3 (imported from main:A at B.hs@3:1-3:9)"
    )
  , ( "Type information 9b: Quasi-quotation (QQ in separate package, check home module info)"
    , ifIdeBackendHaddockTestsEnabled defaultSessionConfig $ \session -> do
        let upd = updateCodeGeneration True
               <> (updateModule "Main.hs" . BSLC.pack . unlines $
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
            idInfo <- getSpanInfo session

            assertIdInfo idInfo "Main" (6,19,8,3) "quasi-quote with quoter parseRoutes (VarName) defined in yesod-routes-1.2.0.1:Yesod.Routes.Parse at <no location info> (home yesod-core-1.2.2:Yesod.Core.Dispatch) (imported from yesod-1.2.1:Yesod at Main.hs@3:1-3:13)"
            assertIdInfo idInfo "Main" (9,26,11,5) "quasi-quote with quoter whamlet (VarName) defined in yesod-core-1.2.2:Yesod.Core.Widget at <no location info> (home yesod-core-1.2.2:Yesod.Core.Widget) (imported from yesod-1.2.1:Yesod at Main.hs@3:1-3:13)"
          _ ->
            putStrLn "WARNING: Skipping due to errors (probably yesod package not installed)"
    )
  , ( "Type information 10: Template Haskell"
    , ifIdeBackendHaddockTestsEnabled defaultSessionConfig $ \session -> do
        let upd = updateCodeGeneration True
               <> (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE TemplateHaskell #-}"
                    , "module A where"
                    , "import Language.Haskell.TH"
                    , "ex1 :: Q Exp"
                    , "ex1 = [| \\x -> x |]"
                    , "ex2 :: Q Type"
                    , "ex2 = [t| String -> String |]"
                    ])
               <> (updateModule "B.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE TemplateHaskell #-}"
                    , "module B where"
                    , "import A"
                    , "ex3 :: $ex2"
                    , "ex3 = $ex1"
                    ])
        updateSessionD session upd 2
        assertNoErrors session
        idInfo <- getSpanInfo session
        assertIdInfo idInfo "A" (4,1,4,4) "ex1 (VarName) :: Q Exp defined in main:A at A.hs@5:1-5:4 (defined locally)"
        assertIdInfo idInfo "A" (4,8,4,9) "Q (TcClsName) defined in template-haskell-2.7.0.0:Language.Haskell.TH.Syntax at <no location info> (home template-haskell-2.7.0.0:Language.Haskell.TH.Syntax) (imported from template-haskell-2.7.0.0:Language.Haskell.TH at A.hs@3:1-3:27)"
        assertIdInfo idInfo "A" (4,10,4,13) "Exp (TcClsName) defined in template-haskell-2.7.0.0:Language.Haskell.TH.Syntax at <no location info> (home template-haskell-2.7.0.0:Language.Haskell.TH.Syntax) (imported from template-haskell-2.7.0.0:Language.Haskell.TH at A.hs@3:1-3:27)"
        assertIdInfo idInfo "A" (5,1,5,4) "ex1 (VarName) :: Q Exp defined in main:A at A.hs@5:1-5:4 (binding occurrence)"
        assertIdInfo idInfo "A" (5,11,5,12) "x (VarName) defined in main:A at A.hs@5:11-5:12 (binding occurrence)"
        assertIdInfo idInfo "A" (5,11,5,12) "x (VarName) defined in main:A at A.hs@5:11-5:12 (binding occurrence)"
        assertIdInfo idInfo "A" (5,16,5,17) "x (VarName) defined in main:A at A.hs@5:11-5:12 (defined locally)"
        assertIdInfo idInfo "A" (5,16,5,17) "x (VarName) defined in main:A at A.hs@5:11-5:12 (defined locally)"
        assertIdInfo idInfo "A" (6,1,6,4) "ex2 (VarName) :: Q Type defined in main:A at A.hs@7:1-7:4 (defined locally)"
        assertIdInfo idInfo "A" (6,8,6,9) "Q (TcClsName) defined in template-haskell-2.7.0.0:Language.Haskell.TH.Syntax at <no location info> (home template-haskell-2.7.0.0:Language.Haskell.TH.Syntax) (imported from template-haskell-2.7.0.0:Language.Haskell.TH at A.hs@3:1-3:27)"
        assertIdInfo idInfo "A" (6,10,6,14) "Type (TcClsName) defined in template-haskell-2.7.0.0:Language.Haskell.TH.Syntax at <no location info> (home template-haskell-2.7.0.0:Language.Haskell.TH.Syntax) (imported from template-haskell-2.7.0.0:Language.Haskell.TH at A.hs@3:1-3:27)"
        assertIdInfo idInfo "A" (7,1,7,4) "ex2 (VarName) :: Q Type defined in main:A at A.hs@7:1-7:4 (binding occurrence)"
        assertIdInfo idInfo "A" (7,11,7,17) "String (TcClsName) defined in base-4.5.1.0:GHC.Base at <no location info> (home base-4.5.1.0:Data.String) (imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9)"
        assertIdInfo idInfo "A" (7,11,7,17) "String (TcClsName) defined in base-4.5.1.0:GHC.Base at <no location info> (home base-4.5.1.0:Data.String) (imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9)"
        assertIdInfo idInfo "A" (7,21,7,27) "String (TcClsName) defined in base-4.5.1.0:GHC.Base at <no location info> (home base-4.5.1.0:Data.String) (imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9)"
        assertIdInfo idInfo "A" (7,21,7,27) "String (TcClsName) defined in base-4.5.1.0:GHC.Base at <no location info> (home base-4.5.1.0:Data.String) (imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9)"
        assertIdInfo idInfo "B" (4,1,4,4) "ex3 (VarName) :: String -> String defined in main:B at B.hs@5:1-5:4 (defined locally)"
        assertIdInfo idInfo "B" (4,8,4,12) "ex2 (VarName) :: Q Type defined in main:A at A.hs@7:1-7:4 (imported from main:A at B.hs@3:1-3:9)"
        assertIdInfo idInfo "B" (5,1,5,4) "ex3 (VarName) :: String -> String defined in main:B at B.hs@5:1-5:4 (binding occurrence)"
        assertIdInfo idInfo "B" (5,7,5,11) "ex1 (VarName) :: Q Exp defined in main:A at A.hs@5:1-5:4 (imported from main:A at B.hs@3:1-3:9)"
    )
  , ( "Type information 11: Take advantage of scope (1)"
    , ifIdeBackendHaddockTestsEnabled defaultSessionConfig $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "main = print True"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        idInfo <- getSpanInfo session
        assertIdInfo idInfo "A" (2,8,2,13) "print (VarName) :: Show a => a -> IO () defined in base-4.5.1.0:System.IO at <no location info> (home base-4.5.1.0:System.IO) (imported from base-4.5.1.0:Prelude at A.hs@1:8-1:9)"
    )
  , ( "Type information 12: Take advantage of scope (2)"
    , ifIdeBackendHaddockTestsEnabled defaultSessionConfig $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "import Data.ByteString (append)"
                    , "foo = append"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        idInfo <- getSpanInfo session
        assertIdInfo idInfo "A" (3,7,3,13) "append (VarName) :: Data.ByteString.Internal.ByteString -> Data.ByteString.Internal.ByteString -> Data.ByteString.Internal.ByteString defined in bytestring-0.9.2.1:Data.ByteString at <no location info> (home bytestring-0.9.2.1:Data.ByteString) (imported from bytestring-0.9.2.1:Data.ByteString at A.hs@2:25-2:31)"
    )
  , ( "Type information 13: Take advantage of scope (3)"
    , ifIdeBackendHaddockTestsEnabled defaultSessionConfig $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "import Data.ByteString"
                    , "foo = append"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        idInfo <- getSpanInfo session
        assertIdInfo idInfo "A" (3,7,3,13) "append (VarName) :: ByteString -> ByteString -> ByteString defined in bytestring-0.9.2.1:Data.ByteString at <no location info> (home bytestring-0.9.2.1:Data.ByteString) (imported from bytestring-0.9.2.1:Data.ByteString at A.hs@2:1-2:23)"
    )
  , ( "Type information 14: Take advantage of scope (4)"
    , ifIdeBackendHaddockTestsEnabled defaultSessionConfig $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "import Data.ByteString (append)"
                    , "import qualified Data.ByteString as BS"
                    , "foo = append"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        idInfo <- getSpanInfo session
        assertIdInfo idInfo "A" (4,7,4,13) "append (VarName) :: BS.ByteString -> BS.ByteString -> BS.ByteString defined in bytestring-0.9.2.1:Data.ByteString at <no location info> (home bytestring-0.9.2.1:Data.ByteString) (imported from bytestring-0.9.2.1:Data.ByteString as 'BS.' at A.hs@3:1-3:39)"
    )
  , ( "Type information 15: Other constructs"
    , ifIdeBackendHaddockTestsEnabled defaultSessionConfig $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ {-  1 -} "{-# LANGUAGE StandaloneDeriving, DoRec #-}"
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
        idInfo <- getSpanInfo session
        assertIdInfo idInfo "A" (4,10,4,12) "Eq (TcClsName) defined in ghc-prim-0.2.0.0:GHC.Classes at <no location info> (home base-4.5.1.0:Data.Eq) (imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9)"
        assertIdInfo idInfo "A" (5,18,5,23) "const (VarName) :: a -> b -> a defined in base-4.5.1.0:GHC.Base at <no location info> (home base-4.5.1.0:Prelude) (imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9)"
        assertIdInfo idInfo "A" (6,19,6,23) "Show (TcClsName) defined in base-4.5.1.0:GHC.Show at <no location info> (home base-4.5.1.0:Text.Show) (imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9)"
        assertIdInfo idInfo "A" (6,24,6,27) "MkT (TcClsName) defined in main:A at A.hs@3:6-3:9 (defined locally)"
        assertIdInfo idInfo "A" (8,10,8,13) "+++ (VarName) :: [a] -> [a] -> [a] defined in main:A at A.hs@7:1-7:6 (defined locally)"
        assertIdInfo idInfo "A" (9,10,9,13) "Int (TcClsName) defined in ghc-prim-0.2.0.0:GHC.Types at <wired into compiler> (home base-4.5.1.0:Data.Int) (wired in to the compiler)"
        assertIdInfo idInfo "A" (17,13,17,14) "x (VarName) :: Int defined in main:A at A.hs@17:3-17:4 (defined locally)"
        assertIdInfo idInfo "A" (17,21,17,22) "x (VarName) :: Int defined in main:A at A.hs@17:3-17:4 (defined locally)"
        assertIdInfo idInfo "A" (17,24,17,25) "y (VarName) :: Int defined in main:A at A.hs@17:5-17:6 (defined locally)"
        assertIdInfo idInfo "A" (17,31,17,32) "x (VarName) :: Int defined in main:A at A.hs@17:3-17:4 (defined locally)"
        assertIdInfo idInfo "A" (17,36,17,37) "z (VarName) :: Int defined in main:A at A.hs@17:7-17:8 (defined locally)"
        assertIdInfo idInfo "A" (17,41,17,42) "x (VarName) :: Int defined in main:A at A.hs@17:3-17:4 (defined locally)"
        assertIdInfo idInfo "A" (17,44,17,45) "y (VarName) :: Int defined in main:A at A.hs@17:5-17:6 (defined locally)"
        assertIdInfo idInfo "A" (17,49,17,50) "z (VarName) :: Int defined in main:A at A.hs@17:7-17:8 (defined locally)"
        assertIdInfo idInfo "A" (18,19,18,21) "xs (VarName) :: [Int] defined in main:A at A.hs@18:19-18:21 (binding occurrence)"
        assertIdInfo idInfo "A" (18,25,18,29) "Just (DataName) defined in base-4.5.1.0:Data.Maybe at <no location info> (home base-4.5.1.0:Data.Maybe) (imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9)"
        assertIdInfo idInfo "A" (18,35,18,37) "xs (VarName) :: [Int] defined in main:A at A.hs@18:19-18:21 (defined locally)"
    )
  , ( "Type information 16: FFI"
    , ifIdeBackendHaddockTestsEnabled defaultSessionConfig $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
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
        idInfo <- getSpanInfo session
        assertIdInfo idInfo "A" (5,28,5,33) "c_sin (VarName) :: CDouble -> CDouble defined in main:A at A.hs@5:28-5:33 (binding occurrence)"
        assertIdInfo idInfo "A" (5,37,5,44) "CDouble (TcClsName) defined in base-4.5.1.0:Foreign.C.Types at <no location info> (home base-4.5.1.0:Foreign.C.Types) (imported from base-4.5.1.0:Foreign.C at A.hs@4:1-4:17)"
        assertIdInfo idInfo "A" (7,21,7,26) "c_sin (VarName) :: CDouble -> CDouble defined in main:A at A.hs@5:28-5:33 (defined locally)"
        assertIdInfo idInfo "A" (10,22,10,29) "andBack (VarName) :: CDouble -> CDouble defined in main:A at A.hs@9:1-9:8 (defined locally)"
        assertIdInfo idInfo "A" (10,33,10,40) "CDouble (TcClsName) defined in base-4.5.1.0:Foreign.C.Types at <no location info> (home base-4.5.1.0:Foreign.C.Types) (imported from base-4.5.1.0:Foreign.C at A.hs@4:1-4:17)"
    )
  , ( "Type information 17: GADTs"
    , ifIdeBackendHaddockTestsEnabled defaultSessionConfig $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
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
        idInfo <- getSpanInfo session
        -- TODO: we get very strange types for some of the constructors
        assertIdInfo idInfo "A" (4,3,4,6) "Num (DataName) :: GHC.Prim.~# * ($a) Int -> Int -> Expr ($a) defined in main:A at A.hs@4:3-4:6 (binding occurrence)"
        assertIdInfo idInfo "A" (4,23,4,26) "Int (TcClsName) defined in ghc-prim-0.2.0.0:GHC.Types at <wired into compiler> (home base-4.5.1.0:Data.Int) (wired in to the compiler)"
        assertIdInfo idInfo "A" (7,3,7,7) "Cond (DataName) :: Expr Bool -> Expr a -> Expr a -> Expr a defined in main:A at A.hs@7:3-7:7 (binding occurrence)"
        assertIdInfo idInfo "A" (7,18,7,19) "a (TvName) defined in main:A at A.hs@7:18-7:19 (binding occurrence)"
        assertIdInfo idInfo "A" (7,54,7,58) "Expr (TcClsName) defined in main:A at A.hs@3:6-3:10 (defined locally)"
        assertIdInfo idInfo "A" (7,59,7,60) "a (TvName) defined in main:A at A.hs@7:18-7:19 (defined locally)"
    )
  , ( "Type information 18: Other types"
    , ifIdeBackendHaddockTestsEnabled defaultSessionConfig $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
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
        idInfo <- getSpanInfo session
        -- TODO: we don't get location info for the fundeps
        -- (this is missing from GHC's AST)
        assertIdInfo idInfo "A" (3,7,3,8) "C (TcClsName) defined in main:A at A.hs@3:7-3:8 (binding occurrence)"
        assertIdInfo idInfo "A" (3,9,3,10) "a (TvName) defined in main:A at A.hs@3:9-3:10 (binding occurrence)"
        assertIdInfo idInfo "A" (4,3,4,4) "f (VarName) defined in main:A at A.hs@4:3-4:4 (defined locally)"
        assertIdInfo idInfo "A" (4,8,4,11) "Int (TcClsName) defined in ghc-prim-0.2.0.0:GHC.Types at <wired into compiler> (home base-4.5.1.0:Data.Int) (wired in to the compiler)"
        assertIdInfo idInfo "A" (4,15,4,16) "a (TvName) defined in main:A at A.hs@3:9-3:10 (defined locally)"
        assertIdInfo idInfo "A" (5,7,5,8) "D (TcClsName) defined in main:A at A.hs@5:7-5:8 (binding occurrence)"
        assertIdInfo idInfo "A" (5,9,5,10) "a (TvName) defined in main:A at A.hs@5:9-5:10 (binding occurrence)"
        assertIdInfo idInfo "A" (5,11,5,12) "b (TvName) defined in main:A at A.hs@5:11-5:12 (binding occurrence)"
        assertIdInfo idInfo "A" (6,3,6,4) "g (VarName) defined in main:A at A.hs@6:3-6:4 (defined locally)"
        assertIdInfo idInfo "A" (6,8,6,9) "a (TvName) defined in main:A at A.hs@5:9-5:10 (defined locally)"
        assertIdInfo idInfo "A" (6,13,6,14) "b (TvName) defined in main:A at A.hs@5:11-5:12 (defined locally)"
        assertIdInfo idInfo "A" (7,6,7,9) "Foo (TcClsName) defined in main:A at A.hs@7:6-7:9 (binding occurrence)"
        assertIdInfo idInfo "A" (7,12,7,15) "Int (TcClsName) defined in ghc-prim-0.2.0.0:GHC.Types at <wired into compiler> (home base-4.5.1.0:Data.Int) (wired in to the compiler)"
        assertIdInfo idInfo "A" (8,13,8,16) "Bar (TcClsName) defined in main:A at A.hs@8:13-8:16 (binding occurrence)"
        assertIdInfo idInfo "A" (8,17,8,18) "a (TvName) defined in main:A at A.hs@8:17-8:18 (binding occurrence)"
        assertIdInfo idInfo "A" (9,15,9,18) "Bar (TcClsName) defined in main:A at A.hs@8:13-8:16 (binding occurrence)"
        assertIdInfo idInfo "A" (9,19,9,22) "Int (TcClsName) defined in ghc-prim-0.2.0.0:GHC.Types at <wired into compiler> (home base-4.5.1.0:Data.Int) (wired in to the compiler)"
        assertIdInfo idInfo "A" (9,25,9,29) "Bool (TcClsName) defined in ghc-prim-0.2.0.0:GHC.Types at <wired into compiler> (home base-4.5.1.0:Data.Bool) (wired in to the compiler)"
    )
  , ( "Type information 19: Default methods"
    , ifIdeBackendHaddockTestsEnabled defaultSessionConfig $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "class Foo a where"
                    , "  foo :: a -> Int"
                    , "  foo _ = succ 1"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        idInfo <- getSpanInfo session
        assertIdInfo idInfo "A" (4,11,4,15) "succ (VarName) :: Enum a1 => a1 -> a1 defined in base-4.5.1.0:GHC.Enum at <no location info> (home base-4.5.1.0:Prelude) (imported from base-4.5.1.0:Prelude at A.hs@1:8-1:9)"
    )
  , ( "Test internal consistency of local id markers"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateModule "M.hs" . BSLC.pack . unlines $
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
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateModule "M.hs" . BSLC.pack . unlines $
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
        let upd = (updateModule "M.hs" . BSLC.pack . unlines $
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
        -- TODO: This test will fail if the user happens to have installed
        -- slightly different versions of these packages
        let base mod = ModuleId {
                moduleName    = Text.pack mod
              , modulePackage = PackageId {
                    packageName    = Text.pack "base"
                  , packageVersion = Just (Text.pack "4.5.1.0")
                  }
              }
            par mod = ModuleId {
                moduleName    = Text.pack mod
              , modulePackage = PackageId {
                    packageName    = Text.pack "parallel"
                  , packageVersion = Just (Text.pack "3.2.0.3")
                  }
              }
        assertSameSet "imports: " (fromJust . imports $ Text.pack "M") $ [
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
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "foobar :: Bool -> Bool"
                    , "foobar = id"
                    ])
               <> (updateModule "B.hs" . BSLC.pack . unlines $
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

        let upd' = (updateModule "A.hs" . BSLC.pack . unlines $
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
    , ifIdeBackendHaddockTestsEnabled defaultSessionConfig $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    ])

        updateSessionD session upd 1
        assertNoErrors session

        autocomplete <- getAutocompletion session
        let completeTru = autocomplete (Text.pack "A") "Tru"
        assertEqual "" "[True (DataName) defined in ghc-prim-0.2.0.0:GHC.Types at <wired into compiler> (home base-4.5.1.0:Data.Bool) (wired in to the compiler)]" (show completeTru)
    )
    -- TODO: Autocomplete test that checks import errors
    -- - Explicitly importing somthing that wasn't exported
    -- - Explicitly hiding something that wasn't exported
    -- - Use of PackageImports without the flag
  , ( "GHC crash 1: No delay, no further requests"
    , withSession defaultSessionConfig $ \session -> do
        crashGhcServer session Nothing
    )
  , ( "GHC crash 2: No delay, follow up request"
    , withSession defaultSessionConfig $ \session -> do
        crashGhcServer session Nothing
        updateSession session (updateEnv "Foo" Nothing) (\_ -> return ())
        assertSourceErrors' session ["Intentional crash"]
    )
  , ( "GHC crash 3: Delay, follow up request"
    , withSession defaultSessionConfig $ \session -> do
        crashGhcServer session (Just 1000000)
        updateSession session (updateEnv "Foo" Nothing) (\_ -> return ())
        threadDelay 2000000
        updateSession session (updateEnv "Foo" Nothing) (\_ -> return ())
        assertSourceErrors' session ["Intentional crash"]
    )
  , ( "GHC crash 4: Make sure session gets restarted on second update"
    , withSession defaultSessionConfig $ \session -> do
        -- Compile some code
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
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
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "Value2") output
             _       -> assertFailure $ "Unexpected result " ++ show result
    )
  , ( "GHC crash 5: Repeated crashes and restarts"
    , withSession defaultSessionConfig $ \session -> do
        -- Compile some code
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
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
             case result of
               RunOk _ -> assertEqual "" (BSLC.pack "Value2") output
               _       -> assertFailure $ "Unexpected result " ++ show result
    )
  , ( "GHC crash 6: Add additional code after update"
    , withSession defaultSessionConfig $ \session -> do
        let updA = (updateCodeGeneration True)
                <> (updateModule "A.hs" . BSLC.pack . unlines $
                     [ "module A where"
                     , "printA :: IO ()"
                     , "printA = putStr \"A\""
                     ])
        let updB = (updateCodeGeneration True)
                <> (updateModule "B.hs" . BSLC.pack . unlines $
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
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "AB") output
             _       -> assertFailure $ "Unexpected result " ++ show result
    )
  , ( "GHC crash 7: Update imported module after update"
    , withSession defaultSessionConfig $ \session -> do
        let updA = (updateCodeGeneration True)
                <> (updateModule "A.hs" . BSLC.pack . unlines $
                     [ "module A where"
                     , "printA :: IO ()"
                     , "printA = putStr \"A\""
                     ])
        let updB = (updateCodeGeneration True)
                <> (updateModule "B.hs" . BSLC.pack . unlines $
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
                 <> (updateModule "A.hs" . BSLC.pack . unlines $
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
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "A2B") output
             _       -> assertFailure $ "Unexpected result " ++ show result
    )
  , ( "GHC crash 8: Update importing module after update"
    , withSession defaultSessionConfig $ \session -> do
        let updA = (updateCodeGeneration True)
                <> (updateModule "A.hs" . BSLC.pack . unlines $
                     [ "module A where"
                     , "printA :: IO ()"
                     , "printA = putStr \"A\""
                     ])
        let updB = (updateCodeGeneration True)
                <> (updateModule "B.hs" . BSLC.pack . unlines $
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
                 <> (updateModule "B.hs" . BSLC.pack . unlines $
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
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "AB2") output
             _       -> assertFailure $ "Unexpected result " ++ show result
    )
  , ( "Parse ghc 'Compiling' messages"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "printA :: IO ()"
                    , "printA = putStr \"A\""
                    ])
               <> (updateModule "B.hs" . BSLC.pack . unlines $
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
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE TemplateHaskell #-}"
                    , "module A where"
                    , "import Language.Haskell.TH"
                    , "foo :: Q Exp"
                    , "foo = [| True |]"
                    ])
               <> (updateModule "Main.hs" . BSLC.pack . unlines $
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

        let upd2 = (updateModule "A.hs" . BSLC.pack . unlines $
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
    , let config = defaultSessionConfig { configGenerateModInfo = False }
      in withSession config $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "hello :: IO ()"
                    , "hello = putStrLn \"Hello World\""
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        mods <- getLoadedModules session
        assertEqual "" [Text.pack "M"] mods
    )
  , ( "Package dependencies"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    ])
               <> (updateModule "B.hs" . BSLC.pack . unlines $
                    [ "module B where"
                    , "import Control.Parallel"
                    ])
               <> (updateModule "C.hs" . BSLC.pack . unlines $
                    [ "module C where"
                    , "import Control.Monad.Cont" -- from mtl
                    ])

        updateSessionD session upd 3
        assertNoErrors session

        deps <- getPkgDeps session
        assertEqual "" "Just [base-4.5.1.0,ghc-prim-0.2.0.0,integer-gmp-0.4.0.0]" (show (deps (Text.pack "A")))
        assertEqual "" "Just [parallel-3.2.0.3,base-4.5.1.0,ghc-prim-0.2.0.0,integer-gmp-0.4.0.0]" (show (deps (Text.pack "B")))
        assertEqual "" "Just [mtl-2.1.2,base-4.5.1.0,ghc-prim-0.2.0.0,integer-gmp-0.4.0.0,transformers-0.3.0.0]" (show (deps (Text.pack "C")))
     )
  , ( "Set command line arguments"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import System.Environment (getArgs)"
                    , "printArgs :: IO ()"
                    , "printArgs = getArgs >>= print"
                    ])
        updateSessionD session upd 1
        assertNoErrors session

        -- Check that default is []
        do runActions <- runStmt session "M" "printArgs"
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "[]\n") output
             _       -> assertFailure $ "Unexpected run result: " ++ show result

        -- Check that we can set command line arguments
        updateSession session (updateArgs ["A", "B", "C"]) (\_ -> return ())
        do runActions <- runStmt session "M" "printArgs"
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "[\"A\",\"B\",\"C\"]\n") output
             _       -> assertFailure $ "Unexpected run result: " ++ show result

        -- Check that we can change command line arguments
        updateSession session (updateArgs ["D", "E"]) (\_ -> return ())
        do runActions <- runStmt session "M" "printArgs"
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "[\"D\",\"E\"]\n") output
             _       -> assertFailure $ "Unexpected run result: " ++ show result

        -- Check that we can clear command line arguments
        updateSession session (updateArgs []) (\_ -> return ())
        do runActions <- runStmt session "M" "printArgs"
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "[]\n") output
             _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Check that command line arguments survive restartSession"
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import System.Environment (getArgs)"
                    , "printArgs :: IO ()"
                    , "printArgs = getArgs >>= print"
                    ])
        updateSessionD session upd 1
        assertNoErrors session

        -- Sanity check: check before restart session
        updateSession session (updateArgs ["A", "B", "C"]) (\_ -> return ())
        do runActions <- runStmt session "M" "printArgs"
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "[\"A\",\"B\",\"C\"]\n") output
             _       -> assertFailure $ "Unexpected run result: " ++ show result

        -- Restart and update the session
        restartSession session Nothing
        updateSessionD session upd 1
        assertNoErrors session

        -- Check that arguments are still here
        do runActions <- runStmt session "M" "printArgs"
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "[\"A\",\"B\",\"C\"]\n") output
             _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Register a package, don't restart session, don't see the package"
    , withSession (withOpts ["-package simple-lib17"]) $ \session -> do
        deletePackage "test/simple-lib17"
        restartSession session (Just defaultSessionInitParams)
        let upd = updateModule "Main.hs" . BSLC.pack . unlines $
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
        let upd = updateModule "Main.hs" . BSLC.pack . unlines $
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
            upd2 = buildExe [(Text.pack m, "Main.hs")]
        updateSessionD session upd2 4
        distDir <- getDistDir session
        out <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "DB exe output"
                    "42\n"
                    out
        deletePackage "test/simple-lib17"
    )
  , ( "Make sure package DB is passed to ghc (configGenerateModInfo False)"
    , let packageOpts = ["-package parallel"]
          config      = defaultSessionConfig {
                            configGenerateModInfo = False
                          , configPackageDBStack  = [GlobalPackageDB]
                          , configStaticOpts      = packageOpts
                          }
      in withSession config $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
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
  , ( "Make sure package DB is passed to ghc (configGenerateModInfo True)"
    , let packageOpts = ["-package parallel"]
          config      = defaultSessionConfig {
                            configGenerateModInfo = True
                          , configPackageDBStack  = [GlobalPackageDB]
                          , configStaticOpts      = packageOpts
                          }
      in withSession config $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
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
          config      = defaultSessionConfig {
                            configGenerateModInfo = True
                          , configPackageDBStack  = [GlobalPackageDB]
                          , configStaticOpts      = packageOpts
                          }
      in withSession config $ \session -> do
        restartSession session Nothing
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
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
            updMod = \mod code -> updateModule mod (BSLC.pack code)

        update $ updateCodeGeneration True
        update $ updateStdoutBufferMode (RunLineBuffering Nothing)
        update $ updateStderrBufferMode (RunLineBuffering Nothing)

        update $ updMod "Foo.hs"
            "module Foo where\n\
            \\n\
            \import Bar\n\
            \\n\
            \foo = bar >> bar\n\
            \\n\
            \foobar = putStrLn \"Baz\"\n"

        update $ updMod "Bar.hs"
            "module Bar where\n\
            \\n\
            \bar = putStrLn \"Hello, world!\"\n"

        do gif <- getSpanInfo sess
           assertIdInfo gif "Bar" (3, 8, 3, 9) "putStrLn (VarName) :: String -> IO () defined in base-4.5.1.0:System.IO at <no location info> (home base-4.5.1.0:System.IO) (imported from base-4.5.1.0:Prelude at Bar.hs@1:8-1:11)"

        update $ updMod "Baz.hs"
            "module Baz where\n\
            \\n\
            \import Foo\n\
            \import Bar\n\
            \\n\
            \baz = foobar\n"

        assertNoErrors sess

        do gif <- getSpanInfo sess
           assertIdInfo gif "Bar" (3, 8, 3, 9) "putStrLn (VarName) :: String -> IO () defined in base-4.5.1.0:System.IO at <no location info> (home base-4.5.1.0:System.IO) (imported from base-4.5.1.0:Prelude at Bar.hs@1:8-1:11)"
           assertIdInfo gif "Baz" (6, 8, 6, 9) "foobar (VarName) :: IO () defined in main:Foo at Foo.hs@7:1-7:7 (imported from main:Foo at Baz.hs@3:1-3:11)"

        update $ updMod "Baz.hs"
            "module Baz where\n\
            \\n\
            \import Foo\n\
            \import Bar\n\
            \\n\
            \baz = foobar >>>> foo >> bar\n"

        assertOneError sess

        do gif <- getSpanInfo sess
           assertIdInfo gif "Bar" (3, 8, 3, 9) "putStrLn (VarName) :: String -> IO () defined in base-4.5.1.0:System.IO at <no location info> (home base-4.5.1.0:System.IO) (imported from base-4.5.1.0:Prelude at Bar.hs@1:8-1:11)"
           -- Baz is broken at this point

        update $ updMod "Baz.hs"
            "module Baz where\n\
            \\n\
            \import Foo\n\
            \import Bar\n\
            \\n\
            \baz = foobar >> foo >> bar\n"

        assertNoErrors sess

        do gif <- getSpanInfo sess
           assertIdInfo gif "Bar" (3, 8, 3, 9) "putStrLn (VarName) :: String -> IO () defined in base-4.5.1.0:System.IO at <no location info> (home base-4.5.1.0:System.IO) (imported from base-4.5.1.0:Prelude at Bar.hs@1:8-1:11)"
           assertIdInfo gif "Baz" (6, 8, 6, 9) "foobar (VarName) :: IO () defined in main:Foo at Foo.hs@7:1-7:7 (imported from main:Foo at Baz.hs@3:1-3:11)"
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
    , ifIdeBackendHaddockTestsEnabled defaultSessionConfig $ \sess -> do
        let cb = \_ -> return ()
            update = flip (updateSession sess) cb
            updMod = \mod code -> updateModule mod (BSLC.pack code)

        update $ updMod "Control/Parallel.hs"
            "module Control.Parallel where\n\
            \\n\
            \import Bar\n\
            \\n\
            \foo = bar >> bar\n\
            \\n\
            \foobar = putStrLn \"Baz\"\n"

        update $ updMod "Bar.hs"
            "module Bar where\n\
            \\n\
            \bar = putStrLn \"Hello, world!\"\n"

        update $ updMod "Baz.hs"
            "module Baz where\n\
            \\n\
            \import Control.Parallel\n\
            \import Bar\n\
            \\n\
            \baz = foobar\n"

        assertNoErrors sess

        do gif <- getSpanInfo sess
           assertIdInfo gif "Bar" (3, 8, 3, 9) "putStrLn (VarName) :: String -> IO () defined in base-4.5.1.0:System.IO at <no location info> (home base-4.5.1.0:System.IO) (imported from base-4.5.1.0:Prelude at Bar.hs@1:8-1:11)"
-- would fail:            assertIdInfo gif "Baz" (6, 8, 6, 9) "foobar (VarName) :: IO () defined in main:Control.Parallel at Control/Parallel.hs@7:1-7:7 (imported from main:Control.Parallel at Baz.hs@3:1-3:24)"

        update $ updMod "Baz.hs"
            "module Baz where\n\
            \\n\
            \import Control.Parallel\n\
            \import Bar\n\
            \\n\
            \baz = foobar >>>> foo >> bar\n"

        assertOneError sess

        do gif <- getSpanInfo sess
           assertIdInfo gif "Bar" (3, 8, 3, 9) "putStrLn (VarName) :: String -> IO () defined in base-4.5.1.0:System.IO at <no location info> (home base-4.5.1.0:System.IO) (imported from base-4.5.1.0:Prelude at Bar.hs@1:8-1:11)"
           -- Baz is broken at this point

        update $ updMod "Baz.hs"
            "module Baz where\n\
            \\n\
            \import Control.Parallel\n\
            \import Bar\n\
            \\n\
            \baz = foobar >> foo >> bar\n"

        assertNoErrors sess

        do gif <- getSpanInfo sess
           assertIdInfo gif "Bar" (3, 8, 3, 9) "putStrLn (VarName) :: String -> IO () defined in base-4.5.1.0:System.IO at <no location info> (home base-4.5.1.0:System.IO) (imported from base-4.5.1.0:Prelude at Bar.hs@1:8-1:11)"
-- would fail:           assertIdInfo gif "Baz" (6, 8, 6, 9) "foobar (VarName) :: IO () defined in main:Control.Parallel at Control/Parallel.hs@7:1-7:7 (imported from main:Control/Parallel at Baz.hs@3:1-3:24)"
    )
  , ( "Consistency of multiple modules of the same name: PackageImports"
    , ifIdeBackendHaddockTestsEnabled defaultSessionConfig $ \sess -> do
        let cb = \_ -> return ()
            update = flip (updateSession sess) cb
            updMod = \mod code -> updateModule mod (BSLC.pack code)

        update $ updMod "Control/Parallel.hs"
            "module Control.Parallel where\n\
            \\n\
            \import Bar\n\
            \\n\
            \foo = bar >> bar\n\
            \\n\
            \par = putStrLn \"Baz\"\n"

        update $ updMod "Bar.hs"
            "module Bar where\n\
            \\n\
            \bar = putStrLn \"Hello, world!\"\n"

        update $ updMod "Baz.hs"
            "{-# LANGUAGE PackageImports #-}\n\
            \module Baz where\n\
            \\n\
            \import \"parallel\" Control.Parallel\n\
            \import Bar\n\
            \\n\
            \baz = par\n"

        assertNoErrors sess

        do gif <- getSpanInfo sess
           assertIdInfo gif "Bar" (3, 8, 3, 9) "putStrLn (VarName) :: String -> IO () defined in base-4.5.1.0:System.IO at <no location info> (home base-4.5.1.0:System.IO) (imported from base-4.5.1.0:Prelude at Bar.hs@1:8-1:11)"
           assertIdInfo gif "Baz" (7, 8, 7, 9) "par (VarName) :: a1 -> b1 -> b1 defined in parallel-X.Y.Z:Control.Parallel at <no location info> (home parallel-X.Y.Z:Control.Parallel) (imported from parallel-X.Y.Z:Control.Parallel at Baz.hs@4:1-4:35)"
    )
  , ( "Quickfix for Updating static files never triggers recompilation"
    , withSession defaultSessionConfig $ \session -> do
        let upd = updateDataFile "A.foo" (BSLC.pack "unboundVarName")
               <> (updateModule "A.hs" . BSLC.pack . unlines $
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
    , withSession defaultSessionConfig $ \session -> do
        let upd = updateDataFile "A.foo" (BSLC.pack "42 is a wrong var name")
               <> (updateModule "A.hs" . BSLC.pack . unlines $
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
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
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
  , ( "Module name visible from 2 packages"
    , let packageOpts = [ "-hide-all-packages"
                        , "-package base"
                        , "-package MonadCatchIO-mtl"
                        , "-package MonadCatchIO-transformers"
                        ]
      in withSession (withOpts packageOpts) $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE PackageImports #-}"
                    , "module A where"
                    , "import Control.Monad.CatchIO"
                    ])
        updateSessionD session upd 1
        assertOneError session
    )
  , ( "Module name visible from 2 packages --- w/o configGenerateModInfo"
    , let packageOpts = [ "-hide-all-packages"
                        , "-package base"
                        , "-package MonadCatchIO-mtl"
                        , "-package MonadCatchIO-transformers"
                        ]
          config      = defaultSessionConfig {
                            configGenerateModInfo = False
                          , configStaticOpts      = packageOpts
                          }
      in withSession config $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "import Control.Monad.CatchIO"
                    ])
        updateSessionD session upd 1
        assertOneError session
    )
  , ( "Module name visible from 2 packages --- picked from -transformers"
    , let packageOpts = [ "-hide-all-packages"
                        , "-package base"
                        , "-package MonadCatchIO-mtl"
                        , "-package MonadCatchIO-transformers"
                        ]
      in ifIdeBackendHaddockTestsEnabled (withOpts packageOpts) $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE PackageImports #-}"
                    , "module A where"
                    , "import \"MonadCatchIO-transformers\" Control.Monad.CatchIO"
                    , "f x = catches (print 1) [x]"
                    ])
        updateSessionD session upd 1
        assertNoErrors session

        -- TODO: abstract away from version numbers
        imports <- getImports session
        let base mod = ModuleId {
                moduleName    = Text.pack mod
              , modulePackage = PackageId {
                    packageName    = Text.pack "base"
                  , packageVersion = Just (Text.pack "4.5.1.0")
                  }
              }
            monadCatchIO_transformers mod = ModuleId {
                moduleName    = Text.pack mod
              , modulePackage = PackageId {
                    packageName    = Text.pack "MonadCatchIO-transformers"
                  , packageVersion = Just (Text.pack "0.3.0.0")
                  }
              }
        assertSameSet "imports: " (fromJust . imports $ Text.pack "A") $ [
            Import {
                importModule    = base "Prelude"
              , importPackage   = Nothing
              , importQualified = False
              , importImplicit  = True
              , importAs        = Nothing
              , importEntities  = ImportAll
              }
          , Import {
                importModule    = monadCatchIO_transformers "Control.Monad.CatchIO"
              , importPackage   = Just (Text.pack "MonadCatchIO-transformers")
              , importQualified = False
              , importImplicit  = False
              , importAs        = Nothing
              , importEntities  = ImportAll
              }
          ]

        idInfo <- getSpanInfo session
        assertIdInfo idInfo "A" (4,7,4,14) "catches (VarName) :: MonadCatchIO m => m a -> [Handler m a] -> m a defined in MonadCatchIO-transformers-X.Y.Z:Control.Monad.CatchIO at <no location info> (home MonadCatchIO-transformers-X.Y.Z:Control.Monad.CatchIO) (imported from MonadCatchIO-transformers-X.Y.Z:Control.Monad.CatchIO at A.hs@3:1-3:57)"
    )
  , ( "Module name visible from 2 packages --- picked from -mtl (expected failure)"
    , let packageOpts = [ "-hide-all-packages"
                        , "-package base"
                        , "-package MonadCatchIO-mtl"
                        , "-package MonadCatchIO-transformers"
                        ]
      in ifIdeBackendHaddockTestsEnabled (withOpts packageOpts) $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE PackageImports #-}"
                    , "module A where"
                    , "import \"MonadCatchIO-mtl\" Control.Monad.CatchIO"
                    , "f x = catches (print 1) [x]"
                    ])
        updateSessionD session upd 1
        assertNoErrors session

        -- TODO: abstract away from version numbers
        imports <- getImports session
        let base mod = ModuleId {
                moduleName    = Text.pack mod
              , modulePackage = PackageId {
                    packageName    = Text.pack "base"
                  , packageVersion = Just (Text.pack "4.5.1.0")
                  }
              }
            monadCatchIO_transformers mod = ModuleId {
                moduleName    = Text.pack mod
              , modulePackage = PackageId {
                    packageName    = Text.pack "MonadCatchIO-mtl"
                  , packageVersion = Just (Text.pack "0.3.0.5")
                  }
              }
        assertSameSet "imports: " (fromJust . imports $ Text.pack "A") $ [
            Import {
                importModule    = base "Prelude"
              , importPackage   = Nothing
              , importQualified = False
              , importImplicit  = True
              , importAs        = Nothing
              , importEntities  = ImportAll
              }
          , Import {
                importModule    = monadCatchIO_transformers "Control.Monad.CatchIO"
              , importPackage   = Just (Text.pack "MonadCatchIO-mtl")
              , importQualified = False
              , importImplicit  = False
              , importAs        = Nothing
              , importEntities  = ImportAll
              }
          ]

        idInfo <- getSpanInfo session
        -- TODO: This is the IdInfo we *should* be getting
        -- assertIdInfo idInfo "A" (4,7,4,14) "catches (VarName) :: MonadCatchIO m => m a -> [Handler m a] -> m a defined in MonadCatchIO-mtl-X.Y.Z:Control.Monad.CatchIO at <no location info> (home MonadCatchIO-mtl-X.Y.Z:Control.Monad.CatchIO) (imported from MonadCatchIO-mtl-X.Y.Z:Control.Monad.CatchIO at A.hs@3:1-3:48)"
        -- but this is what we get instead (see #95):
        assertIdInfo idInfo "A" (4,7,4,14) "catches (VarName) :: MonadCatchIO m => m a -> [Handler m a] -> m a defined in MonadCatchIO-mtl-X.Y.Z:Control.Monad.CatchIO at <no location info> (home MonadCatchIO-mtl-X.Y.Z:Control.Monad.CatchIO) (imported from MonadCatchIO-transformers-X.Y.Z:Control.Monad.CatchIO at A.hs@3:1-3:48)"
    )
  , ("Issue #119"
    , withSession defaultSessionConfig $ \sess -> do
        distDir <- getDistDir sess

        let cb = \_ -> return ()
            update = flip (updateSession sess) cb

        update $ updateCodeGeneration True
        update $ updateStdoutBufferMode (RunLineBuffering Nothing)
        update $ updateStderrBufferMode (RunLineBuffering Nothing)

        -- Compile code and execute

        update $ updateModule "src/Main.hs" . BSLC.pack $
            "main = putStrLn \"Version 1\""
        assertNoErrors sess

        update $ buildExe [(Text.pack "Main", "src/Main.hs")]
        out1 <- readProcess (distDir </> "build" </> "Main" </> "Main") [] []
        assertEqual "" "Version 1\n" out1

        -- Update the code and execute again

        update $ updateModule "src/Main.hs" . BSLC.pack $
            "main = putStrLn \"Version 2\""
        assertNoErrors sess

        update $ buildExe [(Text.pack "Main", "src/Main.hs")]
        out2 <- readProcess (distDir </> "build" </> "Main" </> "Main") [] []
        assertEqual "" "Version 2\n" out2
    )
  , ( "Subexpression types 1: Simple expressions"
    , withSession (withOpts ["-XNoMonomorphismRestriction"]) $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
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
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ {-  1 -} "module A where"
                    , {-  2 -} "import Language.Haskell.TH.Syntax"
                      -- Quotations (expressions, types)
                      --        1234567890123456789
                    , {-  3 -} "qTrue = [|  True |]"
                    , {-  4 -} "qBool = [t| Bool |]"
                      --        1234567890123456789012345
                    , {-  5 -} "qComp x xs = [| x : xs |]"
                    ])
               <> (updateModule "B.hs" . BSLC.pack . unlines $
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
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $ [
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
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $ [
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
    , withSession defaultSessionConfig $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $ [
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
    , withSession defaultSessionConfig $ \sess -> do
        let cb     = \_ -> return ()
            update = flip (updateSession sess) cb

        update $ updateCodeGeneration True
        update $ updateStdoutBufferMode (RunLineBuffering Nothing)
        update $ updateStderrBufferMode (RunLineBuffering Nothing)
        update $ updateGhcOptions $ Just ["-Wall", "-Werror"]

        update $ updateModule "src/Main.hs" $ BSLC.pack $ unlines [
            "module Main where"

          , "import System.Process"
	  , "import System.IO"

          , "main :: IO ()"
          , "main = do"
          , "    (_,Just maybeOut,_,pr) <- createProcess $ CreateProcess"
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
              Just (output, RunOk _) -> assertEqual "" (BSLC.pack "123\n") output
	      Nothing -> assertFailure "Timeout in runWaitAll"
              _       -> assertFailure "Unexpected run result"
	  Nothing ->
	    assertFailure "Timeout in runStmt"
    )
  ]

deletePackage :: FilePath -> IO ()
deletePackage pkgDir = do
  forM_ [["-v0", "unregister", takeFileName pkgDir]] $ \cmd -> do
    (_,_,_,r2) <- Process.createProcess (Process.proc "ghc-pkg" cmd)
                    { Process.cwd = Just pkgDir
                    , Process.std_err = Process.CreatePipe }
    void $ Process.waitForProcess r2

installPackage :: FilePath -> IO ()
installPackage pkgDir = do
  forM_ ["clean", "configure", "build", "copy", "register"] $ \cmd -> do
    (_,_,_,r2) <- Process.createProcess (Process.proc "cabal" ["-v0", cmd])
                    { Process.cwd = Just pkgDir }
    void $ Process.waitForProcess r2

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
            config   = defaultSessionConfig {
                           configGenerateModInfo = genModInfo
                         , configStaticOpts      = opts
                         }
        testCase caseName $ do
          debug dVerbosity $ featureName ++ " / " ++ caseName ++ ":"
          traceEventIO ("TEST " ++ featureName ++ " / " ++ caseName)
          withSession config $ \session -> do
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

updateSessionP :: IdeSession -> IdeSessionUpdate -> [(Int, Int, String)] -> IO ()
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

updateSessionD :: IdeSession -> IdeSessionUpdate -> Int -> IO ()
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

loadModule :: FilePath -> String -> IdeSessionUpdate
loadModule file contents =
    let mod =  "module " ++ mname file ++ " where\n" ++ contents
    in updateModule file (BSLC.pack mod)
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

assertIdInfo :: (ModuleName -> SourceSpan -> [(SourceSpan, SpanInfo)])
             -> String
             -> (Int, Int, Int, Int)
             -> String
             -> Assertion
assertIdInfo idInfo mod (frLine, frCol, toLine, toCol) expected =
    assertEqual "" (ignoreVersions expected) (ignoreVersions actual)
  where
    ignoreVersions :: String -> String
    ignoreVersions s = subRegex (mkRegex versionRegexp) s "X.Y.Z"

    versionRegexp :: String
    versionRegexp = "[0-9]+(\\.[0-9]+)+"

    actual = case idInfo (Text.pack mod) span of
      [] -> ""
      hd : _ -> show $ snd hd

    span = SourceSpan { spanFilePath   = mod ++ ".hs"
                      , spanFromLine   = frLine
                      , spanFromColumn = frCol
                      , spanToLine     = toLine
                      , spanToColumn   = toCol
                      }

assertExpTypes :: (ModuleName -> SourceSpan -> [(SourceSpan, Text)])
               -> String
               -> (Int, Int, Int, Int)
               -> [(Int, Int, Int, Int, String)]
               -> Assertion
assertExpTypes expTypes mod (frLine, frCol, toLine, toCol) expected =
    assertEqual "" expected actual
  where
    actual = flip map (expTypes (Text.pack mod) span) $ \(span', typ) ->
      ( spanFromLine span'
      , spanFromColumn span'
      , spanToLine span'
      , spanToColumn span'
      , Text.unpack typ
      )

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
      if expectedErr `isInfixOf` Text.unpack actual
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

show3errors :: [SourceError] -> String
show3errors errs =
  let shown = List.intercalate "\n" (map show $ take 3 $ errs)
      more | length errs > 3 = "\n... and more ..."
           | otherwise       = ""
  in shown ++ more

restartRun :: [String] -> ExitCode -> Assertion
restartRun code exitCode =
      withSession defaultSessionConfig $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
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

        -- Just one more extra perverse test, since we have the setup ready.
        resOrEx <- runWait runActionsBefore
        case resOrEx of
          Right RunForceCancelled -> return ()
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx

testBufferMode :: RunBufferMode -> Assertion
testBufferMode bufferMode =
  withSession defaultSessionConfig $ \session -> do
    let upd = (updateCodeGeneration True)
           <> (updateStdoutBufferMode bufferMode)
           <> (updateStderrBufferMode bufferMode)
           <> (updateModule "M.hs" . BSLC.pack . unlines $
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
                      Right (RunOk _) ->
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
