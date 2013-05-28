{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Prelude hiding (span, mod)
import Control.Concurrent (threadDelay)
import qualified Control.Exception as Ex
import Control.Monad (liftM, void, forM_, when)
import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as BSSC (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BSLC (pack)
import qualified Data.ByteString.Lazy.UTF8 as BSL8 (fromString)
import Data.List (sort, isPrefixOf, isSuffixOf)
import qualified Data.List as List
import Data.Maybe (fromJust)
import Data.Monoid (mconcat, mempty, (<>))
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import System.Directory
import System.Exit (ExitCode (..))
import System.FilePath
import System.FilePath.Find (always, extension, find)
import System.IO.Temp (withTempDirectory)
import System.Process (readProcess)
import System.Random (randomRIO)
import Data.Text (Text)
import qualified Data.Text as Text

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertBool, assertEqual, assertFailure, (@?=))

import IdeSession
import TestTools
import Debug

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

-- | Run the specified action with a new IDE session, configured to use a
-- temporary directory
withConfiguredSessionDetailed :: Bool -> Maybe [FilePath] -> [String]
                              -> (IdeSession -> IO a)
                              -> IO a
withConfiguredSessionDetailed configGenerateModInfo configPackageDBStack
                              opts io = do
  slashTmp <- getTemporaryDirectory
  withTempDirectory slashTmp "ide-backend-test." $ \configDir -> do
    let sessionConfig = defaultSessionConfig{
                            configDir
                          , configStaticOpts = opts
                          , configGenerateModInfo
                          , configPackageDBStack
                          }
    withSession sessionConfig io

withConfiguredSession :: [String] -> (IdeSession -> IO a) -> IO a
withConfiguredSession = withConfiguredSessionDetailed True Nothing

-- | Run the specified action with a new IDE session
withSession :: SessionConfig -> (IdeSession -> IO a) -> IO a
withSession config = Ex.bracket (initSession config) shutdownSession

-- | Like 'withSession', but with a monadic configuration
withSession' :: IO SessionConfig -> (IdeSession -> IO a) -> IO a
withSession' config' io = do
  config <- config'
  withSession config io

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
        restartSession session
        updateSessionD session mempty (length lm)  -- all compiled anew
        assertNoErrors session
        mex2 <- Ex.try $ runStmt session "Main" "main"
        case mex2 of
          Right runActions -> void $ runWaitAll runActions  -- now runWaitAll
          Left ex -> assertEqual "runStmt" (userError "Module \"Main\" not successfully loaded, when trying to run code.") ex
        restartSession session
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session ->
        -- withConfiguredSession will shutdown the session as well
        shutdownSession session
    )
  , ( "Permit a session within a session and duplicated shutdownSession"
    , withConfiguredSession defOpts $ \session -> do
        loadModulesFrom session "test/ABnoError"
        config <- getSessionConfig session
        let tweakConfig :: Int -> SessionConfig -> IO SessionConfig
            tweakConfig n cfg@SessionConfig{configDir} = do
              let newDir = configDir </> "new" ++ show n
              createDirectory newDir
              return cfg { configDir = newDir }
        withSession' (tweakConfig 2 config) $ \s2 -> do
         withSession' (tweakConfig 3 config) $ \s3 -> do
          withSession' (tweakConfig 4 config) $ \_s4 -> do
           let update2 = loadModule "M.hs" "a = unknownX"
           updateSessionD s2 update2 1
           assertOneError s2
           withSession' (tweakConfig 5 config) $ \s5 -> do
            let update3 = loadModule "M.hs" "a = 3"
            updateSessionD s3 update3 1
            assertNoErrors session
            shutdownSession s5 -- <-- duplicate "nested" shutdown
    )
  , ( "Compile a project: A depends on B, error in A"
    , withConfiguredSession defOpts $ \session -> do
        loadModulesFrom session "test/AerrorB"
        assertSourceErrors session [(Just "A.hs", "No instance for (Num (IO ()))")]
     )
  , ( "Compile a project: A depends on B, error in B"
    , withConfiguredSession defOpts $ \session -> do
        loadModulesFrom session "test/ABerror"
        assertSourceErrors session [(Just "B.hs", "No instance for (Num (IO ()))")]
    )
  , ( "Compile and run a project with some .lhs files"
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
        setCurrentDirectory "test/compiler/utils"
        loadModulesFrom session "."
        setCurrentDirectory "../../../"
        assertNoErrors session
        status0 <- getBuildExeStatus session
        assertEqual "before exe build" Nothing status0
        let m = "Maybes"
            upd = buildExe [(Text.pack m, m <.> "lhs")]
        updateSessionD session upd 4
        status1 <- getBuildExeStatus session
        assertEqual "after exe build" (Just ExitSuccess) status1
        let m2 = "Exception"
            upd2 = buildExe [(Text.pack m2, m2 <.> "hs")]
        updateSessionD session upd2 4
        let m3 = "Main"
            upd3 = buildExe [(Text.pack m3, "Subdir" </> m3 <.> "lhs")]
        updateSessionD session upd3 4
        let upd4 = buildExe [(Text.pack m, m <.> "lhs")]
        updateSessionD session upd4 4
        distDir <- getDistDir session
        out <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "Maybes exe output"
                    "False\n"
                    out
        out2 <- readProcess (distDir </> "build" </> m2 </> m2) [] []
        assertEqual "Exception exe output"
                    ""
                    out2
        out3 <- readProcess (distDir </> "build" </> m3 </> m3) [] []
        assertEqual "Main exe output"
                    ""
                    out3
        status4 <- getBuildExeStatus session
        assertEqual "after all exe builds" (Just ExitSuccess) status4
    )
  , ( "Build haddocks from some .lhs files"
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
  , ( "Reject a program requiring -XNamedFieldPuns, then set the option"
    , let packageOpts = [ "-hide-all-packages"
                        , "-package mtl"
                        , "-package base"
                        , "-package array"
                        , "-package bytestring"
                        , "-package containers"
                        , "-package binary"
                        ]
      in withConfiguredSession packageOpts $ \session -> do
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
        let punOpts = packageOpts ++ [ "-XNamedFieldPuns", "-XRecordWildCards"]
            update2 = updateGhcOptions (Just punOpts)
        (_, lm) <- getModules session
        updateSessionD session update2 (length lm)
        msgs2 <- getSourceErrors session
        assertSomeErrors msgs2
-- TODO: the hack with supplying .h as a data file no longer works;
-- we need a proper support for .h
--      assertNoErrors msgs2
    )
  , ( "Test CWD by reading a data file"
    , withConfiguredSession defOpts $ \session -> do
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
{- Now that we always load the RTS, we're never in this situation
  , ("Reject getSourceErrors without updateSession"
    , withConfiguredSession defOpts $ \session ->
        assertRaises "getSourceErrors session"
          (== userError "This session state does not admit queries.")
          (getSourceErrors session)
    )
-}
  , ("Reject updateSession after shutdownSession"
    , withConfiguredSession defOpts $ \session -> do
        shutdownSession session
        assertRaises "updateSessionD session mempty"
          (== userError "Session already shut down.")
          (updateSessionD session mempty 0)
    )
  , ("Reject getSourceErrors after shutdownSession"
    , withConfiguredSession defOpts $ \session -> do
        shutdownSession session
        assertRaises "getSourceErrors session"
          (== userError "Session already shut down.")
          (getSourceErrors session)
    )
  , ("Reject runStmt after shutdownSession"
    , withConfiguredSession defOpts $ \session -> do
        shutdownSession session
        assertRaises "runStmt session Main main"
          (== userError "Session already shut down.")
          (runStmt session "Main" "main")
    )
  , ( "Test recursive modules"
    , withConfiguredSession defOpts $ \session -> do
        loadModulesFrom session "test/bootMods"
        -- Fails, because special support is needed, similar to .h files.
        -- Proabably, the .hs-boot files should be copied to the src dir,
        -- but not made GHC.load targets.
        assertOneError session
    )
  , ( "Test TH; code generation on"
    , let packageOpts =
            defOpts ++ ["-package template-haskell", "-XTemplateHaskell"]
      in withConfiguredSession packageOpts $ \session -> do
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
    , let packageOpts = defOpts ++ ["-package template-haskell", "-XTemplateHaskell"]
      in withConfiguredSession packageOpts $ \session -> do
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
    , let packageOpts = defOpts ++ ["-package template-haskell", "-XTemplateHaskell"]
      in withConfiguredSession packageOpts $ \session -> do
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
    , let packageOpts = defOpts ++ ["-XCPP"]
      in withConfiguredSession packageOpts $ \session -> do
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
        assertIdInfo idInfo "Good" (8, 1, 8, 2) "x (VarName) :: [a] (binding occurrence)"
    )
  , ( "Reject a wrong CPP directive"
    , let packageOpts = [ "-hide-all-packages"
                        , "-XCPP"
                        ]
      in withConfiguredSession packageOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
        restartSession session

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
                                   configDir        = relativePath
                                 , configStaticOpts = defOpts
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
        restartSession session
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , withConfiguredSession [] $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
    , let packageOpts = [ "-hide-all-packages"
                        , "-package base"
                        , "-package parallel"
                        , "-package old-time"
                        ]
      in withConfiguredSession packageOpts $ \session -> do
        setCurrentDirectory "test/MainModule"
        loadModulesFrom session "."
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
    , let packageOpts = [ "-hide-all-packages"
                        , "-package base"
                        , "-package parallel"
                        , "-package old-time"
                        ]
      in withConfiguredSession packageOpts $ \session -> do
        setCurrentDirectory "test/MainModule"
        loadModulesFrom session "."
        setCurrentDirectory "../../"
        let m = "ParFib.Main"
            upd = buildExe [ (Text.pack m, "ParFib.Main.hs")
                           , (Text.pack "Main", "ParFib.hs") ]
        updateSessionD session upd 4
        let upd2 = buildExe [(Text.pack "Main", "ParFib.hs")]
        updateSessionD session upd2 4
        distDir <- getDistDir session
        fibOut <- readProcess (distDir </> "build" </> m </> m) [] []
        assertEqual "ParFib exe output"
                    "running 'A single file with a code to run in parallel' from test/MainModule, which says fib 24 = 75025\n"
                    fibOut
    )
  , ( "Build executable and fail"
    , let packageOpts = [ "-hide-all-packages"
                        , "-package base"
                        , "-package parallel"
                        , "-package old-time"
                        ]
      in withConfiguredSession packageOpts $ \session -> do
        setCurrentDirectory "test/MainModule"
        loadModulesFrom session "."
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
    , let packageOpts = [ "-hide-all-packages"
                        , "-package base"
                        , "-package parallel"
                        , "-package old-time"
                        ]
      in withConfiguredSession packageOpts $ \session -> do
        setCurrentDirectory "test/MainModule"
        loadModulesFrom session "."
        setCurrentDirectory "../../"
        let upd = buildDoc
        updateSessionD session upd 4
        distDir <- getDistDir session
        indexExists <- doesFileExist $ distDir </> "doc/html/main/index.html"
        assertBool "ParFib haddock files" indexExists
    )
  , ( "Build executable with empty package db stack and fail"
    , let packageOpts = [ "-hide-all-packages"
                        , "-package base"
                        , "-package parallel"
                        , "-package old-time"
                        ]
      in withConfiguredSessionDetailed True {-(Just [])-}Nothing packageOpts
         $ \session -> do
        setCurrentDirectory "test/MainModule"
        loadModulesFrom session "."
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
  , ( "Type information 1: Local identifiers and Prelude"
    , withConfiguredSession defOpts $ \session -> do
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
        assertIdInfo idInfo "A" (2,1,2,2) "a (VarName) :: Int (binding occurrence)"
        assertIdInfo idInfo "A" (3,1,3,2) "b (VarName) :: Int (binding occurrence)"
        assertIdInfo idInfo "A" (3,5,3,6) "a (VarName) :: Int (defined at A.hs@2:1-2:2)"
        assertIdInfo idInfo "A" (3,7,3,8) "+ (VarName) :: Num a => a -> a -> a (defined in base-4.5.1.0:GHC.Num at <no location info>; imported from base-4.5.1.0:Prelude at A.hs@1:8-1:9)"
        assertIdInfo idInfo "A" (4,1,4,2) "c (VarName) :: Bool (binding occurrence)"
        assertIdInfo idInfo "A" (4,5,4,9) "True (DataName) (wired in to the compiler)"
        assertIdInfo idInfo "A" (5,1,5,2) "d (VarName) :: (a -> b -> b) -> b -> [a] -> b (binding occurrence)"
        assertIdInfo idInfo "A" (5,5,5,10) "foldr (VarName) :: (a1 -> b1 -> b1) -> b1 -> [a1] -> b1 (defined in base-4.5.1.0:GHC.Base at <no location info>; imported from base-4.5.1.0:Prelude at A.hs@1:8-1:9)"
        {- TODO: reenable
        assertEqual "Haddock link for A.b should be correct"
                    "main/latest/doc/html/A.html#v:b" $
                    haddockLink (idMapToMap idMapB Map.! SourceSpan "B.hs" 5 8 5 9)
        -}
    )
  , ( "Type information 2: Simple ADTs"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "data T = MkT"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        idInfo <- getSpanInfo session
        assertIdInfo idInfo "A" (2,6,2,7) "T (TcClsName) (binding occurrence)"
        assertIdInfo idInfo "A" (2,10,2,13) "MkT (DataName) :: T (binding occurrence)"
    )
  , ( "Type information 3: Polymorphism"
    , withConfiguredSession defOpts $ \session -> do
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
        assertIdInfo idInfo "A" (2,6,2,12) "TMaybe (TcClsName) (binding occurrence)"
        assertIdInfo idInfo "A" (2,13,2,14) "a (TvName) (binding occurrence)"
        assertIdInfo idInfo "A" (2,17,2,25) "TNothing (DataName) :: TMaybe a (binding occurrence)"
        assertIdInfo idInfo "A" (2,28,2,33) "TJust (DataName) :: a -> TMaybe a (binding occurrence)"
        assertIdInfo idInfo "A" (2,34,2,35) "a (TvName) (defined at A.hs@2:13-2:14)"
        assertIdInfo idInfo "A" (4,1,4,3) "f1 (VarName) :: t -> t (binding occurrence)"
        assertIdInfo idInfo "A" (4,4,4,5) "x (VarName) :: t (binding occurrence)"
        assertIdInfo idInfo "A" (4,8,4,9) "x (VarName) :: t (defined at A.hs@4:4-4:5)"
        assertIdInfo idInfo "A" (5,1,5,3) "f2 (VarName) :: t -> t (binding occurrence)"
        assertIdInfo idInfo "A" (5,7,5,8) "x (VarName) :: t (binding occurrence)"
        assertIdInfo idInfo "A" (5,12,5,13) "x (VarName) :: t (defined at A.hs@5:7-5:8)"
        assertIdInfo idInfo "A" (7,1,7,3) "g1 (VarName) :: t -> t1 -> t (binding occurrence)"
        assertIdInfo idInfo "A" (7,4,7,5) "x (VarName) :: t (binding occurrence)"
        assertIdInfo idInfo "A" (7,6,7,7) "y (VarName) :: t1 (binding occurrence)"
        assertIdInfo idInfo "A" (7,10,7,11) "x (VarName) :: t (defined at A.hs@7:4-7:5)"
        assertIdInfo idInfo "A" (8,1,8,3) "g2 (VarName) :: t -> t1 -> t (binding occurrence)"
        assertIdInfo idInfo "A" (8,7,8,8) "x (VarName) :: t (binding occurrence)"
        assertIdInfo idInfo "A" (8,9,8,10) "y (VarName) :: t1 (binding occurrence)"
        assertIdInfo idInfo "A" (8,14,8,15) "x (VarName) :: t (defined at A.hs@8:7-8:8)"
        assertIdInfo idInfo "A" (10,1,10,3) "h1 (VarName) :: Bool (binding occurrence)"
        assertIdInfo idInfo "A" (10,6,10,10) "h1go (VarName) :: t -> t1 -> t (defined at A.hs@12:5-12:9)"
        assertIdInfo idInfo "A" (10,11,10,15) "True (DataName) (wired in to the compiler)"
        assertIdInfo idInfo "A" (10,16,10,21) "False (DataName) (wired in to the compiler)"
        assertIdInfo idInfo "A" (12,5,12,9) "h1go (VarName) :: t -> t1 -> t (binding occurrence)"
        assertIdInfo idInfo "A" (12,10,12,11) "x (VarName) :: t (binding occurrence)"
        assertIdInfo idInfo "A" (12,12,12,13) "y (VarName) :: t1 (binding occurrence)"
        assertIdInfo idInfo "A" (12,16,12,17) "x (VarName) :: t (defined at A.hs@12:10-12:11)"
        assertIdInfo idInfo "A" (14,1,14,3) "h2 (VarName) :: Bool (binding occurrence)"
        assertIdInfo idInfo "A" (14,6,14,10) "h2go (VarName) :: t -> t1 -> t (defined at A.hs@16:5-16:9)"
        assertIdInfo idInfo "A" (14,11,14,15) "True (DataName) (wired in to the compiler)"
        assertIdInfo idInfo "A" (14,16,14,21) "False (DataName) (wired in to the compiler)"
        assertIdInfo idInfo "A" (16,5,16,9) "h2go (VarName) :: t -> t1 -> t (binding occurrence)"
        assertIdInfo idInfo "A" (16,13,16,14) "x (VarName) :: t (binding occurrence)"
        assertIdInfo idInfo "A" (16,15,16,16) "y (VarName) :: t1 (binding occurrence)"
        assertIdInfo idInfo "A" (16,20,16,21) "x (VarName) :: t (defined at A.hs@16:13-16:14)"
    )
  , ( "Type information 4: Multiple modules"
    , withConfiguredSession defOpts $ \session -> do
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
        assertIdInfo idInfo "A" (2,6,2,7) "T (TcClsName) (binding occurrence)"
        assertIdInfo idInfo "A" (2,10,2,13) "MkT (DataName) :: T (binding occurrence)"
        assertIdInfo idInfo "B" (3,1,3,4) "foo (VarName) :: T (binding occurrence)"
        assertIdInfo idInfo "B" (3,7,3,10) "MkT (DataName) :: T (defined in main:A at A.hs@2:10-2:13; imported from main:A at B.hs@2:1-2:9)"
    )
  , ( "Type information 5: External packages, type sigs, scoped type vars, kind sigs"
    , let opts = defOpts ++ [
                     "-package parallel"
                   , "-XScopedTypeVariables"
                   , "-XKindSignatures"
                   ]
      in withConfiguredSession opts $ \session -> do
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
        assertIdInfo idInfo "A" (3,1,3,2) "e (VarName) :: Bool (binding occurrence)"
        assertIdInfo idInfo "A" (3,5,3,9) "True (DataName) (wired in to the compiler)"
        assertIdInfo idInfo "A" (3,10,3,16) "pseq (VarName) :: a -> b -> b (defined in parallel-3.2.0.3:Control.Parallel at <no location info>; imported from parallel-3.2.0.3:Control.Parallel at A.hs@2:1-2:24)"
        assertIdInfo idInfo "A" (3,17,3,22) "False (DataName) (wired in to the compiler)"
        assertIdInfo idInfo "A" (4,1,4,2) "f (VarName) :: a -> a (defined at A.hs@5:1-5:2)"
        -- The IdMap contains both of these:
        assertIdInfo idInfo "A" (4,6,4,7) "a (TvName) (defined at A.hs@4:6-4:7)"
--        assertIdInfo idInfo "A" (4,6,4,7) "a (TvName) (binding occurrence)"
        assertIdInfo idInfo "A" (4,11,4,12) "a (TvName) (defined at A.hs@4:6-4:7)"
        assertIdInfo idInfo "A" (5,1,5,2) "f (VarName) :: a -> a (binding occurrence)"
        assertIdInfo idInfo "A" (5,3,5,4) "x (VarName) :: a (binding occurrence)"
        assertIdInfo idInfo "A" (5,7,5,8) "x (VarName) :: a (defined at A.hs@5:3-5:4)"
        assertIdInfo idInfo "A" (6,1,6,2) "g (VarName) :: a -> a (defined at A.hs@7:1-7:2)"
        assertIdInfo idInfo "A" (6,13,6,14) "a (TvName) (binding occurrence)"
        assertIdInfo idInfo "A" (6,16,6,17) "a (TvName) (defined at A.hs@6:13-6:14)"
        assertIdInfo idInfo "A" (6,21,6,22) "a (TvName) (defined at A.hs@6:13-6:14)"
        assertIdInfo idInfo "A" (7,1,7,2) "g (VarName) :: a -> a (binding occurrence)"
        assertIdInfo idInfo "A" (7,3,7,4) "x (VarName) :: a (binding occurrence)"
        assertIdInfo idInfo "A" (7,7,7,8) "x (VarName) :: a (defined at A.hs@7:3-7:4)"
        assertIdInfo idInfo "A" (8,1,8,2) "h (VarName) :: a -> a (defined at A.hs@9:1-9:2)"
        assertIdInfo idInfo "A" (8,13,8,14) "a (TvName) (binding occurrence)"
        assertIdInfo idInfo "A" (8,16,8,17) "a (TvName) (defined at A.hs@8:13-8:14)"
        assertIdInfo idInfo "A" (8,21,8,22) "a (TvName) (defined at A.hs@8:13-8:14)"
        assertIdInfo idInfo "A" (9,1,9,2) "h (VarName) :: a -> a (binding occurrence)"
        assertIdInfo idInfo "A" (9,3,9,4) "x (VarName) :: a (binding occurrence)"
        assertIdInfo idInfo "A" (9,7,9,8) "y (VarName) :: a (defined at A.hs@12:5-12:6)"
        assertIdInfo idInfo "A" (11,5,11,6) "y (VarName) :: a (defined at A.hs@12:5-12:6)"
        assertIdInfo idInfo "A" (11,5,11,6) "y (VarName) :: a (defined at A.hs@12:5-12:6)"
        assertIdInfo idInfo "A" (11,10,11,11) "a (TvName) (defined at A.hs@8:13-8:14)"
        assertIdInfo idInfo "A" (11,10,11,11) "a (TvName) (defined at A.hs@8:13-8:14)"
        assertIdInfo idInfo "A" (12,5,12,6) "y (VarName) :: a (binding occurrence)"
        assertIdInfo idInfo "A" (12,9,12,10) "x (VarName) :: a (defined at A.hs@9:3-9:4)"
        assertIdInfo idInfo "A" (13,1,13,2) "i (VarName) :: t a -> t a (defined at A.hs@14:1-14:2)"
        assertIdInfo idInfo "A" (13,13,13,26) "t (TvName) (binding occurrence)"
        assertIdInfo idInfo "A" (13,27,13,28) "a (TvName) (binding occurrence)"
        assertIdInfo idInfo "A" (13,30,13,31) "t (TvName) (defined at A.hs@13:13-13:26)"
        assertIdInfo idInfo "A" (13,32,13,33) "a (TvName) (defined at A.hs@13:27-13:28)"
        assertIdInfo idInfo "A" (13,37,13,38) "t (TvName) (defined at A.hs@13:13-13:26)"
        assertIdInfo idInfo "A" (13,39,13,40) "a (TvName) (defined at A.hs@13:27-13:28)"
        assertIdInfo idInfo "A" (14,1,14,2) "i (VarName) :: t a -> t a (binding occurrence)"
        assertIdInfo idInfo "A" (14,3,14,4) "x (VarName) :: t a (binding occurrence)"
        assertIdInfo idInfo "A" (14,7,14,8) "x (VarName) :: t a (defined at A.hs@14:3-14:4)"
    )
  , ( "Type information 6: Reusing type variables"
    , withConfiguredSession ("-XScopedTypeVariables" : defOpts) $ \session -> do
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
        assertIdInfo idInfo "A" (2,1,2,3) "f1 (VarName) :: (t, t1) -> t (binding occurrence)"
        assertIdInfo idInfo "A" (2,5,2,6) "x (VarName) :: t (binding occurrence)"
        assertIdInfo idInfo "A" (2,8,2,9) "y (VarName) :: t1 (binding occurrence)"
        assertIdInfo idInfo "A" (2,13,2,14) "x (VarName) :: t (defined at A.hs@2:5-2:6)"
        assertIdInfo idInfo "A" (3,1,3,3) "f2 (VarName) :: (t, t1) -> t (binding occurrence)"
        assertIdInfo idInfo "A" (3,5,3,6) "x (VarName) :: t (binding occurrence)"
        assertIdInfo idInfo "A" (3,8,3,9) "y (VarName) :: t1 (binding occurrence)"
        assertIdInfo idInfo "A" (3,13,3,14) "x (VarName) :: t (defined at A.hs@3:5-3:6)"
        assertIdInfo idInfo "A" (4,1,4,3) "f3 (VarName) :: (t, t1) -> t (binding occurrence)"
        assertIdInfo idInfo "A" (4,5,4,6) "x (VarName) :: t (binding occurrence)"
        assertIdInfo idInfo "A" (4,8,4,9) "y (VarName) :: t1 (binding occurrence)"
        assertIdInfo idInfo "A" (4,13,4,15) "f4 (VarName) :: (t2, t3) -> t2 (defined at A.hs@6:5-6:7)"
        assertIdInfo idInfo "A" (4,17,4,18) "x (VarName) :: t (defined at A.hs@4:5-4:6)"
        assertIdInfo idInfo "A" (4,20,4,21) "y (VarName) :: t1 (defined at A.hs@4:8-4:9)"
        assertIdInfo idInfo "A" (6,5,6,7) "f4 (VarName) :: (t2, t3) -> t2 (binding occurrence)"
        assertIdInfo idInfo "A" (6,9,6,10) "x (VarName) :: t2 (binding occurrence)"
        assertIdInfo idInfo "A" (6,12,6,13) "y (VarName) :: t3 (binding occurrence)"
        assertIdInfo idInfo "A" (6,17,6,18) "x (VarName) :: t2 (defined at A.hs@6:9-6:10)"
        assertIdInfo idInfo "A" (7,1,7,3) "f5 (VarName) :: (t, t1) -> t (defined at A.hs@8:1-8:3)"
        assertIdInfo idInfo "A" (7,14,7,15) "t (TvName) (binding occurrence)"
        assertIdInfo idInfo "A" (7,16,7,18) "t1 (TvName) (binding occurrence)"
        assertIdInfo idInfo "A" (7,21,7,22) "t (TvName) (defined at A.hs@7:14-7:15)"
        assertIdInfo idInfo "A" (7,24,7,26) "t1 (TvName) (defined at A.hs@7:16-7:18)"
        assertIdInfo idInfo "A" (7,31,7,32) "t (TvName) (defined at A.hs@7:14-7:15)"
        assertIdInfo idInfo "A" (8,1,8,3) "f5 (VarName) :: (t, t1) -> t (binding occurrence)"
        assertIdInfo idInfo "A" (8,5,8,6) "x (VarName) :: t (binding occurrence)"
        assertIdInfo idInfo "A" (8,8,8,9) "y (VarName) :: t1 (binding occurrence)"
        assertIdInfo idInfo "A" (8,13,8,15) "f6 (VarName) :: (t, t2) -> t (defined at A.hs@11:5-11:7)"
        assertIdInfo idInfo "A" (8,17,8,18) "x (VarName) :: t (defined at A.hs@8:5-8:6)"
        assertIdInfo idInfo "A" (8,20,8,21) "y (VarName) :: t1 (defined at A.hs@8:8-8:9)"
        assertIdInfo idInfo "A" (10,5,10,7) "f6 (VarName) :: (t, t2) -> t (defined at A.hs@11:5-11:7)"
        assertIdInfo idInfo "A" (10,5,10,7) "f6 (VarName) :: (t, t2) -> t (defined at A.hs@11:5-11:7)"
        assertIdInfo idInfo "A" (10,18,10,20) "t2 (TvName) (binding occurrence)"
        assertIdInfo idInfo "A" (10,18,10,20) "t2 (TvName) (binding occurrence)"
        assertIdInfo idInfo "A" (10,23,10,24) "t (TvName) (defined at A.hs@7:14-7:15)"
        assertIdInfo idInfo "A" (10,23,10,24) "t (TvName) (defined at A.hs@7:14-7:15)"
        assertIdInfo idInfo "A" (10,26,10,28) "t2 (TvName) (defined at A.hs@10:18-10:20)"
        assertIdInfo idInfo "A" (10,26,10,28) "t2 (TvName) (defined at A.hs@10:18-10:20)"
        assertIdInfo idInfo "A" (10,33,10,34) "t (TvName) (defined at A.hs@7:14-7:15)"
        assertIdInfo idInfo "A" (10,33,10,34) "t (TvName) (defined at A.hs@7:14-7:15)"
        assertIdInfo idInfo "A" (11,5,11,7) "f6 (VarName) :: (t, t2) -> t (binding occurrence)"
        assertIdInfo idInfo "A" (11,9,11,10) "x (VarName) :: t (binding occurrence)"
        assertIdInfo idInfo "A" (11,12,11,13) "y (VarName) :: t2 (binding occurrence)"
        assertIdInfo idInfo "A" (11,17,11,18) "x (VarName) :: t (defined at A.hs@11:9-11:10)"
    )
  , ( "Type information 7: Qualified imports"
    , withConfiguredSession defOpts $ \session -> do
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
        assertIdInfo idInfo "A" (5,1,5,4) "foo (VarName) :: (Maybe a -> a, [Bool] -> Bool, (b -> b -> c) -> (a1 -> b) -> a1 -> a1 -> c) (binding occurrence)"
        assertIdInfo idInfo "A" (5,8,5,16) "fromJust (VarName) :: Maybe a2 -> a2 (defined in base-4.5.1.0:Data.Maybe at <no location info>; imported from base-4.5.1.0:Data.Maybe at A.hs@2:1-2:18)"
        assertIdInfo idInfo "A" (5,18,5,31) "and (VarName) :: [Bool] -> Bool (defined in base-4.5.1.0:GHC.List at <no location info>; imported from base-4.5.1.0:Data.List as 'Data.List.' at A.hs@3:1-3:27)"
        assertIdInfo idInfo "A" (5,33,5,37) "on (VarName) :: (b1 -> b1 -> c1) -> (a2 -> b1) -> a2 -> a2 -> c1 (defined in base-4.5.1.0:Data.Function at <no location info>; imported from base-4.5.1.0:Data.Function as 'F.' at A.hs@4:1-4:36)"
    )
  , ( "Type information 8: Imprecise source spans"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "main = print True"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        idInfo <- getSpanInfo session
        let infoPrint = "print (VarName) :: Show a => a -> IO () (defined in base-4.5.1.0:System.IO at <no location info>; imported from base-4.5.1.0:Prelude at A.hs@1:8-1:9)"
        assertIdInfo idInfo "A" (2,8,2,13) infoPrint
        assertIdInfo idInfo "A" (2,8,2,8) infoPrint
        assertIdInfo idInfo "A" (2,8,2,9) infoPrint
        assertIdInfo idInfo "A" (2,9,2,9) infoPrint
        assertIdInfo idInfo "A" (2,9,2,10) infoPrint
        assertIdInfo idInfo "A" (2,9,2,13) infoPrint
    )
  , ( "Type information 9: Quasi-quotation"
    , withConfiguredSession ("-package template-haskell" : defOpts) $ \session -> do
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
        assertIdInfo idInfo "B" (4,7,4,14) "quasi-quote with quoter qq (VarName) :: QuasiQuoter (defined in main:A at A.hs@4:1-4:3; imported from main:A at B.hs@3:1-3:9)"
        assertIdInfo idInfo "B" (5,7,5,14) "quasi-quote with quoter qq (VarName) :: QuasiQuoter (defined in main:A at A.hs@4:1-4:3; imported from main:A at B.hs@3:1-3:9)"
        assertIdInfo idInfo "B" (6,7,6,14) "quasi-quote with quoter qq (VarName) :: QuasiQuoter (defined in main:A at A.hs@4:1-4:3; imported from main:A at B.hs@3:1-3:9)"
        assertIdInfo idInfo "B" (7,7,7,14) "quasi-quote with quoter qq (VarName) :: QuasiQuoter (defined in main:A at A.hs@4:1-4:3; imported from main:A at B.hs@3:1-3:9)"
    )
  , ( "Type information 10: Template Haskell"
    , withConfiguredSession ("-package template-haskell" : defOpts) $ \session -> do
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
        assertIdInfo idInfo "A" (4,1,4,4) "ex1 (VarName) :: Q Exp (defined at A.hs@5:1-5:4)"
        assertIdInfo idInfo "A" (4,8,4,9) "Q (TcClsName) (defined in template-haskell-2.7.0.0:Language.Haskell.TH.Syntax at <no location info>; imported from template-haskell-2.7.0.0:Language.Haskell.TH at A.hs@3:1-3:27)"
        assertIdInfo idInfo "A" (4,10,4,13) "Exp (TcClsName) (defined in template-haskell-2.7.0.0:Language.Haskell.TH.Syntax at <no location info>; imported from template-haskell-2.7.0.0:Language.Haskell.TH at A.hs@3:1-3:27)"
        assertIdInfo idInfo "A" (5,1,5,4) "ex1 (VarName) :: Q Exp (binding occurrence)"
        assertIdInfo idInfo "A" (5,11,5,12) "x (VarName) (binding occurrence)"
        assertIdInfo idInfo "A" (5,11,5,12) "x (VarName) (binding occurrence)"
        assertIdInfo idInfo "A" (5,16,5,17) "x (VarName) (defined at A.hs@5:11-5:12)"
        assertIdInfo idInfo "A" (5,16,5,17) "x (VarName) (defined at A.hs@5:11-5:12)"
        assertIdInfo idInfo "A" (6,1,6,4) "ex2 (VarName) :: Q Type (defined at A.hs@7:1-7:4)"
        assertIdInfo idInfo "A" (6,8,6,9) "Q (TcClsName) (defined in template-haskell-2.7.0.0:Language.Haskell.TH.Syntax at <no location info>; imported from template-haskell-2.7.0.0:Language.Haskell.TH at A.hs@3:1-3:27)"
        assertIdInfo idInfo "A" (6,10,6,14) "Type (TcClsName) (defined in template-haskell-2.7.0.0:Language.Haskell.TH.Syntax at <no location info>; imported from template-haskell-2.7.0.0:Language.Haskell.TH at A.hs@3:1-3:27)"
        assertIdInfo idInfo "A" (7,1,7,4) "ex2 (VarName) :: Q Type (binding occurrence)"
        assertIdInfo idInfo "A" (7,11,7,17) "String (TcClsName) (defined in base-4.5.1.0:GHC.Base at <no location info>; imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9)"
        assertIdInfo idInfo "A" (7,11,7,17) "String (TcClsName) (defined in base-4.5.1.0:GHC.Base at <no location info>; imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9)"
        assertIdInfo idInfo "A" (7,21,7,27) "String (TcClsName) (defined in base-4.5.1.0:GHC.Base at <no location info>; imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9)"
        assertIdInfo idInfo "A" (7,21,7,27) "String (TcClsName) (defined in base-4.5.1.0:GHC.Base at <no location info>; imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9)"
        assertIdInfo idInfo "B" (4,1,4,4) "ex3 (VarName) :: String -> String (defined at B.hs@5:1-5:4)"
        assertIdInfo idInfo "B" (4,8,4,12) "ex2 (VarName) :: Q Type (defined in main:A at A.hs@7:1-7:4; imported from main:A at B.hs@3:1-3:9)"
        assertIdInfo idInfo "B" (5,1,5,4) "ex3 (VarName) :: String -> String (binding occurrence)"
        assertIdInfo idInfo "B" (5,7,5,11) "ex1 (VarName) :: Q Exp (defined in main:A at A.hs@5:1-5:4; imported from main:A at B.hs@3:1-3:9)"
    )
  , ( "Type information 11: Take advantage of scope (1)"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "main = print True"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        idInfo <- getSpanInfo session
        assertIdInfo idInfo "A" (2,8,2,13) "print (VarName) :: Show a => a -> IO () (defined in base-4.5.1.0:System.IO at <no location info>; imported from base-4.5.1.0:Prelude at A.hs@1:8-1:9)"
    )
  , ( "Type information 12: Take advantage of scope (2)"
    , withConfiguredSession ("-package bytestring" : defOpts) $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "import Data.ByteString (append)"
                    , "foo = append"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        idInfo <- getSpanInfo session
        assertIdInfo idInfo "A" (3,7,3,13) "append (VarName) :: Data.ByteString.Internal.ByteString -> Data.ByteString.Internal.ByteString -> Data.ByteString.Internal.ByteString (defined in bytestring-0.9.2.1:Data.ByteString at <no location info>; imported from bytestring-0.9.2.1:Data.ByteString at A.hs@2:25-2:31)"
    )
  , ( "Type information 13: Take advantage of scope (3)"
    , withConfiguredSession ("-package bytestring" : defOpts) $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "import Data.ByteString"
                    , "foo = append"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        idInfo <- getSpanInfo session
        assertIdInfo idInfo "A" (3,7,3,13) "append (VarName) :: ByteString -> ByteString -> ByteString (defined in bytestring-0.9.2.1:Data.ByteString at <no location info>; imported from bytestring-0.9.2.1:Data.ByteString at A.hs@2:1-2:23)"
    )
  , ( "Type information 14: Take advantage of scope (4)"
    , withConfiguredSession ("-package bytestring" : defOpts) $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "import Data.ByteString (append)"
                    , "import qualified Data.ByteString as BS"
                    , "foo = append"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        idInfo <- getSpanInfo session
        assertIdInfo idInfo "A" (4,7,4,13) "append (VarName) :: BS.ByteString -> BS.ByteString -> BS.ByteString (defined in bytestring-0.9.2.1:Data.ByteString at <no location info>; imported from bytestring-0.9.2.1:Data.ByteString as 'BS.' at A.hs@3:1-3:39)"
    )
  , ( "Type information 15: Other constructs"
    , withConfiguredSession defOpts $ \session -> do
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
        assertIdInfo idInfo "A" (4,10,4,12) "Eq (TcClsName) (defined in ghc-prim-0.2.0.0:GHC.Classes at <no location info>; imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9)"
        assertIdInfo idInfo "A" (5,18,5,23) "const (VarName) :: a -> b -> a (defined in base-4.5.1.0:GHC.Base at <no location info>; imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9)"
        assertIdInfo idInfo "A" (6,19,6,23) "Show (TcClsName) (defined in base-4.5.1.0:GHC.Show at <no location info>; imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9)"
        assertIdInfo idInfo "A" (6,24,6,27) "MkT (TcClsName) (defined at A.hs@3:6-3:9)"
        assertIdInfo idInfo "A" (8,10,8,13) "+++ (VarName) :: [a] -> [a] -> [a] (defined at A.hs@7:1-7:6)"
        assertIdInfo idInfo "A" (9,10,9,13) "Int (TcClsName) (wired in to the compiler)"
        -- TODO: for some reason the DEPRECATED/WARNING pragmas don't seem to
        -- be translated to HsWarnDecl ?
        assertIdInfo idInfo "A" (17,13,17,14) "x (VarName) :: Int (defined at A.hs@17:3-17:4)"
        assertIdInfo idInfo "A" (17,21,17,22) "x (VarName) :: Int (defined at A.hs@17:3-17:4)"
        assertIdInfo idInfo "A" (17,24,17,25) "y (VarName) :: Int (defined at A.hs@17:5-17:6)"
        assertIdInfo idInfo "A" (17,31,17,32) "x (VarName) :: Int (defined at A.hs@17:3-17:4)"
        assertIdInfo idInfo "A" (17,36,17,37) "z (VarName) :: Int (defined at A.hs@17:7-17:8)"
        assertIdInfo idInfo "A" (17,41,17,42) "x (VarName) :: Int (defined at A.hs@17:3-17:4)"
        assertIdInfo idInfo "A" (17,44,17,45) "y (VarName) :: Int (defined at A.hs@17:5-17:6)"
        assertIdInfo idInfo "A" (17,49,17,50) "z (VarName) :: Int (defined at A.hs@17:7-17:8)"
        assertIdInfo idInfo "A" (18,19,18,21) "xs (VarName) :: [Int] (binding occurrence)"
        assertIdInfo idInfo "A" (18,25,18,29) "Just (DataName) (defined in base-4.5.1.0:Data.Maybe at <no location info>; imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9)"
        assertIdInfo idInfo "A" (18,35,18,37) "xs (VarName) :: [Int] (defined at A.hs@18:19-18:21)"
    )
  , ( "Type information 16: FFI"
    , withConfiguredSession defOpts $ \session -> do
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
        assertIdInfo idInfo "A" (5,28,5,33) "c_sin (VarName) :: CDouble -> CDouble (binding occurrence)"
        assertIdInfo idInfo "A" (5,37,5,44) "CDouble (TcClsName) (defined in base-4.5.1.0:Foreign.C.Types at <no location info>; imported from base-4.5.1.0:Foreign.C at A.hs@4:1-4:17)"
        assertIdInfo idInfo "A" (7,21,7,26) "c_sin (VarName) :: CDouble -> CDouble (defined at A.hs@5:28-5:33)"
        assertIdInfo idInfo "A" (10,22,10,29) "andBack (VarName) :: CDouble -> CDouble (defined at A.hs@9:1-9:8)"
        assertIdInfo idInfo "A" (10,33,10,40) "CDouble (TcClsName) (defined in base-4.5.1.0:Foreign.C.Types at <no location info>; imported from base-4.5.1.0:Foreign.C at A.hs@4:1-4:17)"
    )
  , ( "Type information 17: GADTs"
    , withConfiguredSession defOpts $ \session -> do
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
        assertIdInfo idInfo "A" (4,3,4,6) "Num (DataName) :: ghc-prim:GHC.Prim.~# * ($a) Int -> Int -> Expr ($a) (binding occurrence)"
        -- Check that we get info for the result types
        assertIdInfo idInfo "A" (4,23,4,26) "Int (TcClsName) (wired in to the compiler)"
        -- But the type is ok when we don't bind the variable
        assertIdInfo idInfo "A" (7,3,7,7) "Cond (DataName) :: Expr Bool -> Expr a -> Expr a -> Expr a (binding occurrence)"
        assertIdInfo idInfo "A" (7,18,7,19) "a (TvName) (binding occurrence)"
        assertIdInfo idInfo "A" (7,54,7,58) "Expr (TcClsName) (defined at A.hs@3:6-3:10)"
        assertIdInfo idInfo "A" (7,59,7,60) "a (TvName) (defined at A.hs@7:18-7:19)"
    )
  , ( "Type information 18: Other types"
    , withConfiguredSession defOpts $ \session -> do
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
        assertIdInfo idInfo "A" (3,7,3,8) "C (TcClsName) (binding occurrence)"
        assertIdInfo idInfo "A" (3,9,3,10) "a (TvName) (binding occurrence)"
        assertIdInfo idInfo "A" (4,3,4,4) "f (VarName) (defined at A.hs@4:3-4:4)"
        assertIdInfo idInfo "A" (4,8,4,11) "Int (TcClsName) (wired in to the compiler)"
        assertIdInfo idInfo "A" (4,15,4,16) "a (TvName) (defined at A.hs@3:9-3:10)"
        assertIdInfo idInfo "A" (5,7,5,8) "D (TcClsName) (binding occurrence)"
        assertIdInfo idInfo "A" (5,9,5,10) "a (TvName) (binding occurrence)"
        assertIdInfo idInfo "A" (5,11,5,12) "b (TvName) (binding occurrence)"
        assertIdInfo idInfo "A" (6,3,6,4) "g (VarName) (defined at A.hs@6:3-6:4)"
        assertIdInfo idInfo "A" (6,8,6,9) "a (TvName) (defined at A.hs@5:9-5:10)"
        assertIdInfo idInfo "A" (6,13,6,14) "b (TvName) (defined at A.hs@5:11-5:12)"
        assertIdInfo idInfo "A" (7,6,7,9) "Foo (TcClsName) (binding occurrence)"
        assertIdInfo idInfo "A" (7,12,7,15) "Int (TcClsName) (wired in to the compiler)"
        assertIdInfo idInfo "A" (8,13,8,16) "Bar (TcClsName) (binding occurrence)"
        assertIdInfo idInfo "A" (8,17,8,18) "a (TvName) (binding occurrence)"
        assertIdInfo idInfo "A" (9,15,9,18) "Bar (TcClsName) (binding occurrence)"
        assertIdInfo idInfo "A" (9,19,9,22) "Int (TcClsName) (wired in to the compiler)"
        assertIdInfo idInfo "A" (9,25,9,29) "Bool (TcClsName) (wired in to the compiler)"
    )
  , ( "Test internal consistency of local id markers"
    , withConfiguredSession ("-package pretty" : defOpts) $ \session -> do
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
    , withConfiguredSession ("-package pretty" : defOpts) $ \session -> do
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
    , withConfiguredSession ("-XPackageImports" : "-package parallel" : defOpts) $ \session -> do
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
    , withConfiguredSession defOpts $ \session -> do
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
           assertEqual "" "[foobar (VarName) :: Bool -> Bool (defined in main:A at A.hs@3:1-3:7; imported from main:A at B.hs@2:1-2:9)]" (show completeFoob)

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
           let expected = "[foobar (VarName) :: Int -> Int (defined in main:A at A.hs@3:1-3:7; imported from main:A at B.hs@2:1-2:9),"
                       ++ "foobar' (VarName) :: () -> () (defined in main:A at A.hs@5:1-5:8; imported from main:A at B.hs@2:1-2:9)]"
           assertEqual "" expected (show completeFoob)
    )
    -- TODO: Autocomplete test that checks import errors
    -- - Explicitly importing somthing that wasn't exported
    -- - Explicitly hiding something that wasn't exported
    -- - Use of PackageImports without the flag
  , ( "GHC crash 1: No delay, no further requests"
    , withConfiguredSession defOpts $ \session -> do
        crashGhcServer session Nothing
    )
  , ( "GHC crash 2: No delay, follow up request"
    , withConfiguredSession defOpts $ \session -> do
        crashGhcServer session Nothing
        assertRaises ""
          (\(ExternalException stderr _) -> stderr == show (userError "Intentional crash"))
          (updateSession session (updateEnv "Foo" Nothing) (\_ -> return ()))
    )
  , ( "GHC crash 3: Delay, follow up request"
    , withConfiguredSession defOpts $ \session -> do
        crashGhcServer session (Just 1000000)
        updateSession session (updateEnv "Foo" Nothing) (\_ -> return ())
        threadDelay 2000000
        assertRaises ""
          (\(ExternalException stderr _) -> stderr == show (userError "Intentional crash"))
          (updateSession session (updateEnv "Foo" Nothing) (\_ -> return ()))
    )
  , ( "getLoadedModules while configGenerateModInfo off"
    , withConfiguredSessionDetailed False Nothing defOpts $ \session -> do
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
  ]

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

defOpts :: [String]
defOpts = [ "-hide-all-packages", "-package base" ]

-- Set of projects and options to use for them.
projects :: [(String, FilePath, [String])]
projects =
  [ ("A depends on B, throws exception", "test/ABnoError", defOpts)
  , ( "Cabal code"
    , "test/Cabal"
    , [ "-hide-all-packages"
      , "-package base"
      , "-package deepseq"
      , "-package filepath"
      , "-package directory"
      , "-package process"
      , "-package old-time"
      , "-package containers"
      , "-package array"
      , "-package pretty"
      , "-package bytestring"
      , "-package unix"
      ]
    )
  , ("A single file with a code to run in parallel"
    , "test/MainModule"
    , [ "-hide-all-packages"
      , "-package base"
      , "-package parallel"
      , "-package old-time"
      ])
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
          withConfiguredSessionDetailed genModInfo Nothing opts $ \session -> do
            (originalUpdate, lm) <- getModulesFrom session originalSourcesDir
            check session originalUpdate lm
  in [ testGroup "Full integration tests on multiple projects"
       $ map (groupProject False) $ zip multipleTests [1 :: Int ..]
     , testGroup "Synthetic integration tests"
       $ map (uncurry testCase) syntheticTests
     ]

main :: IO ()
main = defaultMain tests

-- Extra debug facilities. Normally turned off.

displayCounter :: Int -> Progress -> Assertion
displayCounter i p = do
  debug dVerbosity $ show p
  assertBool (show p ++ " exceeds " ++ show i) (progressStep p <= i)

updateSessionD :: IdeSession -> IdeSessionUpdate -> Int -> IO ()
updateSessionD session update i = do
  updateSession session update (displayCounter i)
  {-
  msgs <- getSourceErrors session
  debug dVerbosity $ "getSourceErrors after update: "
                     ++ List.intercalate "\n" (map show msgs)
  -}

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

assertIdInfo :: (Text -> SourceSpan -> [(SourceSpan, SpanInfo)])
             -> String
             -> (Int, Int, Int, Int)
             -> String
             -> Assertion
assertIdInfo idInfo mod (frLine, frCol, toLine, toCol) typ =
    when (typ /= (show . snd . head $ idInfo (Text.pack mod) span)) $
      putStrLn $ "Warning: Expected: " ++ typ ++ "\n"
              ++ "              got: " ++ (show . snd . head $ idInfo (Text.pack mod) span)
    -- assertEqual "" typ (show . snd . head $ idInfo (Text.pack mod) span)
  where
    span = SourceSpan { spanFilePath   = mod ++ ".hs"
                      , spanFromLine   = frLine
                      , spanFromColumn = frCol
                      , spanToLine     = toLine
                      , spanToColumn   = toCol
                      }

assertSourceErrors' :: IdeSession -> [String] -> Assertion
assertSourceErrors' session = assertSourceErrors session . map
  (\err -> (Nothing, err))

assertSourceErrors :: IdeSession -> [(Maybe FilePath, String)] -> Assertion
assertSourceErrors session expected = do
  errs <- getSourceErrors session
  if length errs /= length expected
    then assertFailure $ "Unexpected source errors: " ++ show3errors errs
    else forM_ (zip expected errs) $ \((mFilePath, expectedErr), SourceError _ loc actual) -> do
           case mFilePath of
             Nothing           -> return ()
             Just expectedPath -> case loc of
                                    ProperSpan (SourceSpan actualPath _ _ _ _) ->
                                      assertBool "Wrong file" $ expectedPath `isSuffixOf` actualPath
                                    _ ->
                                      assertFailure $ "Expected location"
           assertBool ("Unexpected error: " ++ Text.unpack actual) $
             expectedErr `isPrefixOf` Text.unpack actual

assertNoErrors :: IdeSession -> Assertion
assertNoErrors session = do
  errs <- getSourceErrors session
  assertBool ("Unexpected errors: " ++ show3errors errs) $ null errs

assertSomeErrors :: [SourceError] -> Assertion
assertSomeErrors msgs = do
  assertBool "Type error lost" $ length msgs >= 1

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
      withConfiguredSession defOpts $ \session -> do
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
        restartSession session

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
  withConfiguredSession defOpts $ \session -> do
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
