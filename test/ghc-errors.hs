module Main (main) where

import Control.Concurrent (threadDelay)
import qualified Control.Exception as Ex
import Control.Monad (liftM, void)
import qualified Data.ByteString.Char8 as BSSC (pack)
import qualified Data.ByteString.Lazy.Char8 as BSLC (pack)
import Data.List (sort)
import qualified Data.List as List
import Data.Maybe (catMaybes)
import Data.Monoid (mconcat, mempty, (<>))
import System.Directory
import System.Environment (getArgs)
import System.Exit (ExitCode (..))
import System.FilePath (dropExtension, makeRelative, (</>))
import System.FilePath.Find (always, extension, find)
import System.IO.Temp (withTempDirectory)
import System.Random (randomIO)

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertBool, assertEqual, assertFailure)

import Common
import GhcServer
import IdeSession
import ModuleName (ModuleName)
import qualified ModuleName as MN
import RunAPI
import TestTools

-- Tests using various functions of the IdeSession API
-- and a variety of small test Haskell projects.

-- | Update the session with all modules of the given directory.
loadModulesFrom :: IdeSession -> FilePath -> IO ()
loadModulesFrom session originalSourcesDir = do
  debug dVerbosity $ "\nCopying files from: " ++ originalSourcesDir
                     ++ " to: " ++ getSourcesDir session
  -- Send the source files from 'originalSourcesDir' to 'configSourcesDir'
  -- using the IdeSession's update mechanism.
  originalFiles <- find always
                        ((`elem` hsExtentions) `liftM` extension)
                        originalSourcesDir
  -- HACK: here we fake module names, guessing them from file names.
  let tryFromPath f p = do
        mex <- Ex.try $ Ex.evaluate $ MN.fromFilePath p
        return $ case mex of
          Right n  -> Just (n, f)
          Left _ex -> let _ = _ex :: Ex.ErrorCall in Nothing
  triedModules <-
     mapM (\ f -> tryFromPath f
                  $ dropExtension $ makeRelative originalSourcesDir f)
          originalFiles
  let originalModules = catMaybes triedModules
      upd (m, f) = updateModuleFromFile m f
      -- Let's also disable ChangeCodeGeneration, to keep the test stable
      -- in case the default value of CodeGeneration changes.
      originalUpdate = updateCodeGeneration False
                       <> (mconcat $ map upd originalModules)
  updateSessionD session originalUpdate (length originalFiles)

-- | Run the specified action with a new IDE session, configured to use a
-- temporary directory
withConfiguredSession :: [String] -> (IdeSession -> IO a) -> IO a
withConfiguredSession opts io = do
  slashTmp <- getTemporaryDirectory
  withTempDirectory slashTmp "ide-backend-test." $ \configDir -> do
    let sessionConfig = SessionConfig{ configDir
                                     , configStaticOpts = opts
                                     }
    withSession sessionConfig io

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
multipleTests :: [(String, IdeSession -> Assertion)]
multipleTests =
  [ ("Just typecheck", \_ -> return ())
  , ("Overwrite with error"
    , \session -> do
        -- Overwrite one of the copied files.
        (m, _) <- getModules session
        let update = loadModule m "a = unknownX"
        updateSessionD session update 1  -- at most 1 recompiled
        msgs <- getSourceErrors session
        assertSomeErrors msgs
    )
  , ("Overwrite with the same module name in all files"
    , \session -> do
        (_, lm) <- getModules session
        let upd m =
              updateModule m (BSLC.pack "module Wrong where\na = 1")
            update = mconcat $ map upd lm
        updateSessionD session update 2
        msgs <- getSourceErrors session
        if length lm >= 2
          then assertSomeErrors msgs
          else assertNoErrors msgs
    )
  , ("Overwrite modules many times"
    , \session -> do
        -- Overwrite one of the copied files with an error.
        (m1, lm) <- getModules session
        let update1 =
              updateCodeGeneration False
              <> loadModule m1 "a = unknownX"
        updateSessionD session update1 1
        updateSessionD session mempty 1  -- was an error, so trying again
        msgs2 <- getSourceErrors session
        assertSomeErrors msgs2
        -- Overwrite all files, many times, with correct modules.
        let upd m = loadModule m "x = unknownX"
                    <> loadModule m "y = 2"
                    <> updateCodeGeneration True
            update2 = mconcat $ map upd lm
        updateSessionD session update2 (length lm)
        msgs4 <- getSourceErrors session
        assertNoErrors msgs4
        -- Overwrite again with the error.
        updateSessionD session update1 1  -- drop bytecode, don't recompile
        msgs5 <- getSourceErrors session
        assertOneError msgs5
        assertRaises "runStmt session Main main"
          (== userError "Cannot run before the code is generated.")
          (runStmt session (MN.fromString "Main") "main")
      )
    , ("Run the sample code; don't fail without an explanation"
      , \session -> do
        (_, lm) <- getModules session
        let update = updateCodeGeneration True
        updateSessionD session update (length lm)  -- all recompiled
        mex <- Ex.try $ runStmt session (MN.fromString "Main") "main"
        case mex of
          Right runActions -> void $ runWaitAll runActions
          Left ex -> assertEqual "runStmt" (userError "Module \"Main\" not successfully loaded, when trying to run code.") ex
      )
    , ("Run automatically corrected code; don't fail at all"
      , \session -> do
        (_, lm) <- getModules session
        let upd m = loadModule m "x = 1"
            update =
              updateModule (MN.fromString "TotallyMain")
                           (BSLC.pack "module TotallyMain where\nmain = print \"test run\"")
              <> mconcat (map upd lm)
        updateSessionD session update (length lm + 1)
        updateSessionD session mempty 0
        let update2 = updateCodeGeneration True
        updateSessionD session update2 (length lm + 1)
        runActions <- runStmt session (MN.fromString "TotallyMain") "main"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" output (BSLC.pack "\"test run\"\n")
          RunProgException _ex ->
            assertFailure "Unexpected exception raised by the running code."
          RunGhcException _ex  ->
            assertFailure "Manually corrected code not run successfully"
      )
    , ("Make sure deleting modules removes them from the directory"
      , \session -> do
        (_, lm) <- getModules session
        let update = mconcat $ map updateModuleDelete lm
        updateSessionD session update 0
        msgs <- getSourceErrors session
        assertNoErrors msgs
        let update2 = updateCodeGeneration True
        updateSessionD session update2 0  -- 0: nothing to generate code from
      )
    , ("Make sure restartSession does not lose source files"
      , \session -> do
        (_, lm) <- getModules session
        serverBefore <- getGhcServer session
        let update = updateCodeGeneration True
        updateSessionD session update (length lm)  -- all recompiled
        mex <- Ex.try $ runStmt session (MN.fromString "Main") "main"
        case mex of
          Right _runActions -> return ()  -- don't runWaitAll
          Left ex -> assertEqual "runStmt" (userError "Module \"Main\" not successfully loaded, when trying to run code.") ex
        restartSession session
        updateSessionD session mempty (length lm)  -- all compiled anew
        mex2 <- Ex.try $ runStmt session (MN.fromString "Main") "main"
        case mex2 of
          Right runActions -> void $ runWaitAll runActions  -- now runWaitAll
          Left ex -> assertEqual "runStmt" (userError "Module \"Main\" not successfully loaded, when trying to run code.") ex
        restartSession session
        let update2 = mconcat $ map updateModuleDelete lm
        updateSessionD session update2 0  -- if any file missing, would yell
        msgs <- getSourceErrors session
        assertNoErrors msgs
        let update3 = updateCodeGeneration True
        updateSessionD session update3 0  -- 0: nothing to generate code from
        exitCodeBefore <- getRpcExitCode serverBefore
        assertEqual "exitCodeBefore" (Just ExitSuccess) exitCodeBefore  -- TODO: should probably be ExitSuccess
      )
  ]

syntheticTests :: [(String, Assertion)]
syntheticTests =
  [ ( "Maintain list of compiled modules"
    , withConfiguredSession defOpts $ \session -> do
        let m = MN.fromString "A"
        updateSessionD session (loadModule m "a = 5") 1
        assertEqual "[m]" [m] =<< getLoadedModules session
        let m2 = MN.fromString "A2"
        updateSessionD session (loadModule m2 "import A\na2 = A.a") 1
        assertEqual "[m, m2]" (sort [m, m2])
          =<< (liftM sort $ getLoadedModules session)
        let m3 = MN.fromString "A3"
        updateSessionD session (loadModule m3 "") 1
        assertEqual "[m, m2, m3]" (sort [m, m2, m3])
          =<< (liftM sort $ getLoadedModules session)
        let m4 = MN.fromString "Wrong"
        updateSessionD session (loadModule m4 "import A\na2 = A.a + c") 1
        assertEqual "Wrong" (sort [m, m2, m3])
          =<< (liftM sort $ getLoadedModules session)
        updateSessionD session (loadModule m "a = c") 1
        assertEqual "[m3]" [m3] =<< getLoadedModules session
    )
  , ( "Duplicate shutdown"
    , withConfiguredSession defOpts $ \session ->
        -- withConfiguredSession will shutdown the session as well
        shutdownSession session
    )
  , ( "Permit a session within a session and duplicated shutdownSession"
    , withConfiguredSession defOpts $ \session -> do
        loadModulesFrom session "test/ABnoError"
        let config = getSessionConfig session
            tweakConfig :: Int -> SessionConfig -> IO SessionConfig
            tweakConfig n cfg@SessionConfig{configDir} = do
              let newDir = configDir </> "new" ++ show n
              createDirectory newDir
              return cfg { configDir = newDir }
        withSession' (tweakConfig 2 config) $ \s2 -> do
         withSession' (tweakConfig 3 config) $ \s3 -> do
          withSession' (tweakConfig 4 config) $ \_s4 -> do
           let update2 = loadModule (MN.fromString "M") "a = unknownX"
           updateSessionD s2 update2 1
           msgs2 <- getSourceErrors s2
           assertOneError msgs2
           withSession' (tweakConfig 5 config) $ \s5 -> do
            let update3 = loadModule (MN.fromString "M") "a = 3"
            updateSessionD s3 update3 1
            msgs3 <- getSourceErrors s3
            assertNoErrors msgs3
            shutdownSession s5 -- <-- duplicate "nested" shutdown
    )
  , ( "Compile a project: A depends on B, error in A"
    , withConfiguredSession defOpts $ \session -> do
        loadModulesFrom session "test/AerrorB"
        msgs <- getSourceErrors session
        assertOneError msgs
    )
  , ( "Compile a project: A depends on B, error in B"
    , withConfiguredSession defOpts $ \session -> do
        loadModulesFrom session "test/ABerror"
        msgs <- getSourceErrors session
        assertOneError msgs
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
        let update2 = loadModule (MN.fromString "Main")
              "main = readFile \"datafile.dat\" >>= putStrLn"
        updateSessionD session update2 1
        msgs <- getSourceErrors session
        assertNoErrors msgs
        let update3 = updateCodeGeneration True
        updateSessionD session update3 1
        runActions <- runStmt session (MN.fromString "Main") "main"
        (output, _) <- runWaitAll runActions
        assertEqual "compare test data content"
          (BSLC.pack "test data content\n") output
        let update4 = updateDataFile "datafile.dat"
                                     (BSLC.pack "new content")
                      <> update2
        updateSessionD session update4 1
        runActions2 <- runStmt session (MN.fromString "Main") "main"
        (output2, _) <- runWaitAll runActions2
        assertEqual "compare new content"
          (BSLC.pack "new content\n") output2
    )
  , ("Reject getSourceErrors without updateSession"
    , withConfiguredSession defOpts $ \session ->
        assertRaises "getSourceErrors session"
          (== userError "This session state does not admit queries.")
          (getSourceErrors session)
    )
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
          (runStmt session (MN.fromString "Main") "main")
    )
  , ( "Reject a wrong CPP directive"
    , let packageOpts = [ "-hide-all-packages"
                        , "-XCPP"
                        ]
      in withConfiguredSession packageOpts $ \session -> do
        let update = loadModule (MN.fromString "M") "#ifdef"
                     <> updateCodeGeneration True
        updateSessionD session update 1
        msgs <- getSourceErrors session
        assertSomeErrors msgs
        assertRaises "runStmt session Main main"
          (== userError "Module \"Main\" not successfully loaded, when trying to run code.")
          (runStmt session (MN.fromString "Main") "main")
    )
  , ( "Reject a module with mangled header"
    , withConfiguredSession defOpts $ \session -> do
        let update = updateModule (MN.fromString "M")
                                  (BSLC.pack "module very-wrong where")
        updateSessionD session update 1
        msgs <- getSourceErrors session
        assertOneError msgs
        let update2 = updateModule (MN.fromString "M")
                                   (BSLC.pack "module M.1.2.3.8.T where")
        updateSessionD session update2 1
        msgs2 <- getSourceErrors session
        assertOneError msgs2
        let update3 = updateModuleDelete (MN.fromString "M")
                      <> updateModule (MN.ModuleName ["M.1.2.3.8.T"])
                                      (BSLC.pack "module M.1.2.3.8.T where")
        updateSessionD session update3 1
        msgs3 <- getSourceErrors session
        assertOneError msgs3
        let update4 =
              updateModule (MN.ModuleName ["M", "1", "2", "3", "8", "T"])
                           (BSLC.pack "module M.1.2.3.8.T where")
        updateSessionD session update4 1
        msgs4 <- getSourceErrors session
        assertOneError msgs4
        assertRaises "MN.fromString M.1.2.3.8.T"
          (\e -> show (e :: Ex.ErrorCall) == "ModuleName.fromString: invalid module name \"M.1.2.3.8.T\"")
          (return $! MN.toString $ MN.fromString "M.1.2.3.8.T")
    )
  , ( "Interrupt runStmt (after 1 sec)"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule (MN.fromString "M") . BSLC.pack . unlines $
                    [ "module M where"
                    , "import Control.Concurrent (threadDelay)"
                    , "loop :: IO ()"
                    , "loop = threadDelay 100000 >> loop"
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session (MN.fromString "M") "loop"
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
               <> (updateModule (MN.fromString "M") . BSLC.pack . unlines $
                    [ "module M where"
                    , "import Control.Concurrent (threadDelay)"
                    , "loop :: IO ()"
                    , "loop = threadDelay 100000 >> loop"
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session (MN.fromString "M") "loop"
        interrupt runActions
        resOrEx <- runWait runActions
        case resOrEx of
          Right (RunProgException "AsyncException: user interrupt") -> return ()
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
    )
  , ( "Interrupt runStmt (black hole; after 1 sec)"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule (MN.fromString "M") . BSLC.pack . unlines $
                    [ "module M where"
                    , "loop :: IO ()"
                    , "loop = loop"
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session (MN.fromString "M") "loop"
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
               <> (updateModule (MN.fromString "M") . BSLC.pack . unlines $
                    [ "module M where"
                    , "hello :: IO ()"
                    , "hello = putStrLn \"Hello World\""
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session (MN.fromString "M") "hello"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" (BSLC.pack "Hello World\n") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Capture stdout (single putStr)"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule (MN.fromString "M") . BSLC.pack . unlines $
                    [ "module M where"
                    , "hello :: IO ()"
                    , "hello = putStr \"Hello World\""
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session (MN.fromString "M") "hello"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" (BSLC.pack "Hello World") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Capture stdout (single putStr with delay)"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule (MN.fromString "M") . BSLC.pack . unlines $
                    [ "module M where"
                    , "import Control.Concurrent (threadDelay)"
                    , "import System.IO"
                    , "hello :: IO ()"
                    , "hello = hSetBuffering stdout LineBuffering >> putStr \"hello\" >> threadDelay 1000000 >> putStr \"hi\""
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session (MN.fromString "M") "hello"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" (BSLC.pack "hellohi") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Capture stdout (multiple putStrLn)"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule (MN.fromString "M") . BSLC.pack . unlines $
                    [ "module M where"
                    , "hello :: IO ()"
                    , "hello = do putStrLn \"Hello World 1\""
                    , "           putStrLn \"Hello World 2\""
                    , "           putStrLn \"Hello World 3\""
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session (MN.fromString "M") "hello"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" (BSLC.pack "Hello World 1\nHello World 2\nHello World 3\n") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Capture stdout (mixed putStr and putStrLn)"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule (MN.fromString "M") . BSLC.pack . unlines $
                    [ "module M where"
                    , "hello :: IO ()"
                    , "hello = do putStrLn \"Hello World 1\""
                    , "           putStr   \"Hello World 2\""
                    , "           putStrLn \"Hello World 3\""
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session (MN.fromString "M") "hello"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" (BSLC.pack "Hello World 1\nHello World 2Hello World 3\n") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Capture stdin (simple echo process)"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule (MN.fromString "M") . BSLC.pack . unlines $
                    [ "module M where"
                    , "echo :: IO ()"
                    , "echo = getLine >>= putStrLn"
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session (MN.fromString "M") "echo"
        supplyStdin runActions (BSSC.pack "ECHO!\n")
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" (BSLC.pack "ECHO!\n") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Capture stdin (infinite echo process)"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule (MN.fromString "M") . BSLC.pack . unlines $
                    [ "module M where"
                    , "import System.IO"
                    , "import Control.Monad"
                    , "echo :: IO ()"
                    , "echo = do hSetBuffering stdout LineBuffering"
                    , "          forever $ getLine >>= putStrLn"
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session (MN.fromString "M") "echo"

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
               <> (updateModule (MN.fromString "M") . BSLC.pack . unlines $
                    [ "module M where"
                    , "echo :: IO ()"
                    , "echo = getLine >>= putStrLn"
                    , "echoReverse :: IO ()"
                    , "echoReverse = getLine >>= putStrLn . reverse"
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs

        do runActions <- runStmt session (MN.fromString "M") "echo"
           supplyStdin runActions (BSSC.pack "ECHO!\n")
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "ECHO!\n") output
             _       -> assertFailure $ "Unexpected run result: " ++ show result

        do runActions <- runStmt session (MN.fromString "M") "echoReverse"
           supplyStdin runActions (BSSC.pack "!OHCE\n")
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "ECHO!\n") output
             _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Make sure we can terminate the IDE session when code is running"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule (MN.fromString "M") . BSLC.pack . unlines $
                    [ "module M where"
                    , "echo :: IO ()"
                    , "echo = (getLine >>= putStrLn) >> echo"
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        _runActions <- runStmt session (MN.fromString "M") "echo"
        return ()
     )
  , ( "Capture stderr"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule (MN.fromString "M") . BSLC.pack . unlines $
                    [ "module M where"
                    , "import System.IO"
                    , "hello :: IO ()"
                    , "hello = hPutStrLn stderr \"Hello World\""
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session (MN.fromString "M") "hello"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" (BSLC.pack "Hello World\n") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Merge stdout and stderr"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule (MN.fromString "M") . BSLC.pack . unlines $
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
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session (MN.fromString "M") "hello"
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
               <> (updateModule (MN.fromString "M") . BSLC.pack . unlines $
                    [ "module M where"
                    , "import System.Environment (getEnv)"
                    , "printFoo :: IO ()"
                    , "printFoo = getEnv \"Foo\" >>= putStr"
                    , "printBar :: IO ()"
                    , "printBar = getEnv \"Bar\" >>= putStr"
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs

        -- At the start, both Foo and Bar are undefined
        do runActions <- runStmt session (MN.fromString "M") "printFoo"
           (_, result) <- runWaitAll runActions
           case result of
             RunProgException ex -> assertEqual "" ex "IOException: Foo: getEnv: does not exist (no environment variable)"
             _ -> assertFailure $ "Unexpected result " ++ show result
        do runActions <- runStmt session (MN.fromString "M") "printBar"
           (_, result) <- runWaitAll runActions
           case result of
             RunProgException ex -> assertEqual "" ex "IOException: Bar: getEnv: does not exist (no environment variable)"
             _ -> assertFailure $ "Unexpected result " ++ show result

        -- Update Foo, leave Bar undefined
        updateSession session (updateEnv "Foo" (Just "Value1")) (\_ -> return ())
        do runActions <- runStmt session (MN.fromString "M") "printFoo"
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "Value1") output
             _       -> assertFailure $ "Unexpected result " ++ show result
        do runActions <- runStmt session (MN.fromString "M") "printBar"
           (_, result) <- runWaitAll runActions
           case result of
             RunProgException ex -> assertEqual "" ex "IOException: Bar: getEnv: does not exist (no environment variable)"
             _ -> assertFailure $ "Unexpected result " ++ show result

        -- Update Bar, leave Foo defined
        updateSession session (updateEnv "Bar" (Just "Value2")) (\_ -> return ())
        do runActions <- runStmt session (MN.fromString "M") "printFoo"
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "Value1") output
             _       -> assertFailure $ "Unexpected result " ++ show result
        do runActions <- runStmt session (MN.fromString "M") "printBar"
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "Value2") output
             _       -> assertFailure $ "Unexpected result " ++ show result

        -- Unset Foo, leave Bar defined
        updateSession session (updateEnv "Foo" Nothing) (\_ -> return ())
        do runActions <- runStmt session (MN.fromString "M") "printFoo"
           (_, result) <- runWaitAll runActions
           case result of
             RunProgException ex -> assertEqual "" ex "IOException: Foo: getEnv: does not exist (no environment variable)"
             _ -> assertFailure $ "Unexpected result " ++ show result
        do runActions <- runStmt session (MN.fromString "M") "printBar"
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "Value2") output
             _       -> assertFailure $ "Unexpected result " ++ show result
    )
  , ( "Update during run"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule (MN.fromString "M") . BSLC.pack . unlines $
                    [ "module M where"
                    , "loop :: IO ()"
                    , "loop = loop"
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        _runActions <- runStmt session (MN.fromString "M") "loop"
        assertRaises ""
          (== userError "Cannot update session in running mode")
          (updateSessionD session upd 1)
    )
  , ( "getSourceErrors during run"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule (MN.fromString "M") . BSLC.pack . unlines $
                    [ "{-# OPTIONS_GHC -Wall #-}"
                    , "module M where"
                    , "loop = loop"
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        case msgs of
          -- We expect a 'top-level identifier without type' warning
          [SrcError KindWarning _ _ _ _] -> return ()
          _ -> assertFailure "Unexpected source errors"
        _runActions <- runStmt session (MN.fromString "M") "loop"
        msgs' <- getSourceErrors session
        assertEqual "Running code does not affect getSourceErrors" msgs msgs'
    )
  , ( "getLoadedModules during run"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule (MN.fromString "M") . BSLC.pack . unlines $
                    [ "{-# OPTIONS_GHC -Wall #-}"
                    , "module M where"
                    , "loop = loop"
                    ])
        updateSessionD session upd 1
        mods <- getLoadedModules session
        assertEqual "" [MN.fromString "M"] mods
        _runActions <- runStmt session (MN.fromString "M") "loop"
        mods' <- getLoadedModules session
        assertEqual "Running code does not affect getLoadedModules" mods mods'
    )
  , ( "Interrupt, then capture stdout"
    , withConfiguredSession defOpts $ \session -> do
        updateSession session (updateCodeGeneration True) (\_ -> return ())
        let upd1 = updateModule (MN.fromString "Main") . BSLC.pack . unlines $
                     [ "import Control.Monad"
                     , "main = forever $ print 1"
                     ]
            upd2 = updateModule (MN.fromString "Main") . BSLC.pack . unlines $
                     [ "main = print 1234" ]

        do updateSessionD session upd1 1
           runActions <- runStmt session (MN.fromString "Main") "main"
           interrupt runActions
           delay <- randomIO :: IO Int
           threadDelay (delay `mod` 1000000) -- Between 0 and 1 sec
           void $ runWaitAll runActions

        do updateSessionD session upd2 1
           runActions <- runStmt session (MN.fromString "Main") "main"
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "1234\n") output
             _       -> assertFailure $ "Unexpected result: " ++ show result
    )
  , ( "Restart session (snippet doesn't swallow exceptions; after .1 sec)"
    , restartRun    [ "module M where"
                    , "loop :: IO ()"
                    , "loop = loop"
                    ] ExitSuccess
    )
  -- , ( "Restart session (snippet swallow some exceptions; after .1 sec)"
  --   , restartRun    [ "module M where"
  --                   , "import qualified Control.Exception as Ex"
  --                   , "swallow a ex = let _ = (ex :: Ex.SomeException) in a"
  --                   , "loop :: IO ()"
  --                   , "loop = let l = Ex.catch l (swallow l) in l"
  --                   ] (ExitFailure 1)  -- TODO: should probably be ExitSuccess
  --   )
  -- , ( "Restart session (snippet swallows all exceptions; after .1 sec)"
  --   , restartRun    [ "module M where"
  --                   , "import qualified Control.Exception as Ex"
  --                   , "swallow a ex = let _ = (ex :: Ex.SomeException) in a"
  --                   , "l x = if length x > 999 then l [] else l (1 : x)"
  --                   , "loop :: IO ()"
  --                   , "loop = Ex.catch (l []) (swallow loop)"
  --                   ] (ExitFailure 1)  -- TODO: should probably be ExitSuccess
  --   )
  -- , ( "Restart session (snippet swallows all and no allocation; after .1 sec)"
  --   , restartRun    [ "module M where"
  --                   , "import qualified Control.Exception as Ex"
  --                   , "swallow a ex = let _ = (ex :: Ex.SomeException) in a"
  --                   , "l = l"
  --                   , "loop :: IO ()"
  --                   , "loop = Ex.catch l (swallow loop)"
  --                   ] (ExitFailure 1)  -- TODO: should probably be ExitSuccess
  --   )
  ]

defOpts :: [String]
defOpts = [ "-no-user-package-conf" ]

-- Set of projects and options to use for them.
projects :: [(String, FilePath, [String])]
projects =
  [ ("A depends on B, throws exception", "test/ABnoError", defOpts)
  , ("Our own code, package 'ghc' missing", ".", [])
  , ( "A subdirectory of Cabal code"
    , "test/Cabal"
    , defOpts
    )
  , ("A single file with a code to run in parallel"
    , "test/MainModule"
    , [ "-hide-all-packages"
      , "-package parallel"
      , "-package base"
      , "-package old-time"
      ])
  ]

-- Driver
tests :: [Test]
tests =
  let groupProject ((featureName, check), k) =
        testGroup featureName $ map (caseFeature featureName check k) projects
      caseFeature featureName check k
                  (projectName, originalSourcesDir, opts) = do
        let caseName = projectName ++ " (" ++ show k ++ ")"
        testCase caseName $ do
          debug dVerbosity $ featureName ++ " / " ++ caseName ++ ":"
          withConfiguredSession opts $ \session -> do
            loadModulesFrom session originalSourcesDir
            check session
  in [ testGroup "Full integration tests on multiple projects"
       $ map groupProject $ zip multipleTests [1 :: Int ..]
     , testGroup "Synthetic integration tests"
       $ map (uncurry testCase) syntheticTests
     ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    "--server" : opts -> ghcServer opts  -- @opts@ are GHC static flags
    _ -> defaultMain tests


-- Extra debug facilities. Normally turned off.

displayCounter :: Int -> Progress -> Assertion
displayCounter i p = do
  debug dVerbosity $ show p
  assertBool (show p ++ " exceeds " ++ show i) (progressStep p <= i)

updateSessionD :: IdeSession -> IdeSessionUpdate -> Int -> IO ()
updateSessionD session update i = do
  updateSession session update (displayCounter i)
  msgs <- getSourceErrors session
  debug dVerbosity $ "getSourceErrors after update: "
                     ++ List.intercalate "\n" (map formatSourceError msgs)

-- Extra test tools.

getModules :: IdeSession -> IO (ModuleName, [ModuleName])
getModules session = do
  let configSourcesDir = getSourcesDir session
  originalFiles <- find always
                        ((`elem` hsExtentions) `liftM` extension)
                        configSourcesDir
  let tryFromPath f p = do
        mex <- Ex.try $ Ex.evaluate $ MN.fromFilePath p
        return $ case mex of
          Right n  -> Just (n, f)
          Left _ex -> let _ = _ex :: Ex.ErrorCall in Nothing
  triedModules <-
     mapM (\ f -> tryFromPath f
                  $ dropExtension $ makeRelative configSourcesDir f)
          originalFiles
  let originalModules = catMaybes triedModules
      m = case originalModules of
        [] -> error "The test directory is empty."
        (x, _) : _ -> x
      -- We need to evaluate the list of modules or they get updated
      -- partway into the updateModule calls they are used in
      -- and troubles ensue. This is the case, e.g.,
      -- in "Run automatically corrected code; don't fail at all".
      !_ = length originalModules
  return (m, map fst originalModules)

loadModule :: ModuleName -> String -> IdeSessionUpdate
loadModule m contents =
  let name = MN.toString m
  in updateModule m . BSLC.pack
     $ "module " ++ name ++ " where\n" ++ contents

assertNoErrors :: [SourceError] -> Assertion
assertNoErrors msgs =
  assertBool ("Unexpected errors: " ++ show3errors msgs)
    $ null msgs

assertSomeErrors :: [SourceError] -> Assertion
assertSomeErrors msgs = do
  assertBool "Type error lost" $ length msgs >= 1

assertOneError :: [SourceError] -> Assertion
assertOneError msgs = do
  assertSomeErrors msgs
  assertBool ("Too many type errors: " ++ show3errors msgs)
    $ length msgs <= 1

show3errors :: [SourceError] -> String
show3errors msgs =
  let shown = List.intercalate "\n" (map formatSourceError $ take 3 $ msgs)
      more | length msgs > 3 = "\n... and more ..."
           | otherwise       = ""
  in shown ++ more

restartRun :: [String] -> ExitCode -> Assertion
restartRun code exitCode =
      withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule (MN.fromString "M") . BSLC.pack . unlines $
                     code)
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertNoErrors msgs
        runActionsBefore <- runStmt session (MN.fromString "M") "loop"
        threadDelay 100000
        serverBefore <- getGhcServer session
        restartSession session
        updateSessionD session upd 1
        msgs2 <- getSourceErrors session
        assertNoErrors msgs2
        exitCodeBefore <- getRpcExitCode serverBefore
        assertEqual "exitCodeBefore" (Just exitCode) exitCodeBefore
        serverAfter <- getGhcServer session
        exitCodeAfter <- getRpcExitCode serverAfter
        assertEqual "exitCodeAfter" Nothing exitCodeAfter
        -- Just one more extra perverse test, since we have the setup ready.
        assertRaises "runWait runActionsBefore after restartSession"
          (\e -> let _ = e :: Ex.BlockedIndefinitelyOnMVar in True)
          (runWait runActionsBefore)
