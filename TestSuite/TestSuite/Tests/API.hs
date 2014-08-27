-- Test use and abuse of the ide-backend API (such as two calls to shutdown etc)
--
-- Since many of these tests have a non-standard use of the API they often do
-- not use the withAvailableSession infrastructure.
module TestSuite.Tests.API (testGroupAPI) where

import Control.Concurrent
import Control.Exception
import Data.Monoid
import System.Exit
import Test.Tasty
import Test.HUnit
import qualified Data.ByteString.Lazy.Char8 as L (unlines)
import qualified Data.Text                  as T

import IdeSession
import TestSuite.State
import TestSuite.Session
import TestSuite.Assertions

testGroupAPI :: TestSuiteEnv -> TestTree
testGroupAPI env = testGroup "API use and abuse" [
    stdTest env "Duplicate shutdown"                                               testDuplicateShutdown
  , stdTest env "Permit a session within a session and duplicated shutdownSession" testNestedSessions
  , stdTest env "Reject updateSession after shutdownSession"                       testRejectUpdateSession
  , stdTest env "Reject getSourceErrors after shutdownSession"                     testRejectGetSourceErrors
  , stdTest env "Reject runStmt after shutdownSession"                             testRejectRunStmt
  , stdTest env "Fail on empty package DB"                                         testEmptyPackageDB
  , stdTest env "Two calls to runStmt"                                             testTwiceRunStmt
  , stdTest env "Two calls to runExe"                                              testTwiceRunExe
  , stdTest env "Make sure we can terminate the IDE session when code is running"  test_Terminate_CodeRunning
  , stdTest env "Make sure we can terminate the IDE session when exe is running"   test_Terminate_ExeRunning
  , stdTest env "getSourceErrors during run"                                       test_getSourceErrors_CodeRunning
  , stdTest env "getLoadedModules during run"                                      test_getLoadedModules_CodeRunning
  , stdTest env "getLoadedModules while configGenerateModInfo off"                 test_getLoadedModules_ConfigGenerateModInfoOff
  , stdTest env "Call runWait after termination (normal termination)"              test_runWait_AfterTermination
  , stdTest env "Call runWait after termination (interrupted)"                     test_runWait_AfterTermination_Int
  , stdTest env "Call runWait after termination (restarted session)"               test_runWait_AfterTermination_Restarted
  ]

testDuplicateShutdown :: TestSuiteEnv -> Assertion
testDuplicateShutdown env =
    withSession (defaultServerConfig env) $
      -- withSession itself also does a shutdownSession
      shutdownSession

testNestedSessions :: TestSuiteEnv -> Assertion
testNestedSessions env =
    withSession (defaultServerConfig env) $ \session -> do
      loadModulesFrom session "test/ABnoError"

      withSession (defaultServerConfig env) $ \s2 -> do
       withSession (defaultServerConfig env) $ \s3 -> do
        withSession (defaultServerConfig env) $ \_s4 -> do
         let update2 = loadModule "M.hs" "a = unknownX"
         updateSessionD s2 update2 1
         assertOneError s2
         withSession (defaultServerConfig env) $ \s5 -> do
          let update3 = loadModule "M.hs" "a = 3"
          updateSessionD s3 update3 1
          assertNoErrors session
          shutdownSession s5 -- <-- duplicate "nested" shutdown

testRejectUpdateSession :: TestSuiteEnv -> Assertion
testRejectUpdateSession env =
    withSession (defaultServerConfig env) $ \session -> do
      shutdownSession session
      assertRaises "updateSessionD session mempty"
        (== userError "Session already shut down.")
        (updateSessionD session mempty 0)

testRejectGetSourceErrors :: TestSuiteEnv -> Assertion
testRejectGetSourceErrors env =
    withSession (defaultServerConfig env) $ \session -> do
      shutdownSession session
      assertRaises "getSourceErrors session"
        (== userError "Session already shut down.")
        (getSourceErrors session)

testRejectRunStmt :: TestSuiteEnv -> Assertion
testRejectRunStmt env =
    withSession (defaultServerConfig env) $ \session -> do
      shutdownSession session
      assertRaises "runStmt session Main main"
        (== userError "State not idle")
        (runStmt session "Main" "main")

testEmptyPackageDB :: TestSuiteEnv -> Assertion
testEmptyPackageDB env =
    assertRaises ""
      (\e -> e == userError "Invalid package DB stack: []")
      (withSession cfg $ \_ -> return ())
  where
    cfg = (defaultServerConfig env) {
        testSuiteServerPackageDBStack = Just []
      }

testTwiceRunStmt :: TestSuiteEnv -> Assertion
testTwiceRunStmt env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session

    do runActions <- runStmt session "M" "echo"
       supplyStdin runActions "ECHO!\n"
       (output, result) <- runWaitAll runActions
       assertEqual "" RunOk result
       assertEqual "" "ECHO!\n" output

    do runActions <- runStmt session "M" "echoReverse"
       supplyStdin runActions "!OHCE\n"
       (output, result) <- runWaitAll runActions
       assertEqual "" RunOk result
       assertEqual "" "ECHO!\n" output
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "echo :: IO ()"
            , "echo = getLine >>= putStrLn"
            , "echoReverse :: IO ()"
            , "echoReverse = getLine >>= putStrLn . reverse"
            ])

testTwiceRunExe :: TestSuiteEnv -> Assertion
testTwiceRunExe env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session

    let m      = "M"
        updExe = buildExe [] [(T.pack m, "M.hs")]
    updateSessionD session updExe 2

    do runActions <- runExe session "M"
       supplyStdin runActions "!OHCE\n"
       (output, result) <- runWaitAll runActions
       assertEqual "" result ExitSuccess
       assertEqual "" "ECHO!\n" output

    do runActions <- runExe session "M"
       supplyStdin runActions "!OHCE\n"
       (output, result) <- runWaitAll runActions
       assertEqual "" result ExitSuccess
       assertEqual "" "ECHO!\n" output
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "main :: IO ()"
            , "main = getLine >>= putStrLn . reverse"
            ])

test_Terminate_CodeRunning :: TestSuiteEnv -> Assertion
test_Terminate_CodeRunning env = do
    runActions <- withAvailableSession' env dontReuse $ \session -> do
      updateSessionD session upd 1
      assertNoErrors session
      runStmt session "M" "echo"
    forceCancel runActions
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "echo :: IO ()"
            , "echo = (getLine >>= putStrLn) >> echo"
            ])

test_Terminate_ExeRunning :: TestSuiteEnv -> Assertion
test_Terminate_ExeRunning env = do
    runActions <- withAvailableSession' env dontReuse $ \session -> do
      updateSessionD session upd 1

      let m      = "M"
          updExe = buildExe [] [(T.pack m, "M.hs")]
      updateSessionD session updExe 2

      assertNoErrors session
      runExe session "M"
    forceCancel runActions
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "main :: IO ()"
            , "main = (getLine >>= putStrLn) >> main"
            ])

test_getSourceErrors_CodeRunning :: TestSuiteEnv -> Assertion
test_getSourceErrors_CodeRunning env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertSourceErrors' session ["Top-level binding with no type signature"]
    errs <- getSourceErrors session

    do runActions <- runStmt session "M" "loop"
       errs'      <- getSourceErrors session
       assertEqual "Running code does not affect getSourceErrors" errs errs'
       forceCancel runActions

    do let m      = "M"
           updExe = buildExe [] [(T.pack m, "M.hs")]

       updateSessionD session updExe 2
       errs' <- getSourceErrors session
       assertEqual "" errs errs'

       runActions <- runExe session m
       errs''    <- getSourceErrors session
       assertEqual "Running exes does not affect getSourceErrors" errs errs''

       forceCancel runActions
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "{-# OPTIONS_GHC -Wall #-}"
            , "module M where"
            , "loop = loop"
            , "main :: IO ()"
            , "main = loop"
           ])

test_getLoadedModules_CodeRunning :: TestSuiteEnv -> Assertion
test_getLoadedModules_CodeRunning env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    mods <- getLoadedModules session
    assertEqual "" [T.pack "M"] mods

    runActions <- runStmt session "M" "loop"
    mods'      <- getLoadedModules session
    assertEqual "Running code does not affect getLoadedModules" mods mods'
    forceCancel runActions
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "{-# OPTIONS_GHC -Wall #-}"
            , "module M where"
            , "loop = loop"
            ])

test_runWait_AfterTermination :: TestSuiteEnv -> Assertion
test_runWait_AfterTermination env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    runActions <- runStmt session "M" "hello"
    (output, result) <- runWaitAll runActions
    assertEqual "" RunOk result
    assertEqual "" "Hello World\n" output
    result' <- runWait runActions
    assertEqual "" result' (Right RunOk)

    let m      = "M"
        updExe = buildExe [] [(T.pack m, "M.hs")]
    updateSessionD session updExe 2
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "Hello World\n"
                outExe
    assertEqual "after runExe" ExitSuccess statusExe

    result2 <- runWait runActionsExe
    assertEqual "" result2 (Right ExitSuccess)
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "hello :: IO ()"
            , "hello = putStrLn \"Hello World\""
            , "main :: IO ()"
            , "main = hello"
            ])

test_runWait_AfterTermination_Int :: TestSuiteEnv -> Assertion
test_runWait_AfterTermination_Int env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session

    let m      = "M"
        updExe = buildExe [] [(T.pack m, "M.hs")]
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
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "import Control.Concurrent (threadDelay)"
            , "loop :: IO ()"
            , "loop = threadDelay 100000 >> loop"
            , "main :: IO ()"
            , "main = loop"
            ])

test_runWait_AfterTermination_Restarted :: TestSuiteEnv -> Assertion
test_runWait_AfterTermination_Restarted env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    runActions <- runStmt session "M" "loop"
    threadDelay 1000000
    restartSession session
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

    let m      = "M"
        updExe = buildExe [] [(T.pack m, "M.hs")]
    updateSessionD session updExe 2
    runActionsExe <- runExe session m
    threadDelay 1000000
    restartSession session
    -- restartSession would not suffice, since session restart
    -- doesn't stop the exe, so we need to interrupt manually.
    interrupt runActionsExe
    resOrEx <- runWait runActionsExe
    case resOrEx of
      Right result -> assertEqual "after runExe" (ExitFailure 2) result
      _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
    result' <- runWait runActionsExe
    assertEqual "" result' (Right $ ExitFailure 2)
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "import Control.Concurrent (threadDelay)"
            , "loop :: IO ()"
            , "loop = threadDelay 100000 >> loop"
            , "main :: IO ()"
            , "main = loop"
            ])

test_getLoadedModules_ConfigGenerateModInfoOff :: TestSuiteEnv -> Assertion
test_getLoadedModules_ConfigGenerateModInfoOff env = withAvailableSession' env (withModInfo False) $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    assertLoadedModules session "" ["M"]
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "hello :: IO ()"
            , "hello = putStrLn \"Hello World\""
            ])

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

withSession :: TestSuiteServerConfig -> (IdeSession -> IO a) -> IO a
withSession cfg = bracket (startNewSession cfg) shutdownSession
