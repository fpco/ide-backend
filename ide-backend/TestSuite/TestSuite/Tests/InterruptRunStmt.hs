module TestSuite.Tests.InterruptRunStmt (testGroupInterruptRunStmt) where

import Control.Concurrent
import Control.Monad
import Data.Monoid
import Test.Tasty
import Test.HUnit
import qualified Data.ByteString.Lazy.Char8 as L (unlines)

import IdeSession
import TestSuite.Assertions
import TestSuite.Session
import TestSuite.State

testGroupInterruptRunStmt :: TestSuiteEnv -> TestTree
testGroupInterruptRunStmt env = testGroup "Interrupt runStmt" [
    stdTest env "After 1 sec"                                   testAfter1sec
  , stdTest env "Immediately"                                   testImmediately
  , stdTest env "Black hole, after 1 sec)"                      testBlackHole
  , stdTest env "Many times, preferably without deadlock (#58)" testManyTimes
  ]

testAfter1sec :: TestSuiteEnv -> Assertion
testAfter1sec env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    runActions <- runStmt session "M" "loop"
    threadDelay 1000000
    interrupt runActions
    resOrEx <- runWait runActions
    case resOrEx of
      Right result -> assertBool "" (isAsyncException result)
      _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "import Control.Concurrent (threadDelay)"
            , "loop :: IO ()"
            , "loop = threadDelay 100000 >> loop"
            ])

testImmediately :: TestSuiteEnv -> Assertion
testImmediately env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    runActions <- runStmt session "M" "loop"
    interrupt runActions
    resOrEx <- runWait runActions
    case resOrEx of
      Right result -> assertBool "" (isAsyncException result)
      _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "import Control.Concurrent (threadDelay)"
            , "loop :: IO ()"
            , "loop = threadDelay 100000 >> loop"
            ])

testBlackHole :: TestSuiteEnv -> Assertion
testBlackHole env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    runActions <- runStmt session "M" "loop"
    threadDelay 1000000
    forceCancel runActions -- Black hole cannot (always) be interrupted using an exception
    resOrEx <- runWait runActions
    case resOrEx of
      Right RunForceCancelled -> return ()
      _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "loop :: IO ()"
            , "loop = loop"
            ])

testManyTimes :: TestSuiteEnv -> Assertion
testManyTimes env = withAvailableSession env $ \session -> do
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
    assertEqual "" "Hi!\n" output
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "Main.hs" "main = putStrLn \"Hi!\" >> getLine >> return ()")
       <> (updateStdoutBufferMode (RunLineBuffering Nothing))
