module TestSuite.Tests.InterruptRunExe (testGroupInterruptRunExe) where

import Control.Concurrent
import Control.Monad
import Data.Monoid
import System.Exit
import Test.Tasty
import Test.HUnit
import qualified Data.ByteString.Lazy.Char8 as L (unlines)
import qualified Data.Text                  as T

import IdeSession
import TestSuite.Assertions
import TestSuite.Session
import TestSuite.State

testGroupInterruptRunExe :: TestSuiteEnv -> TestTree
testGroupInterruptRunExe env = testGroup "Interrupt runExe" [
    stdTest env "After 1 sec"                                   testAfter1sec
  , stdTest env "Immediately"                                   testImmediately
  , stdTest env "Black hole, after 1 sec)"                      testBlackHole
  , stdTest env "Many times, preferably without deadlock (#58)" testManyTimes
  ]

testAfter1sec :: TestSuiteEnv -> Assertion
testAfter1sec env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    let m = "M"
        updExe = buildExe [] [(T.pack m, "M.hs")]
    updateSessionD session updExe 2
    runActionsExe <- runExe session m
    threadDelay 1000000
    interrupt runActionsExe
    resOrEx <- runWait runActionsExe
    case resOrEx of
      Right result -> assertEqual "after runExe" (ExitFailure 2) result
      _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "import Control.Concurrent (threadDelay)"
            , "main :: IO ()"
            , "main = threadDelay 100000 >> main"
            ])

testImmediately :: TestSuiteEnv -> Assertion
testImmediately env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    let m = "M"
        updExe = buildExe [] [(T.pack m, "M.hs")]
    updateSessionD session updExe 2
    runActionsExe <- runExe session m
    interrupt runActionsExe
    resOrEx <- runWait runActionsExe
    case resOrEx of
      Right result -> assertEqual "after runExe" (ExitFailure 2) result
      _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "import Control.Concurrent (threadDelay)"
            , "main :: IO ()"
            , "main = threadDelay 100000 >> main"
            ])

testBlackHole :: TestSuiteEnv -> Assertion
testBlackHole env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    let m = "M"
        updExe = buildExe [] [(T.pack m, "M.hs")]
    updateSessionD session updExe 2
    runActionsExe <- runExe session m
    threadDelay 1000000
    resOrExe <- runWait runActionsExe
    -- Here the result differs from runStmt, because the loop is detected
    -- and reported.
    case resOrExe of
      Left result -> assertEqual "after runExe" "M: <<loop>>\n" result
      _ -> assertFailure $ "Unexpected run result: " ++ show resOrExe
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "main :: IO ()"
            , "main = main"
            ])

testManyTimes :: TestSuiteEnv -> Assertion
testManyTimes env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session

    let m = "Main"
        updExe = buildExe [] [(T.pack m, "Main.hs")]
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
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "Main.hs" "main = putStrLn \"Hi!\" >> getLine")
       <> (updateStdoutBufferMode (RunLineBuffering Nothing))
