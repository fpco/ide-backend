module TestSuite.Tests.SessionRestart (testGroupSessionRestart) where

import Control.Concurrent
import Data.Monoid
import System.Exit
import Test.Tasty
import Test.HUnit
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L (unlines)
import qualified Data.Text                  as T

import IdeSession
import TestSuite.Assertions
import TestSuite.Session
import TestSuite.State

testGroupSessionRestart :: TestSuiteEnv -> TestTree
testGroupSessionRestart env = testGroup "Session restart" [
    stdTest env "Well-behaved snippet; after .1 sec"                                   test_WellBehaved
  , stdTest env "Blackhole, snippet doesn't swallow exceptions; after .1 sec"          test_Blackhole_DontSwallow
  , stdTest env "Snippet swallows all exceptions; after .1 sec"                        test_Swallow
  , stdTest env "Black hole, swallow all exceptions; after .1 sec"                     test_Blackhole_Swallow
  , stdTest env "Evil snippet with infinite stack of exception handlers; after .1 sec" test_Evil
  , stdTest env "Make sure environment is restored after session restart"              test_EnvRestored
  ]

test_WellBehaved :: TestSuiteEnv -> Assertion
test_WellBehaved env =
  restartRun env [ "module M where"
                 , "import Control.Concurrent (threadDelay)"
                 , "loop :: IO ()"
                 , "loop = threadDelay 10000 >> loop"
                 ] ExitSuccess

test_Blackhole_DontSwallow :: TestSuiteEnv -> Assertion
test_Blackhole_DontSwallow env =
  restartRun env [ "module M where"
                 , "loop :: IO ()"
                 , "loop = loop"
                 ] ExitSuccess


test_Swallow :: TestSuiteEnv -> Assertion
test_Swallow env =
  restartRun env [ "module M where"
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

test_Blackhole_Swallow :: TestSuiteEnv -> Assertion
test_Blackhole_Swallow env =
  restartRun env [ "module M where"
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

test_Evil :: TestSuiteEnv -> Assertion
test_Evil env =
  restartRun env [ "module M where"
                 , ""
                 , "import qualified Control.Exception as Ex"
                 , ""
                 , "loop :: IO ()"
                 , "loop = Ex.catch loop $ \\e -> let _ = e :: Ex.SomeException in loop"
                 ] ExitSuccess

test_EnvRestored :: TestSuiteEnv -> Assertion
test_EnvRestored env = withAvailableSession env $ \session -> do
    -- Set environment
    updateSession session (updateEnv [("Foo", Just "Value1")]) (\_ -> return ())

    -- Compile and run the code on the first server
    updateSessionD session upd 1
    assertNoErrors session
    do runActions <- runStmt session "M" "printFoo"
       (output, result) <- runWaitAll runActions
       assertEqual "" RunOk result
       assertEqual "" "Value1" output

    do let m = "M"
           updExe = buildExe [] [(T.pack m, "M.hs")]
       updateSessionD session updExe 2
       runActionsExe <- runExe session m
       (outExe, statusExe) <- runWaitAll runActionsExe
       assertEqual "Output from runExe"
                   "Value1"
                   outExe
       assertEqual "after runExe" ExitSuccess statusExe

    -- Start a new server
    serverBefore <- getGhcServer session
    restartSession session

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
       assertEqual "" "Value1" output

    do let m = "M"
           updExe = buildExe [] [(T.pack m, "M.hs")]
       updateSessionD session updExe 2
       runActionsExe <- runExe session m
       (outExe, statusExe) <- runWaitAll runActionsExe
       assertEqual "Output from runExe"
                   "Value1"
                   outExe
       assertEqual "after runExe" ExitSuccess statusExe
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "import System.Environment (getEnv)"
            , "printFoo :: IO ()"
            , "printFoo = getEnv \"Foo\" >>= putStr"
            , "main :: IO ()"
            , "main = printFoo"
            ])


{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

restartRun :: TestSuiteEnv -> [L.ByteString] -> ExitCode -> Assertion
restartRun env code exitCode = withAvailableSession env $ \session -> do
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

    -- Force cancel the old snippet
    forceCancel runActionsBefore
    resOrEx <- runWait runActionsBefore
    case resOrEx of
      Right RunForceCancelled -> return ()
      _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" $ L.unlines code)
