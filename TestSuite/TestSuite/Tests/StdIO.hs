module TestSuite.Tests.StdIO (testGroupStdIO) where

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

testGroupStdIO :: TestSuiteEnv -> TestTree
testGroupStdIO env = testGroup "Standard I/O" [
    stdTest env "Capture stdout (single putStrLn)"              test_CaptureStdout_SinglePutStrLn
  , stdTest env "Capture stdout (single putStr)"                test_CaptureStdout_SinglePutStr
  , stdTest env "Capture stdout (single putStr with delay)"     test_CaptureStdout_SinglePutStr_Delay
  , stdTest env "Capture stdout (multiple putStrLn)"            test_CaptureStdout_MultiplePutStrLn
  , stdTest env "Capture stdout (mixed putStr and putStrLn)"    test_CaptureStdout_Mixed
  , stdTest env "Capture stdin (simple echo process)"           test_CaptureStdin_SimpleEcho
  , stdTest env "Capture stdin (infinite echo process)"         test_CaptureStdin_InfiniteEcho
  , stdTest env "Capture stdin (interleave runStmt and runExe)" test_Interleaved
  , stdTest env "Capture stderr"                                test_Stderr
  , stdTest env "Merge stdout and stderr"                       test_Merge
  , stdTest env "Merge stdout and stderr (in runExe)"           test_Merge_runExe
  ]

test_CaptureStdout_SinglePutStrLn :: TestSuiteEnv -> Assertion
test_CaptureStdout_SinglePutStrLn env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    runActions <- runStmt session "M" "hello"
    (output, result) <- runWaitAll runActions
    assertEqual "" RunOk result
    assertEqual "" "Hello World\n" output
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "hello :: IO ()"
            , "hello = putStrLn \"Hello World\""
            ])

test_CaptureStdout_SinglePutStr :: TestSuiteEnv -> Assertion
test_CaptureStdout_SinglePutStr env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    runActions <- runStmt session "M" "hello"
    (output, result) <- runWaitAll runActions
    assertEqual "" RunOk result
    assertEqual "" "Hello World" output
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "hello :: IO ()"
            , "hello = putStr \"Hello World\""
            ])

test_CaptureStdout_SinglePutStr_Delay :: TestSuiteEnv -> Assertion
test_CaptureStdout_SinglePutStr_Delay env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    runActions <- runStmt session "M" "hello"
    (output, result) <- runWaitAll runActions
    assertEqual "" RunOk result
    assertEqual "" "hellohi" output
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "import Control.Concurrent (threadDelay)"
            , "import System.IO"
            , "hello :: IO ()"
            , "hello = hSetBuffering stdout LineBuffering >> putStr \"hello\" >> threadDelay 1000000 >> putStr \"hi\""
            ])

test_CaptureStdout_MultiplePutStrLn :: TestSuiteEnv -> Assertion
test_CaptureStdout_MultiplePutStrLn env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    runActions <- runStmt session "M" "hello"
    (output, result) <- runWaitAll runActions
    assertEqual "" RunOk result
    assertEqual "" "Hello World 1\nHello World 2\nHello World 3\n" output
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "hello :: IO ()"
            , "hello = do putStrLn \"Hello World 1\""
            , "           putStrLn \"Hello World 2\""
            , "           putStrLn \"Hello World 3\""
            ])

test_CaptureStdout_Mixed :: TestSuiteEnv -> Assertion
test_CaptureStdout_Mixed env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    runActions <- runStmt session "M" "hello"
    (output, result) <- runWaitAll runActions
    assertEqual "" RunOk result
    assertEqual "" "Hello World 1\nHello World 2Hello World 3\n" output
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "hello :: IO ()"
            , "hello = do putStrLn \"Hello World 1\""
            , "           putStr   \"Hello World 2\""
            , "           putStrLn \"Hello World 3\""
            ])

test_CaptureStdin_SimpleEcho :: TestSuiteEnv -> Assertion
test_CaptureStdin_SimpleEcho env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    runActions <- runStmt session "M" "echo"
    supplyStdin runActions "ECHO!\n"
    (output, result) <- runWaitAll runActions
    assertEqual "" RunOk result
    assertEqual "" "ECHO!\n" output
    let m = "M"
        updExe = buildExe [] [(T.pack m, "M.hs")]
    updateSessionD session updExe 2
    runActionsExe <- runExe session m
    supplyStdin runActionsExe "ECHO!\n"
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "ECHO!\n"
                outExe
    assertEqual "after runExe" ExitSuccess statusExe
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "echo :: IO ()"
            , "echo = getLine >>= putStrLn"
            , "main :: IO ()"
            , "main = echo"
            ])

test_CaptureStdin_InfiniteEcho :: TestSuiteEnv -> Assertion
test_CaptureStdin_InfiniteEcho env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    runActions <- runStmt session "M" "echo"

    do supplyStdin runActions "ECHO 1!\n"
       result <- runWait runActions
       assertEqual "" (Left "ECHO 1!\n") result

    do supplyStdin runActions "ECHO 2!\n"
       result <- runWait runActions
       assertEqual "" (Left "ECHO 2!\n") result

    do interrupt runActions
       resOrEx <- runWait runActions
       case resOrEx of
         Right result -> assertBool "" (isAsyncException result)
         _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "import System.IO"
            , "import Control.Monad"
            , "echo :: IO ()"
            , "echo = do hSetBuffering stdout LineBuffering"
            , "          forever $ getLine >>= putStrLn"
            , "main :: IO ()"
            , "main = echo"
            ])

test_Interleaved :: TestSuiteEnv -> Assertion
test_Interleaved env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session

    let m = "M"
        updExe = buildExe [] [(T.pack m, "M.hs")]
    updateSessionD session updExe 2

    runActions <- runStmt session "M" "echo"
    runActionsExe <- runExe session m

    do supplyStdin runActions "ECHO 1!\n"
       result <- runWait runActions
       assertEqual "" (Left "ECHO 1!\n") result

    do supplyStdin runActionsExe "ECHO 1!\n"
       result <- runWait runActionsExe
       assertEqual "" (Left "ECHO 1!\n") result

    do supplyStdin runActions "ECHO 2!\n"
       result <- runWait runActions
       assertEqual "" (Left "ECHO 2!\n") result

    do supplyStdin runActionsExe "ECHO 2!\n"
       result <- runWait runActionsExe
       assertEqual "" (Left "ECHO 2!\n") result

    do interrupt runActions
       resOrEx <- runWait runActions
       case resOrEx of
         Right result -> assertBool "" (isAsyncException result)
         _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx

    do supplyStdin runActionsExe "ECHO 3!\n"
       result <- runWait runActionsExe
       assertEqual "" (Left "ECHO 3!\n") result

    do interrupt runActionsExe
       resOrEx <- runWait runActionsExe
       case resOrEx of
         Right result -> assertEqual "after runExe" (ExitFailure 2) result
         _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "import System.IO"
            , "import Control.Monad"
            , "echo :: IO ()"
            , "echo = do hSetBuffering stdout LineBuffering"
            , "          forever $ getLine >>= putStrLn"
            , "main :: IO ()"
            , "main = echo"
            ])

test_Stderr :: TestSuiteEnv -> Assertion
test_Stderr env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    runActions <- runStmt session "M" "hello"
    (output, result) <- runWaitAll runActions
    assertEqual "" RunOk result
    assertEqual "" "Hello World\n" output
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "import System.IO"
            , "hello :: IO ()"
            , "hello = hPutStrLn stderr \"Hello World\""
            ])

test_Merge :: TestSuiteEnv -> Assertion
test_Merge env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    runActions <- runStmt session "M" "hello"
    (output, result) <- runWaitAll runActions
    let expectedOutput = L.concat [
            "Hello World 1\n"
          , "Hello World 2\n"
          , "Hello World 3"
          , "Hello World 4"
          , "Hello World 5\n"
          , "Hello World 6\n"
          , "Hello World 7"
          , "Hello World 8"
          ]
    assertEqual "" RunOk result
    assertEqual "" expectedOutput output
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
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

test_Merge_runExe :: TestSuiteEnv -> Assertion
test_Merge_runExe env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session

    let m = "M"
        updExe = buildExe [] [(T.pack m, "M.hs")]
    updateSessionD session updExe 2

    runActions <- runExe session "M"
    (output, result) <- runWaitAll runActions
    let expectedOutput = L.concat [
            "Hello World 1\n"
          , "Hello World 2\n"
          , "Hello World 3"
          , "Hello World 4"
          , "Hello World 5\n"
          , "Hello World 6\n"
          , "Hello World 7"
          , "Hello World 8"
          ]
    assertEqual "" result ExitSuccess
    assertEqual "" expectedOutput output
  where
    -- Note that we have to set buffering here, to match the default
    -- buffering for snippets.
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
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
