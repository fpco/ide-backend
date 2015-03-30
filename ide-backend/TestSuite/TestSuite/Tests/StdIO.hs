module TestSuite.Tests.StdIO (testGroupStdIO) where

import Control.Concurrent
import Control.Monad
import Data.Monoid
import System.Exit
import System.Random
import Test.Tasty
import Test.HUnit
import qualified Data.ByteString.UTF8       as S
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.UTF8  as L
import qualified Data.Text                  as T

import IdeSession
import TestSuite.Assertions
import TestSuite.Session
import TestSuite.State

testGroupStdIO :: TestSuiteEnv -> TestTree
testGroupStdIO env = testGroup "Standard I/O" $ [
    stdTest env "Capture stdout (single putStrLn)"               test_CaptureStdout_SinglePutStrLn
  , stdTest env "Capture stdout (single putStr)"                 test_CaptureStdout_SinglePutStr
  , stdTest env "Capture stdout (single putStr with delay)"      test_CaptureStdout_SinglePutStr_Delay
  , stdTest env "Capture stdout (multiple putStrLn)"             test_CaptureStdout_MultiplePutStrLn
  , stdTest env "Capture stdout (mixed putStr and putStrLn)"     test_CaptureStdout_Mixed
  , stdTest env "Capture stdin (simple echo process)"            test_CaptureStdin_SimpleEcho
  , stdTest env "Capture stdin (infinite echo process)"          test_CaptureStdin_InfiniteEcho
  , stdTest env "Capture stderr"                                 test_Stderr
  , stdTest env "Merge stdout and stderr"                        test_Merge
  , stdTest env "Interrupt, then capture stdout"                 test_Interrupt_CaptureStdout
  , stdTest env "Snippet closes stdin; next snippet unaffected"  test_ClosesStdin
  , stdTest env "Snippet closes stdin (interrupted 'interact'); next snippet unaffected" test_ClosesStdin_Interact
  , stdTest env "Snippet closes stdout; next snippet unaffected" test_ClosesStdout
  , stdTest env "Snippet closes stderr; next snippet unaffected" test_ClosesStderr
  , stdTest env "Snippet closes stderr, using timeout buffering" test_ClosesStderr_Timeout
  , stdTest env "Make sure encoding is UTF8"                     test_UTF8
  ] ++ exeTests env [
    stdTest env "Capture stdin (interleave runStmt and runExe)"  test_Interleaved
  , stdTest env "Merge stdout and stderr (in runExe)"            test_Merge_runExe
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
       <> (updateSourceFile "M.hs" . unlinesUtf8 $
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
       <> (updateSourceFile "M.hs" . unlinesUtf8 $
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
       <> (updateSourceFile "M.hs" . unlinesUtf8 $
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
       <> (updateSourceFile "M.hs" . unlinesUtf8 $
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
       <> (updateSourceFile "M.hs" . unlinesUtf8 $
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

    do runActions <- runStmt session "M" "echo"
       supplyStdin runActions "ECHO!\n"
       (output, result) <- runWaitAll runActions
       assertEqual "" RunOk result
       assertEqual "" "ECHO!\n" output

    ifTestingExe env $ do
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
       <> (updateSourceFile "M.hs" . unlinesUtf8 $
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
       <> (updateSourceFile "M.hs" . unlinesUtf8 $
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
         Right result -> assertEqual "after runExe" (ExitFailure (-2)) result -- SIGINT
         _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . unlinesUtf8 $
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
       <> (updateSourceFile "M.hs" . unlinesUtf8 $
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
       <> (updateStdoutBufferMode RunNoBuffering)
       <> (updateStderrBufferMode RunNoBuffering)
       <> (updateSourceFile "M.hs" . unlinesUtf8 $
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
    assertEqual "" ExitSuccess result 
    assertEqual "" expectedOutput output
  where
    -- Note that we have to set buffering here, to match the default
    -- buffering for snippets.
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . unlinesUtf8 $
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

test_Interrupt_CaptureStdout :: TestSuiteEnv -> Assertion
test_Interrupt_CaptureStdout env = withAvailableSession env $ \session -> do
    updateSession session (updateCodeGeneration True) (\_ -> return ())
    let upd1 = updateSourceFile "Main.hs" . unlinesUtf8 $
                 [ "import Control.Monad"
                 , "main = forever $ print 1"
                 ]
        upd2 = updateSourceFile "Main.hs" . unlinesUtf8 $
                 [ "main = print 1234" ]

    do updateSessionD session upd1 1
       runActions <- runStmt session "Main" "main"
       -- TODO: Not sure why 'interrupt' doesn't work here.
       --interrupt runActions
       forceCancel runActions
       randomRIO (0, 1000000) >>= threadDelay -- Wait between 0 and 1sec
       void $ runWaitAll runActions

    ifTestingExe env $ do
       let m = "Main"
           updExe = buildExe [] [(T.pack m, "Main.hs")]
       updateSessionD session updExe 2
       runActionsExe <- runExe session m
       interrupt runActionsExe
       randomRIO (0, 1000000) >>= threadDelay -- Wait between 0 and 1sec
       void $ runWaitAll runActionsExe

    do updateSessionD session upd2 1
       runActions <- runStmt session "Main" "main"
       (output, result) <- runWaitAll runActions
       assertEqual "" RunOk result
       assertEqual "" "1234\n" output

    ifTestingExe env $ do
       let m = "Main"
           updExe = buildExe [] [(T.pack m, "Main.hs")]
       updateSessionD session updExe 2
       runActionsExe <- runExe session m
       (outExe, statusExe) <- runWaitAll runActionsExe
       assertEqual "Output from runExe"
                   "1234\n"
                   outExe
       assertEqual "after runExe" ExitSuccess statusExe

test_ClosesStdin :: TestSuiteEnv -> Assertion
test_ClosesStdin env = withAvailableSession env $ \session -> do
    updateSession session updates2 $ const $ return ()
    ra2 <- runStmt session "Main" "main"
    out2b <- runWait ra2
    assertEqual "" out2b (Right RunOk)

    updateSession session updates3 $ const $ return ()
    ra3 <- runStmt session "Main" "main"
    supplyStdin ra3 "Michael\n"
    (output, out3b) <- runWaitAll ra3
    assertEqual "" RunOk out3b
    assertEqual "" "Michael\n" output
  where
    updates2 = mconcat
        [ updateCodeGeneration True
        , updateSourceFile "Main.hs" "import System.IO\nmain = hClose stdin"
        ]

    updates3 =
      updateSourceFile "Main.hs" "main = getLine >>= putStrLn"

test_ClosesStdin_Interact :: TestSuiteEnv -> Assertion
test_ClosesStdin_Interact env = withAvailableSession env $ \session -> do
    updateSession session updates2 $ const $ return ()
    ra2 <- runStmt session "Main" "main"
    supplyStdin ra2 "hello\n"
    out2a <- runWait ra2
    out2a @?= Left "hello\n"
    interrupt ra2
    out2b <- runWait ra2
    case out2b of
      Right result -> assertBool "" (isAsyncException result)
      _ -> assertFailure $ "Unexpected run result: " ++ show out2b

    updateSession session updates3 $ const $ return ()
    ra3 <- runStmt session "Main" "main"
    out3a <- runWait ra3
    out3a @?= Left "Hi!\n"
    supplyStdin ra3 "Michael\n"
    out3b <- runWait ra3
    assertEqual "" out3b (Right RunOk)
  where
    updates2 = mconcat
        [ updateCodeGeneration True
        , updateSourceFile "Main.hs" "main = getContents >>= putStr"
        , updateStdoutBufferMode $ RunLineBuffering Nothing
        ]

    updates3 = mconcat
        [ updateCodeGeneration True
        , updateSourceFile "Main.hs" "main = putStrLn \"Hi!\" >> getLine >> return ()"
        , updateStdoutBufferMode $ RunLineBuffering Nothing
        ]

test_ClosesStdout :: TestSuiteEnv -> Assertion
test_ClosesStdout env = withAvailableSession env $ \session -> do
    updateSession session updates2 $ const $ return ()
    ra2 <- runStmt session "Main" "main"
    out2b <- runWait ra2
    assertEqual "" out2b (Right RunOk)

    updateSession session updates3 $ const $ return ()
    ra3 <- runStmt session "Main" "main"
    supplyStdin ra3 "Michael\n"
    (output, out3b) <- runWaitAll ra3
    assertEqual "" RunOk out3b
    assertEqual "" "Michael\n" output
  where
    updates2 = mconcat
        [ updateCodeGeneration True
        , updateSourceFile "Main.hs" "import System.IO\nmain = hClose stdout"
        ]

    updates3 =
      updateSourceFile "Main.hs" "main = getLine >>= putStrLn"

test_ClosesStderr :: TestSuiteEnv -> Assertion
test_ClosesStderr env = withAvailableSession env $ \session -> do
    updateSession session updates2 $ const $ return ()
    ra2 <- runStmt session "Main" "main"
    out2b <- runWait ra2
    assertEqual "" out2b (Right RunOk)

    updateSession session updates3 $ const $ return ()
    ra3 <- runStmt session "Main" "main"
    supplyStdin ra3 "Michael\n"
    (output, out3b) <- runWaitAll ra3
    assertEqual "" RunOk out3b
    assertEqual "" "Michael\n" output
  where
    updates2 = mconcat
        [ updateCodeGeneration True
        , updateSourceFile "Main.hs" "import System.IO\nmain = hClose stderr"
        ]

    updates3 =
      updateSourceFile "Main.hs" "import System.IO\nmain = getLine >>= hPutStrLn stderr"

test_ClosesStderr_Timeout :: TestSuiteEnv -> Assertion
test_ClosesStderr_Timeout env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    ra <- runStmt session "Main" "main"
    forM_ [1 :: Int .. 3] $ \i -> do
      result <- runWait ra
      result @?= Left (S.fromString $ show i ++ "\n")

    finalResult <- runWait ra
    assertEqual "" finalResult (Right RunOk)
  where
    upd = mconcat [
              updateCodeGeneration True
            , updateStdoutBufferMode $ RunLineBuffering Nothing
            , updateStderrBufferMode $ RunBlockBuffering (Just 4096) (Just 250000)
            , updateSourceFile "Main.hs" . unlinesUtf8 $ [
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

test_UTF8 :: TestSuiteEnv -> Assertion
test_UTF8 env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    runActions <- runStmt session "M" "main"
    (output, result) <- runWaitAll runActions
    assertEqual "" RunOk result
    assertEqual "" "你好. 怎么样?\n" (L.toString output)

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
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . unlinesUtf8 $
            [ "module M where"
            , "main :: IO ()"
            , "main = putStrLn \"你好. 怎么样?\""
            ])

unlinesUtf8 :: [String] -> L.ByteString
unlinesUtf8 = L.fromString . unlines
