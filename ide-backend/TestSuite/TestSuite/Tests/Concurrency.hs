module TestSuite.Tests.Concurrency (testGroupConcurrency) where

import Prelude hiding (span, mod)
import Control.Concurrent
import Control.Monad
import Data.Monoid
import Test.HUnit
import Test.Tasty
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L (unlines)
import qualified Data.ByteString.Lazy.UTF8  as L
import qualified Data.ByteString            as S
import qualified Data.ByteString.UTF8       as S

import IdeSession
import TestSuite.State
import TestSuite.Session
import TestSuite.Assertions

testGroupConcurrency :: TestSuiteEnv -> TestTree
testGroupConcurrency env = testGroup "Concurrency" [
    stdTest env "Concurrent snippets 1: Run same snippet multiple times"         testSameSnippet
  , stdTest env "Concurrent snippets 2: Execute different snippets concurrently" testDifferentSnippets
  ]

testSameSnippet :: TestSuiteEnv -> Assertion
testSameSnippet env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session

    -- Execute all snippets, but leave them waiting for input
    snippets <- forM ["foo", "bar", "baz", "Foo", "Bar", "Baz", "FOO", "BAR", "BAZ"] $ \str -> do
      let expectedResult = L.concat (replicate 100 (L.fromString (str ++ "\n")))
      runActions <- runStmt session "M" "echo"
      return (runActions, str, expectedResult)

    -- Start all snippets and collect all their output concurrently
    testResults <- forM snippets $ \(runActions, str, _expectedResult) -> do
      testResult <- newEmptyMVar
      _ <- forkIO $ do
        supplyStdin runActions (S.fromString (str ++ "\n"))
        putMVar testResult =<< runWaitAll' runActions
      return testResult

    -- Wait for all test results, and compare against expected results
    forM_ (zip snippets testResults) $ \((_runActions, _str, expectedResult), testResult) -> do
      (output, result) <- takeMVar testResult
      assertEqual "" RunOk result
      assertEqual "" expectedResult output
  where
    upd = (updateCodeGeneration True)
       <> (updateStdoutBufferMode $ RunLineBuffering Nothing)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "import Control.Monad"
            , "echo :: IO ()"
            , "echo = do str <- getLine"
            , "          replicateM_ 100 $ putStrLn str"
            ])

testDifferentSnippets :: TestSuiteEnv -> Assertion
testDifferentSnippets env = withAvailableSession env $ \session -> do
    -- Execute all snippets, but leave them waiting for input
    snippets <- forM ["foo", "bar", "baz", "Foo", "Bar", "Baz", "FOO", "BAR", "BAZ"] $ \str -> do
      let upd = (updateCodeGeneration True)
             <> (updateStdoutBufferMode $ RunLineBuffering Nothing)
             <> (updateSourceFile "M.hs" $ L.fromString . unlines $
                  [ "module M where"
                  , "import Control.Monad"
                  , "echo :: IO ()"
                  , "echo = do _waiting <- getLine"
                  , "          replicateM_ 100 $ putStrLn " ++ show str
                  ])
      updateSessionD session upd 1
      assertNoErrors session

      let expectedResult = L.concat (replicate 100 (L.fromString (str ++ "\n")))
      runActions <- runStmt session "M" "echo"
      return (runActions, expectedResult)

    -- Start all snippets and collect all their output concurrently
    testResults <- forM snippets $ \(runActions, _expectedResult) -> do
      testResult <- newEmptyMVar
      _ <- forkIO $ do
        supplyStdin runActions "\n"
        putMVar testResult =<< runWaitAll' runActions
      return testResult

    -- Wait for all test results, and compare against expected results
    forM_ (zip snippets testResults) $ \((_runActions, expectedResult), testResult) -> do
      (output, result) <- takeMVar testResult
      assertEqual "" RunOk result
      assertEqual "" expectedResult output

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

runWaitAll' :: forall a. RunActions a -> IO (L.ByteString, a)
runWaitAll' RunActions{runWait} = go []
  where
    go :: [S.ByteString] -> IO (L.ByteString, a)
    go acc = do
      resp <- runWait
      case resp of
        Left  bs        -> go (bs : acc)
        Right runResult -> return (L.fromChunks (reverse acc), runResult)
