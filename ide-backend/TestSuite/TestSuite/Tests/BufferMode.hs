module TestSuite.Tests.BufferMode (testGroupBufferMode) where

import Data.List (partition)
import Data.Monoid
import Test.Tasty
import Test.HUnit
import qualified Data.ByteString.UTF8       as S
import qualified Data.ByteString.Lazy.Char8 as L (unlines)

import IdeSession
import TestSuite.Assertions
import TestSuite.Session
import TestSuite.State

testGroupBufferMode :: TestSuiteEnv -> TestTree
testGroupBufferMode env = testGroup "Buffer modes" [
    stdTest env "Buffer modes: RunNoBuffering"                                      test_RunNoBuffering
  , stdTest env "Buffer modes: RunLineBuffering, no timeout"                        test_RunLineBuffering_NoTimeout
  , stdTest env "Buffer modes: RunBlockBuffering, no timeout"                       test_RunBlockBuffering_NoTimeout
  , stdTest env "Buffer modes: RunLineBuffering, with timeout"                      test_RunLineBuffering_Timeout
  , stdTest env "Buffer modes: RunBlockBuffering, with timeout"                     test_RunBlockBuffering_Timeout
  , stdTest env "Buffer modes: RunBlockBuffering, buffer never fills, with timeout" test_RunBlockBuffering_Timeout_BufferNeverFills
  ]

test_RunNoBuffering :: TestSuiteEnv -> Assertion
test_RunNoBuffering env = testBufferMode env RunNoBuffering

test_RunLineBuffering_NoTimeout :: TestSuiteEnv -> Assertion
test_RunLineBuffering_NoTimeout env = testBufferMode env (RunLineBuffering Nothing)

test_RunBlockBuffering_NoTimeout :: TestSuiteEnv -> Assertion
test_RunBlockBuffering_NoTimeout env = testBufferMode env (RunBlockBuffering (Just 5) Nothing)

test_RunLineBuffering_Timeout :: TestSuiteEnv -> Assertion
test_RunLineBuffering_Timeout env = testBufferMode env (RunLineBuffering (Just 1000000))

test_RunBlockBuffering_Timeout :: TestSuiteEnv -> Assertion
test_RunBlockBuffering_Timeout env = testBufferMode env (RunBlockBuffering (Just 4) (Just 1000000))

test_RunBlockBuffering_Timeout_BufferNeverFills :: TestSuiteEnv -> Assertion
test_RunBlockBuffering_Timeout_BufferNeverFills env = testBufferMode env (RunBlockBuffering (Just 4096) (Just 1000000))

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

testBufferMode :: TestSuiteEnv -> RunBufferMode -> Assertion
testBufferMode env bufferMode = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session

    runActions <- runStmt session "M" "printCs"
    let go acc = do ret <- runWait runActions
                    case ret of
                      Left bs -> do
                        go (S.toString bs : acc)
                      Right RunOk ->
                        verify bufferMode (reverse acc)
                      Right res ->
                        assertFailure $ "Program terminated abnormally: " ++ show res
    go []
  where
    upd = (updateCodeGeneration True)
       <> (updateStdoutBufferMode bufferMode)
       <> (updateStderrBufferMode bufferMode)
       <> (updateSourceFile "M.hs" . L.unlines $
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
      let (withBreak, withoutBreak) = partition ((== '\n') . last) outp
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
    chunkOn x xs = let (firstChunk, rest) = span (/= x) xs
                   in case rest of
                        (x' : rest') -> (firstChunk ++ [x']) : chunkOn x rest'
                        []           -> [firstChunk]
