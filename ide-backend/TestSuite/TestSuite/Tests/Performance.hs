module TestSuite.Tests.Performance (testGroupPerformance) where

import Prelude hiding (span)
import Control.Monad
import Data.Monoid
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout
import Test.HUnit
import Test.Tasty
import qualified Control.Exception          as Ex
import qualified Data.ByteString.Lazy.UTF8  as L
import qualified System.Environment         as System

import IdeSession
import TestSuite.State
import TestSuite.Session
import TestSuite.Assertions

testGroupPerformance :: TestSuiteEnv -> TestTree
testGroupPerformance env = testGroup "Performance" [
  --   stdTest env "Perf: Load testPerfMs modules in one call 1"                           test_OneCall_1
  -- , stdTest env "Perf: Load testPerfMs modules in one call 2"                           test_OneCall_2
  -- , stdTest env "Perf: Load testPerfMs modules in many calls 1"                         test_ManyCalls_1
  -- , stdTest env "Perf: Load testPerfMs modules in many calls 2"                         test_ManyCalls_2
  -- , stdTest env "Perf: Load 4xtestPerfMs modules, each batch in one call 1"             test_Batch_OneCall_1
  -- , stdTest env "Perf: Load 4xtestPerfMs modules, each batch in one call 2"             test_Batch_OneCall_2
  -- , stdTest env "Perf: Update a module testPerfTimes with no context 1"                 test_NoContext_1
  -- , stdTest env "Perf: Update a module testPerfTimes with no context 2"                 test_NoContext_2
  -- , stdTest env "Perf: Update a module testPerfTimes with testPerfMs modules 1"         test_UpdateModule_1
  -- , stdTest env "Perf: Update a module testPerfTimes with testPerfMs modules 2"         test_UpdateModule_2
  -- , stdTest env "Perf: Update and run a module testPerfTimes with testPerfMs modules 1" test_UpdateAndRun_1
  -- , stdTest env "Perf: Update and run a module testPerfTimes with testPerfMs modules 2" test_UpdateAndRun_2
  ]

test_OneCall_1 :: TestSuiteEnv -> Assertion
test_OneCall_1 env = withAvailableSession env $ \session -> limitPerfTest $ do
    updateSessionD session (updateCodeGeneration True) 1
    updateSessionD session updates testPerfMs
    assertNoErrors session
  where
    updates = foldr (\n ups -> updKN n n <> ups)
                    mempty
                    [1..testPerfMs]

test_OneCall_2 :: TestSuiteEnv -> Assertion
test_OneCall_2 env = withAvailableSession env $ \session -> limitPerfTest $ do
    updateSessionD session (updateCodeGeneration True) 1
    updateSessionD session updates testPerfMs
    assertNoErrors session
  where
    updates = foldr (\n ups -> updDepKN n n <> ups)
                    mempty
                    [1..testPerfMs]

test_ManyCalls_1 :: TestSuiteEnv -> Assertion
test_ManyCalls_1 env = withAvailableSession env $ \session -> limitPerfTest $ do
    updateSessionD session (updateCodeGeneration True) 1
    mapM_ (\up -> updateSessionD session up 1) updates
    assertNoErrors session
  where
    updates = map (\n -> updKN n n) [1..testPerfMs]

test_ManyCalls_2 :: TestSuiteEnv -> Assertion
test_ManyCalls_2 env = withAvailableSession env $ \session -> limitPerfTest $ do
    updateSessionD session (updateCodeGeneration True) 1
    mapM_ (\up -> updateSessionD session up 1) updates
    assertNoErrors session
  where
    updates = map (\n -> updDepKN n n) [1..testPerfMs]

test_Batch_OneCall_1 :: TestSuiteEnv -> Assertion
test_Batch_OneCall_1 env = withAvailableSession env $ \session -> limitPerfTest $ do
    updateSessionD session (updateCodeGeneration True) 1
    updateSessionD session updates1 testPerfMs
    updateSessionD session updates2 testPerfMs
    updateSessionD session updates1 testPerfMs
    updateSessionD session updates2 testPerfMs
    assertNoErrors session
  where
    updates1 = foldr (\n ups -> updKN n n <> ups)
                    mempty
                    [1..testPerfMs]
    updates2 = foldr (\n ups -> updKN 42 n <> ups)
                    mempty
                    [1..testPerfMs]

test_Batch_OneCall_2 :: TestSuiteEnv -> Assertion
test_Batch_OneCall_2 env = withAvailableSession env $ \session -> limitPerfTest $ do
    updateSessionD session (updateCodeGeneration True) 1
    updateSessionD session updates1 testPerfMs
    updateSessionD session updates2 testPerfMs
    updateSessionD session updates1 testPerfMs
    updateSessionD session updates2 testPerfMs
    assertNoErrors session
  where
    updates1 = foldr (\n ups -> updDepKN n n <> ups)
                    mempty
                    [1..testPerfMs]
    updates2 = foldr (\n ups -> updDepKN 42 n <> ups)
                    mempty
                    [1..testPerfMs]

test_NoContext_1 :: TestSuiteEnv -> Assertion
test_NoContext_1 env = withAvailableSession env $ \session -> limitPerfTest $ do
    updateSessionD session (updateCodeGeneration True) 1
    mapM_ (\k -> updateSessionD session (upd k) 1) [1..testPerfTimes]
    assertNoErrors session
  where
    upd k = updKN k 1

test_NoContext_2 :: TestSuiteEnv -> Assertion
test_NoContext_2 env = withAvailableSession env $ \session -> limitPerfTest $ do
    updateSessionD session (updateCodeGeneration True) 1
    mapM_ (\k -> updateSessionD session (upd k) 1) [1..testPerfTimes]
    assertNoErrors session
  where
    upd k = updDepKN k 1

test_UpdateModule_1 :: TestSuiteEnv -> Assertion
test_UpdateModule_1 env = withAvailableSession env $ \session -> limitPerfTest $ do
    updateSessionD session (updateCodeGeneration True) 1
    updateSessionD session updates testPerfMs
    mapM_ (\k -> updateSessionD session (upd k) 1) [1..testPerfTimes]
    assertNoErrors session
  where
    updates = foldr (\n ups -> ups <> updKN n n)
                    mempty
                    [1..testPerfMs]

    upd k = updKN k (testPerfMs `div` 2)

test_UpdateModule_2 :: TestSuiteEnv -> Assertion
test_UpdateModule_2 env = withAvailableSession env $ \session -> limitPerfTest $ do
    updateSessionD session (updateCodeGeneration True) 1
    updateSessionD session updates testPerfMsFixed
    mapM_ (\k -> updateSessionD session (upd k) (1 + testPerfMsFixed `div` 2)) [1..testPerfTimes]
    assertNoErrors session
  where
    updates = foldr (\n ups -> ups <> updDepKN n n)
                    mempty
                    [1..testPerfMsFixed]

    testPerfMsFixed = 10  -- dependencies force recompilation: slow
    upd k = updDepKN k (testPerfMsFixed `div` 2)

test_UpdateAndRun_1 :: TestSuiteEnv -> Assertion
test_UpdateAndRun_1 env = withAvailableSession env $ \session -> limitPerfTest  $ do
    updateSessionD session (updateCodeGeneration True) 1
    updateSessionD session updates testPerfMsFixed
    mapM_ (\k -> do
      updateSessionD session (upd k) 1
      runActions <- runStmt session mdiv2 "m"
      void $ runWaitAll runActions
      ) [1..testPerfTimes]
    assertNoErrors session
  where
    updates = foldr (\n ups -> ups <> updKN n n)
                    mempty
                    [1..testPerfMsFixed]

    testPerfMsFixed = testPerfMs * 1 `div` 2  -- running has overheads
    upd k = updKN k (testPerfMsFixed `div` 2)
    mdiv2 = "M" ++ show (testPerfMsFixed `div` 2)

test_UpdateAndRun_2 :: TestSuiteEnv -> Assertion
test_UpdateAndRun_2 env = withAvailableSession env $ \session -> limitPerfTest $ do
    updateSessionD session (updateCodeGeneration True) 1
    updateSessionD session updates testPerfMsFixed
    mapM_ (\k -> do
      updateSessionD session (upd k) (1 + testPerfMsFixed `div` 2)
      runActions <- runStmt session mdiv2 "m"
      void $ runWaitAll runActions
      ) [1..testPerfTimes]
    assertNoErrors session
  where
    updates = foldr (\n ups -> ups <> updDepKN n n)
                    mempty
                    [1..testPerfMsFixed]

    testPerfMsFixed = 8  -- dependencies force recompilation: slow
    upd k = updDepKN k (testPerfMsFixed `div` 2)
    mdiv2 = "M" ++ show (testPerfMsFixed `div` 2)

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

limitPerfTest :: IO () -> IO ()
limitPerfTest t = do
  mu <- timeout (testPerfLimit * 1000000) t
  case mu of
    Nothing -> fail "Performance test did not finish within alotted time"
    Just () -> return ()

-- TODO: This should use tasty command line arguments instead
testPerfMs :: Int
{-# NOINLINE testPerfMs #-}
testPerfMs = read $ unsafePerformIO $
  System.getEnv "IDE_BACKEND_testPerfMs"
  `Ex.catch` (\(_ :: Ex.IOException) -> return "20")

-- TODO: This should use tasty command line arguments instead
testPerfTimes :: Int
{-# NOINLINE testPerfTimes #-}
testPerfTimes = read $ unsafePerformIO $
  System.getEnv "IDE_BACKEND_testPerfTimes"
  `Ex.catch` (\(_ :: Ex.IOException) -> return "20")

-- TODO: This should use tasty command line arguments instead
testPerfLimit :: Int
{-# NOINLINE testPerfLimit #-}
testPerfLimit = read $ unsafePerformIO $
  System.getEnv "IDE_BACKEND_testPerfLimit"
  `Ex.catch` (\(_ :: Ex.IOException) -> return "30")

updKN :: Int -> Int -> IdeSessionUpdate
updKN k n =
  let moduleN = L.fromString $ unlines $
              [ "module M" ++ show n ++ " where"
              , "import Control.Concurrent (threadDelay)"
              , "m :: IO ()"
              , "m = threadDelay " ++ show k
              ]
  in updateSourceFile ("M" ++ show n ++ ".hs") moduleN

updDepKN :: Int -> Int -> IdeSessionUpdate
updDepKN k n =
  let depN | n <= 1 = ("System.IO", ".hFlush System.IO.stdout")
           | otherwise = ("M" ++ show (n - 1), ".m")
      moduleN = L.fromString $ unlines $
              [ "module M" ++ show n ++ " where"
              , "import Control.Concurrent (threadDelay)"
              , "import qualified " ++ fst depN
              , "m :: IO ()"
              , "m = threadDelay " ++ show k ++ " >> "
                ++ fst depN ++ snd depN
              ]
  in updateSourceFile ("M" ++ show n ++ ".hs") moduleN
