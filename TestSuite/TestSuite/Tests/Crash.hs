module TestSuite.Tests.Crash (testGroupCrash) where

import Prelude hiding (span)
import Control.Concurrent
import Control.Monad
import Data.Monoid
import Test.Tasty
import Test.HUnit
import qualified Data.ByteString.Lazy.Char8 as L (unlines)

import IdeSession
import TestSuite.State
import TestSuite.Session
import TestSuite.Assertions

testGroupCrash :: TestSuiteEnv -> TestTree
testGroupCrash env = testGroup "GHC crash" [
    stdTest env "GHC crash 1: No delay, no further requests"                     test_NoDelay_NoFurtherRequests
  , stdTest env "GHC crash 2: No delay, follow up request"                       test_NoDelay_FollowUpRequest
  , stdTest env "GHC crash 3: Delay, follow up request"                          test_Delay_FollowUpRequest
  , stdTest env "GHC crash 4: Make sure session gets restarted on second update" test_SessionRestart
  , stdTest env "GHC crash 5: Repeated crashes and restarts"                     test_RepeatedCrashes
  , stdTest env "GHC crash 6: Add additional code after update"                  test_AddCode
  , stdTest env "GHC crash 7: Update imported module after update"               test_ImportedModule
  , stdTest env "GHC crash 8: Update importing module after update"              test_ImportingModule
  ]

test_NoDelay_NoFurtherRequests :: TestSuiteEnv -> Assertion
test_NoDelay_NoFurtherRequests env = withAvailableSession env $ \session -> do
    crashGhcServer session Nothing

test_NoDelay_FollowUpRequest :: TestSuiteEnv -> Assertion
test_NoDelay_FollowUpRequest env = withAvailableSession env $ \session -> do
    crashGhcServer session Nothing
    updateSession session (updateEnv [("Foo", Nothing)]) (\_ -> return ())
    actualErrs <- getSourceErrors session
    assertEqual "" expectedErrs actualErrs
  where
    expectedErrs = [
        SourceError {
            errorKind = KindServerDied
          , errorSpan = TextSpan "<<server died>>"
          , errorMsg  = "user error (Intentional crash)"
          }
      ]

test_Delay_FollowUpRequest :: TestSuiteEnv -> Assertion
test_Delay_FollowUpRequest env = withAvailableSession env $ \session -> do
    crashGhcServer session (Just 1000000)
    updateSession session (updateEnv [("Foo", Nothing)]) (\_ -> return ())
    threadDelay 2000000
    updateSession session (updateEnv [("Foo", Nothing)]) (\_ -> return ())
    assertSourceErrors' session ["Intentional crash"]

test_SessionRestart :: TestSuiteEnv -> Assertion
test_SessionRestart env = withAvailableSession env $ \session -> do
    -- Compile some code
    updateSessionP session upd compilingProgress
    assertNoErrors session

    -- Now crash the server
    crashGhcServer session Nothing

    -- The next request fails, and we get a source error
    updateSessionP session (updateEnv [("Foo", Just "Value1")]) []
    assertSourceErrors' session ["Intentional crash"]

    -- The next request, however, succeeds (and restarts the server,
    -- and recompiles the code)
    updateSessionP session (updateEnv [("Foo", Just "Value2")]) compilingProgress

    -- The code should have recompiled and we should be able to execute it
    do runActions <- runStmt session "M" "printFoo"
       (output, result) <- runWaitAll runActions
       assertEqual "" RunOk result
       assertEqual "" "Value2" output
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "import System.Environment (getEnv)"
            , "printFoo :: IO ()"
            , "printFoo = getEnv \"Foo\" >>= putStr"
            ])

    compilingProgress = [(1, 1, "Compiling M")]

test_RepeatedCrashes :: TestSuiteEnv -> Assertion
test_RepeatedCrashes env = withAvailableSession env $ \session -> do
    -- Compile some code
    updateSessionP session upd compilingProgress
    assertNoErrors session

    -- We repeat the test of 'crash 4' a number of times
    replicateM_ 5 $ do
      -- Now crash the server
      crashGhcServer session Nothing

      -- The next request fails, and we get a source error
      updateSessionP session (updateEnv [("Foo", Just "Value1")]) []
      assertSourceErrors' session ["Intentional crash"]

      -- The next request, however, succeeds (and restarts the server,
      -- and recompiles the code)
      updateSessionP session (updateEnv [("Foo", Just "Value2")]) compilingProgress

      -- The code should have recompiled and we should be able to execute it
      do runActions <- runStmt session "M" "printFoo"
         (output, result) <- runWaitAll runActions
         assertEqual "" RunOk result
         assertEqual "" "Value2" output
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "import System.Environment (getEnv)"
            , "printFoo :: IO ()"
            , "printFoo = getEnv \"Foo\" >>= putStr"
            ])

    compilingProgress = [(1, 1, "Compiling M")]

test_AddCode :: TestSuiteEnv -> Assertion
test_AddCode env = withAvailableSession env $ \session -> do
    updateSessionP session updA (compProgA 1)
    assertNoErrors session

    -- Now crash the server
    crashGhcServer session Nothing

    -- The next request fails, and we get a source error
    updateSessionP session updB []
    assertSourceErrors' session ["Intentional crash"]

    -- The next request, however, succeeds (and restarts the server,
    -- and recompiles the code)
    updateSessionP session updB $ (compProgA 2 ++ compProgB 2)

    -- The code should have recompiled and we should be able to execute it
    do runActions <- runStmt session "B" "printAB"
       (output, result) <- runWaitAll runActions
       assertEqual "" RunOk result
       assertEqual "" "AB" output
  where
    updA = (updateCodeGeneration True)
        <> (updateSourceFile "A.hs" . L.unlines $
             [ "module A where"
             , "printA :: IO ()"
             , "printA = putStr \"A\""
             ])
    updB = (updateCodeGeneration True)
        <> (updateSourceFile "B.hs" . L.unlines $
             [ "module B where"
             , "import A"
             , "printAB :: IO ()"
             , "printAB = printA >> putStr \"B\""
             ])

    compProgA n = [(1, n, "Compiling A")]
    compProgB n = [(2, n, "Compiling B")]

test_ImportedModule :: TestSuiteEnv -> Assertion
test_ImportedModule env = withAvailableSession env $ \session -> do
    updateSessionP session (mconcat [updA, updB]) (compProgA 2 ++ compProgB 2)
    assertNoErrors session

    -- Now crash the server
    crashGhcServer session Nothing

    -- The next request fails, and we get a source error
    let updA2 = (updateCodeGeneration True)
             <> (updateSourceFile "A.hs" . L.unlines $
                  [ "module A where"
                  , "printA :: IO ()"
                  , "printA = putStr \"A2\""
                  ])
    updateSessionP session updA2 []
    assertSourceErrors' session ["Intentional crash"]

    -- The next request, however, succeeds (and restarts the server,
    -- and recompiles the code)
    updateSessionP session updA2 $ mconcat [compProgA 2, compProgB 2]

    -- The code should have recompiled and we should be able to execute it
    do runActions <- runStmt session "B" "printAB"
       (output, result) <- runWaitAll runActions
       assertEqual "" RunOk result
       assertEqual "" "A2B" output
  where
    updA = (updateCodeGeneration True)
        <> (updateSourceFile "A.hs" . L.unlines $
             [ "module A where"
             , "printA :: IO ()"
             , "printA = putStr \"A\""
             ])
    updB = (updateCodeGeneration True)
        <> (updateSourceFile "B.hs" . L.unlines $
             [ "module B where"
             , "import A"
             , "printAB :: IO ()"
             , "printAB = printA >> putStr \"B\""
             ])

    compProgA n = [(1, n, "Compiling A")]
    compProgB n = [(2, n, "Compiling B")]


test_ImportingModule :: TestSuiteEnv -> Assertion
test_ImportingModule env = withAvailableSession env $ \session -> do
    updateSessionP session (mconcat [updA, updB]) (compProgA 2 ++ compProgB 2)
    assertNoErrors session

    -- Now crash the server
    crashGhcServer session Nothing

    -- The next request fails, and we get a source error
    let updB2 = (updateCodeGeneration True)
             <> (updateSourceFile "B.hs" . L.unlines $
                  [ "module B where"
                  , "import A"
                  , "printAB :: IO ()"
                  , "printAB = printA >> putStr \"B2\""
                  ])
    updateSessionP session updB2 []
    assertSourceErrors' session ["Intentional crash"]

    -- The next request, however, succeeds (and restarts the server,
    -- and recompiles the code)
    updateSessionP session updB2 $ mconcat [compProgA 2, compProgB 2]

    -- The code should have recompiled and we should be able to execute it
    do runActions <- runStmt session "B" "printAB"
       (output, result) <- runWaitAll runActions
       assertEqual "" RunOk result
       assertEqual "" "AB2" output
  where
    updA = (updateCodeGeneration True)
        <> (updateSourceFile "A.hs" . L.unlines $
             [ "module A where"
             , "printA :: IO ()"
             , "printA = putStr \"A\""
             ])
    updB = (updateCodeGeneration True)
        <> (updateSourceFile "B.hs" . L.unlines $
             [ "module B where"
             , "import A"
             , "printAB :: IO ()"
             , "printAB = printA >> putStr \"B\""
             ])

    compProgA n = [(1, n, "Compiling A")]
    compProgB n = [(2, n, "Compiling B")]
