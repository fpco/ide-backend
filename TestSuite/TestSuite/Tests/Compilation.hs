module TestSuite.Tests.Compilation (testGroupCompilation) where

import Test.HUnit
import Test.Tasty

import IdeSession
import TestSuite.State
import TestSuite.Session
import TestSuite.Assertions

testGroupCompilation :: TestSuiteEnv -> TestTree
testGroupCompilation env = testGroup "Compilation" [
    stdTest env "Compile a project: A depends on B, error in A"  test_AdependsB_errorA
  , stdTest env "Compile a project: A depends on B, error in B"  test_AdependsB_errorB
  , stdTest env "Compile and run a project with some .lhs files" test_lhs
  ]

test_AdependsB_errorA :: TestSuiteEnv -> Assertion
test_AdependsB_errorA env = withAvailableSession env $ \session -> do
  loadModulesFrom session "test/AerrorB"
  assertSourceErrors session [[(Just "A.hs", "No instance for (Num (IO ()))")]]

test_AdependsB_errorB :: TestSuiteEnv -> Assertion
test_AdependsB_errorB env = withAvailableSession env $ \session -> do
  loadModulesFrom session "test/ABerror"
  assertSourceErrors session [[(Just "B.hs", "No instance for (Num (IO ()))")]]

test_lhs :: TestSuiteEnv -> Assertion
test_lhs env = withAvailableSession env $ \session -> do
  loadModulesFrom session "test/compiler/utils"
  assertNoErrors session
  let update2 = updateCodeGeneration True
  updateSessionD session update2 4
  assertNoErrors session
  runActions <- runStmt session "Maybes" "main"
  (output, result) <- runWaitAll runActions
  assertEqual "" RunOk result
  assertEqual "" output "False\n"
