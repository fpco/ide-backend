module TestSuite.Tests.Pseudoterminal (testGroupPseudoterminal) where

import Data.Monoid
import Test.Tasty
import Test.HUnit

import IdeSession
import TestSuite.Session
import TestSuite.Assertions
import TestSuite.State

testGroupPseudoterminal :: TestSuiteEnv -> TestTree
testGroupPseudoterminal env = testGroup "Pseudoterminal" $ [
    stdTest env "Pseudoterminal echoes" test_Echoes
  , stdTest env "Pseudoterminal is terminal" test_IsTerminal
  ]

test_Echoes :: TestSuiteEnv -> Assertion
test_Echoes env = withAvailableSession env $ \session -> do
    let update =
          (updateCodeGeneration True) <>
          (loadModule "Main.hs" "main = (readLn :: IO Int) >>= print")
    updateSessionD session update 1
    assertNoErrors session
    actions <- runStmtPty session "Main" "main"
    supplyStdin actions "1\n"
    assertEqual "after runWait" ("1\r\n1\r\n", RunOk) =<< runWaitAll actions

test_IsTerminal :: TestSuiteEnv -> Assertion
test_IsTerminal env = withAvailableSession env $ \session -> do
    let update =
          (updateCodeGeneration True) <>
          (loadModule "Main.hs" code)
        code = unlines
          [ "import System.IO (hIsTerminalDevice, stdout)"
          , "main = hIsTerminalDevice stdout >>= print"
          ]
    updateSessionD session update 1
    assertNoErrors session
    actions <- runStmtPty session "Main" "main"
    assertEqual "after runWait" ("True\r\n", RunOk) =<< runWaitAll actions
