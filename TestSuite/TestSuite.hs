module Main where

import Test.HUnit
import Test.Tasty

import IdeSession
import TestSuite.State
import TestSuite.Tests.TypeInformation
import TestSuite.Tests.SessionState
import TestSuite.Tests.Compilation
import TestSuite.Tests.InterruptRunStmt
import TestSuite.Tests.InterruptRunExe
import TestSuite.Tests.StdIO

-- | Sanity check: make sure we can communicate with the server at all
-- and that we get the expected version
testGetGhcVersion :: TestSuiteEnv -> Assertion
testGetGhcVersion env = withAvailableSession env $ \session -> do
  version <- getGhcVersion session
  assertEqual ""  (testSuiteEnvGhcVersion env) version

allTests :: String -> TestSuiteEnv -> TestTree
allTests name env = testGroup name [
    stdTest env "getGhcVersion" testGetGhcVersion
  , testGroupSessionState     env
  , testGroupCompilation      env
  , testGroupInterruptRunStmt env
  , testGroupInterruptRunExe  env
  , testGroupStdIO            env
  , testGroupTypeInformation  env
  ]

main :: IO ()
main =
    defaultMainWithIngredients ings $ testSuite $ \env ->
      let TestSuiteConfig{..} = testSuiteEnvConfig env
          env74 = env { testSuiteEnvGhcVersion = GHC742 }
          env78 = env { testSuiteEnvGhcVersion = GHC78  }
      in testGroup "IDE backend tests" $
           (if testSuiteConfigTest74 then [allTests "GHC 7.4.2" env74] else [])
        ++ (if testSuiteConfigTest78 then [allTests "GHC 7.8.3" env78] else [])
  where
    ings = includingOptions testSuiteCommandLineOptions
         : defaultIngredients
