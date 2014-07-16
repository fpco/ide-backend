module Main where

import Test.Tasty

import IdeSession
import TestSuite.State
import TestSuite.Tests.TypeInformation

allTests :: String -> TestSuiteEnv -> TestTree
allTests name env = testGroup name [
    testGroupTypeInformation env
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
