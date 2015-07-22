module Main where

import Data.List (isPrefixOf)
import System.Process (readProcess)
import System.Environment
import Test.HUnit
import Test.Tasty

import IdeSession
import TestSuite.State
import TestSuite.Tests.API
import TestSuite.Tests.Autocompletion
import TestSuite.Tests.BufferMode
import TestSuite.Tests.BuildDoc
import TestSuite.Tests.BuildExe
import TestSuite.Tests.BuildLicenses
import TestSuite.Tests.C
import TestSuite.Tests.CabalMacros
import TestSuite.Tests.Compilation
import TestSuite.Tests.Compliance
import TestSuite.Tests.Concurrency
import TestSuite.Tests.Crash
import TestSuite.Tests.Debugger
import TestSuite.Tests.FFI
import TestSuite.Tests.Integration
import TestSuite.Tests.InterruptRunExe
import TestSuite.Tests.InterruptRunStmt
import TestSuite.Tests.Issues
import TestSuite.Tests.Packages
import TestSuite.Tests.Performance
import TestSuite.Tests.Pseudoterminal
import TestSuite.Tests.SessionRestart
import TestSuite.Tests.SessionState
import TestSuite.Tests.SnippetEnvironment
import TestSuite.Tests.StdIO
import TestSuite.Tests.TH
import TestSuite.Tests.TypeInformation
import TestSuite.Tests.UpdateTargets

-- | Sanity check: make sure we can communicate with the server at all
-- and that we get the expected version
testGetGhcVersion :: TestSuiteEnv -> Assertion
testGetGhcVersion env = withAvailableSession env $ \session -> do
  version <- getGhcVersion session
  assertEqual ""  (testSuiteEnvGhcVersion env) version

allTests :: String -> TestSuiteEnv -> TestTree
allTests name env = testGroup name [
    stdTest env "getGhcVersion" testGetGhcVersion
  , testGroupIntegration        env
  , testGroupAPI                env
  , testGroupSessionState       env
  , testGroupCompilation        env
  , testGroupUpdateTargets      env
  , testGroupInterruptRunStmt   env
  , testGroupInterruptRunExe    env
  , testGroupStdIO              env
  , testGroupBufferMode         env
  , testGroupSnippetEnvironment env
  , testGroupTypeInformation    env
  , testGroupAutocompletion     env
  , testGroupFFI                env
  , testGroupC                  env
  , testGroupBuildExe           env
  , testGroupBuildDoc           env
  , testGroupBuildLicenses      env
  , testGroupCabalMacros        env
  , testGroupTH                 env
  , testGroupIssues             env
  , testGroupPackages           env
  , testGroupSessionRestart     env
  , testGroupCrash              env
  , testGroupCompliance         env
  , testGroupDebugger           env
  , testGroupConcurrency        env
  , testGroupPerformance        env
  , testGroupPseudoterminal     env
  ]

main :: IO ()
main = do
    -- Yes, this is hacky, but I couldn't figure out how to easily do
    -- this with tasty's API...
    args <- getArgs
    let noGhcSpecified =
           "--test-74"  `notElem` args &&
           "--test-78"  `notElem` args &&
           "--test-710" `notElem` args
    args' <-
      if noGhcSpecified
        then do
          putStrLn "No GHC version specified (--test-74, --test-78, --test-710)."
          putStrLn "Asking local GHC for its version, and defaulting to that."
          output <- readProcess "ghc" ["--version"] ""
          putStrLn output
          let version = last (words output)
              versionCode =
                if "7.4"  `isPrefixOf` version then "74"  else
                if "7.8"  `isPrefixOf` version then "78"  else
                if "7.10" `isPrefixOf` version then "710" else
                error ("No tests for GHC version " ++ version)
          -- GHC_PACKAGE_PATH is set by stack, and needs to be cleared.
          unsetEnv "GHC_PACKAGE_PATH"
          putStrLn $ "Assuming --test-" ++ versionCode
          return (("--test-" ++ versionCode) : args)
        else return args
    withArgs args' $ defaultMainWithIngredients ings $ testSuite $ \env ->
      let TestSuiteConfig{..} = testSuiteEnvConfig env
          env74  = env { testSuiteEnvGhcVersion = GHC_7_4  }
          env78  = env { testSuiteEnvGhcVersion = GHC_7_8  }
          env710 = env { testSuiteEnvGhcVersion = GHC_7_10 }
      in testGroup "IDE backend tests" $
           (if testSuiteConfigTest74  then [allTests "GHC 7.4"  env74]  else [])
        ++ (if testSuiteConfigTest78  then [allTests "GHC 7.8"  env78]  else [])
        ++ (if testSuiteConfigTest710 then [allTests "GHC 7.10" env710] else [])
  where
    ings = includingOptions testSuiteCommandLineOptions
         : defaultIngredients
