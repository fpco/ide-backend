module TestSuite.Tests.BuildLicenses (testGroupBuildLicenses) where

import Control.Monad
import Data.Monoid
import System.Directory
import System.FilePath
import System.Exit
import Test.Tasty
import Test.HUnit

import IdeSession
import TestSuite.State
import TestSuite.Session
import TestSuite.Assertions

testGroupBuildLicenses :: TestSuiteEnv -> TestTree
testGroupBuildLicenses env = testGroup "Build licenses" [
    stdTest env "Build licenses from NamedFieldPuns (with errors)" test_NamedFieldPuns
  , stdTest env "Build licenses with wrong cabal files and fail"   test_wrongCabalFile
  ]

test_NamedFieldPuns :: TestSuiteEnv -> Assertion
test_NamedFieldPuns env = withAvailableSession' env (withOpts ["-hide-package monads-tf"]) $ \session -> do
    loadModulesFrom session "test/Puns"
    assertMoreErrors session
    cabalsPath <- canonicalizePath "test/Puns/cabals"
    let upd = buildLicenses cabalsPath
    updateSessionD session upd 99
    assertMoreErrors session
    distDir <- getDistDir session
    licensesWarns <- readFile $ distDir </> "licenses.stderr"
    assertEqual "licensesWarns length" 3 (length $ lines licensesWarns)
    status <- getBuildLicensesStatus session
    assertEqual "after license build" (Just ExitSuccess) status
    licenses <- readFile $ distDir </> "licenses.txt"
    assertBool "licenses length" $ length licenses >= 27142

test_wrongCabalFile :: TestSuiteEnv -> Assertion
test_wrongCabalFile env = withAvailableSession' env (withOpts ["-hide-package monads-tf"]) $ \session -> do
    loadModulesFrom session "test/Puns"
    assertMoreErrors session
    cabalsPath <- canonicalizePath "test/Puns/cabals/parse_error"
    let updL = buildLicenses cabalsPath
        punOpts = ["-XNamedFieldPuns", "-XRecordWildCards"]
        upd = updL <> updateDynamicOpts punOpts
    updateSessionD session upd 99
    assertNoErrors session
    status <- getBuildLicensesStatus session
    assertEqual "after license parse_error" (Just ExitSuccess) status
    distDir <- getDistDir session
    licensesErr <- readFile $ distDir </> "licenses.stderr"
    assertEqual "licensesErr length" 18 (length $ lines licensesErr)
    cabalsPath2 <- canonicalizePath "test/Puns/cabals/no_text_error"
    let upd2 = buildLicenses cabalsPath2
    updateSessionD session upd2 99
    status2 <- getBuildLicensesStatus session
    assertEqual "after license no_text_error" (Just ExitSuccess) status2
    licensesErr2 <- readFile $ distDir </> "licenses.stderr"
    assertEqual "licensesErr2 length" 18 (length $ lines licensesErr2)
