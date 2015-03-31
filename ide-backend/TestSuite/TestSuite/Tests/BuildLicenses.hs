module TestSuite.Tests.BuildLicenses (testGroupBuildLicenses) where

import Data.Monoid
import Distribution.License (License (..))
import System.Directory
import System.Exit
import System.FilePath
import Test.HUnit
import Test.Tasty
import qualified Data.Text as T

import IdeSession
import TestSuite.State
import TestSuite.Session
import TestSuite.Assertions

testGroupBuildLicenses :: TestSuiteEnv -> TestTree
testGroupBuildLicenses env = testGroup "Build licenses" [
    stdTest env "Build licenses from NamedFieldPuns without errors"                      test_NamedFieldPunsCorrect
  , stdTest env "Build licenses from NamedFieldPuns (with errors)"                       test_NamedFieldPunsErrors
  , stdTest env "Build licenses with wrong cabal files and fail"                         test_wrongCabalFile
  , stdTest env "Build licenses from ParFib"                                             test_ParFib
  , stdTest env "Build licenses from Cabal"                                              test_Cabal
  , stdTest env "Build licenses from 1000 packages fixed in config with no license file" test_1000_noLicense
  , stdTest env "Build licenses from 1000 packages fixed in config with no useful info"  test_1000_noUsefulInfo
  , stdTest env "Build licenses from TH with a wrong cabals dir and don't fail"          test_TH
  ]

test_NamedFieldPunsCorrect :: TestSuiteEnv -> Assertion
test_NamedFieldPunsCorrect env = withAvailableSession' env (withGhcOpts ["-hide-package monads-tf"]) $ \session -> do
    let punOpts = ["-XNamedFieldPuns", "-XRecordWildCards"]
        update2 = updateGhcOpts punOpts
    updateSessionD session update2 0
    loadModulesFrom session "TestSuite/inputs/Puns"
    assertNoErrors session
    cabalsPath <- canonicalizePath "TestSuite/inputs/Puns/cabals"
    let upd = buildLicenses cabalsPath
    updateSessionD session upd 99
    assertNoErrors session
    distDir <- getDistDir session
    licensesWarns <- readFile $ distDir </> "licenses.stderr"
    assertEqual "licensesWarns length" 3 (length $ lines licensesWarns)
    status <- getBuildLicensesStatus session
    assertEqual "after license build" (Just ExitSuccess) status
    licenses <- readFile $ distDir </> "licenses.txt"
    assertBool ("licenses length (" ++ show (length licenses) ++ ")") $ length licenses >= 25000

test_NamedFieldPunsErrors :: TestSuiteEnv -> Assertion
test_NamedFieldPunsErrors env = withAvailableSession' env (withGhcOpts ["-hide-package monads-tf"]) $ \session -> do
    loadModulesFrom session "TestSuite/inputs/Puns"
    assertMoreErrors session
    cabalsPath <- canonicalizePath "TestSuite/inputs/Puns/cabals"
    let upd = buildLicenses cabalsPath
    updateSessionD session upd 99
    assertMoreErrors session
    distDir <- getDistDir session
    licensesWarns <- readFile $ distDir </> "licenses.stderr"
    assertEqual "licensesError length" 1 (length $ lines licensesWarns)
    status <- getBuildLicensesStatus session
    assertEqual "after license build" (Just $ ExitFailure 1) status

test_wrongCabalFile :: TestSuiteEnv -> Assertion
test_wrongCabalFile env = withAvailableSession' env (withGhcOpts ["-hide-package monads-tf"]) $ \session -> do
    loadModulesFrom session "TestSuite/inputs/Puns"
    assertMoreErrors session
    cabalsPath <- canonicalizePath "TestSuite/inputs/Puns/cabals/parse_error"
    let updL = buildLicenses cabalsPath
        punOpts = ["-XNamedFieldPuns", "-XRecordWildCards"]
        upd = updL <> updateGhcOpts punOpts
    updateSessionD session upd 99
    assertNoErrors session
    status <- getBuildLicensesStatus session
    assertEqual "after license parse_error" (Just ExitSuccess) status
    distDir <- getDistDir session
    licensesErr <- readFile $ distDir </> "licenses.stderr"
    assertEqual "licensesErr length" 18 (length $ lines licensesErr)
    cabalsPath2 <- canonicalizePath "TestSuite/inputs/Puns/cabals/no_text_error"
    let upd2 = buildLicenses cabalsPath2
    updateSessionD session upd2 99
    status2 <- getBuildLicensesStatus session
    assertEqual "after license no_text_error" (Just ExitSuccess) status2
    licensesErr2 <- readFile $ distDir </> "licenses.stderr"
    assertEqual "licensesErr2 length" 18 (length $ lines licensesErr2)

test_ParFib :: TestSuiteEnv -> Assertion
test_ParFib env = withAvailableSession env $ \session -> do
    withCurrentDirectory "TestSuite/inputs/MainModule" $ do
      loadModulesFrom session "."
      assertNoErrors session
    cabalsPath <- canonicalizePath "TestSuite/inputs/MainModule/cabals"
    let upd = buildLicenses cabalsPath
    updateSessionD session upd 6
    distDir <- getDistDir session
    licensesErrs <- readFile $ distDir </> "licenses.stderr"
    assertEqual "licensesErrs" "" licensesErrs
    status <- getBuildLicensesStatus session
    assertEqual "after license build" (Just ExitSuccess) status
    licenses <- readFile $ distDir </> "licenses.txt"
    assertBool ("licenses length (" ++ show (length licenses) ++ ")") $ length licenses >= 14165

test_Cabal :: TestSuiteEnv -> Assertion
test_Cabal env = withAvailableSession env $ \session -> do
    withCurrentDirectory (testInputPathCabal env) $ do
      loadModulesFrom session "."
      assertNoErrors session
    cabalsPath <- canonicalizePath "TestSuite/inputs/Puns/cabals"  -- 7 packages missing
    let upd = buildLicenses cabalsPath
    updateSessionD session upd 99
    distDir <- getDistDir session
    licensesErrs <- readFile $ distDir </> "licenses.stderr"
    assertBool "licensesErrs length" $ length (lines licensesErrs) < 10
    status <- getBuildLicensesStatus session
    assertEqual "after license build" (Just ExitSuccess) status
    licenses <- readFile $ distDir </> "licenses.txt"
    assertBool "licenses length" $ length licenses >= 21423

test_1000_noLicense :: TestSuiteEnv -> Assertion
test_1000_noLicense env = withAvailableSession env $ \session -> do
    distDir <- getDistDir session
    ideConfig <- getSessionConfig session
    let liStdoutLog = distDir </> "licenses.stdout"
        liStderrLog = distDir </> "licenses.stderr"
        liArgs      =
          LicenseArgs{ liPackageDBStack = configPackageDBStack ideConfig
                     , liExtraPathDirs  = configExtraPathDirs ideConfig
                     , liLicenseExc     = configLicenseExc ideConfig
                     , liDistDir        = distDir
                     , liStdoutLog
                     , liStderrLog
                     , licenseFixed     = lics
                     , liCabalsDir      = "test"
                     , liPkgs           = pkgs
                     }
    status <- buildLicsFromPkgs False liArgs
    assertEqual "after license build" ExitSuccess status
    licenses <- readFile $ distDir </> "licenses.txt"
    assertBool "licenses length" $ length licenses >= 1527726
  where
    licenseFixedConfig :: Int -> [( String
                                  , (Maybe License, Maybe FilePath, Maybe String)
                                  )]
    licenseFixedConfig 0 = []
    licenseFixedConfig n = ("p" ++ show n, (Just BSD3, Nothing, Nothing))
                         : licenseFixedConfig (n - 1)

    lics = licenseFixedConfig 1000
    pkgs = map (\(name, _) ->
                 PackageId{ packageName    = T.pack name
                          , packageVersion = Just "1.0"
                          , packageKey     = T.pack name -- ?? TODO
                          }
               ) lics

test_1000_noUsefulInfo :: TestSuiteEnv -> Assertion
test_1000_noUsefulInfo env = withAvailableSession env $ \session -> do
    distDir <- getDistDir session
    ideConfig <- getSessionConfig session
    let liStdoutLog = distDir </> "licenses.stdout"
        liStderrLog = distDir </> "licenses.stderr"
        liArgs      =
          LicenseArgs{ liPackageDBStack = configPackageDBStack ideConfig
                     , liExtraPathDirs  = configExtraPathDirs ideConfig
                     , liLicenseExc     = configLicenseExc ideConfig
                     , liDistDir        = distDir
                     , liStdoutLog
                     , liStderrLog
                     , licenseFixed     = lics
                     , liCabalsDir      = "test"
                     , liPkgs           = pkgs
                     }
    status <- buildLicsFromPkgs False liArgs
    assertEqual "after license build" ExitSuccess status
    licenses <- readFile $ distDir </> "licenses.txt"
    assertBool "licenses length" $ length licenses >= 63619
  where
    licenseFixedConfig :: Int -> [( String
                                  , (Maybe License, Maybe FilePath, Maybe String)
                                  )]
    licenseFixedConfig 0 = []
    licenseFixedConfig n = ("p" ++ show n, (Just BSD3, Just "TestSuite/inputs/BSD_TEST", Nothing))
                         : licenseFixedConfig (n - 1)

    lics = licenseFixedConfig 1000
    pkgs = map (\(name, _) ->
                 PackageId{ packageName    = T.pack name
                          , packageVersion = Just "1.0"
                          , packageKey     = T.pack name -- ?? TODO
                          }
               ) lics

test_TH :: TestSuiteEnv -> Assertion
test_TH env = withAvailableSession' env (withGhcOpts ["-XTemplateHaskell"]) $ \session -> do
    withCurrentDirectory "TestSuite/inputs" $ do
      (originalUpdate, lm) <- getModulesFrom "TH"
      let update = originalUpdate <> updateCodeGeneration True
      updateSessionD session update (length lm)
    assertNoErrors session
    let upd = buildLicenses "--/fooo /fooo/foo"
    updateSessionD session upd 99
    distDir <- getDistDir session
    licensesErrs <- readFile $ distDir </> "licenses.stderr"
    assertBool "licensesErrs length" $ length licensesErrs > 0
    status <- getBuildLicensesStatus session
    assertEqual "after license build" (Just ExitSuccess) status
