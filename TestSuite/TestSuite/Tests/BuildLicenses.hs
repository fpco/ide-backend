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
    stdTest env "Build licenses from NamedFieldPuns (with errors)"                       test_NamedFieldPuns
  , stdTest env "Build licenses with wrong cabal files and fail"                         test_wrongCabalFile
  , stdTest env "Build licenses from ParFib"                                             test_ParFib
  , stdTest env "Build licenses from Cabal"                                              test_Cabal
  , stdTest env "Build licenses from 1000 packages fixed in config with no license file" test_1000_noLicense
  , stdTest env "Build licenses from 1000 packages fixed in config with no useful info"  test_1000_noUsefulInfo
  , stdTest env "Build licenses from TH with a wrong cabals dir and don't fail"          test_TH
  ]

test_NamedFieldPuns :: TestSuiteEnv -> Assertion
test_NamedFieldPuns env = withAvailableSession' env (withDynOpts ["-hide-package monads-tf"]) $ \session -> do
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
test_wrongCabalFile env = withAvailableSession' env (withDynOpts ["-hide-package monads-tf"]) $ \session -> do
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

test_ParFib :: TestSuiteEnv -> Assertion
test_ParFib env = withAvailableSession env $ \session -> do
    withCurrentDirectory "test/MainModule" $ do
      loadModulesFrom session "."
      assertNoErrors session
    cabalsPath <- canonicalizePath "test/MainModule/cabals"
    let upd = buildLicenses cabalsPath
    updateSessionD session upd 6
    distDir <- getDistDir session
    licensesErrs <- readFile $ distDir </> "licenses.stderr"
    assertEqual "licensesErrs" "" licensesErrs
    status <- getBuildLicensesStatus session
    assertEqual "after license build" (Just ExitSuccess) status
    licenses <- readFile $ distDir </> "licenses.txt"
    assertBool "licenses length" $ length licenses >= 21409

test_Cabal :: TestSuiteEnv -> Assertion
test_Cabal env = withAvailableSession env $ \session -> do
    withCurrentDirectory "test/Cabal" $ do
      loadModulesFrom session "."
      assertNoErrors session
    cabalsPath <- canonicalizePath "test/Puns/cabals"  -- 7 packages missing
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
    licenseFixedConfig n = ("p" ++ show n, (Just BSD3, Just "test/BSD_TEST", Nothing))
                         : licenseFixedConfig (n - 1)

    lics = licenseFixedConfig 1000
    pkgs = map (\(name, _) ->
                 PackageId{ packageName    = T.pack name
                          , packageVersion = Just "1.0"
                          }
               ) lics

test_TH :: TestSuiteEnv -> Assertion
test_TH env = withAvailableSession' env (withDynOpts ["-XTemplateHaskell"]) $ \session -> do
    withCurrentDirectory "test" $ do
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
