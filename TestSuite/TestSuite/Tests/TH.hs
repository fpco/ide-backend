module TestSuite.Tests.TH (testGroupTH) where

import Data.Monoid
import Data.Version
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import Test.HUnit
import Test.Tasty
import qualified Data.ByteString.Lazy as L
import qualified Data.Text            as T

import IdeSession
import TestSuite.Assertions
import TestSuite.State
import TestSuite.Session

testGroupTH :: TestSuiteEnv -> TestTree
testGroupTH env = testGroup "TH" [
    stdTest env "Code generation on"                                       test_codeGenOn
  , stdTest env "Build executable from TH"                                 test_buildExe
  , stdTest env "Build haddocks from TH"                                   test_TH
  , stdTest env "Build .cabal from TH with a wrong libname and don't fail" test_Cabal
  ]

test_codeGenOn :: TestSuiteEnv -> Assertion
test_codeGenOn env = withAvailableSession' env (withDynOpts ["-XTemplateHaskell"]) $ \session -> do
    withCurrentDirectory "test" $ do
      (originalUpdate, lm) <- getModulesFrom session "TH"
      let update = originalUpdate <> updateCodeGeneration True
      updateSessionD session update (length lm)
    assertNoErrors session
    runActions <- runStmt session "TH.TH" "main"
    (output, result) <- runWaitAll runActions
    assertEqual "" RunOk result
    assertEqual "" output "(True,43)\n"

test_buildExe :: TestSuiteEnv -> Assertion
test_buildExe env = withAvailableSession' env (withDynOpts ["-XTemplateHaskell"]) $ \session -> do
    withCurrentDirectory "test" $ do
      (originalUpdate, lm) <- getModulesFrom session "TH"
      let update = originalUpdate <> updateCodeGeneration True
      updateSessionD session update (length lm)
    assertNoErrors session
    let m = "TH.TH"
        upd = buildExe ["-rtsopts=all", "-O0"] [(T.pack m, "TH/TH.hs")]
    updateSessionD session upd 3
    distDir <- getDistDir session
    out <- readProcess (distDir </> "build" </> m </> m)
                       ["+RTS", "-K4M", "-RTS"] []
    assertEqual "TH.TH exe output"
                "(True,43)\n"
                out
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "(True,43)\n"
                outExe
    assertEqual "after runExe" ExitSuccess statusExe

test_TH :: TestSuiteEnv -> Assertion
test_TH env = withAvailableSession' env (withDynOpts ["-XTemplateHaskell"]) $ \session -> do
    withCurrentDirectory "test" $ do
      (originalUpdate, lm) <- getModulesFrom session "TH"
      let update = originalUpdate <> updateCodeGeneration True
      updateSessionD session update (length lm)
    assertNoErrors session
    let upd = buildDoc
    updateSessionD session upd 1
    distDir <- getDistDir session
    indexExists <- doesFileExist $ distDir </> "doc/html/main/index.html"
    assertBool "TH.TH haddock files" indexExists
    hoogleExists <- doesFileExist $ distDir </> "doc/html/main/main-1.0.txt"
    assertBool "TH.TH hoogle files" hoogleExists

test_Cabal :: TestSuiteEnv -> Assertion
test_Cabal env = withAvailableSession' env (withDynOpts ["-XTemplateHaskell"]) $ \session -> do
    withCurrentDirectory "test" $ do
      (originalUpdate, lm) <- getModulesFrom session "TH"
      let update = originalUpdate <> updateCodeGeneration True
      updateSessionD session update (length lm)
    assertNoErrors session
    dotCabalFromName <- getDotCabal session
    let dotCabal = dotCabalFromName "--///fo/name" $ Version [-1, -9] []
    assertBool ".cabal not empty" $ not $ L.null dotCabal
