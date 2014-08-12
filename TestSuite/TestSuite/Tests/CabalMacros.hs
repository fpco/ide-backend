module TestSuite.Tests.CabalMacros (testGroupCabalMacros) where

import Control.Exception
import Data.Monoid
import System.Exit
import System.FilePath
import System.Process
import Test.HUnit
import Test.Tasty
import qualified Data.ByteString.Lazy.Char8 as L (unlines)
import qualified Data.ByteString.Lazy       as L
import qualified Data.Text                  as T

import IdeSession
import TestSuite.State
import TestSuite.Session
import TestSuite.Assertions

testGroupCabalMacros :: TestSuiteEnv -> TestTree
testGroupCabalMacros env = testGroup "Cabal macros" [
    stdTest env "Use cabal macro MIN_VERSION for a package we really depend on"       test_MinVersionForDependency
  , stdTest env "Use cabal macro MIN_VERSION for a package we don't really depend on" test_MinVersionForNonDependency
  , stdTest env "Use cabal macro VERSION by checking if defined"                      test_checkCabalMacroDefined
  , stdTest env "Use cabal macro VERSION by including an external macros file"        test_includeExternalMacros
  , stdTest env "Caching cabal macros"                                                test_caching
  ]

test_MinVersionForDependency :: TestSuiteEnv -> Assertion
test_MinVersionForDependency env = withAvailableSession' env (withGhcOpts ["-XCPP"]) $ \session -> do
    macros <- getCabalMacros session
    assertBool "Main with cabal macro exe output" (not $ L.null macros)
    -- assertEqual "Main with cabal macro exe output" (BSLC.pack "") macros
    updateSessionD session update 1
    assertNoErrors session
    let update2 = updateCodeGeneration True
    updateSessionD session update2 1
    assertNoErrors session
    runActions <- runStmt session "Main" "main"
    (output, _) <- runWaitAll runActions
    assertEqual "result of ifdefed print 5" "5\n" output
    let m = "Main"
        upd = buildExe [] [(T.pack m, "Main.hs")]
    updateSessionD session upd 2
    distDir <- getDistDir session
    mOut <- readProcess (distDir </> "build" </> m </> m) [] []
    assertEqual "Main with cabal macro exe output" "5\n" mOut
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "5\n"
                outExe
    assertEqual "after runExe" ExitSuccess statusExe
  where
    update = updateSourceFile "Main.hs" $ L.unlines
      [ "#if !MIN_VERSION_base(999,0,0)"
      , "main = print 5"
      , "#else"
      , "terrible error"
      , "#endif"
      ]

test_MinVersionForNonDependency :: TestSuiteEnv -> Assertion
test_MinVersionForNonDependency env = withAvailableSession' env (withGhcOpts ["-XCPP"]) $ \session -> do
    updateSessionD session update 1
    assertNoErrors session
    let update2 = updateCodeGeneration True
    updateSessionD session update2 1
    assertNoErrors session
    runActions <- runStmt session "Main" "main"
    (output, _) <- runWaitAll runActions
    assertEqual "result of ifdefed print 5" "5\n" output

{- FIXME
  let m = "Main"
      upd = buildExe [] [(Text.pack m, "Main.hs")]
  updateSessionD session upd 2
  assertNoErrors session
  distDir <- getDistDir session
  mOut <- readProcess (distDir </> "build" </> m </> m) [] []
  assertEqual "Main with cabal macro exe output" "5\n" mOut

  runActionsExe <- runExe session m
  (outExe, statusExe) <- runWaitAll runActionsExe
  assertEqual "Output from runExe"
              "5\n"
              outExe
  assertEqual "after runExe" ExitSuccess statusExe
 -}
  where
    update = updateSourceFile "Main.hs" $ L.unlines
      [ "#if !MIN_VERSION_containers(999,0,0)"
      , "main = print 5"
      , "#else"
      , "terrible error"
      , "#endif"
      ]

test_checkCabalMacroDefined :: TestSuiteEnv -> Assertion
test_checkCabalMacroDefined env = withAvailableSession' env (withGhcOpts ["-XCPP"]) $ \session -> do
    macros <- getCabalMacros session
    assertBool "M with cabal macro exe output" (not $ L.null macros)
    updateSessionD session (update <> updateCodeGeneration True) 1
    assertNoErrors session
    runActions <- runStmt session "M" "main"
    (output, _) <- runWaitAll runActions
    assertEqual "result of ifdefed print 5" "5\n" output
    let m = "M"
        upd = buildExe [] [(T.pack m, "M.hs")]
    updateSessionD session upd 2
    distDir <- getDistDir session
    mOut <- readProcess (distDir </> "build" </> m </> m) [] []
    assertEqual "M with cabal macro exe output" "5\n" mOut
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "5\n"
                outExe
    assertEqual "after runExe" ExitSuccess statusExe
  where
    update = updateSourceFile "M.hs" $ L.unlines
      [ "module M where"
      , "#ifdef VERSION_base"
      , ""
      , "#if defined(VERSION_base)"
      , "main = print 5"
      , "#else"
      , "terrible error"
      , "#endif"
      , ""
      , "#else"
      , "terrible error 2"
      , "#endif"
      ]

test_includeExternalMacros :: TestSuiteEnv -> Assertion
test_includeExternalMacros env = withAvailableSession' env (withGhcOpts ["-XCPP"]) $ \session -> do
    macros <- getCabalMacros session
    assertBool "M with cabal macro exe output" (not $ L.null macros)
    updateSessionD session (update <> updateCodeGeneration True) 1
    assertNoErrors session
    runActions <- runStmt session "M" "main"
    (output, _) <- runWaitAll runActions
    assertEqual "result of ifdefed print 5" "False\n" output
    let m = "M"
        upd = buildExe [] [(T.pack m, "M.hs")]
    updateSessionD session upd 2
    distDir <- getDistDir session
    mOut <- readProcess (distDir </> "build" </> m </> m) [] []
    assertEqual "M with cabal macro exe output" "False\n" mOut
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "False\n"
                outExe
    assertEqual "after runExe" ExitSuccess statusExe
  where
    update = updateSourceFile "M.hs" (L.unlines
      [ "module M where"
      , "#include \"cabal_macros.h\""
      , "main = print $ MY_VERSION_base == \"foo\""
      ])
      <> updateSourceFile "cabal_macros.h" (L.unlines
      [ "#define MY_VERSION_base \"4.5.1.0\""
      ])


test_caching :: TestSuiteEnv -> Assertion
test_caching env = do
    -- Get macros from standard session
    macros <- withSession (defaultServerConfig env) getCabalMacros

    -- Initialize new session with these macros
    do let cfg = (defaultServerConfig env) {
                     testSuiteServerCabalMacros = Just macros
                   }
       withSession cfg $ \session -> do
         let update = (updateCodeGeneration True)
                   <> (updateGhcOpts ["-XCPP"])
                   <> (updateSourceFile "Main.hs" $ L.unlines [
                           "#if !MIN_VERSION_base(999,0,0)"
                         , "main = print 5"
                         , "#else"
                         , "terrible error"
                         , "#endif"
                         ])
         updateSessionD session update 1
         assertNoErrors session
         runActions <- runStmt session "Main" "main"
         (output, _) <- runWaitAll runActions
         assertEqual "result of ifdefed print 5" "5\n" output
         let m = "Main"
             updExe = buildExe [] [(T.pack m, "Main.hs")]
         updateSessionD session updExe 2
         runActionsExe <- runExe session m
         (outExe, statusExe) <- runWaitAll runActionsExe
         assertEqual "Output from runExe"
                     "5\n"
                     outExe
         assertEqual "after runExe" ExitSuccess statusExe

    -- Test custom macros
    do let customMacros = "#define HELLO 1"
           cfg = (defaultServerConfig env) {
                     testSuiteServerCabalMacros = Just customMacros
                   }
       withSession cfg $ \session -> do
         let update = (updateCodeGeneration True)
                   <> (updateGhcOpts ["-XCPP"])
                   <> (updateSourceFile "Main.hs" $ L.unlines [
                           "#if HELLO"
                         , "main = print 6"
                         , "#else"
                         , "main = print 7"
                         , "#endif"
                         ])
         updateSessionD session update 1
         assertNoErrors session
         runActions <- runStmt session "Main" "main"
         (output, _) <- runWaitAll runActions
         assertEqual "result of ifdefed print 6" "6\n" output
         let m = "Main"
             updExe = buildExe [] [(T.pack m, "Main.hs")]
         updateSessionD session updExe 2
         runActionsExe <- runExe session m
         (outExe, statusExe) <- runWaitAll runActionsExe
         assertEqual "Output from runExe"
                     "7\n"  -- FIXME
                     outExe
         assertEqual "after runExe" ExitSuccess statusExe

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

withSession :: TestSuiteServerConfig -> (IdeSession -> IO a) -> IO a
withSession cfg = bracket (startNewSession cfg) shutdownSession
