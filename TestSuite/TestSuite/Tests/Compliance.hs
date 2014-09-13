module TestSuite.Tests.Compliance (testGroupCompliance) where

import Prelude hiding (span, mod)
import Data.Monoid
import System.FilePath
import Test.HUnit
import Test.Tasty

import IdeSession
import TestSuite.State
import TestSuite.Session
import TestSuite.Assertions

testGroupCompliance :: TestSuiteEnv -> TestTree
testGroupCompliance env = testGroup "Standards compliance" [
    testGroup "NondecreasingIndentation" $ [
        stdTest env "GHC API should fail without -XNondecreasingIndentation"            test_failWithoutXNDI_GHC
      , stdTest env "both should pass with -XNondecreasingIndentation"                  test_passWithXNDI
      , stdTest env "both should pass with -XNondecreasingIndentation in SessionConfig" test_passWithXNDI_asStaticOpt
      , stdTest env "both should pass with -XHaskell98"                                 test_passWithXH98
      , stdTest env "both should pass with -XHaskell98 in SessionConfig"                test_passWithXH98_asStaticOpt
      ] ++ exeTests env [
        stdTest env "buildExe should fail without -XNondecreasingIndentation"           test_failWithoutXNDI_buildExe
      ]
  ]

test_failWithoutXNDI_GHC :: TestSuiteEnv -> Assertion
test_failWithoutXNDI_GHC env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    -- The error messages are different in 7.4 and 7.8.
    -- TODO: can use 'env' to have different assertions for 7.4 and 7.8
    assertOneError session
  where
    src = "module Main where\n\
          \main = do\n\
          \    let foo = do\n\
          \        putStrLn \"hello\"\n\
          \    foo"
    upd = updateSourceFile "src/Main.hs" src

test_failWithoutXNDI_buildExe :: TestSuiteEnv -> Assertion
test_failWithoutXNDI_buildExe env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session

    let m    = "Main"
        updE = buildExe ["-XHaskell2010"] [(m, "src/Main.hs")]
    updateSessionD session updE 1
    distDir     <- getDistDir session
    buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
    -- The error messages are different in 7.4 and 7.8.
    -- TODO: can use 'env' to have different assertions for 7.4 and 7.8
    assertEqual "buildStderr empty" False (null buildStderr)
  where
    src = "module Main where\n\
          \main = do\n\
          \    let foo = do\n\
          \        putStrLn \"hello\"\n\
          \    foo"
    upd = updateSourceFile "src/Main.hs" src
       <> updateGhcOpts ["-XHaskell98"]

test_passWithXNDI :: TestSuiteEnv -> Assertion
test_passWithXNDI env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session

    ifTestingExe env $ do
       let m    = "Main"
           updE = buildExe [] [(m, "src/Main.hs")]
       updateSessionD session updE 1
       distDir     <- getDistDir session
       buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
       assertEqual "buildStderr empty" True (null buildStderr)
  where
    src = "module Main where\n\
          \main = do\n\
          \    let foo = do\n\
          \        putStrLn \"hello\"\n\
          \    foo"
    upd = updateSourceFile "src/Main.hs" src
       <> updateGhcOpts ["-XNondecreasingIndentation"]

test_passWithXNDI_asStaticOpt :: TestSuiteEnv -> Assertion
test_passWithXNDI_asStaticOpt env = withAvailableSession' env (withGhcOpts ["-XNondecreasingIndentation"]) $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session

    ifTestingExe env $ do
       let m    = "Main"
           updE = buildExe [] [(m, "src/Main.hs")]
       updateSessionD session updE 1
       distDir     <- getDistDir session
       buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
       assertEqual "buildStderr empty" True (null buildStderr)
  where
    src = "module Main where\n\
          \main = do\n\
          \    let foo = do\n\
          \        putStrLn \"hello\"\n\
          \    foo"
    upd = updateSourceFile "src/Main.hs" src

test_passWithXH98 :: TestSuiteEnv -> Assertion
test_passWithXH98 env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session

    ifTestingExe env $ do
       let m    = "Main"
           updE = buildExe [] [(m, "src/Main.hs")]
       updateSessionD session updE 1
       distDir     <- getDistDir session
       buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
       assertEqual "buildStderr empty" True (null buildStderr)
  where
    src = "module Main where\n\
          \main = do\n\
          \    let foo = do\n\
          \        putStrLn \"hello\"\n\
          \    foo"
    upd = updateSourceFile "src/Main.hs" src
       <> updateGhcOpts ["-XHaskell98"]


test_passWithXH98_asStaticOpt :: TestSuiteEnv -> Assertion
test_passWithXH98_asStaticOpt env = withAvailableSession' env (withGhcOpts ["-XHaskell98"]) $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session

    ifTestingExe env $ do
       let m    = "Main"
           updE = buildExe [] [(m, "src/Main.hs")]
       updateSessionD session updE 2 -- Should be 1 (#189)
       distDir     <- getDistDir session
       buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
       assertEqual "buildStderr empty" True (null buildStderr)
  where
    src = "module Main where\n\
          \main = do\n\
          \    let foo = do\n\
          \        putStrLn \"hello\"\n\
          \    foo"
    upd = updateSourceFile "src/Main.hs" src
