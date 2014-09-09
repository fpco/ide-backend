module TestSuite.Tests.BuildDoc (testGroupBuildDoc) where

import Control.Monad
import System.Directory
import System.Exit
import System.FilePath
import Test.Tasty
import Test.HUnit

import IdeSession
import TestSuite.State
import TestSuite.Session
import TestSuite.Assertions

testGroupBuildDoc :: TestSuiteEnv -> TestTree
testGroupBuildDoc env = testGroup "Build haddocks" [
    stdTest env "From some .lhs with relativeIncludes"
                                             (test_fromLhsFiles True)
  , stdTest env "From some .lhs files"       (test_fromLhsFiles False)
  , stdTest env "Fail"                       test_fail
  , stdTest env "From ParFib with relativeIncludes"
                                             (test_ParFib True)
  , stdTest env "From ParFib files"          (test_ParFib False)
  ]

test_fromLhsFiles :: Bool -> TestSuiteEnv -> Assertion
test_fromLhsFiles relativeIncludes env = withAvailableSession env
                                         $ \session -> do
    when relativeIncludes $
      updateSessionD session (updateRelativeIncludes ["", "Subdir"]) 0
    status0 <- getBuildDocStatus session
    when relativeIncludes $ assertEqual "before module loading" Nothing status0
    withCurrentDirectory "test/compiler/utils" $ loadModulesFrom session "."
    assertNoErrors session
    let upd = buildDoc
    updateSessionD session upd 1
    distDir <- getDistDir session
    docStderr <- readFile $ distDir </> "doc/ide-backend-doc.stderr"
    assertEqual "doc stderr empty" "" docStderr
    status1 <- getBuildDocStatus session
    assertEqual "after doc build" (Just ExitSuccess) status1
    indexExists <- doesFileExist $ distDir </> "doc/html/main/index.html"
    assertBool ".lhs haddock files" indexExists
    hoogleExists <- doesFileExist $ distDir </> "doc/html/main/main-1.0.txt"
    assertBool ".lhs hoogle files" hoogleExists
    mainExists <- doesFileExist $ distDir </> "doc/html/main/Main.html"
    when relativeIncludes $ assertBool ".lhs Main haddock file" mainExists

test_fail :: TestSuiteEnv -> Assertion
test_fail env = withAvailableSession env $ \session -> do
    withCurrentDirectory "test/ABerror" $ loadModulesFrom session "."
    assertOneError session
    let upd = buildDoc
    -- Note that the stderr log file here is empty, but exit code is 1:
    updateSessionD session upd 1
    status1 <- getBuildDocStatus session
    assertEqual "failure after doc build" (Just $ ExitFailure 1) status1

test_ParFib :: Bool -> TestSuiteEnv -> Assertion
test_ParFib relativeIncludes env = withAvailableSession env $ \session -> do
    -- Warning: this also results in @compiler/@ written to the top-level
    -- session directory. Unless it breaks other tests, let's keep it,
    -- so that we see what happens with strange @updateRelativeIncludes@
    -- arguments and notice if other changes to code cause any larger breakage.
    when relativeIncludes $
      updateSessionD session
                     (updateRelativeIncludes ["", "../compiler/utils/Subdir"])
                     0
    withCurrentDirectory "test/MainModule" $ loadModulesFrom session "."
    when relativeIncludes $
      withCurrentDirectory "test/MainModule" $
        loadModulesFrom session "../compiler/utils/Subdir"
    assertNoErrors session
    let upd = buildDoc
    updateSessionD session upd 1
    distDir <- getDistDir session
    docStderr <- readFile $ distDir </> "doc/ide-backend-doc.stderr"
    assertEqual "doc stderr empty" "" docStderr
    status1 <- getBuildDocStatus session
    assertEqual "after doc build" (Just ExitSuccess) status1
    indexExists <- doesFileExist $ distDir </> "doc/html/main/index.html"
    assertBool "ParFib haddock files" indexExists
    hoogleExists <- doesFileExist $ distDir </> "doc/html/main/main-1.0.txt"
    assertBool "ParFib hoogle files" hoogleExists
    mainExists <- doesFileExist $ distDir </> "doc/html/main/Main.html"
    when relativeIncludes $ assertBool ".lhs Main haddock file" mainExists
