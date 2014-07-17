module TestSuite.Tests.BuildExe (testGroupBuildExe) where

import System.FilePath
import System.Exit
import System.Process
import Test.Tasty
import Test.HUnit
import qualified Data.Text as T

import IdeSession
import TestSuite.State
import TestSuite.Session
import TestSuite.Assertions

testGroupBuildExe :: TestSuiteEnv -> TestTree
testGroupBuildExe env = testGroup "Build executable" [
    stdTest env "From some .lhs files"                                  test_fromLhsFiles
  , stdTest env "From some .lhs files with dynamic include path change" test_fromLhsFiles_DynamicIncludePathChange
  ]

-- TODO: We should mark this session as dont-reuse (there is no point)
test_fromLhsFiles :: TestSuiteEnv -> Assertion
test_fromLhsFiles env = withAvailableSession' env (withIncludes ["test/compiler/utils"] (defaultSessionSetup env)) $ \session -> do
    loadModulesFrom session "test/compiler/utils"
    assertNoErrors session
    status0 <- getBuildExeStatus session
    assertEqual "before exe build" Nothing status0
    distDir <- getDistDir session

    let m = "Maybes"
        upd = buildExe [] [(T.pack m, m <.> "lhs")]
    updateSessionD session upd 4
    assertNoErrors session
    status1 <- getBuildExeStatus session
    assertEqual "after exe build" (Just ExitSuccess) status1
    let m2 = "Exception"
        upd2 = buildExe [] [(T.pack m2, m2 <.> "hs")]
    updateSessionD session upd2 3
    let m3 = "Main"
        upd3 = buildExe [] [(T.pack m3, "Subdir" </> m3 <.> "lhs")]
    updateSessionD session upd3 1
    out <- readProcess (distDir </> "build" </> m </> m) [] []
    assertEqual "Maybes exe output"
                "False\n"
                out
    runActions1 <- runExe session m
    (outExe1, statusExe1) <- runWaitAll runActions1
    assertEqual "Maybes exe output from runExe 1"
                "False\n"
                outExe1
    assertEqual "after runExe 1" ExitSuccess statusExe1

    out2 <- readProcess (distDir </> "build" </> m2 </> m2) [] []
    assertEqual "Exception exe output"
                ""
                out2
    runActions2 <- runExe session m2
    (outExe2, statusExe2) <- runWaitAll runActions2
    assertEqual "Maybes exe output from runExe 2"
                ""
                outExe2
    assertEqual "after runExe 2" ExitSuccess statusExe2

    out3 <- readProcess (distDir </> "build" </> m3 </> m3) [] []
    assertEqual "Main exe output"
                ""
                out3
    runActions3 <- runExe session m3
    (outExe3, statusExe3) <- runWaitAll runActions3
    assertEqual "Maybes exe output from runExe 3"
                ""
                outExe3
    assertEqual "after runExe 3" ExitSuccess statusExe3

    status4 <- getBuildExeStatus session
    assertEqual "after all exe builds" (Just ExitSuccess) status4

test_fromLhsFiles_DynamicIncludePathChange :: TestSuiteEnv -> Assertion
test_fromLhsFiles_DynamicIncludePathChange env = withAvailableSession env $ \session -> do
    loadModulesFrom session "test/compiler/utils"
    assertNoErrors session
    let m = "Maybes"
        upd0 = buildExe [] [(T.pack m, m <.> "lhs")]
    updateSessionD session upd0 0
    assertNoErrors session
    status0 <- getBuildExeStatus session
    -- Expected failure! The updateRelativeIncludes below is really needed.
    assertEqual "after exe build 1" (Just $ ExitFailure 1) status0
    updateSessionD session
                   (updateRelativeIncludes ["test/compiler/utils"])
                   4
    assertNoErrors session
    distDir <- getDistDir session

    updateSessionD session upd0 4
    status1 <- getBuildExeStatus session
    assertEqual "after exe build 2" (Just ExitSuccess) status1
    out <- readProcess (distDir </> "build" </> m </> m) [] []
    assertEqual "Maybes exe output"
                "False\n"
                out
    runActions1 <- runExe session m
    (outExe1, statusExe1) <- runWaitAll runActions1
    assertEqual "Maybes exe output from runExe 1"
                "False\n"
                outExe1
    assertEqual "after runExe 1" ExitSuccess statusExe1

    let m2 = "Exception"
        upd2 = buildExe [] [(T.pack m2, m2 <.> "hs")]
    updateSessionD session upd2 1
    out2 <- readProcess (distDir </> "build" </> m2 </> m2) [] []
    assertEqual "Exception exe output"
                ""
                out2
    runActions2 <- runExe session m2
    (outExe2, statusExe2) <- runWaitAll runActions2
    assertEqual "Maybes exe output from runExe 2"
                ""
                outExe2
    assertEqual "after runExe 2" ExitSuccess statusExe2

    let m3 = "Main"
        upd3 = buildExe [] [(T.pack m3, "Subdir" </> m3 <.> "lhs")]
    updateSessionD session upd3 1
    out3 <- readProcess (distDir </> "build" </> m3 </> m3) [] []
    assertEqual "Main exe output"
                ""
                out3
    runActions3 <- runExe session m3
    (outExe3, statusExe3) <- runWaitAll runActions3
    assertEqual "Maybes exe output from runExe 3"
                ""
                outExe3
    assertEqual "after runExe 3" ExitSuccess statusExe3

    let upd4 = buildExe [] [(T.pack m, m <.> "lhs")]
    updateSessionD session upd4 2
    status4 <- getBuildExeStatus session
    assertEqual "after all exe builds" (Just ExitSuccess) status4
