module TestSuite.Tests.UpdateTargets (testGroupUpdateTargets) where

import Data.Monoid
import System.Exit
import System.FilePath
import System.Process
import Test.Tasty
import Test.HUnit
import qualified Data.ByteString.Lazy.UTF8 as L
import qualified Data.Text                 as T

import IdeSession
import TestSuite.State
import TestSuite.Session
import TestSuite.Assertions

testGroupUpdateTargets :: TestSuiteEnv -> TestTree
testGroupUpdateTargets env = testGroup "Update targets" [
    stdTest env "[{} < A, {A} < B, {A} < C], require [A]"                                             test_1
  , stdTest env "[{} < A, {A} < B, {A} < C], require [A], error in B"                                 test_2
  , stdTest env "[{} < A, {A} < B, {A} < C], require [B]"                                             test_3
  , stdTest env "[{} < A, {A} < B, {A} < C], require [B], error in C"                                 test_4
  , stdTest env "[{} < A, {A} < B, {A} < C], require [A], error in A"                                 test_5
  , stdTest env "[{} < A, {A} < B, {A} < C], require [B], error in A"                                 test_6
  , stdTest env "[{} < A, {A} < B, {A} < C], require [B], error in B"                                 test_7
  , stdTest env "[{} < A, {A} < B, {A} < C], require [B, C]"                                          test_8
  , stdTest env "[{} < A, {A} < B, {A} < C], require [B, C], then [B]"                                test_9
  , stdTest env "[{} < A, {A} < B, {A} < C], require [B, C], then [B] with modified B"                test_10
  , stdTest env "[{} < A, {A} < B, {A} < C], require [B, C], then [B] with modified B and error in C" test_11
  , stdTest env "[{} < A, {A} < B, {A} < C], require [B, C], then [B] with error in C"                test_12
  , stdTest env "Switch from one to another relative include path for the same module name with TargetsInclude"        test_Switch_TargetsInclude
  , stdTest env "Switch from one to another relative include path for the same module name with TargetsExclude"        test_Switch_TargetsExclude
  , stdTest env "Switch from one to another relative include path with TargetsInclude and the main module not in path" test_Switch_TargetsInclude_MainNotInPath
  ]

test_1 :: TestSuiteEnv -> Assertion
test_1 env = withAvailableSession env $ \session -> do
    updateSessionD session (mconcat [
        modAn "0"
      , modBn "0"
      , modCn "0"
      , updateTargets (TargetsInclude ["A.hs"])
      ]) 1
    assertNoErrors session
    assertLoadedModules session "" ["A"]

    buildExeTargetHsSucceeds session "A"

test_2 :: TestSuiteEnv -> Assertion
test_2 env = withAvailableSession env $ \session -> do
    updateSessionD session (mconcat [
        modAn "0"
      , modBn "invalid"
      , modCn "0"
      , updateTargets (TargetsInclude ["A.hs"])
      ]) 1
    assertNoErrors session
    assertLoadedModules session "" ["A"]

    buildExeTargetHsSucceeds session "A"

test_3 :: TestSuiteEnv -> Assertion
test_3 env = withAvailableSession env $ \session -> do
    updateSessionD session (mconcat [
        modAn "0"
      , modBn "0"
      , modCn "0"
      , updateTargets (TargetsInclude ["B.hs"])
      ]) 2
    assertNoErrors session
    assertLoadedModules session "" ["A", "B"]

    buildExeTargetHsSucceeds session "B"

test_4 :: TestSuiteEnv -> Assertion
test_4 env = withAvailableSession env $ \session -> do
    updateSessionD session (mconcat [
        modAn "0"
      , modBn "0"
      , modCn "invalid"
      , updateTargets (TargetsInclude ["B.hs"])
      ]) 2
    assertNoErrors session
    assertLoadedModules session "" ["A", "B"]

    buildExeTargetHsSucceeds session "B"
    buildExeTargetHsFails session "C"

test_5 :: TestSuiteEnv -> Assertion
test_5 env = withAvailableSession env $ \session -> do
    updateSessionD session (mconcat [
        modAn "invalid"
      , modBn "0"
      , modCn "0"
      , updateTargets (TargetsInclude ["A.hs"])
      ]) 1
    assertOneError session
    assertLoadedModules session "" []

    buildExeTargetHsFails session "A"

test_6 :: TestSuiteEnv -> Assertion
test_6 env = withAvailableSession env $ \session -> do
    updateSessionD session (mconcat [
        modAn "invalid"
      , modBn "0"
      , modCn "0"
      , updateTargets (TargetsInclude ["B.hs"])
      ]) 2
    assertOneError session
    assertLoadedModules session "" []

    buildExeTargetHsFails session "B"

test_7 :: TestSuiteEnv -> Assertion
test_7 env = withAvailableSession env $ \session -> do
    updateSessionD session (mconcat [
        modAn "0"
      , modBn "invalid"
      , modCn "0"
      , updateTargets (TargetsInclude ["B.hs"])
      ]) 2
    assertOneError session
    assertLoadedModules session "" ["A"]

    buildExeTargetHsFails session "B"
    -- Only fails due to
    -- "Source errors encountered. Not attempting to build executables."
    -- buildExeTargetHsSucceeds session "A"

test_8 :: TestSuiteEnv -> Assertion
test_8 env = withAvailableSession env $ \session -> do
    updateSessionD session (mconcat [
        modAn "0"
      , modBn "0"
      , modCn "0"
      , updateTargets (TargetsInclude ["B.hs", "C.hs"])
      ]) 3
    assertNoErrors session
    assertLoadedModules session "" ["A", "B", "C"]
    autocomplete <- getAutocompletion session
    assertEqual "we have autocompletion info for C" 2 $
      length (autocomplete "C" "sp") -- span, split

    buildExeTargetHsSucceeds session "B"
    buildExeTargetHsSucceeds session "C"

test_9 :: TestSuiteEnv -> Assertion
test_9 env = withAvailableSession env $ \session -> do
    updateSessionD session (mconcat [
        modAn "0"
      , modBn "0"
      , modCn "0"
      , updateTargets (TargetsInclude ["B.hs", "C.hs"])
      ]) 3
    assertNoErrors session
    assertLoadedModules session "" ["A", "B", "C"]
    do autocomplete <- getAutocompletion session
       assertEqual "we have autocompletion info for C" 2 $
         length (autocomplete "C" "sp") -- span, split

    buildExeTargetHsSucceeds session "C"

    updateSessionD session (mconcat [
        updateTargets (TargetsInclude ["B.hs"])
      ]) 2
    assertLoadedModules session "" ["A", "B"]
    do autocomplete <- getAutocompletion session
       assertEqual "we no longer have autocompletion info for C" 0 $
         length (autocomplete "C" "sp") -- span, split

    buildExeTargetHsSucceeds session "B"

test_10 :: TestSuiteEnv -> Assertion
test_10 env = withAvailableSession env $ \session -> do
    updateSessionD session (mconcat [
        modAn "0"
      , modBn "0"
      , modCn "0"
      , updateTargets (TargetsInclude ["B.hs", "C.hs"])
      ]) 3
    assertNoErrors session
    assertLoadedModules session "" ["A", "B", "C"]
    do autocomplete <- getAutocompletion session
       assertEqual "we have autocompletion info for C" 2 $
         length (autocomplete "C" "sp") -- span, split

    buildExeTargetHsSucceeds session "C"

    updateSessionD session (mconcat [
        modBn "1"
      , updateTargets (TargetsInclude ["B.hs"])
      ]) 2
    assertLoadedModules session "" ["A", "B"]
    do autocomplete <- getAutocompletion session
       assertEqual "autocompletion info for C cleared" 0 $
         length (autocomplete "C" "sp")

    buildExeTargetHsSucceeds session "B"

test_11 :: TestSuiteEnv -> Assertion
test_11 env = withAvailableSession env $ \session -> do
    updateSessionD session (mconcat [
        modAn "0"
      , modBn "0"
      , modCn "0"
      , updateTargets (TargetsInclude ["B.hs", "C.hs"])
      ]) 3
    assertNoErrors session
    assertLoadedModules session "" ["A", "B", "C"]
    do autocomplete <- getAutocompletion session
       assertEqual "we have autocompletion info for C" 2 $
         length (autocomplete "C" "sp") -- span, split

    updateSessionD session (mconcat [
        modBn "1"
      , modCn "invalid"
      , updateTargets (TargetsInclude ["B.hs"])
      ]) 2
    assertLoadedModules session "" ["A", "B"]
    do autocomplete <- getAutocompletion session
       assertEqual "autocompletion info for C cleared" 0 $
         length (autocomplete "C" "sp")

    buildExeTargetHsFails session "C"
    buildExeTargetHsSucceeds session "B"

test_12 :: TestSuiteEnv -> Assertion
test_12 env = withAvailableSession env $ \session -> do
    updateSessionD session (mconcat [
        modAn "0"
      , modBn "0"
      , modCn "0"
      , updateTargets (TargetsInclude ["B.hs", "C.hs"])
      ]) 3
    assertNoErrors session
    assertLoadedModules session "" ["A", "B", "C"]
    do autocomplete <- getAutocompletion session
       assertEqual "we have autocompletion info for C" 2 $
         length (autocomplete "C" "sp") -- span, split

    updateSessionD session (mconcat [
        modCn "invalid"
      , updateTargets (TargetsInclude ["B.hs"])
      ]) 2
    assertLoadedModules session "" ["A", "B"]
    do autocomplete <- getAutocompletion session
       assertEqual "autocompletion info for C cleared" 0 $
         length (autocomplete "C" "sp")

    buildExeTargetHsFails session "C"
    buildExeTargetHsSucceeds session "B"

test_Switch_TargetsInclude :: TestSuiteEnv -> Assertion
test_Switch_TargetsInclude env = withAvailableSession env $ \session -> do
    loadModulesFrom' session "test/AnotherA" (TargetsInclude ["test/AnotherA/A.hs"])
    assertOneError session
    updateSessionD session (updateCodeGeneration True) 0
    updateSessionD session
                   (updateSourceFileFromFile "test/ABnoError/B.hs")
                   0
    assertOneError session
    updateSessionD session
                   (updateSourceFileFromFile "test/AnotherB/B.hs")
                   0
    assertOneError session

    updateSessionD session
                   (updateRelativeIncludes ["", "test/AnotherA", "test/ABnoError"])
                   2  -- note the recompilation
    assertNoErrors session

    runActions <- runStmt session "Main" "main"
    (output, _) <- runWaitAll runActions
    assertEqual "output" "\"running 'A depends on B, no errors' from test/ABnoError\"\n" output

    distDir <- getDistDir session
    let m = "Main"
        updE = buildExe [] [(T.pack m, "test/AnotherA/A.hs")]
    updateSessionD session updE 3
    status <- getBuildExeStatus session
    assertEqual "after exe build" (Just ExitSuccess) status
    (stExc, out, _) <-
       readProcessWithExitCode (distDir </> "build" </> m </> m) [] []
    assertEqual "A throws exception" (ExitFailure 1) stExc
    assertEqual "exe output with old include path"
                "\"running 'A depends on B, no errors' from test/ABnoError\"\n"
                out
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "\"running 'A depends on B, no errors' from test/ABnoError\"\nMain: A.hs throws exception\n"
                outExe
    assertEqual "after runExe" (ExitFailure 1) statusExe

    let updE2 = buildExe [] [(T.pack m, "A.hs")]
    updateSessionD session updE2 0
    status2 <- getBuildExeStatus session
    assertEqual "after exe build2" (Just ExitSuccess) status2
    (stExc2, out2, _) <-
      readProcessWithExitCode (distDir </> "build" </> m </> m) [] []
    assertEqual "A throws exception" (ExitFailure 1) stExc2
    assertEqual "exe output with old include path"
                "\"running 'A depends on B, no errors' from test/ABnoError\"\n"
                out2
    runActionsExe2 <- runExe session m
    (outExe2, statusExe2) <- runWaitAll runActionsExe2
    assertEqual "Output from runExe 2"
                "\"running 'A depends on B, no errors' from test/ABnoError\"\nMain: A.hs throws exception\n"
                outExe2
    assertEqual "after runExe" (ExitFailure 1) statusExe2

    updateSessionD session
                   (updateRelativeIncludes ["test/AnotherA", "test/AnotherB"])
                   2
    assertNoErrors session

    runActions3 <- runStmt session "Main" "main"
    (output3, _) <- runWaitAll runActions3
    assertEqual "output3" "\"running A with another B\"\n" output3

    updateSessionD session
                   (updateSourceFileDelete "test/ABnoError/B.hs")
                   0  -- already recompiled above
    assertNoErrors session

    runActions35 <- runStmt session "Main" "main"
    (output35, _) <- runWaitAll runActions35
    assertEqual "output35" "\"running A with another B\"\n" output35

    -- And this one works OK even without updateSourceFileDelete
    -- and even without session restart.
    let updE3 = buildExe [] [(T.pack m, "test/AnotherA/A.hs")]
    updateSessionD session updE3 1
    status3 <- getBuildExeStatus session
    -- Path "" no longer in include paths here!
    assertEqual "after exe build3" (Just $ ExitFailure 1) status3

    let updE4 = buildExe [] [(T.pack m, "A.hs")]
    updateSessionD session updE4 2
    status4 <- getBuildExeStatus session
    assertEqual "after exe build4" (Just ExitSuccess) status4
    (stExc4, out4, _) <-
      readProcessWithExitCode (distDir </> "build" </> m </> m) [] []
    assertEqual "A throws exception" (ExitFailure 1) stExc4
    assertEqual "exe output with new include path"
                "\"running A with another B\"\n"
                out4
    runActionsExe4 <- runExe session m
    (outExe4, statusExe4) <- runWaitAll runActionsExe4
    assertEqual "Output from runExe 4"
                "\"running A with another B\"\nMain: A.hs throws exception\n"
                outExe4
    assertEqual "after runExe" (ExitFailure 1) statusExe4

test_Switch_TargetsExclude :: TestSuiteEnv -> Assertion
test_Switch_TargetsExclude env = withAvailableSession env $ \session -> do
    loadModulesFrom' session "test/AnotherA" (TargetsExclude [])
    assertOneError session

    updateSessionD session (updateCodeGeneration True) 0
    updateSessionD session
                   (updateSourceFileFromFile "test/ABnoError/B.hs")
                   2
    assertNoErrors session
    updateSessionD session
                   (updateRelativeIncludes ["", "test/AnotherA", "test/ABnoError"])
                   2  -- with TargetsExclude [], this is superfluous
    assertNoErrors session

    runActions <- runStmt session "Main" "main"
    (output, _) <- runWaitAll runActions
    assertEqual "output" "\"running 'A depends on B, no errors' from test/ABnoError\"\n" output

    distDir <- getDistDir session
    let m = "Main"
        updE = buildExe [] [(T.pack m, "test/AnotherA/A.hs")]
    updateSessionD session updE 3
    status <- getBuildExeStatus session
    assertEqual "after exe build" (Just ExitSuccess) status
    (stExc, out, _) <-
       readProcessWithExitCode (distDir </> "build" </> m </> m) [] []
    assertEqual "A throws exception" (ExitFailure 1) stExc
    assertEqual "exe output with old include path"
                "\"running 'A depends on B, no errors' from test/ABnoError\"\n"
                out
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "\"running 'A depends on B, no errors' from test/ABnoError\"\nMain: A.hs throws exception\n"
                outExe
    assertEqual "after runExe" (ExitFailure 1) statusExe


    let updE2 = buildExe [] [(T.pack m, "A.hs")]
    updateSessionD session updE2 0
    status2 <- getBuildExeStatus session
    assertEqual "after exe build2" (Just ExitSuccess) status2
    (stExc2, out2, _) <-
      readProcessWithExitCode (distDir </> "build" </> m </> m) [] []
    assertEqual "A throws exception" (ExitFailure 1) stExc2
    assertEqual "exe output with old include path"
                "\"running 'A depends on B, no errors' from test/ABnoError\"\n"
                out2
    runActionsExe2 <- runExe session m
    (outExe2, statusExe2) <- runWaitAll runActionsExe2
    assertEqual "Output from runExe 2"
                "\"running 'A depends on B, no errors' from test/ABnoError\"\nMain: A.hs throws exception\n"
                outExe2
    assertEqual "after runExe" (ExitFailure 1) statusExe2

    updateSessionD session
                   (updateSourceFileDelete "test/ABnoError/B.hs")
                   0
    assertOneError session
    updateSessionD session
                   (updateSourceFileFromFile "test/AnotherB/B.hs")
                   2
    assertNoErrors session

    updateSessionD session
                   (updateTargets (TargetsInclude ["test/AnotherA/A.hs"]))
                   0
    assertOneError session

    updateSessionD session
                   (updateRelativeIncludes ["test/AnotherA", "test/AnotherB"])
                   2  -- with TargetsExclude, this would be superfluous
    assertNoErrors session  -- fixed the error from above
    updateSessionD session
                   (updateTargets  (TargetsExclude []))
                   2  -- recompilation due to session restart only
    assertNoErrors session

    runActions3 <- runStmt session "Main" "main"
    (output3, _) <- runWaitAll runActions3
    assertEqual "output3" "\"running A with another B\"\n" output3

    let updE3 = buildExe [] [(T.pack m, "test/AnotherA/A.hs")]
    updateSessionD session updE3 1
    status3 <- getBuildExeStatus session
    -- Path "" no longer in include paths here!
    assertEqual "after exe build3" (Just $ ExitFailure 1) status3

    let updE4 = buildExe [] [(T.pack m, "A.hs")]
    updateSessionD session updE4 2
    status4 <- getBuildExeStatus session
    assertEqual "after exe build4" (Just ExitSuccess) status4
    (stExc4, out4, _) <-
      readProcessWithExitCode (distDir </> "build" </> m </> m) [] []
    assertEqual "A throws exception" (ExitFailure 1) stExc4
    assertEqual "exe output with new include path"
                "\"running A with another B\"\n"
                out4
    runActionsExe4 <- runExe session m
    (outExe4, statusExe4) <- runWaitAll runActionsExe4
    assertEqual "Output from runExe 4"
                "\"running A with another B\"\nMain: A.hs throws exception\n"
                outExe4
    assertEqual "after runExe" (ExitFailure 1) statusExe4

test_Switch_TargetsInclude_MainNotInPath :: TestSuiteEnv -> Assertion
test_Switch_TargetsInclude_MainNotInPath env = withAvailableSession env $ \session -> do
    -- Since we set the target explicitly, ghc will need to be able to find
    -- the other module (B) on its own; that means it will need an include
    -- path to <ideSourcesDir>/test/ABnoError
    loadModulesFrom' session "test/ABnoError" (TargetsInclude ["test/ABnoError/A.hs"])
    assertOneError session
    updateSessionD session
                   (updateSourceFileFromFile "test/AnotherB/B.hs")
                   0
    assertOneError session

    updateSessionD session (updateCodeGeneration True) 0
    updateSessionD session
                   (updateRelativeIncludes ["test/ABnoError"])
                   2  -- note the recompilation
    assertNoErrors session

    runActions <- runStmt session "Main" "main"
    (output, _) <- runWaitAll runActions
    assertEqual "output" "\"running 'A depends on B, no errors' from test/ABnoError\"\n" output

    updateSessionD session
                   (updateRelativeIncludes ["test/AnotherB"])  -- A not in path
                   2
    assertNoErrors session

    runActions3 <- runStmt session "Main" "main"
    (output3, _) <- runWaitAll runActions3
    assertEqual "output3" "\"running A with another B\"\n" output3

    updateSessionD session
                   (updateSourceFileDelete "test/ABnoError/B.hs")
                   0  -- already recompiled above
    assertNoErrors session

    runActions35 <- runStmt session "Main" "main"
    (output35, _) <- runWaitAll runActions35
    assertEqual "output35" "\"running A with another B\"\n" output35

    distDir <- getDistDir session
    let m = "Main"

    let updE4 = buildExe [] [(T.pack m, "A.hs")]
    updateSessionD session updE4 1
    status4 <- getBuildExeStatus session
    assertEqual "after exe build4" (Just $ ExitFailure 1) status4
      -- Failure due to no A in path.

    updateSessionD session
                   (updateRelativeIncludes ["", "test/AnotherB"])  -- A still not in path
                   2
    assertNoErrors session

    -- buildExe with full paths works though, if the includes have ""
    let updE41 = buildExe [] [(T.pack m, "test/ABnoError/A.hs")]
    updateSessionD session updE41 2
    status41 <- getBuildExeStatus session
    assertEqual "after exe build41" (Just ExitSuccess) status41
    (stExc41, out41, _) <-
      readProcessWithExitCode (distDir </> "build" </> m </> m) [] []
    assertEqual "A throws exception" (ExitFailure 1) stExc41
    assertEqual "exe output with new include path"
                "\"running A with another B\"\n"
                out41
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe 41"
                "\"running A with another B\"\nMain: A.hs throws exception\n"
                outExe
    assertEqual "after runExe" (ExitFailure 1) statusExe

    updateSessionD session
                   (updateRelativeIncludes ["test/AnotherB", "test/ABnoError"])  -- A again in path
                   2
    assertNoErrors session

    runActions4 <- runStmt session "Main" "main"
    (output4, _) <- runWaitAll runActions4
    assertEqual "output4" "\"running A with another B\"\n" output4

    -- A again in path, so this time this works
    updateSessionD session updE4 2
    status45 <- getBuildExeStatus session
    assertEqual "after exe build45" (Just ExitSuccess) status45
    (stExc45, out45, _) <-
      readProcessWithExitCode (distDir </> "build" </> m </> m) [] []
    assertEqual "A throws exception" (ExitFailure 1) stExc45
    assertEqual "exe output with new include path"
                "\"running A with another B\"\n"
                out45
    runActionsExe4 <- runExe session m
    (outExe4, statusExe4) <- runWaitAll runActionsExe4
    assertEqual "Output from runExe 45"
                "\"running A with another B\"\nMain: A.hs throws exception\n"
                outExe4
    assertEqual "after runExe" (ExitFailure 1) statusExe4

    updateSessionD session
                   (updateRelativeIncludes ["test/ABnoError"])
                   0
    assertOneError session  -- correct

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

buildExeTargetHsSucceeds :: IdeSession -> String -> IO ()
buildExeTargetHsSucceeds session m = do
  let updE = buildExe [] [(T.pack m, m <.> "hs")]
  updateSessionD session updE 4
  distDir <- getDistDir session
  buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
  assertEqual "buildStderr empty" "" buildStderr
  status <- getBuildExeStatus session
  assertEqual "after exe build" (Just ExitSuccess) status

buildExeTargetHsFails :: IdeSession -> String -> IO ()
buildExeTargetHsFails session m = do
  let updE = buildExe [] [(T.pack m, m <.> "hs")]
  updateSessionD session updE 4
  status <- getBuildExeStatus session
  assertEqual "after exe build" (Just $ ExitFailure 1) status

modAn, modBn, modCn :: String -> IdeSessionUpdate ()
modAn n = updateSourceFile "A.hs" $ L.fromString $ unlines [
    "module A (foo, main) where"
  , "foo :: Int"
  , "foo = " ++ n
  , "main :: IO ()"
  , "main = return ()"
  ]
modBn n = updateSourceFile "B.hs" $ L.fromString $ unlines [
    "module B (bar, main) where"
  , "import A (foo)"
  , "bar :: Int"
  , "bar = foo + " ++ n
  , "main :: IO ()"
  , "main = return ()"
  ]
modCn n = updateSourceFile "C.hs" $ L.fromString $ unlines [
    "module C (baz, main) where"
  , "import A (foo)"
  , "baz :: Int"
  , "baz = foo + " ++ n
  , "main :: IO ()"
  , "main = return ()"
  ]
