-- | Rather than doing spot-checks for specific features, load some bigger
-- projects and make sure nothing goes horribly wrong
module TestSuite.Tests.Integration (testGroupIntegration) where

import Control.Exception
import Control.Monad
import Data.Monoid
import System.Exit
import Test.Tasty
import Test.HUnit hiding (test)

import IdeSession
import TestSuite.Assertions
import TestSuite.Session
import TestSuite.State

testGroupIntegration :: TestSuiteEnv -> TestTree
testGroupIntegration env = testGroup "Integration" $ integrationTests env [
    integrationTest env "Run the sample code; succeed or raise an exception"         test_runSampleCode
  , integrationTest env "Overwrite with error"                                       test_overwriteWithError
  , integrationTest env "Overwrite with the same module name in all files"           test_overwriteWithSameModuleName
  , integrationTest env "Overwrite modules many times"                               test_overwriteModulesManyTimes
  , integrationTest env "Overwrite all with exception-less code and run it"          test_overwriteWithExceptionFreeCode
  , integrationTest env "Make sure deleting modules removes them from the directory" test_deletingModulesRemovesFiles
  , integrationTest env "Make sure restartSession does not lose source files"        test_dontLoseFilesInRestart
  ]

{-------------------------------------------------------------------------------
  "Projects" setup
-------------------------------------------------------------------------------}

type IntegrationTest = IdeSession -> IdeSessionUpdate -> [String] -> Assertion

integrationTest :: TestSuiteEnv -> String -> IntegrationTest -> TestTree
integrationTest env name test = testGroup name $
  map (testWithProject env test) (projects env)

testWithProject :: TestSuiteEnv -> IntegrationTest -> Project -> TestTree
testWithProject env test Project{..} = do
    stdTest env projectName $ \env' -> do
      (upd, lm) <- getModulesFrom projectSourcesDir
      withAvailableSession' env' cfg $ \session -> test session upd lm
  where
    cfg = withModInfo False
        . withGhcOpts projectOptions

data Project = Project {
     projectName       :: String
   , projectSourcesDir :: FilePath
   , projectOptions    :: [String]
   }

-- Set of projects and options to use for them.
projects :: TestSuiteEnv -> [Project]
projects env = [
    Project {
        projectName       = "A depends on B, throws exception"
      , projectSourcesDir = "TestSuite/inputs/ABnoError"
      , projectOptions    = []
      }
  , Project {
        projectName       = "Cabal code"
      , projectSourcesDir = testInputPathCabal env
      , projectOptions    = []
      }
  , Project {
        projectName       = "A single file with a code to run in parallel"
      , projectSourcesDir = "TestSuite/inputs/MainModule"
      , projectOptions    = []
      }
  ]

{-------------------------------------------------------------------------------
  The tests we run on each project
-------------------------------------------------------------------------------}

test_overwriteWithError :: IntegrationTest
test_overwriteWithError session originalUpdate lm = do
    updateSessionD session originalUpdate (length lm)
    -- No errors in the original test code.
    assertNoErrors session
    -- Overwrite one of the copied files.
    (_, ms) <- getModules session
    let update = loadModule (head ms) "a = unknownX"
    updateSessionD session update (length ms)  -- we don't know how many modules get recompiled
    assertSourceErrors' session ["Not in scope: `unknownX'"]

test_overwriteWithSameModuleName :: IntegrationTest
test_overwriteWithSameModuleName session originalUpdate lm = do
    updateSessionD session update 2
    if length lm >= 2
      then assertSourceErrors' session ["defined in multiple files"]
      else assertNoErrors session
  where
    upd m  = updateSourceFile m "module Wrong where\na = 1"
    update = originalUpdate <> mconcat (map upd lm)

test_overwriteModulesManyTimes :: IntegrationTest
test_overwriteModulesManyTimes session originalUpdate lm0 = do
    let doubleUpdate = mempty <> originalUpdate <> originalUpdate <> mempty
    -- Updates are idempotent, so no errors and no recompilation.
    updateSessionD session doubleUpdate (length lm0)
    assertNoErrors session
    -- Overwrite one of the copied files with an error.
    (_, lm) <- getModules session
    let update1 = updateCodeGeneration False
               <> loadModule (head lm) "a = unknownX"
    updateSessionD session update1 (length lm)
    assertSourceErrors' session ["Not in scope: `unknownX'"]
    updateSessionD session mempty 1  -- was an error, so trying again
    assertSourceErrors' session ["Not in scope: `unknownX'"]
    -- Overwrite all files, many times, with correct code eventually.
    let upd m   = loadModule m "x = unknownX"
               <> loadModule m "y = 2"
               <> updateCodeGeneration True
        update2 = mconcat $ map upd lm
    updateSessionD session update2 (length lm)
    assertNoErrors session
    -- Overwrite again with the error.
    updateSessionD session update1 1  -- drop bytecode, don't recompile
    assertSourceErrors' session ["Not in scope: `unknownX'"]
    assertRaises "runStmt session Main main"
      (== userError "Cannot run before the code is generated.")
      (runStmt session "Main" "main")

test_runSampleCode :: IntegrationTest
test_runSampleCode session originalUpdate lm = do
    updateSessionD session originalUpdate (length lm)
    updateSessionD session update (length lm)  -- all recompiled
    assertNoErrors session
    mex <- try $ runStmt session "Main" "main"
    case mex of
      Right runActions -> void $ runWaitAll runActions
      Left ex -> assertEqual "runStmt" (userError "Module \"Main\" not successfully loaded, when trying to run code.") ex
  where
    update = updateCodeGeneration True

test_overwriteWithExceptionFreeCode :: IntegrationTest
test_overwriteWithExceptionFreeCode session originalUpdate lm = do
    -- Compile from scratch, generating code from the start.
    updateSessionD session update2 (length lm + 1)
    assertNoErrors session
    runActions <- runStmt session "Central.TotallyMain" "main"
    (output, result) <- runWaitAll runActions
    assertEqual "" RunOk result
    assertEqual "" output "\"test run\"\n"
  where
    upd m   = loadModule m "x = 1"
    update  = originalUpdate
           <> updateSourceFile
                "Central/TotallyMain.hs"
                "module Central.TotallyMain where\nmain = print \"test run\""
           <> mconcat (map upd lm)
    update2 = update <> updateCodeGeneration True

test_deletingModulesRemovesFiles :: IntegrationTest
test_deletingModulesRemovesFiles session originalUpdate lm = do
    updateSessionD session originalUpdate (length lm)
    updateSessionD session updateDel 0
    assertNoErrors session
    -- The updates cancel each other out.
    updateSessionD session (originalUpdate <> updateDel) 0
    updateSessionD session update2 0  -- 0: nothing to generate code from
    assertNoErrors session
  where
    updateDel = mconcat $ map updateSourceFileDelete lm
    update2   = updateCodeGeneration True

test_dontLoseFilesInRestart :: IntegrationTest
test_dontLoseFilesInRestart session originalUpdate lm = do
    updateSessionD session update (length lm)
    serverBefore <- getGhcServer session
    mex <- try $ runStmt session "Main" "main"
    case mex of
      Right _runActions -> return ()  -- don't runWaitAll
      Left ex -> assertEqual "runStmt" (userError "Module \"Main\" not successfully loaded, when trying to run code.") ex
    restartSession session
    updateSessionD session mempty (length lm)  -- all compiled anew
    assertNoErrors session
    mex2 <- try $ runStmt session "Main" "main"
    case mex2 of
      Right runActions -> void $ runWaitAll runActions  -- now runWaitAll
      Left ex -> assertEqual "runStmt" (userError "Module \"Main\" not successfully loaded, when trying to run code.") ex
    restartSession session
    updateSessionD session update2 0  -- if any file missing, would yell
    assertNoErrors session
    updateSessionD session update3 0  -- 0: nothing to generate code from
    exitCodeBefore <- getGhcExitCode serverBefore
    assertEqual "exitCodeBefore" (Just ExitSuccess) exitCodeBefore
  where
    update  = originalUpdate <> updateCodeGeneration True
    update2 = mconcat $ map updateSourceFileDelete lm
    update3 = updateCodeGeneration True
