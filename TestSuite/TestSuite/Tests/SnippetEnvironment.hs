-- | Tests for the snippet environment (cwd, available files, env vars, etc.)
module TestSuite.Tests.SnippetEnvironment (testGroupSnippetEnvironment) where

import Data.Monoid
import System.Exit
import System.FilePath
import System.Process
import Test.Tasty
import Test.HUnit
import qualified Data.ByteString.Lazy.Char8 as L (unlines)
import qualified Data.ByteString.Lazy.UTF8  as L
import qualified Data.Text                  as T

import IdeSession
import TestSuite.Session
import TestSuite.Assertions
import TestSuite.State

testGroupSnippetEnvironment :: TestSuiteEnv -> TestTree
testGroupSnippetEnvironment env = testGroup "Snippet environment" [
    stdTest env "Set environment variables"                                test_SetEnvVars
  , stdTest env "Set environment variables in runExe"                      test_SetEnvVars_runExe
  , stdTest env "Test CWD by reading a data file"                          test_readDataFile
  , stdTest env "Test CWD in executable building"                          test_CwdInExeBuilding
  , stdTest env "Set command line arguments"                               test_SetCmdLineArgs
  , stdTest env "Check that command line arguments survive restartSession" test_CmdLineArgsAfterRestart
  ]

test_SetEnvVars :: TestSuiteEnv -> Assertion
test_SetEnvVars env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session

    -- At the start, both Foo and Bar are undefined
    do runActions <- runStmt session "M" "printFoo"
       (_, result) <- runWaitAll runActions
       case result of
         RunProgException ex -> assertEqual "" ex "IOException: Foo: getEnv: does not exist (no environment variable)"
         _ -> assertFailure $ "Unexpected result " ++ show result
    do runActions <- runStmt session "M" "printBar"
       (_, result) <- runWaitAll runActions
       case result of
         RunProgException ex -> assertEqual "" ex "IOException: Bar: getEnv: does not exist (no environment variable)"
         _ -> assertFailure $ "Unexpected result " ++ show result

    -- Update Foo, leave Bar undefined
    updateSession session (updateEnv [("Foo", Just "Value1")]) (\_ -> return ())
    do runActions <- runStmt session "M" "printFoo"
       (output, result) <- runWaitAll runActions
       assertEqual "" RunOk result
       assertEqual "" "Value1" output
    do runActions <- runStmt session "M" "printBar"
       (_, result) <- runWaitAll runActions
       assertEqual "" result (RunProgException "IOException: Bar: getEnv: does not exist (no environment variable)")

    -- Update Bar, leave Foo defined
    updateSession session (updateEnv [("Foo", Just "Value1"), ("Bar", Just "Value2")]) (\_ -> return ())
    do runActions <- runStmt session "M" "printFoo"
       (output, result) <- runWaitAll runActions
       assertEqual "" RunOk result
       assertEqual "" "Value1" output
    do runActions <- runStmt session "M" "printBar"
       (output, result) <- runWaitAll runActions
       assertEqual "" RunOk result
       assertEqual "" "Value2" output

    -- Unset Foo, leave Bar defined
    updateSession session (updateEnv [("Foo", Nothing), ("Bar", Just "Value2")]) (\_ -> return ())
    do runActions <- runStmt session "M" "printFoo"
       (_, result) <- runWaitAll runActions
       assertEqual "" result (RunProgException "IOException: Foo: getEnv: does not exist (no environment variable)")
    do runActions <- runStmt session "M" "printBar"
       (output, result) <- runWaitAll runActions
       assertEqual "" RunOk result
       assertEqual "" "Value2" output

    -- Rely on statelessness of updateEnv to reset Bar
    updateSession session (updateEnv []) (\_ -> return ())
    do runActions <- runStmt session "M" "printFoo"
       (_, result) <- runWaitAll runActions
       case result of
         RunProgException ex -> assertEqual "" ex "IOException: Foo: getEnv: does not exist (no environment variable)"
         _ -> assertFailure $ "Unexpected result " ++ show result
    do runActions <- runStmt session "M" "printBar"
       (_, result) <- runWaitAll runActions
       case result of
         RunProgException ex -> assertEqual "" ex "IOException: Bar: getEnv: does not exist (no environment variable)"
         _ -> assertFailure $ "Unexpected result " ++ show result
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "import System.Environment (getEnv)"
            , "printFoo :: IO ()"
            , "printFoo = getEnv \"Foo\" >>= putStr"
            , "printBar :: IO ()"
            , "printBar = getEnv \"Bar\" >>= putStr"
            ])

test_SetEnvVars_runExe :: TestSuiteEnv -> Assertion
test_SetEnvVars_runExe env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    let m = "M"
        updExe = buildExe [] [(T.pack m, "M.hs")]
    updateSessionD session updExe 2
    assertNoErrors session

    -- At the start, both Foo and Bar are undefined
    do updateSessionD session (updateArgs ["Foo"]) 1
       runActions <- runExe session "M"
       (_, result) <- runWaitAll runActions
       assertEqual "" result (ExitFailure 1)
    do updateSessionD session (updateArgs ["Bar"]) 1
       runActions <- runExe session "M"
       (_, result) <- runWaitAll runActions
       assertEqual "" result (ExitFailure 1)

    -- Update Foo, leave Bar undefined
    updateSession session (updateEnv [("Foo", Just "Value1")]) (\_ -> return ())
    do updateSessionD session (updateArgs ["Foo"]) 1
       runActions <- runExe session "M"
       (output, result) <- runWaitAll runActions
       assertEqual "" result ExitSuccess
       assertEqual "" "Value1" output
    do updateSessionD session (updateArgs ["Bar"]) 1
       runActions <- runExe session "M"
       (_, result) <- runWaitAll runActions
       assertEqual "" result (ExitFailure 1)

    -- Update Bar, leave Foo defined
    updateSession session (updateEnv [("Foo", Just "Value1"), ("Bar", Just "Value2")]) (\_ -> return ())
    do updateSessionD session (updateArgs ["Foo"]) 1
       runActions <- runExe session "M"
       (output, result) <- runWaitAll runActions
       assertEqual "" result ExitSuccess
       assertEqual "" "Value1" output
    do updateSessionD session (updateArgs ["Bar"]) 1
       runActions <- runExe session "M"
       (output, result) <- runWaitAll runActions
       assertEqual "" result ExitSuccess
       assertEqual "" "Value2" output

    -- Unset Foo, leave Bar defined
    updateSession session (updateEnv [("Foo", Nothing), ("Bar", Just "Value2")]) (\_ -> return ())
    do updateSessionD session (updateArgs ["Foo"]) 1
       runActions <- runExe session "M"
       (_, result) <- runWaitAll runActions
       assertEqual "" result (ExitFailure 1)
    do updateSessionD session (updateArgs ["Bar"]) 1
       runActions <- runExe session "M"
       (output, result) <- runWaitAll runActions
       assertEqual "" result ExitSuccess
       assertEqual "" "Value2" output

    -- Rely on statelessness of updateEnv to reset Bar
    updateSession session (updateEnv []) (\_ -> return ())
    do updateSessionD session (updateArgs ["Foo"]) 1
       runActions <- runExe session "M"
       (_, result) <- runWaitAll runActions
       assertEqual "" result (ExitFailure 1)
    do updateSessionD session (updateArgs ["Bar"]) 1
       runActions <- runExe session "M"
       (_, result) <- runWaitAll runActions
       assertEqual "" result (ExitFailure 1)
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "import System.Environment"
            , "main :: IO ()"
            , "main = do"
            , "  args <- getArgs"
            , "  case args of"
            , "    [\"Foo\"] -> getEnv \"Foo\" >>= putStr"
            , "    [\"Bar\"] -> getEnv \"Bar\" >>= putStr"
            , "    _ -> fail \"wrong args\""
            ])

test_readDataFile :: TestSuiteEnv -> Assertion
test_readDataFile env = withAvailableSession env $ \session -> do
    let update = updateDataFile "datafile.dat" "test data content"
    updateSessionD session update 0
    let update2 = loadModule "Main.hs"
          "main = readFile \"datafile.dat\" >>= putStrLn"
    updateSessionD session update2 1
    assertNoErrors session
    let update3 = updateCodeGeneration True
    updateSessionD session update3 1
    runActions <- runStmt session "Main" "main"
    (output, _) <- runWaitAll runActions
    assertEqual "compare test data content" "test data content\n" output
    let m = "Main"
        updExe = buildExe [] [(T.pack m, "Main.hs")]
    updateSessionD session updExe 2
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "test data content\n"
                outExe
    assertEqual "after runExe" ExitSuccess statusExe
    let update4 = updateDataFile "datafile.dat" "new content"
                  <> update2
    updateSessionD session update4 1
    runActions2 <- runStmt session "Main" "main"
    (output2, _) <- runWaitAll runActions2
    assertEqual "compare new content" "new content\n" output2
    let updExe2 = buildExe [] [(T.pack m, "Main.hs")]
    updateSessionD session updExe2 2
    runActionsExe2 <- runExe session m
    (outExe2, statusExe2) <- runWaitAll runActionsExe2
    assertEqual "Output from runExe"
                "new content\n"
                outExe2
    assertEqual "after runExe" ExitSuccess statusExe2

test_CwdInExeBuilding :: TestSuiteEnv -> Assertion
test_CwdInExeBuilding env = withAvailableSession env $ \session -> do
    let update = updateCodeGeneration True
                 <> updateDataFile "test.txt" "test data"
    let update2 = updateSourceFile "Main.hs" $ L.unlines
          [ "{-# LANGUAGE TemplateHaskell #-}"
          , "module Main where"
          , "import Language.Haskell.TH.Syntax"
          , "main = putStrLn $(qRunIO (readFile \"test.txt\") >>= lift)"
          ]
    updateSessionD session (update <> update2) 1
    assertNoErrors session
    runActions <- runStmt session "Main" "main"
    (output, _) <- runWaitAll runActions
    assertEqual "compare test data" "test data\n" output
    let m = "Main"
        upd = buildExe [] [(T.pack m, "Main.hs")]
    updateSessionD session upd 2
    distDir <- getDistDir session
    out <- readProcess (distDir </> "build" </> m </> m) [] []
    assertEqual "CWD exe output" (L.toString output) out
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                output
                outExe
    assertEqual "after runExe" ExitSuccess statusExe

test_SetCmdLineArgs :: TestSuiteEnv -> Assertion
test_SetCmdLineArgs env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session

    let m = "M"
        updExe = buildExe [] [(T.pack m, "M.hs")]
    updateSessionD session updExe 2

    -- Check that default is []
    do runActions <- runStmt session "M" "printArgs"
       (output, result) <- runWaitAll runActions
       assertEqual "" RunOk result
       assertEqual "" "[]\n" output

    do runActionsExe <- runExe session m
       (outExe, statusExe) <- runWaitAll runActionsExe
       assertEqual "after runExe" ExitSuccess statusExe
       assertEqual "Output from runExe"
                   "[]\n"
                   outExe

    -- Check that we can set command line arguments
    updateSession session (updateArgs ["A", "B", "C"]) (\_ -> return ())
    do runActions <- runStmt session "M" "printArgs"
       (output, result) <- runWaitAll runActions
       assertEqual "" RunOk result
       assertEqual "" "[\"A\",\"B\",\"C\"]\n" output

    do runActionsExe <- runExe session m
       (outExe, statusExe) <- runWaitAll runActionsExe
       assertEqual "after runExe" ExitSuccess statusExe
       assertEqual "Output from runExe"
                   "[\"A\",\"B\",\"C\"]\n"
                   outExe

    -- Check that we can change command line arguments
    updateSession session (updateArgs ["D", "E"]) (\_ -> return ())
    do runActions <- runStmt session "M" "printArgs"
       (output, result) <- runWaitAll runActions
       assertEqual "" RunOk result
       assertEqual "" "[\"D\",\"E\"]\n" output

    do runActionsExe <- runExe session m
       (outExe, statusExe) <- runWaitAll runActionsExe
       assertEqual "after runExe" ExitSuccess statusExe
       assertEqual "Output from runExe"
                   "[\"D\",\"E\"]\n"
                   outExe

    -- Check that we can clear command line arguments
    updateSession session (updateArgs []) (\_ -> return ())
    do runActions <- runStmt session "M" "printArgs"
       (output, result) <- runWaitAll runActions
       assertEqual "" RunOk result
       assertEqual "" "[]\n" output

    do runActionsExe <- runExe session m
       (outExe, statusExe) <- runWaitAll runActionsExe
       assertEqual "after runExe" ExitSuccess statusExe
       assertEqual "Output from runExe"
                   "[]\n"
                   outExe
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" $ L.unlines
            [ "module M where"
            , "import System.Environment (getArgs)"
            , "printArgs :: IO ()"
            , "printArgs = getArgs >>= print"
            , "main :: IO ()"
            , "main = printArgs"
            ])

test_CmdLineArgsAfterRestart :: TestSuiteEnv -> Assertion
test_CmdLineArgsAfterRestart env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session

    let m = "M"
        updExe = buildExe [] [(T.pack m, "M.hs")]
    updateSessionD session updExe 2

    -- Sanity check: check before restart session
    updateSession session (updateArgs ["A", "B", "C"]) (\_ -> return ())
    do runActions <- runStmt session "M" "printArgs"
       (output, result) <- runWaitAll runActions
       assertEqual "" RunOk result
       assertEqual "" "[\"A\",\"B\",\"C\"]\n" output

    do runActionsExe <- runExe session m
       (outExe, statusExe) <- runWaitAll runActionsExe
       assertEqual "after runExe" ExitSuccess statusExe
       assertEqual "Output from runExe"
                   "[\"A\",\"B\",\"C\"]\n"
                   outExe

    -- Restart and update the session
    restartSession session
    updateSessionD session upd 1
    assertNoErrors session

    -- Check that arguments are still here
    do runActions <- runStmt session "M" "printArgs"
       (output, result) <- runWaitAll runActions
       assertEqual "" RunOk result
       assertEqual "" "[\"A\",\"B\",\"C\"]\n" output

    do runActionsExe <- runExe session m
       (outExe, statusExe) <- runWaitAll runActionsExe
       assertEqual "after runExe" ExitSuccess statusExe
       assertEqual "Output from runExe"
                   "[\"A\",\"B\",\"C\"]\n"
                   outExe
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" $ L.unlines
            [ "module M where"
            , "import System.Environment (getArgs)"
            , "printArgs :: IO ()"
            , "printArgs = getArgs >>= print"
            , "main :: IO ()"
            , "main = printArgs"
            ])
