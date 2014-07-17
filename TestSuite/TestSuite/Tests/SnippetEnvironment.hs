-- | Tests for the snippet environment (cwd, available files, env vars, etc.)
module TestSuite.Tests.SnippetEnvironment (testGroupSnippetEnvironment) where

import Data.Monoid
import System.Exit
import Test.Tasty
import Test.HUnit
import qualified Data.ByteString.Lazy.Char8 as L (unlines)
import qualified Data.Text                  as T

import IdeSession
import TestSuite.Session
import TestSuite.Assertions
import TestSuite.State

testGroupSnippetEnvironment :: TestSuiteEnv -> TestTree
testGroupSnippetEnvironment env = testGroup "Snippet environment" [
    stdTest env "Set environment variables"           test_SetEnvVars
  , stdTest env "Set environment variables in runExe" test_SetEnvVars_runExe
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
