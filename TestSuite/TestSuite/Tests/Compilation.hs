module TestSuite.Tests.Compilation (testGroupCompilation) where

import Data.IORef
import Data.Monoid
import System.Exit
import System.FilePath
import System.Process
import Test.HUnit
import Test.Tasty
import qualified Data.ByteString.Lazy.UTF8  as L
import qualified Data.ByteString.Lazy.Char8 as L (unlines)
import qualified Data.Text                  as T

import IdeSession
import TestSuite.State
import TestSuite.Session
import TestSuite.Assertions

testGroupCompilation :: TestSuiteEnv -> TestTree
testGroupCompilation env = testGroup "Compilation" [
    stdTest env "Compile a project: A depends on B, error in A"                    test_AdependsB_errorA
  , stdTest env "Compile a project: A depends on B, error in B"                    test_AdependsB_errorB
  , stdTest env "Compile and run a project with some .lhs files"                   test_lhs
  , stdTest env "Test recursive modules"                                           test_recursiveModules
  , stdTest env "Test recursive modules with dynamic include path change"          test_dynamicIncludePathChange
  , stdTest env "Test CPP: ifdefed module header"                                  test_CPP_ifdefModuleHeader
  , stdTest env "Reject a wrong CPP directive"                                     test_rejectWrongCPP
  , stdTest env "Reject a program requiring -XNamedFieldPuns, then set the option" test_NamedFieldPuns
  , stdTest env "Don't recompile unnecessarily (single module)"                    test_DontRecompile_SingleModule
  , stdTest env "Don't recompile unnecessarily (A depends on B)"                   test_DontRecompile_Depends
  ]

test_AdependsB_errorA :: TestSuiteEnv -> Assertion
test_AdependsB_errorA env = withAvailableSession env $ \session -> do
    loadModulesFrom session "test/AerrorB"
    assertSourceErrors session [[(Just "A.hs", "No instance for (Num (IO ()))")]]

test_AdependsB_errorB :: TestSuiteEnv -> Assertion
test_AdependsB_errorB env = withAvailableSession env $ \session -> do
    loadModulesFrom session "test/ABerror"
    assertSourceErrors session [[(Just "B.hs", "No instance for (Num (IO ()))")]]

test_lhs :: TestSuiteEnv -> Assertion
test_lhs env = withAvailableSession env $ \session -> do
    loadModulesFrom session "test/compiler/utils"
    assertNoErrors session
    let update2 = updateCodeGeneration True
    updateSessionD session update2 4
    assertNoErrors session
    runActions <- runStmt session "Maybes" "main"
    (output, result) <- runWaitAll runActions
    assertEqual "" RunOk result
    assertEqual "" output "False\n"

test_recursiveModules :: TestSuiteEnv -> Assertion
test_recursiveModules env = withAvailableSession' env (withIncludes ["test/bootMods"]) $ \session -> do
    loadModulesFrom session "test/bootMods"
    assertNoErrors session

    let m = "Main"
        upd = buildExe [] [(T.pack m, "C" <.> "hs")]
    updateSessionD session upd 7
    distDir <- getDistDir session
    buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
    assertEqual "buildStderr empty" "" buildStderr
    status <- getBuildExeStatus session
    assertEqual "after exe build" (Just ExitSuccess) status
    out <- readProcess (distDir </> "build" </> m </> m) [] []
    assertEqual "" "C\n" out
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "C\n"
                outExe
    assertEqual "after runExe" ExitSuccess statusExe

test_dynamicIncludePathChange :: TestSuiteEnv -> Assertion
test_dynamicIncludePathChange env = withAvailableSession env $ \session -> do
    loadModulesFrom session "test/bootMods"
    assertOneError session

    updateSessionD session
                   (updateRelativeIncludes ["test/bootMods"])
                   4
    assertNoErrors session

    let m = "Main"
        upd = buildExe [] [(T.pack m, "C" <.> "hs")]
    updateSessionD session upd 7
    distDir <- getDistDir session
    buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
    assertEqual "buildStderr empty" "" buildStderr
    status <- getBuildExeStatus session
    assertEqual "after exe build" (Just ExitSuccess) status
    out <- readProcess (distDir </> "build" </> m </> m) [] []
    assertEqual "" "C\n" out
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "C\n"
                outExe
    assertEqual "after runExe" ExitSuccess statusExe

test_CPP_ifdefModuleHeader :: TestSuiteEnv -> Assertion
test_CPP_ifdefModuleHeader env = withAvailableSession' env (withOpts ["-XCPP"]) $ \session -> do
    updateSessionD session update 1
    assertNoErrors session
    assertIdInfo session "Good" (8,1,8,2) "x" VarName "[a]" "main:Good" "Good.hs@8:1-8:2" "" "binding occurrence"
  where
    update = updateSourceFile "Good.hs" $ L.unlines
      [ "#if __GLASGOW_HASKELL__ < 600"
      , "module Bad where"
      , "import Data.List"
      , "#else"
      , "module Good where"
      , "import Data.Monoid"
      , "#endif"
      , "x = mappend [] []"
      ]

test_rejectWrongCPP :: TestSuiteEnv -> Assertion
test_rejectWrongCPP env = withAvailableSession' env (withOpts ["-XCPP"]) $ \session -> do
    updateSessionD session update 1
    msgs <- getSourceErrors session
    -- Due to a GHC bug there are now 2 errors. TODO; when it's fixed,
    -- assert a single specific error here.
    assertSomeErrors msgs
    assertRaises "runStmt session Main main"
      (== userError "Module \"Main\" not successfully loaded, when trying to run code.")
      (runStmt session "Main" "main")
  where
    update = loadModule "M.hs" "#ifdef"
          <> updateCodeGeneration True


test_NamedFieldPuns :: TestSuiteEnv -> Assertion
test_NamedFieldPuns env = withAvailableSession' env (withOpts ["-hide-package monads-tf"]) $ \session -> do
    withCurrentDirectory "test/Puns" $ do
      loadModulesFrom session "."
      assertMoreErrors session
      let punOpts = ["-XNamedFieldPuns", "-XRecordWildCards"]
          update2 = updateDynamicOpts punOpts
      (_, lm) <- getModules session
      updateSessionD session update2 (length lm)
    assertNoErrors session
    let m = "GHC.RTS.Events"
        upd2 = buildExe [] [(T.pack m, "GHC/RTS/Events.hs")]
    updateSessionD session upd2 4
    distDir <- getDistDir session
    buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
    assertEqual "buildStderr empty" "" buildStderr

test_DontRecompile_SingleModule :: TestSuiteEnv -> Assertion
test_DontRecompile_SingleModule env = withAvailableSession env $ \session -> do
    counter <- newCounter

    updateSession session upd (\_ -> incCounter counter)
    assertCounter counter 1

    resetCounter counter
    updateSession session upd (\_ -> incCounter counter)
    assertCounter counter 0
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "A.hs" . L.unlines $
            [ "module A where"
            , "a :: IO ()"
            , "a = print 'a'"
            ])

test_DontRecompile_Depends :: TestSuiteEnv -> Assertion
test_DontRecompile_Depends env = withAvailableSession env $ \session -> do
    counter <- newCounter

    -- Initial compilation needs to recompile for A and B
    updateSession session upd (\_ -> incCounter counter)
    assertCounter counter 2

    -- Overwriting B with the same code requires no compilation at all
    resetCounter counter
    updateSession session (updB 0) (\_ -> incCounter counter)
    assertCounter counter 0

    -- Nor does overwriting A with the same code
    resetCounter counter
    updateSession session (updA 0) (\_ -> incCounter counter)
    assertCounter counter 0

    -- Giving B a new interface means both A and B need to be recompiled
    resetCounter counter
    updateSession session (updB 1) (\_ -> incCounter counter)
    assertCounter counter 2

    -- Changing the interface of A only requires recompilation of A
    resetCounter counter
    updateSession session (updA 1) (\_ -> incCounter counter)
    assertCounter counter 1
  where
    -- 'updA' is defined so that the interface of 'updA n' is different
    -- to the interface of 'updA m' (with n /= m)
    updA n = updateSourceFile "A.hs" . L.unlines $
               [ "module A where"
               , "import B"
               ]
              ++
               [ L.fromString $ "a" ++ show i ++ " = b" ++ show i
               | i <- [0 .. n :: Int]
               ]
    updB n = updateSourceFile "B.hs" . L.unlines $
               [ "module B where"
               ]
              ++
               [ L.fromString $ "b" ++ show i ++ " = return () :: IO ()"
               | i <- [0 .. n :: Int]
               ]
    upd = updateCodeGeneration True <> updA 0 <> updB 0

{-------------------------------------------------------------------------------
  Auxiliary: counter
-------------------------------------------------------------------------------}

newtype Counter = Counter (IORef Int)

newCounter :: IO Counter
newCounter = do
  c <- newIORef 0
  return (Counter c)

resetCounter :: Counter -> IO ()
resetCounter (Counter c) = writeIORef c 0

incCounter :: Counter -> IO ()
incCounter (Counter c) = readIORef c >>= writeIORef c . (+ 1)

assertCounter :: Counter -> Int -> Assertion
assertCounter (Counter c) i = do
  j <- readIORef c
  assertEqual "" i j
