module TestSuite.Tests.Compilation (testGroupCompilation) where

import Control.Monad
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
    stdTest env "Compile a project: A depends on B, error in A"                                         test_AdependsB_errorA
  , stdTest env "Compile a project: A depends on B, error in B"                                         test_AdependsB_errorB
  , stdTest env "Compile and run a project with some .lhs files"                                        test_lhs
  , stdTest env "Test recursive modules"                                                                test_recursiveModules
  , stdTest env "Test recursive modules with dynamic include path change"                               test_dynamicIncludePathChange
  , stdTest env "Test CPP: ifdefed module header"                                                       test_CPP_ifdefModuleHeader
  , stdTest env "Reject a wrong CPP directive"                                                          test_rejectWrongCPP
  , stdTest env "Reject a program requiring -XNamedFieldPuns, then set the option"                      test_NamedFieldPuns
  , stdTest env "Don't recompile unnecessarily (single module)"                                         test_DontRecompile_SingleModule
  , stdTest env "Don't recompile unnecessarily (A depends on B)"                                        test_DontRecompile_Depends
  , stdTest env "Support for hs-boot files (#155)"                                                      test_HsBoot
  , stdTest env "Support for lhs-boot files (#155)"                                                     test_LhsBoot
  , stdTest env "Support for hs-boot files from a subdirectory (#177)"                                  test_HsBoot_SubDir
  , stdTest env "Support for hs-boot files from a subdirectory (#177) with dynamic include path change" test_HsBoot_SubDir_InclPathChange
  , stdTest env "Relative include paths (#156)"                                                         test_RelInclPath
  , stdTest env "Relative include paths (#156) with dynamic include path change"                        test_RelInclPath_InclPathChange
  , stdTest env "Parse ghc 'Compiling' messages"                                                        test_ParseCompiling
  , stdTest env "Parse ghc 'Compiling' messages (with TH)"                                              test_ParseCompiling_TH
  , stdTest env "Reject a module with mangled header"                                                   test_RejectMangledHeader
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
test_CPP_ifdefModuleHeader env = withAvailableSession' env (withDynOpts ["-XCPP"]) $ \session -> do
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
test_rejectWrongCPP env = withAvailableSession' env (withDynOpts ["-XCPP"]) $ \session -> do
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
test_NamedFieldPuns env = withAvailableSession' env (withDynOpts ["-hide-package monads-tf"]) $ \session -> do
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

test_HsBoot :: TestSuiteEnv -> Assertion
test_HsBoot env = withAvailableSession env $ \session -> do
    updateSessionD session upd 3
    assertNoErrors session

    let m = "A"
        updE = buildExe ["-rtsopts", "-O1"] [(T.pack m, m <.> "hs")]
    updateSessionD session updE 4
    distDir <- getDistDir session
    buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
    assertEqual "buildStderr empty" "" buildStderr
    status <- getBuildExeStatus session
    assertEqual "after exe build" (Just ExitSuccess) status
    out <- readProcess (distDir </> "build" </> m </> m)
                       ["+RTS", "-C0.005", "-RTS"] []
    assertEqual "" "[1,1,2,3,5,8,13,21,34,55]\n" out
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "[1,1,2,3,5,8,13,21,34,55]\n"
                outExe
    assertEqual "after runExe" ExitSuccess statusExe
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "A.hs" $ L.unlines [
              "module A where"

            , "import {-# SOURCE #-} B"

            , "f :: Int -> Int"
            , "f 0 = 1"
            , "f 1 = 1"
            , "f n = g (n - 1) + g (n - 2)"

            , "main :: IO ()"
            , "main = print $ map f [0..9]"
            ])
       <> (updateSourceFile "B.hs" $ L.unlines [
              "module B where"

            , "import A"

            , "g :: Int -> Int"
            , "g = f"
            ])
       <> (updateSourceFile "B.hs-boot" $ L.unlines [
              "module B where"

            , "g :: Int -> Int"
            ])

test_LhsBoot :: TestSuiteEnv -> Assertion
test_LhsBoot env = withAvailableSession env $ \session -> do
    updateSessionD session upd 3
    assertNoErrors session

    let m = "A"
        updE = buildExe [] [(T.pack m, m <.> "lhs")]
    updateSessionD session updE 4
    distDir <- getDistDir session
    buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
    assertEqual "buildStderr empty" "" buildStderr
    status <- getBuildExeStatus session
    assertEqual "after exe build" (Just ExitSuccess) status
    out <- readProcess (distDir </> "build" </> m </> m) [] []
    assertEqual "" "[1,1,2,3,5,8,13,21,34,55]\n" out
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "[1,1,2,3,5,8,13,21,34,55]\n"
                outExe
    assertEqual "after runExe" ExitSuccess statusExe
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "A.lhs" $ L.unlines [
              "> module A where"

            , "> import {-# SOURCE #-} B"

            , "> f :: Int -> Int"
            , "> f 0 = 1"
            , "> f 1 = 1"
            , "> f n = g (n - 1) + g (n - 2)"

            , "> main :: IO ()"
            , "> main = print $ map f [0..9]"
            ])
       <> (updateSourceFile "B.lhs" $ L.unlines [
              "> module B where"

            , "> import A"

            , "> g :: Int -> Int"
            , "> g = f"
            ])
       <> (updateSourceFile "B.lhs-boot" $ L.unlines [
              "> module B where"

            , "> g :: Int -> Int"
            ])

test_HsBoot_SubDir :: TestSuiteEnv -> Assertion
test_HsBoot_SubDir env = withAvailableSession' env (withIncludes ["src"]) $ \session -> do
    updateSessionD session update 3
    assertNoErrors session

    let m = "B"
        updE = buildExe [] [(T.pack m, m <.> "hs")]
    updateSessionD session updE 4
    distDir <- getDistDir session
    buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
    assertEqual "buildStderr empty" "" buildStderr
    status <- getBuildExeStatus session
    assertEqual "after exe build" (Just ExitSuccess) status
    out <- readProcess (distDir </> "build" </> m </> m) [] []
    assertEqual "" "42\n" out
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "42\n"
                outExe
    assertEqual "after runExe" ExitSuccess statusExe
  where
    ahs     = "module A where\nimport B( TB(..) )\nnewtype TA = MkTA Int\nf :: TB -> TA\nf (MkTB x) = MkTA x"
    ahsboot = "module A where\nnewtype TA = MkTA Int"
    bhs     = "module B where\nimport {-# SOURCE #-} A( TA(..) )\ndata TB = MkTB !Int\ng :: TA -> TB\ng (MkTA x) = MkTB x\nmain = print 42"

    update = updateSourceFile "src/A.hs" ahs
          <> updateSourceFile "src/A.hs-boot" ahsboot
          <> updateSourceFile "src/B.hs" bhs
          <> updateCodeGeneration True

test_HsBoot_SubDir_InclPathChange :: TestSuiteEnv -> Assertion
test_HsBoot_SubDir_InclPathChange env = withAvailableSession env $ \session -> do
    updateSessionD session update 3
    assertOneError session

    updateSessionD session
                   (updateRelativeIncludes ["src"])
                   3
    assertNoErrors session

    let m = "B"
        updE = buildExe [] [(T.pack m, m <.> "hs")]
    updateSessionD session updE 4
    distDir <- getDistDir session
    buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
    assertEqual "buildStderr empty" "" buildStderr
    status <- getBuildExeStatus session
    assertEqual "after exe build" (Just ExitSuccess) status
    out <- readProcess (distDir </> "build" </> m </> m) [] []
    assertEqual "" "42\n" out
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "42\n"
                outExe
    assertEqual "after runExe" ExitSuccess statusExe
  where
    ahs     = "module A where\nimport B( TB(..) )\nnewtype TA = MkTA Int\nf :: TB -> TA\nf (MkTB x) = MkTA x"
    ahsboot = "module A where\nnewtype TA = MkTA Int"
    bhs     = "module B where\nimport {-# SOURCE #-} A( TA(..) )\ndata TB = MkTB !Int\ng :: TA -> TB\ng (MkTA x) = MkTB x\nmain = print 42"

    update = updateSourceFile "src/A.hs" ahs
          <> updateSourceFile "src/A.hs-boot" ahsboot
          <> updateSourceFile "src/B.hs" bhs
          <> updateCodeGeneration True

test_RelInclPath :: TestSuiteEnv -> Assertion
test_RelInclPath env = withAvailableSession' env (withIncludes ["test/ABnoError"]) $ \session -> do
    -- Since we set the target explicitly, ghc will need to be able to find
    -- the other module (B) on its own; that means it will need an include
    -- path to <ideSourcesDir>/test/ABnoError
    loadModulesFrom' session "test/ABnoError" (TargetsInclude ["test/ABnoError/A.hs"])
    assertNoErrors session

    let updE = buildExe [] [(T.pack "Main", "test/ABnoError/A.hs")]
    updateSessionD session updE 3
    status <- getBuildExeStatus session
    assertEqual "after exe build" (Just ExitSuccess) status

    let updE2 = buildExe [] [(T.pack "Main", "A.hs")]
    updateSessionD session updE2 0
    status2 <- getBuildExeStatus session
    assertEqual "after exe build" (Just ExitSuccess) status2

test_RelInclPath_InclPathChange :: TestSuiteEnv -> Assertion
test_RelInclPath_InclPathChange env = withAvailableSession env $ \session -> do
    -- Since we set the target explicitly, ghc will need to be able to find
    -- the other module (B) on its own; that means it will need an include
    -- path to <ideSourcesDir>/test/ABnoError
    loadModulesFrom' session "test/ABnoError" (TargetsInclude ["test/ABnoError/A.hs"])
    assertOneError session

    updateSessionD session
                   (updateRelativeIncludes ["test/ABnoError"])
                   2  -- note the recompilation
    assertNoErrors session

    let updE = buildExe [] [(T.pack "Main", "test/ABnoError/A.hs")]
    updateSessionD session updE 1
    status <- getBuildExeStatus session
    -- Path "" no longer in include paths here!
    assertEqual "after exe build" (Just $ ExitFailure 1) status

    let updE2 = buildExe [] [(T.pack "Main", "A.hs")]
    updateSessionD session updE2 2
    status2 <- getBuildExeStatus session
    assertEqual "after exe build" (Just ExitSuccess) status2

test_ParseCompiling :: TestSuiteEnv -> Assertion
test_ParseCompiling env = withAvailableSession env $ \session -> do
    progressUpdatesRef <- newIORef []
    updateSession session upd $ \p -> do
      progressUpdates <- readIORef progressUpdatesRef
      writeIORef progressUpdatesRef (progressUpdates ++ [p])
    assertNoErrors session

    progressUpdates <- readIORef progressUpdatesRef
    assertEqual "" [(1, 2, Just "Compiling A"), (2, 2, Just "Compiling B")]
                 (map abstract progressUpdates)
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "A.hs" . L.unlines $
            [ "module A where"
            , "printA :: IO ()"
            , "printA = putStr \"A\""
            ])
       <> (updateSourceFile "B.hs" . L.unlines $
            [ "module B where"
            , "import A"
            , "printAB :: IO ()"
            , "printAB = printA >> putStr \"B\""
            ])

    abstract (Progress {..}) = ( progressStep
                               , progressNumSteps
                               , T.unpack `liftM` progressParsedMsg
                               )

test_ParseCompiling_TH :: TestSuiteEnv -> Assertion
test_ParseCompiling_TH env = withAvailableSession env $ \session -> do
    progressUpdatesRef <- newIORef []
    updateSession session upd $ \p -> do
      progressUpdates <- readIORef progressUpdatesRef
      writeIORef progressUpdatesRef (progressUpdates ++ [p])
    assertNoErrors session

    do progressUpdates <- readIORef progressUpdatesRef
       assertEqual "" [(1, 2, Just "Compiling A"), (2, 2, Just "Compiling Main")]
                      (map abstract progressUpdates)

    -- Now we touch A, triggering recompilation of both A and B
    -- This will cause ghc to report "[TH]" as part of the progress message
    -- (at least on the command line). It doesn't seem to happen with the
    -- API; but just in case, we check that we still get the right messages
    -- (and not, for instance, '[TH]' as the module name).

    writeIORef progressUpdatesRef []
    updateSession session upd2 $ \p -> do
      progressUpdates <- readIORef progressUpdatesRef
      writeIORef progressUpdatesRef (progressUpdates ++ [p])
    assertNoErrors session

    do progressUpdates <- readIORef progressUpdatesRef
       assertEqual "" [(1, 2, Just "Compiling A"), (2, 2, Just "Compiling Main")]
                      (map abstract progressUpdates)
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "A.hs" . L.unlines $
            [ "{-# LANGUAGE TemplateHaskell #-}"
            , "module A where"
            , "import Language.Haskell.TH"
            , "foo :: Q Exp"
            , "foo = [| True |]"
            ])
       <> (updateSourceFile "Main.hs" . L.unlines $
            [ "{-# LANGUAGE TemplateHaskell #-}"
            , "module Main where"
            , "import A"
            , "main :: IO ()"
            , "main = print $foo"
            ])

    upd2 = (updateSourceFile "A.hs" . L.unlines $
            [ "{-# LANGUAGE TemplateHaskell #-}"
            , "module A where"
            , "import Language.Haskell.TH"
            , "foo :: Q Exp"
            , "foo = [| False |]"
            ])

    abstract (Progress {..}) = ( progressStep
                               , progressNumSteps
                               , T.unpack `liftM` progressParsedMsg
                               )

test_RejectMangledHeader :: TestSuiteEnv -> Assertion
test_RejectMangledHeader env = withAvailableSession env $ \session -> do
    updateSessionD session update 1
    assertSourceErrors' session ["parse error on input `very'"]

    updateSessionD session update2 1
    assertSourceErrors' session ["parse error on input `.'"]
  where
    update  = updateSourceFile "M.hs" "module very-wrong where"
    update2 = updateSourceFile "M.hs" "module M.1.2.3.8.T where"

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
