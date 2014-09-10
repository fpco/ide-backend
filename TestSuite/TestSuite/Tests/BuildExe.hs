module TestSuite.Tests.BuildExe (testGroupBuildExe) where

import Data.Monoid
import System.FilePath
import System.Exit
import System.Process
import Test.Tasty
import Test.HUnit
import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy.Char8 as L (unlines)

import IdeSession
import TestSuite.State
import TestSuite.Session
import TestSuite.Assertions

testGroupBuildExe :: TestSuiteEnv -> TestTree
testGroupBuildExe env = testGroup "Build executable" $ exeTests env [
    stdTest env "From some .lhs files"                                  test_fromLhsFiles
  , stdTest env "From some .lhs files with dynamic include path change" test_fromLhsFiles_DynamicIncludePathChange
  , stdTest env "Build executable from 2 TH files"                      test_2TH
  , stdTest env "Build executable from Main"                            test_fromMain
  , stdTest env "Build executable from Main with explicit -package"     test_explicitPackage
  , stdTest env "Build executable from ParFib.Main"                     test_ParfibMain
  , stdTest env "Build executable with a wrong filename and fail"       test_wrongFilename
  , stdTest env "buildExe on code with type errors (#160)"              test_typeErrors
  ]

-- TODO: We should mark this session as dont-reuse (there is no point)
test_fromLhsFiles :: TestSuiteEnv -> Assertion
test_fromLhsFiles env = withAvailableSession' env (withIncludes ["test/compiler/utils"]) $ \session -> do
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

    let m2   = "Exception"
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

    let m3   = "Main"
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

test_2TH :: TestSuiteEnv -> Assertion
test_2TH env = withAvailableSession env $ \session -> do
    updateSessionD session upd 2
    assertNoErrors session
    let m    = "Main"
        upd2 = buildExe [] [(T.pack m, "B.hs")]
    updateSessionD session upd2 3
    distDir <- getDistDir session
    buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
    assertEqual "buildStderr empty" "" buildStderr
  where
    upd = updateCodeGeneration True
       <> (updateSourceFile "A.hs" . L.unlines $
            [ "{-# LANGUAGE TemplateHaskell #-}"
            , "module A where"
            , "import Language.Haskell.TH"
            , "ex1 :: Q Exp"
            , "ex1 = [| \\x -> x |]"
            , "ex2 :: Q Type"
            , "ex2 = [t| String -> String |]"
            , "ex3 :: Q [Dec]"
            , "ex3 = [d| foo x = x |]"
            ])
       <> (updateSourceFile "B.hs" . L.unlines $
            [ "{-# LANGUAGE TemplateHaskell #-}"
            , "module Main where"
            , "import A"
              -- Types and expressions
            , "ex5 :: $ex2"
            , "ex5 = $ex1"
              -- Just to test slightly larger expressions
            , "ex6 :: $(return =<< ex2)"
            , "ex6 = $(ex1 >>= return)"
              -- Declarations
            , "$ex3"
              -- Outcome
            , "main :: IO ()"
            , "main = print $ $ex1 42"
            ])

test_fromMain :: TestSuiteEnv -> Assertion
test_fromMain env = withAvailableSession env $ \session -> do
    withCurrentDirectory "test/MainModule" $ do
      loadModulesFrom session "."
      assertNoErrors session
    let m   = "Main"
        upd = buildExe [] [(T.pack m, "ParFib.hs")]
    updateSessionD session upd 3
    distDir <- getDistDir session
    fibOut <- readProcess (distDir </> "build" </> m </> m) [] []
    assertEqual "ParFib exe output"
                "running 'A single file with a code to run in parallel' from test/MainModule, which says fib 24 = 75025\n"
                fibOut
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "running 'A single file with a code to run in parallel' from test/MainModule, which says fib 24 = 75025\n"
                outExe
    assertEqual "after runExe" ExitSuccess statusExe

test_explicitPackage :: TestSuiteEnv -> Assertion
test_explicitPackage env = withAvailableSession' env (withGhcOpts packageOpts) $ \session -> do
    withCurrentDirectory "test/MainModule" $ do
      loadModulesFrom session "."
      assertNoErrors session
    let m   = "Main"
        upd = buildExe [] [(T.pack m, "ParFib.hs")]
    updateSessionD session upd 3
    distDir <- getDistDir session
    fibOut <- readProcess (distDir </> "build" </> m </> m) [] []
    assertEqual "ParFib exe output"
                "running 'A single file with a code to run in parallel' from test/MainModule, which says fib 24 = 75025\n"
                fibOut
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "running 'A single file with a code to run in parallel' from test/MainModule, which says fib 24 = 75025\n"
                outExe
    assertEqual "after runExe" ExitSuccess statusExe
  where
    packageOpts = [ "-hide-all-packages"
                  , "-package base"
                  , "-package parallel"
                  , "-package old-time"
                  ]

test_ParfibMain :: TestSuiteEnv -> Assertion
test_ParfibMain env = withAvailableSession env $ \session -> do
    withCurrentDirectory "test/MainModule" $ do
      loadModulesFrom session "."
      assertNoErrors session
    let m   = "ParFib.Main"
        upd = buildExe [] [ (T.pack m, "ParFib.Main.hs")
                          , (T.pack "Main", "ParFib.hs") ]
    updateSessionD session upd 4
    assertNoErrors session
    let upd2 = buildExe [] [(T.pack "Main", "ParFib.hs")]
    updateSessionD session upd2 0
    distDir <- getDistDir session
    fibOut <- readProcess (distDir </> "build" </> m </> m) [] []
    assertEqual "ParFib exe output"
                "running 'A single file with a code to run in parallel' from test/MainModule, which says fib 24 = 75025\n"
                fibOut
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "running 'A single file with a code to run in parallel' from test/MainModule, which says fib 24 = 75025\n"
                outExe
    assertEqual "after runExe" ExitSuccess statusExe

test_wrongFilename :: TestSuiteEnv -> Assertion
test_wrongFilename env = withAvailableSession env $ \session -> do
    withCurrentDirectory "test/MainModule" $ do
      loadModulesFrom session "."
      assertNoErrors session
    let m   = "Main"
        upd = buildExe [] [(T.pack m, "foooooooooooooooo.hs")]
    updateSessionD session upd 1
    assertNoErrors session
    status1 <- getBuildExeStatus session
    assertEqual "failure after exe build" (Just $ ExitFailure 1) status1

test_typeErrors :: TestSuiteEnv -> Assertion
test_typeErrors env = withAvailableSession env $ \session -> do
    updateSessionD session upd1 1
    assertOneError session

    updateSessionD session upd2 1

    distDir <- getDistDir session
    status <- getBuildExeStatus session
    assertEqual "" (Just $ ExitFailure 1) status
    buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
    assertEqual "" "Source errors encountered. Not attempting to build executables." buildStderr
  where
    upd1 = (updateCodeGeneration True)
        <> (updateSourceFile "Main.hs" "main = foo")
    upd2 = buildExe [] [("Main", "Main.hs")]
