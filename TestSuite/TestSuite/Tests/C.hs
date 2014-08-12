module TestSuite.Tests.C (testGroupC) where

import Data.Monoid
import System.Exit
import Test.Tasty
import Test.HUnit
import qualified Data.ByteString.Lazy.Char8 as L (unlines)
import qualified Data.Text                  as T

import IdeSession
import TestSuite.Assertions
import TestSuite.Session
import TestSuite.State

testGroupC :: TestSuiteEnv -> TestTree
testGroupC env = testGroup "Using C files" [
    stdTest env "Basic functionality, recompiling Haskell modules when necessary" test_Basic
  , stdTest env "C files in subdirs"                                              test_subdirs
  , stdTest env "Errors and warnings in the C code"                               test_errors
  , stdTest env "Errors in C file, then update C file (#201)"                     test_errorsThenUpdate
  , stdTest env "C header files in subdirectories (#212)"                         test_headersInSubdirs
  , stdTest env "C code writes to stdout (#210)"                                  test_stdout
  ]

test_Basic :: TestSuiteEnv -> Assertion
test_Basic env = withAvailableSession env $ \session -> do
    -- TODO: Ideally, we'd fix this jump in the reported total number of
    -- progress messages
    updateSessionP session upd [
        (1, 1, "Compiling MC.c")
      , (2, 2, "Compiling M")
      ]
    assertNoErrors session
    do runActions <- runStmt session "M" "hello"
       (output, result) <- runWaitAll runActions
       assertEqual "" RunOk result
       assertEqual "" "12345\n" output

    do let m = "M"
           updExe = buildExe [] [(T.pack m, "M.hs")]
       updateSessionD session updExe 2
       runActionsExe <- runExe session m
       (outExe, statusExe) <- runWaitAll runActionsExe
       assertEqual "Output from runExe"
                   "12345\n"
                   outExe
       assertEqual "after runExe" ExitSuccess statusExe

    -- Update the Haskell module without updating the C module
    updateSessionP session upd2 [
        (1, 1, "Compiling M")
      ]
    assertNoErrors session
    do runActions <- runStmt session "M" "hello"
       (output, result) <- runWaitAll runActions
       assertEqual "" RunOk result
       assertEqual "" "12346\n" output

    do let m = "M"
           updExe = buildExe [] [(T.pack m, "M.hs")]
       updateSessionD session updExe 2
       runActionsExe <- runExe session m
       (outExe, statusExe) <- runWaitAll runActionsExe
       assertEqual "Output from runExe"
                   "12346\n"
                   outExe
       assertEqual "after runExe" ExitSuccess statusExe

    -- Update the C code without updating the Haskell module
    updateSessionP session upd3 [
        (1, 1, "Compiling MC.c")
      , (2, 2, "Compiling M")
      ]
    assertNoErrors session
    do runActions <- runStmt session "M" "hello"
       (output, result) <- runWaitAll runActions
       assertEqual "" RunOk result
       assertEqual "" "54322\n" output
    let m = "M"
        updExe = buildExe [] [(T.pack m, "M.hs")]
    updateSessionD session updExe 3
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "54322\n"
                outExe
    assertEqual "after runExe" ExitSuccess statusExe
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "import Foreign.C"
            , "foreign import ccall \"foo\" c_f :: CInt"
            , "hello :: IO ()"
            , "hello = print c_f"
            , "main :: IO ()"
            , "main = hello"
            ])
       <> (updateSourceFile "MC.c" . L.unlines $
            [ "int foo() {"
            , "  return 12345;"
            , "}"
            ])

    upd2 = (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "import Foreign.C"
            , "foreign import ccall \"foo\" c_f :: CInt"
            , "hello :: IO ()"
            , "hello = print (c_f + 1)"
            , "main :: IO ()"
            , "main = hello"
            ])

    upd3 = (updateSourceFile "MC.c" . L.unlines $
            [ "int foo() {"
            , "  return 54321;"
            , "}"
            ])

test_subdirs :: TestSuiteEnv -> Assertion
test_subdirs env = withAvailableSession env $ \session -> do
    updateSessionP session upd [
        (1, 2, "Compiling a/MC.c")
      , (2, 2, "Compiling b/MC.c")
      , (3, 3, "Compiling M")
      ]
    assertNoErrors session
    do runActions <- runStmt session "M" "hello"
       (output, result) <- runWaitAll runActions
       assertEqual "" RunOk result
       assertEqual "" "79\n" output
    let m = "M"
        updExe = buildExe [] [(T.pack m, "M.hs")]
    updateSessionD session updExe 2
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "79\n"
                outExe
    assertEqual "after runExe" ExitSuccess statusExe
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "import Foreign.C"
            , "foreign import ccall \"foo\" c_f :: CInt"
            , "foreign import ccall \"bar\" c_g :: CInt"
            , "hello :: IO ()"
            , "hello = print (c_f + c_g)"
            , "main :: IO ()"
            , "main = hello"
            ])
       <> (updateSourceFile "a/MC.c" . L.unlines $
            [ "int foo() {"
            , "  return 56;"
            , "}"
            ])
            -- intentionally same name for the file
       <> (updateSourceFile "b/MC.c" . L.unlines $
            [ "int bar() {"
            , "  return 23;"
            , "}"
            ])

test_errors :: TestSuiteEnv -> Assertion
test_errors env = withAvailableSession env $ \session -> do
    updateSessionP session upd [
        (1, 1, "Compiling MC.c")
      , (2, 2, "Compiling M")
      ]
    errors <- getSourceErrors session
    case errors of
      [e1, e2] -> do
        -- Currently we generate precisely one gcc error
        assertEqual "" (TextSpan (T.pack "<gcc error>")) (errorSpan e1)
        -- The ByteCodeLink exception because of the missing symbol
        assertEqual "" (TextSpan (T.pack "<from GhcException>")) (errorSpan e2)
      _ -> assertFailure $ "Unexpected errors: " ++ show errors
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "import Foreign.C"
            , "foreign import ccall \"foo\" c_f :: CInt"
            , "hello :: IO ()"
            , "hello = print c_f"
            ])
       <> (updateSourceFile "MC.c" . L.unlines $ [
              "int f() {"
            , "  return thisSymbolDoesNotExist;"
            , "}"
            , ""
            , "void g() {"
            , "  return 1;"
            , "}"
            ])

test_errorsThenUpdate :: TestSuiteEnv -> Assertion
test_errorsThenUpdate env = withAvailableSession env $ \session -> do
    updateSessionD session upd 3
    assertNoErrors session

    runActions <- runStmt session "Main" "main"
    (output, result) <- runWaitAll runActions
    assertEqual "" result RunOk
    assertEqual "" output "42\n"

    updateSessionD session (updateSourceFile "test.c" "invalid") 3
    assertSomeErrors session

    updateSessionD session (updateSourceFile "test.c" cLBS) 3
    assertNoErrors session
  where
    mainLBS = L.unlines $ [
        "import Foreign"
      , "import Foreign.C"
      , "import Foreign.C.Types"
      , "foreign import ccall safe \"test_c_func\" test_c_func :: CInt"
      , "main = print test_c_func"
      ]
    cLBS = L.unlines $ [
        "int test_c_func() { return 42; }"
      ]

    upd = updateCodeGeneration True
       <> updateSourceFile "Main.hs" mainLBS
       <> updateSourceFile "test.c" cLBS

test_headersInSubdirs :: TestSuiteEnv -> Assertion
test_headersInSubdirs env = withAvailableSession' env (withIncludes ["include"]) $ \session -> do
    let go upd = do
            updateSessionD session upd 3
            assertNoErrors session

    go $ updateGhcOpts ["-Iinclude"]
      <> updateSourceFile "include/blankheader.h" hfile
      <> updateSourceFile "hello.c" cfile
      <> updateSourceFile "Main.hs" hsfile
  where
    hfile = "#define foo \"hello\\n\""
    cfile = L.unlines $
        [ "#include <stdio.h>"
        , "#include <blankheader.h>"
        , "void hello(void) { printf(foo); }"
        ]
    hsfile = L.unlines $
        [ "{-# LANGUAGE ForeignFunctionInterface #-}"
        , "module Main where"
        , "foreign import ccall \"hello\" hello :: IO ()"
        , "main = hello"
        ]

test_stdout :: TestSuiteEnv -> Assertion
test_stdout env = withAvailableSession env $ \session -> do
    updateSessionD session upd 3
    assertNoErrors session

    ra <- runStmt session "Main" "main"
    (output, result) <- runWaitAll ra
    assertEqual "" result RunOk
    assertEqual "" output "hello\n"
  where
    cfile = L.unlines $
        [ "#include <stdio.h>"
        , "void hello(void) { printf(\"hello\\n\"); }"
        ]
    hsfile = L.unlines $
        [ "{-# LANGUAGE ForeignFunctionInterface #-}"
        , "module Main where"
        , "import System.IO"
        , "foreign import ccall \"hello\" hello :: IO ()"
        , "main = hello"
        ]

    upd = updateCodeGeneration True
       <> updateSourceFile "hello.c" cfile
       <> updateSourceFile "Main.hs" hsfile

