-- Tests for specific issues
module TestSuite.Tests.Issues (testGroupIssues) where

import Prelude hiding (span, mod)
import Control.Concurrent
import Control.Monad
import Data.List (isInfixOf)
import Data.Monoid
import System.Exit
import System.FilePath
import System.Process
import System.Timeout
import Test.HUnit
import Test.Tasty
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L (unlines)
import qualified Data.ByteString.Lazy.UTF8  as L
import qualified Data.Text                  as T

import IdeSession
import TestSuite.State
import TestSuite.Session
import TestSuite.Assertions

testGroupIssues :: TestSuiteEnv -> TestTree
testGroupIssues env = testGroup "Issues" [
    stdTest env "#119"                                                                            test119
  , stdTest env "#125: Hang when snippet calls createProcess with close_fds set to True"          test125
  , stdTest env "#118: ghc qAddDependentFile patch"                                               test118
  , withOK  env " #32: Paths in type errors"                                                      test32
  , stdTest env "#134: Updating dependent data files multiple times per second"                   test134
  , stdTest env "#115: Dynamically setting/unsetting -Werror"                                     test115
  , stdTest env "#185: Invalid GHC option and option warnings"                                    test185
  , stdTest env "#145: GHC bug #8333"                                                             test145
  , stdTest env "#170: GHC API expects 'main' to be present in 'Main'"                            test170_GHC
  , stdTest env "#170: buildExe doesn't expect 'Main.main' to be present nor to be in IO"         test170_buildExe
  , stdTest env "#169: Data files should not leak into compilation if referenced"                 test169
  , stdTest env "#169: Data files should not leak in exe building"                                test169_buildExe
  , stdTest env "#169: Data files should not leak in exe building, with a extra modules"          test169_buildExe_extraModules
  , stdTest env "#169: Data files should not leak in exe building, with a non-'Main' main module" test169_buildExe_nonMain
  , withOK  env "#194: Start server without bracket (need MANUAL check that server will die)"     test194
  , stdTest env "#213: Missing location information"                                              test213
  , stdTest env "#214: Changing linker flags"                                                     test214
  , stdTest env "#219: runStmt gets corrupted by async exceptions"                                test219
  , stdTest env "#220: Calling forceCancel can have detrimental side-effects"                     test220
  , stdTest env " #94: Quickfix for Updating static files never triggers recompilation"           test94
  , stdTest env " #94: Quickfix for Updating static files never triggers --- illegal var name"    test94_illegalVarName
  , stdTest env " #94: Quickfix for Updating static files never triggers --- missing file"        test94_missingFile
  ]

test119 :: TestSuiteEnv -> Assertion
test119 env = withAvailableSession env $ \session -> do
    distDir <- getDistDir session

    let cb = \_ -> return ()
        update = flip (updateSession session) cb

    update $ updateCodeGeneration True
    update $ updateStdoutBufferMode (RunLineBuffering Nothing)
    update $ updateStderrBufferMode (RunLineBuffering Nothing)

    -- Compile code and execute

    update $ updateSourceFile "src/Main.hs" "main = putStrLn \"Version 1\""
    assertNoErrors session

    update $ buildExe [] [(T.pack "Main", "src/Main.hs")]
    out1 <- readProcess (distDir </> "build" </> "Main" </> "Main") [] []
    assertEqual "" "Version 1\n" out1
    runActionsExe <- runExe session "Main"
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "Version 1\n"
                outExe
    assertEqual "after runExe" ExitSuccess statusExe

    -- Update the code and execute again

    update $ updateSourceFile "src/Main.hs" "main = putStrLn \"Version 2\""
    assertNoErrors session

    update $ buildExe [] [(T.pack "Main", "src/Main.hs")]
    out2 <- readProcess (distDir </> "build" </> "Main" </> "Main") [] []
    assertEqual "" "Version 2\n" out2
    runActionsExe2 <- runExe session "Main"
    (outExe2, statusExe2) <- runWaitAll runActionsExe2
    assertEqual "Output from runExe"
                "Version 2\n"
                outExe2
    assertEqual "after runExe" ExitSuccess statusExe2

test125 :: TestSuiteEnv -> Assertion
test125 env = withAvailableSession env $ \session -> do
    let cb     = \_ -> return ()
        update = flip (updateSession session) cb

    update $ updateCodeGeneration True
    update $ updateStdoutBufferMode (RunLineBuffering Nothing)
    update $ updateStderrBufferMode (RunLineBuffering Nothing)
    update $ updateDynamicOpts ["-Wall", "-Werror"]

    update $ updateSourceFile "src/Main.hs" $ L.unlines [
        "module Main where"

      , "import System.Process"
      , "import System.IO"

      , "main :: IO ()"
      , "main = do"
      , "    (_,Just maybeOut,_,pr) <- createProcess $ (shell \"foo\")"
      , "        { cmdspec      = ShellCommand \"echo 123\""
      , "        , cwd          = Nothing"
      , "        , env          = Nothing"
      , "        , std_in       = CreatePipe"
      , "        , std_out      = CreatePipe"
      , "        , std_err      = CreatePipe"
      , "        , close_fds    = True"
      , "        , create_group = False"
      , "        }"
      , "    putStr =<< hGetContents maybeOut"
      , "    terminateProcess pr"
      ]

    assertNoErrors session
    mRunActions <- timeout 2000000 $ runStmt session "Main" "main"
    case mRunActions of
      Just runActions -> do
        mRunResult <- timeout 2000000 $ runWaitAll runActions
        case mRunResult of
          Just (output, RunOk) -> assertEqual "" "123\n" output
          Nothing -> assertFailure "Timeout in runWaitAll"
          _       -> assertFailure "Unexpected run result"
      Nothing ->
        assertFailure "Timeout in runStmt"

test118 :: TestSuiteEnv -> Assertion
test118 env = withAvailableSession env $ \session -> do
    let cb     = \_ -> return ()
        update = flip (updateSession session) cb

    update $ mconcat
        [ updateSourceFile "Main.hs" mainContents
        , updateDataFile "foo.hamlet" "invalid"
        ]

    -- Error message expected, invalid data file
    assertOneError session

    update $ updateDataFile "foo.hamlet" "42"
    assertNoErrors session

    update $ updateSourceFile "Main.hs" $ mainContents `L.append` "\n\n-- Trigger a recompile"
    assertNoErrors session

    update $ updateDataFile "foo.hamlet" "invalid"
    assertOneError session
  where
    mainContents = L.unlines
      [ "{-# LANGUAGE TemplateHaskell #-}"
      , "import Language.Haskell.TH.Syntax"
      , "main = print ($(do"
      , "  qAddDependentFile \"foo.hamlet\""
      , "  s <- qRunIO $ readFile \"foo.hamlet\""
      , "  lift $ (read s :: Int)"
      , "  ) :: Int)"
      ]

test32 :: TestSuiteEnv -> IO String
test32 env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    errs <- getSourceErrors session
    let none p = all (not . p)
    let containsFullPath e =
          "session." `isInfixOf` T.unpack (errorMsg e)
    fixme session "#32" $ assertBool "" (none containsFullPath errs)
  where
    upd = (updateSourceFile "A.hs" . L.unlines $
            [ "module A where"
            , "f x = show . read"
            ])

test134 :: TestSuiteEnv -> Assertion
test134 env = withAvailableSession env $ \session -> do
    let cb     = \_ -> return ()
        update = flip (updateSession session) cb
        str i  = L.fromString $ show (i :: Int) ++ "\n"

    update $ mconcat
        [ updateSourceFile "Main.hs" mainContents
        , updateDataFile "foo.hamlet" (str 1)
        , updateCodeGeneration True
        ]
    assertNoErrors session

    do runActions <- runStmt session "Main" "main"
       (output, result) <- runWaitAll runActions
       assertEqual "" RunOk result
       assertEqual "" (str 1) output

    forM_ [2 .. 99] $ \i -> do
      update $ updateDataFile "foo.hamlet" (str i)
      runActions <- runStmt session "Main" "main"
      (output, result) <- runWaitAll runActions
      assertEqual "" RunOk result
      assertEqual "" (str i) output
  where
    mainContents = L.unlines
        [ "{-# LANGUAGE TemplateHaskell #-}"
        , "import Language.Haskell.TH.Syntax"
        , "main = print ($(do"
        , "  qAddDependentFile \"foo.hamlet\""
        , "  s <- qRunIO $ readFile \"foo.hamlet\""
        , "  lift $ (read s :: Int)"
        , "  ) :: Int)"
        ]

test115 :: TestSuiteEnv -> Assertion
test115 env = withAvailableSession env $ \session -> do
    updateSessionD session upd1 1
    errs1 <- getSourceErrors session
    unless (any (== KindError) $ map errorKind errs1) $
      assertFailure $ "Expected some errors in " ++ show3errors errs1

    updateSessionD session upd2 1
    errs2 <- getSourceErrors session
    when (any (== KindError) $ map errorKind errs2) $
      assertFailure $ "Expected only warnings in " ++ show3errors errs2
  where
    upd1 = (updateDynamicOpts ["-Wall", "-Werror"])
        <> (updateSourceFile "src/Main.hs" $ L.unlines [
               "module Main where"
             , "main = putStrLn \"Hello 1\""
             ])

    upd2 = (updateDynamicOpts ["-Wall"])
        <> (updateSourceFile "src/Main.hs" $ L.unlines [
               "module Main where"
             , "main = putStrLn \"Hello 2\""
             ])

test185 :: TestSuiteEnv -> Assertion
test185 env = withAvailableSession env $ \session -> do
    updateSessionD session upd 0
    errs <- getSourceErrors session
    -- We expect two warnings (one deprecated, one unrecognized)
    assertEqual "" 2 (length errs)
  where
    upd = updateDynamicOpts ["-fglasgow-exts","-thisOptionDoesNotExist"]

test145 :: TestSuiteEnv -> Assertion
test145 env = withAvailableSession env $ \session -> do
    updateSessionD session upd1 1
    assertNoErrors session
  where
    upd1 = (updateCodeGeneration True)
        <> (updateDynamicOpts ["-XScopedTypeVariables", "-O"])
        <> (updateSourceFile "Main.hs" "main = let (x :: String) = \"hello\" in putStrLn x")

test170_GHC :: TestSuiteEnv -> Assertion
test170_GHC env = withAvailableSession env $ \session -> do
    updateSessionD session update 2
    assertOneError session
  where
    update = updateDataFile   "Data/Monoid.hs" "module Data.Monoid where\nfoo = doesnotexist"
          <> updateSourceFile "Main.hs"        "module Main where\nimport Data.Monoid\nmain2222 = return ()"

test170_buildExe :: TestSuiteEnv -> Assertion
test170_buildExe env = withAvailableSession env $ \session -> do
    updateSessionD session update 2
    assertNoErrors session
    let updE = buildExe [] [(T.pack "Main", "Main.hs")]
    updateSessionD session updE 1
    status <- getBuildExeStatus session
    assertEqual "buildExe doesn't know 'main' is in IO"
      (Just $ ExitFailure 1) status
  where
    update = updateDataFile   "Data/Monoid.hs" "module Data.Monoid where\nfoo = doesnotexist"
          <> updateSourceFile "Main.hs"        "module Main where\nimport Data.Monoid\nmain = return ()"

test169 :: TestSuiteEnv -> Assertion
test169 env = withAvailableSession env $ \session -> do
    updateSessionD session update 2
    assertNoErrors session
  where
    update = updateDataFile   "Data/Monoid.hs" "module Data.Monoid where\nfoo = doesnotexist"
          <> updateSourceFile "Main.hs"        "module Main where\nimport Data.Monoid\nmain = return ()"

test169_buildExe :: TestSuiteEnv -> Assertion
test169_buildExe env = withAvailableSession env $ \session -> do
    updateSessionD session update 2
    assertNoErrors session
    let updE = buildExe [] [(T.pack "Main", "Main.hs")]
    updateSessionD session updE 2
    distDir <- getDistDir session
    buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
    assertEqual "" "" buildStderr
    status <- getBuildExeStatus session
    assertEqual "after exe build" (Just ExitSuccess) status

    let updDoc = buildDoc
    updateSessionD session updDoc 1
    statusDoc <- getBuildDocStatus session
    assertEqual "after doc build" (Just ExitSuccess) statusDoc
  where
    update = updateDataFile   "Data/Monoid.hs" "module Data.Monoid where\nfoo = doesnotexist"
          <> updateSourceFile "Main.hs"        "module Main where\nimport Data.Monoid\nmain :: IO()\nmain = return ()"

test169_buildExe_extraModules :: TestSuiteEnv -> Assertion
test169_buildExe_extraModules env = withAvailableSession env $ \session -> do
    updateSessionD session update 4
    assertNoErrors session
    let updE = buildExe [] [(T.pack "Main", "Main.hs")]
    updateSessionD session updE 4
    distDir <- getDistDir session
    buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
    assertEqual "" "" buildStderr
    status <- getBuildExeStatus session
    assertEqual "after exe build" (Just ExitSuccess) status

    let updDoc = buildDoc
    updateSessionD session updDoc 1
    statusDoc <- getBuildDocStatus session
    assertEqual "after doc build" (Just ExitSuccess) statusDoc
  where
    update = updateDataFile   "Data/Monoid.hs" "module Data.Monoid where\nfoo = doesnotexist"
          <> updateSourceFile "Main.hs"        "module Main where\nimport Data.Monoid\nmain :: IO()\nmain = return ()"
          <> updateSourceFile "NonMain.hs"     "module NonMain where\nimport Data.Monoid\nmain :: IO()\nmain = return ()"
          <> updateSourceFile "NonMain2.hs"    "module NonMain2 where\nimport Data.Monoid\nmain :: IO()\nmain = return ()"

test169_buildExe_nonMain :: TestSuiteEnv -> Assertion
test169_buildExe_nonMain env = withAvailableSession env $ \session -> do
    updateSessionD session update 4
    assertNoErrors session
    let updE = buildExe [] [(T.pack "NonMain", "NonMain.hs")]
    updateSessionD session updE 4
    distDir <- getDistDir session
    buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
    assertEqual "" "" buildStderr
    status <- getBuildExeStatus session
    assertEqual "after exe build" (Just ExitSuccess) status

    let updDoc = buildDoc
    updateSessionD session updDoc 1
    statusDoc <- getBuildDocStatus session
    assertEqual "after doc build" (Just ExitSuccess) statusDoc
  where
    update = updateDataFile "Data/Monoid.hs" "module Data.Monoid where\nfoo = doesnotexist"
          <> updateSourceFile "Main.hs"      "module Main where\nimport Data.Monoid\nmain :: IO()\nmain = return ()"
          <> updateSourceFile "NonMain.hs"   "module NonMain where\nimport Data.Monoid\nmain :: IO()\nmain = return ()"
          <> updateSourceFile "NonMain2.hs"  "module NonMain2 where\nimport Data.Monoid\nmain :: IO()\nmain = return ()"

test194 :: TestSuiteEnv -> IO String
test194 env = do
    _session <- startNewSession (defaultServerConfig env)
    threadDelay 2000000
    -- TODO: Add process ID in OK message
    return "PLEASE VERIFY THAT SERVER HAS TERMINATED"

test213 :: TestSuiteEnv -> Assertion
test213 env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    version <- getGhcVersion session
    case version of
      GHC742 -> -- 7.4.2 just reports the first module error
        assertSourceErrors session [
            [(Just "Main.hs", "Could not find module")]
          ]
      GHC78 -> -- 7.8 reports both; make sure we have location info (#213)
        assertSourceErrors session [
            [(Just "Main.hs", "Could not find module")]
          , [(Just "Main.hs", "Could not find module")]
          ]
  where
    upd = updateSourceFile "Main.hs" $ L.unlines
        [ "import DoesNotExist1"
        , "import DoesNotExist2"
        ]

test214 :: TestSuiteEnv -> Assertion
test214 env = withAvailableSession env $ \session -> do
    updateSessionD session upd 2
    assertNoErrors session
  where
    upd = updateCodeGeneration True
       <> updateDynamicOpts ["-lz"]
       <> updateSourceFile "Main.hs" "import GHC.Prim"
       <> updateSourceFile "foo.c" (L.unlines
            [ "#include <zlib.h>"
            , "int streaming_commons_inflate_init2(z_stream *stream, int window_bits) {"
            , "return inflateInit2(stream, window_bits);}"
            ])

test219 :: TestSuiteEnv -> Assertion
test219 env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session

    _ <- timeout 1 $ runStmt session "Main" "main"

    runActions <- runStmt session "Main" "main"
    (_output, result) <- runWaitAll runActions
    assertEqual "" RunOk result
  where
    upd = updateSourceFile "Main.hs" "main = return ()"
       <> updateCodeGeneration True

test220 :: TestSuiteEnv -> Assertion
test220 env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session

    replicateM_ 100 $ do
        runActions <- runStmt session "Main" "main"
        (_output, result) <- runWaitAll runActions
        assertEqual "" RunOk result
        forceCancel runActions
  where
    upd = updateSourceFile "Main.hs" "main = return ()"
       <> updateCodeGeneration True

test94 :: TestSuiteEnv -> Assertion
test94 env = withAvailableSession env $ \session -> do
     updateSessionD session upd 1
     assertOneError session

     updateSessionD session upd2 1
     assertNoErrors session
  where
     upd = updateDataFile "A.foo" "unboundVarName"
        <> (updateSourceFile "A.hs" . L.unlines $
             [ "{-# LANGUAGE TemplateHaskell #-}"
             , "module A where"
             , "import Language.Haskell.TH.Syntax"
             , "goodVarName = 42"
             , "foo :: Int"
             , "foo = $(do"
             , "          qAddDependentFile \"A.foo\""
             , "          s <- qRunIO (readFile \"A.foo\")"
             , "          return $ VarE (mkName s))"
             ])

     upd2 = updateDataFile "A.foo" "goodVarName"

test94_illegalVarName :: TestSuiteEnv -> Assertion
test94_illegalVarName env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertOneError session

    updateSessionD session upd2 1
    assertNoErrors session
  where
    upd = updateDataFile "A.foo" "42 is a wrong var name"
       <> (updateSourceFile "A.hs" . L.unlines $
            [ "{-# LANGUAGE TemplateHaskell #-}"
            , "module A where"
            , "import Language.Haskell.TH.Syntax"
            , "goodVarName = 42"
            , "foo :: Int"
            , "foo = $(do"
            , "          qAddDependentFile \"A.foo\""
            , "          s <- qRunIO (readFile \"A.foo\")"
            , "          return $ VarE (mkName s))"
            ])

    upd2 = updateDataFile "A.foo" "goodVarName"

test94_missingFile :: TestSuiteEnv -> Assertion
test94_missingFile env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertOneError session

    updateSessionD session upd2 1
    assertNoErrors session
  where
    upd = (updateSourceFile "A.hs" . L.unlines $
            [ "{-# LANGUAGE TemplateHaskell #-}"
            , "module A where"
            , "import Language.Haskell.TH.Syntax"
            , "foo :: String"
            , "foo = $(do"
            , "          qAddDependentFile \"A.foo\""
            , "          x <- qRunIO (readFile \"A.foo\")"
            , "          lift x)"
            ])

    upd2 = updateDataFile "A.foo" "fooString"
