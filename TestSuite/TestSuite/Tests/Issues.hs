-- Tests for specific issues
{-# LANGUAGE CPP #-}
module TestSuite.Tests.Issues (testGroupIssues) where

import Prelude hiding (span, mod)
import Control.Concurrent
import Control.Exception (bracket)
import Control.Monad
import Data.List (isInfixOf)
import Data.Monoid
import System.Exit
import System.FilePath
import System.Process
import System.Timeout
import Test.HUnit
import Test.Tasty
import qualified Data.ByteString.Lazy       as L hiding (all)
import qualified Data.ByteString.Lazy.Char8 as L (unlines, all)
import qualified Data.ByteString.Lazy.UTF8  as L
import qualified Data.Text                  as T

import IdeSession
import TestSuite.State
import TestSuite.Session
import TestSuite.Assertions

testGroupIssues :: TestSuiteEnv -> TestTree
testGroupIssues env = testGroup "Issues" $ [
    withOK  env " #32: Paths in type errors"                                                      test32
  , stdTest env " #94: Quickfix for Updating static files never triggers --- illegal var name"    test94_illegalVarName
  , stdTest env " #94: Quickfix for Updating static files never triggers --- missing file"        test94_missingFile
  , stdTest env " #94: Quickfix for Updating static files never triggers recompilation"           test94
  , stdTest env "#115: Dynamically setting/unsetting -Werror"                                     test115
  , stdTest env "#118: ghc qAddDependentFile patch"                                               test118
  , stdTest env "#125: Hang when snippet calls createProcess with close_fds set to True"          test125
  , stdTest env "#134: Updating dependent data files multiple times per second"                   test134
  , stdTest env "#145: GHC bug #8333"                                                             test145
  , stdTest env "#169: Data files should not leak into compilation if referenced"                 test169
  , stdTest env "#170: GHC API expects 'main' to be present in 'Main'"                            test170_GHC
  , stdTest env "#185: Invalid GHC option and option warnings"                                    test185
  , stdTest env "#191: Limit stack size (#191)"                                                   test191
  , withOK  env "#194: Start server without bracket (need MANUAL check that server will die)"     test194
  , stdTest env "#213: Missing location information"                                              test213
  , stdTest env "#214: Changing linker flags"                                                     test214
  , stdTest env "#219: runStmt gets corrupted by async exceptions"                                test219
  , stdTest env "#220: Calling forceCancel can have detrimental side-effects"                     test220
  , stdTest env "#224: Package flags are not reset correctly"                                     test224
  , stdTest env "#229: Client library does not detect snippet server crash"                       test229
  ] ++ exeTests env [
    stdTest env "#119: Re-building an executable after a code change does not rebuild"            test119
  , stdTest env "#169: Data files should not leak in exe building"                                test169_buildExe
  , stdTest env "#169: Data files should not leak in exe building, with a extra modules"          test169_buildExe_extraModules
  , stdTest env "#169: Data files should not leak in exe building, with a non-'Main' main module" test169_buildExe_nonMain
  , stdTest env "#170: buildExe doesn't expect 'Main.main' to be present nor to be in IO"         test170_buildExe
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
    update $ updateGhcOpts ["-Wall", "-Werror"]

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
    upd1 = (updateGhcOpts ["-Wall", "-Werror"])
        <> (updateSourceFile "src/Main.hs" $ L.unlines [
               "module Main where"
             , "main = putStrLn \"Hello 1\""
             ])

    upd2 = (updateGhcOpts ["-Wall"])
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
    upd = updateGhcOpts ["-fglasgow-exts","-thisOptionDoesNotExist"]

test145 :: TestSuiteEnv -> IO ()
test145 env = withAvailableSession env $ \session -> do
    -- Setting -O is officially no longer supported from 7.10 and up
    -- See <https://ghc.haskell.org/trac/ghc/ticket/10052>
    if testSuiteEnvGhcVersion env >= GHC_7_10
      then skipTest "-O no longer supported"
      else do
        updateSessionD session upd1 1
        assertNoErrors session
  where
    upd1 = (updateCodeGeneration True)
        <> (updateGhcOpts ["-XScopedTypeVariables", "-O"])
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
    if version < GHC_7_8
      then
        -- 7.4.2 just reports the first module error
        assertSourceErrors session [
            [(Just "Main.hs", "Could not find module")]
          ]
      else
        -- 7.8 and up report both; make sure we have location info (#213)
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
    updateSessionD session setup 0

    -- Try to load without enabling -lz. Will fail
    updateSessionD session source 2
    assertSomeErrors session

    -- Now set the -lz option, and try again. Should succeed now.
    updateSessionD session linkLz 2
    assertNoErrors session

    -- Actually run the code, just to be sure linking worked properly
    runActions <- runStmt session "Main" "print_zlib_version"
    (output, result) <- runWaitAll runActions
    assertEqual "" RunOk result
    -- We're not really sure what version to expect; but it should probably look
    -- like a version anyway :)
    assertBool ("unexpected version " ++ show output) (isVersion output)
  where
    setup, source, linkLz :: IdeSessionUpdate
    setup  = updateCodeGeneration True
    linkLz = updateGhcOpts ["-lz"]
    source = updateSourceFile "Main.hs" "foreign import ccall \"print_zlib_version\" print_zlib_version :: IO ()"
          <> updateSourceFile "foo.c" (L.unlines
               [ "#include <zlib.h>"
               , "#include <stdio.h>"
               , "void print_zlib_version() {"
               , "  fputs(zlibVersion(), stderr);"
               , "}"
               ])

    isVersion :: L.ByteString -> Bool
    isVersion = L.all (`elem` ('.' : ['0'..'9']))

test219 :: TestSuiteEnv -> Assertion
test219 env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session

    _ <- timeout 1 $
           bracket (runStmt session "Main" "main")
                   (forceCancel)
                   (\_ -> return ())

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

test224 :: TestSuiteEnv -> Assertion
test224 env = withAvailableSession env $ \session -> do
    do updateSessionD session upd1 1
       assertNoErrors session

       runActions <- runStmt session "Main" "main"
       result <- runWaitAll runActions
       assertEqual "" ("Hi 1\n", RunOk) result

    do updateSessionD session upd2 1
       assertNoErrors session

       runActions <- runStmt session "Main" "main"
       result <- runWaitAll runActions
       assertEqual "" ("Hi 2\n", RunOk) result
  where
    upd1 = updateGhcOpts ["-hide-all-packages", "-package base"]
        <> updateCodeGeneration True
        <> updateSourceFile "Main.hs" "main = putStrLn \"Hi 1\""

    upd2 = updateGhcOpts []
        <> updateCodeGeneration True
        <> updateSourceFile "Main.hs" "main = putStrLn \"Hi 2\""

test229 :: TestSuiteEnv -> Assertion
#if defined(darwin_HOST_OS)
test229 env = withAvailableSession env $ \_session -> do
    skipTest "Known failure on OSX"
#else
test229 env = withAvailableSession env $ \session -> do
    updateSessionD session upd 2
    assertNoErrors session

    runActions <- runStmt session "Main" "crash"
    (_output, result) <- runWaitAll runActions
    case result of
      RunGhcException _ -> return ()
      _ -> assertFailure $ "Unexpected run result " ++ show result
  where
    upd :: IdeSessionUpdate
    upd    = updateCodeGeneration True
          <> updateSourceFile "Main.hs" "foreign import ccall \"crash\" crash :: IO ()"
          <> updateSourceFile "foo.c" (L.unlines
               [ "#include <stdio.h>"
               , "void crash() {"
               , "  char *p = NULL;"
               , "  *p = '!';"
               , "  printf(\"We won't get this far\\n\");"
               , "}"
               ])
#endif

test191 :: TestSuiteEnv -> Assertion
test191 env = withAvailableSession env $ \session -> do
    when (testSuiteEnvGhcVersion env == GHC_7_4) $
      skipTest "Known failure on 7.4.2 (#191)"

    updateSessionD session upd 1
    assertNoErrors session

    -- In the default session stack limit is 8M. test1 should fail:
    do runActions <- runStmt session "Main" "test1"
       (_output, result) <- runWaitAll runActions
       case result of
         RunProgException _ -> return ()
         _ -> assertFailure $ "Unexpected run result " ++ show result

    -- But test2 should work:
    do runActions <- runStmt session "Main" "test2"
       (_output, result) <- runWaitAll runActions
       case result of
         RunOk -> return ()
         _ -> assertFailure $ "Unexpected run result " ++ show result

    -- But after we change the stack size, test2 too will crash:
    -- (Note that we cannot set this value _too_ low otherwise the main server
    -- itself, rather than the snippet, will crash. See Issue #266.)
    updateSessionD session (updateRtsOpts ["-K256K"]) 1
    do runActions <- runStmt session "Main" "test2"
       (_output, result) <- runWaitAll runActions
       case result of
         RunProgException _ -> return ()
         _ -> assertFailure $ "Unexpected run result " ++ show result

    -- But test3 will still work:
    do runActions <- runStmt session "Main" "test3"
       (_output, result) <- runWaitAll runActions
       case result of
         RunOk -> return ()
         _ -> assertFailure $ "Unexpected run result " ++ show result
  where
    upd :: IdeSessionUpdate
    upd = updateCodeGeneration True
       <> (updateSourceFile "Main.hs" $ L.unlines [
              "overflow :: Int -> IO Int"
            , "overflow n = do"
            , "  n' <- overflow (n + 1)"
            , "  return (n + n')"
            , ""
            , "test1 :: IO ()"
            , "test1 = print =<< overflow 0"
            , ""
            , "test2 :: IO ()"
            , "test2 = print $ foldl (+) 0 [1..100000]"
            , ""
            , "test3 :: IO ()"
            , "test3 = print (1 :: Int)"
            ])
