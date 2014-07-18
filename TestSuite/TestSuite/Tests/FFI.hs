module TestSuite.Tests.FFI (testGroupFFI) where

import Data.Monoid
import Data.Version
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import Test.Tasty
import Test.HUnit
import qualified Data.ByteString.UTF8  as S
import qualified Data.ByteString.Char8 as S (breakSubstring, append)
import qualified Data.ByteString.Lazy  as L
import qualified Data.Text             as T

import IdeSession
import TestSuite.Assertions
import TestSuite.Session
import TestSuite.State

testGroupFFI :: TestSuiteEnv -> TestTree
testGroupFFI env = testGroup "Using the FFI" [
    stdTest env "via GHC API"                                                                     test_FFI_via_API
  , stdTest env "via GHC API with restartSession"                                                 test_FFI_via_API_restartSession
  , stdTest env "via GHC API with deleting and re-adding the .c file"                             test_deleteReadd
  , stdTest env "via GHC API with deleting and adding a different .c file"                        test_deleteAddDifferent
  , stdTest env "from a subdir and compiled via buildExe"                                         test_fromSubdir_buildExe
  , stdTest env "with TH and MIN_VERSION_base via buildExe"                                       test_MinVersion
  , stdTest env "with withIncludes, TH and MIN_VERSION_base via buildExe"                         test_MinVersion_withIncludes
  , stdTest env "with withIncludes, TH and MIN_VERSION_base via buildExe and with restartSession" test_MinVersion_restartSession
  , stdTest env "with withIncludes and TargetsExclude"                                            test_TargetsExclude
  , stdTest env "with dynamic include, TH and MIN_VERSION_base via buildExe"                      test_DynamicInclude
  , stdTest env "with dynamic include and TargetsInclude"                                         test_DynamicInclude_TargetsInclude
  ]

test_FFI_via_API :: TestSuiteEnv -> Assertion
test_FFI_via_API env = withAvailableSession env $ \session -> do
    updateSessionD session upd 3
    assertNoErrors session
    runActions <- runStmt session "Main" "main"
    (output, result) <- runWaitAll runActions
    case result of
      RunOk -> assertEqual "" "42\n" output
      _     -> assertFailure $ "Unexpected run result: " ++ show result
  where
    upd = mconcat [
        updateCodeGeneration True
      , updateSourceFileFromFile "test/FFI/Main.hs"
      , updateSourceFileFromFile "test/FFI/life.c"
      , updateSourceFileFromFile "test/FFI/life.h"
      ]

test_FFI_via_API_restartSession :: TestSuiteEnv -> Assertion
test_FFI_via_API_restartSession env = withAvailableSession env $ \session -> do
    updateSessionD session upd 3
    assertNoErrors session

    restartSession session Nothing
    updateSessionD session mempty 3
    assertNoErrors session

    runActions <- runStmt session "Main" "main"
    (output, result) <- runWaitAll runActions
    case result of
      RunOk -> assertEqual "" "42\n" output
      _     -> assertFailure $ "Unexpected run result: " ++ show result
  where
    upd = mconcat [
        updateCodeGeneration True
      , updateSourceFileFromFile "test/FFI/Main.hs"
      , updateSourceFileFromFile "test/FFI/life.c"
      , updateSourceFileFromFile "test/FFI/life.h"
      ]

test_deleteReadd :: TestSuiteEnv -> Assertion
test_deleteReadd env = withAvailableSession env $ \session -> do
    updateSessionD session upd 3
    assertNoErrors session

    updateSessionD session (updateSourceFileDelete "test/FFI/life.c") 0
    assertNoErrors session

    updateSessionD session (updateSourceFileFromFile "test/FFI/life.c") 4
    assertNoErrors session

    updateSessionD session (updateSourceFileDelete "test/FFI/life.c") 0
    assertNoErrors session

    restartSession session Nothing
    updateSessionD session mempty 1
    assertOneError session

    updateSessionD session (updateSourceFileFromFile "test/FFI/life.c") 4
    assertNoErrors session

    runActions <- runStmt session "Main" "main"
    (output, result) <- runWaitAll runActions
    case result of
      RunOk -> assertEqual "" "42\n" output
      _     -> assertFailure $ "Unexpected run result: " ++ show result
  where
    upd = mconcat [
        updateCodeGeneration True
      , updateSourceFileFromFile "test/FFI/Main.hs"
      , updateSourceFileFromFile "test/FFI/life.c"
      , updateSourceFileFromFile "test/FFI/life.h"
      ]

test_deleteAddDifferent :: TestSuiteEnv -> Assertion
test_deleteAddDifferent env = withAvailableSession env $ \session -> do
    updateSessionD session upd 3
    assertNoErrors session

    updateSessionD session (updateSourceFileDelete "test/FFI/life.c"
                            <> updateSourceFileDelete "test/FFI/life.h") 0
    assertNoErrors session

{- duplicate definition for symbol...    errorMsg = "Server killed"
    updateSessionD session (updateSourceFileFromFile "test/FFI/ffiles/life.c"
                            <> updateSourceFileFromFile "test/FFI/ffiles/local.h"
                            <> updateSourceFileFromFile "test/FFI/ffiles/life.h") 4
    assertNoErrors session
    runActions <- runStmt session "Main" "main"
    (output, result) <- runWaitAll runActions
    case result of
      RunOk -> assertEqual "" "42\n" output
      _     -> assertFailure $ "Unexpected run result: " ++ show result
-}
  where
    upd = mconcat [
        updateCodeGeneration True
      , updateSourceFileFromFile "test/FFI/Main.hs"
      , updateSourceFileFromFile "test/FFI/life.c"
      , updateSourceFileFromFile "test/FFI/life.h"
      ]

test_fromSubdir_buildExe :: TestSuiteEnv -> Assertion
test_fromSubdir_buildExe env = withAvailableSession env $ \session -> do
    updateSessionD session upd 3
    assertNoErrors session
    let m = "Main"
        upd2 = buildExe [] [(T.pack m, "test/FFI/Main2.hs")]
    updateSessionD session upd2 1
    distDir <- getDistDir session
    buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
    assertEqual "buildStderr empty" "" buildStderr
    exeOut <- readProcess (distDir </> "build" </> m </> m) [] []
    assertEqual "FFI exe output" "42\n" exeOut
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "42\n"
                outExe
    assertEqual "after runExe" ExitSuccess statusExe
  where
    upd = mconcat [
        updateCodeGeneration True
      , updateSourceFileFromFile "test/FFI/Main2.hs"
      , updateSourceFileFromFile "test/FFI/ffiles/life.c"
      , updateSourceFileFromFile "test/FFI/ffiles/life.h"
      , updateSourceFileFromFile "test/FFI/ffiles/local.h"
      ]

test_MinVersion :: TestSuiteEnv -> Assertion
test_MinVersion env = withAvailableSession env $ \session -> do
    withCurrentDirectory "test/FFI" $ updateSessionD session upd 4
    assertNoErrors session
    let m = "Main"
        upd2 = buildExe [] [(T.pack m, "Main3.hs")]
    updateSessionD session upd2 3
    distDir <- getDistDir session
    buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
    assertEqual "buildStderr empty" "" buildStderr
    exeOut <- readProcess (distDir </> "build" </> m </> m) [] []
    assertEqual "FFI exe output" "84\n" exeOut
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "84\n"
                outExe
    assertEqual "after runExe" ExitSuccess statusExe
  where
    upd = mconcat [
        updateCodeGeneration True
      , updateSourceFileFromFile "Main3.hs"
      , updateSourceFileFromFile "A.hs"
      , updateSourceFileFromFile "ffiles/life.c"
      , updateSourceFileFromFile "ffiles/life.h"
      , updateSourceFileFromFile "ffiles/local.h"
      ]

test_MinVersion_withIncludes :: TestSuiteEnv -> Assertion
test_MinVersion_withIncludes env = withAvailableSession' env (withIncludes ["test/FFI"]) $ \session -> do
    updateSessionD session upd 4
    assertNoErrors session
    let m = "Main"
        upd2 = buildExe [] [(T.pack m, "Main3.hs")]
    updateSessionD session upd2 3
    distDir <- getDistDir session
    buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
    assertEqual "buildStderr empty" "" buildStderr
    exeOut <- readProcess (distDir </> "build" </> m </> m) [] []
    assertEqual "FFI exe output" "84\n" exeOut
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "84\n"
                outExe
    assertEqual "after runExe" ExitSuccess statusExe
  where
    upd = mconcat [
        updateCodeGeneration True
      , updateSourceFileFromFile "test/FFI/Main3.hs"
      , updateSourceFileFromFile "test/FFI/A.hs"
      , updateSourceFileFromFile "test/FFI/ffiles/life.c"
      , updateSourceFileFromFile "test/FFI/ffiles/life.h"
      , updateSourceFileFromFile "test/FFI/ffiles/local.h"
      ]

test_MinVersion_restartSession :: TestSuiteEnv -> Assertion
test_MinVersion_restartSession env = withAvailableSession' env (withIncludes ["test/FFI"]) $ \session -> do
    updateSessionD session upd 4
    assertNoErrors session

    restartSession session Nothing
    updateSessionD session mempty 4
    assertNoErrors session

    updateSessionD session (updateSourceFileDelete "test/FFI/ffiles/life.c"
                            <> updateSourceFileDelete "test/FFI/ffiles/life.h"
                            <> updateSourceFileDelete "test/FFI/ffiles/local.h") 0
    assertNoErrors session

    restartSession session Nothing
    updateSessionD session mempty 4
    assertOneError session

    updateSessionD session (updateSourceFileFromFile "test/FFI/life.h"
                            <> updateSourceFileFromFile "test/FFI/life.c") 4
    assertNoErrors session

    let m = "Main"
        upd2 = buildExe [] [(T.pack m, "Main3.hs")]
    updateSessionD session upd2 3
    distDir <- getDistDir session
    buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
    assertEqual "buildStderr empty" "" buildStderr
    exeOut <- readProcess (distDir </> "build" </> m </> m) [] []
    assertEqual "FFI exe output" "84\n" exeOut
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "84\n"
                outExe
    assertEqual "after runExe" ExitSuccess statusExe
  where
    upd = mconcat [
        updateCodeGeneration True
      , updateSourceFileFromFile "test/FFI/Main3.hs"
      , updateSourceFileFromFile "test/FFI/A.hs"
      , updateSourceFileFromFile "test/FFI/ffiles/life.c"
      , updateSourceFileFromFile "test/FFI/ffiles/life.h"
      , updateSourceFileFromFile "test/FFI/ffiles/local.h"
      ]

test_TargetsExclude :: TestSuiteEnv -> Assertion
test_TargetsExclude env = withAvailableSession' env (withIncludes ["test/FFI"]) $ \session -> do
    updateSessionD session upd 4
    assertNoErrors session
    updateSessionD session (updateSourceFileDelete "test/FFI/ffiles/life.c") 0
    assertNoErrors session

    restartSession session Nothing
    updateSessionD session mempty 1
    assertOneError session

        -- Without the restartSession we get
{-
GHCi runtime linker: fatal error: I found a duplicate definition for symbol
   meaningOfLife
whilst processing object file
   /tmp/ide-backend-test.28928/dist.28928/objs/test/FFI/ffiles/life.o
This could be caused by:
   * Loading two different object files which export the same symbol
   * Specifying the same object file twice on the GHCi command line
   * An incorrect `package.conf' entry, causing some object to be
     loaded twice.
GHCi cannot safely continue in this situation.  Exiting now.  Sorry.

  Using the FFI via GHC API with deleting and adding a different .c file: [Failed]
Unexpected errors: SourceError {errorKind = KindServerDied, errorSpan = <<server died>>, errorMsg = "Server killed"}
-}
    updateSessionD session (updateSourceFileFromFile "test/FFI/life.c"
                            <> updateSourceFileFromFile "test/FFI/life.h") 5
    assertNoErrors session

    let m = "Main"
        upd2 = buildExe [] [(T.pack m, "Main3.hs")]
    updateSessionD session upd2 3
    distDir <- getDistDir session
    buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
    assertEqual "buildStderr empty" "" buildStderr
    exeOut <- readProcess (distDir </> "build" </> m </> m) [] []
    assertEqual "FFI exe output" "84\n" exeOut
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "84\n"
                outExe
    assertEqual "after runExe" ExitSuccess statusExe

    -- This is the one test where test the .cabal file; doing this
    -- consistently in other tests is painful because the precise format of
    -- the generated file changes so often
    dotCabalFromName <- getDotCabal session
    let dotCabal = dotCabalFromName "libName" $ Version [1, 0] []
    version <- getGhcVersion session
    case version of
      GHC742 -> assertEqual "dotCabal" "name: libName\nversion: X.Y.Z\ncabal-version: X.Y.Z\nbuild-type: Simple\nlicense: AllRightsReserved\nlicense-file: \"\"\ndata-dir: \"\"\n \nlibrary\n    build-depends: array ==X.Y.Z, base ==X.Y.Z,\n                   containers ==X.Y.Z, deepseq ==X.Y.Z, ghc-prim ==X.Y.Z,\n                   integer-gmp ==X.Y.Z, pretty ==X.Y.Z, template-haskell ==X.Y.Z\n    exposed-modules: A\n    exposed: True\n    buildable: True\n    c-sources: test/FFI/life.c\n    default-language: Haskell2010\n    other-extensions: TemplateHaskell\n    install-includes: life.h local.h life.h\n    hs-source-dirs: test/FFI\n \n " (ignoreVersions dotCabal)
      GHC78  -> assertEqual "dotCabal" "name: libName\nversion: X.Y.Z\ncabal-version: X.Y.Z\nbuild-type: Simple\nlicense: AllRightsReserved\nlicense-file: \"\"\ndata-dir: \"\"\n \nlibrary\n    build-depends: array ==X.Y.Z, base ==X.Y.Z,\n                   containers ==X.Y.Z, deepseq ==X.Y.Z, ghc-prim ==X.Y.Z,\n                   integer-gmp ==X.Y.Z, pretty ==X.Y.Z, template-haskell ==X.Y.Z\n    exposed-modules: A\n    exposed: True\n    buildable: True\n    c-sources: test/FFI/life.c\n    default-language: Haskell2010\n    other-extensions: TemplateHaskell\n    install-includes: life.h local.h life.h\n    hs-source-dirs: test/FFI\n \n " (ignoreVersions dotCabal)
    let pkgDir = distDir </> "dotCabal.test"
    createDirectoryIfMissing False pkgDir
    L.writeFile (pkgDir </> "libName.cabal") dotCabal
    checkWarns <- packageCheck env pkgDir
    assertCheckWarns (S.fromString checkWarns)
  where
    upd = mconcat [
        updateCodeGeneration True
      , updateSourceFileFromFile "test/FFI/Main.hs"
      , updateSourceFileFromFile "test/FFI/Main2.hs"
      , updateSourceFileFromFile "test/FFI/Main3.hs"
      , updateSourceFileFromFile "test/FFI/A.hs"
      , updateSourceFileFromFile "test/FFI/ffiles/life.c"
      , updateSourceFileFromFile "test/FFI/ffiles/life.h"
      , updateSourceFileFromFile "test/FFI/ffiles/local.h"
      , updateTargets (TargetsExclude ["test/FFI/life.c", "test/FFI/life.h", "life.c", "life.h", "test/FFI/Main.hs", "test/FFI/Main2.hs"])
      ]

test_DynamicInclude :: TestSuiteEnv -> Assertion
test_DynamicInclude env = withAvailableSession env $ \session -> do
    updateSessionD session
                   (updateRelativeIncludes [])
                   0
    updateSessionD session upd 4
    assertNoErrors session

    updateSessionD session
                   (updateRelativeIncludes ["test/FFI"])
                   4
    assertNoErrors session

    let m = "Main"
        upd2 = buildExe [] [(T.pack m, "Main3.hs")]
    updateSessionD session upd2 3
    distDir <- getDistDir session
    buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
    assertEqual "buildStderr empty" "" buildStderr
    exeOut <- readProcess (distDir </> "build" </> m </> m) [] []
    assertEqual "FFI exe output" "84\n" exeOut
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "84\n"
                outExe
    assertEqual "after runExe" ExitSuccess statusExe
  where
    upd = mconcat [
        updateCodeGeneration True
      , updateSourceFileFromFile "test/FFI/Main3.hs"
      , updateSourceFileFromFile "test/FFI/A.hs"
      , updateSourceFileFromFile "test/FFI/ffiles/life.c"
      , updateSourceFileFromFile "test/FFI/ffiles/life.h"
      , updateSourceFileFromFile "test/FFI/ffiles/local.h"
      ]

test_DynamicInclude_TargetsInclude :: TestSuiteEnv -> Assertion
test_DynamicInclude_TargetsInclude env = withAvailableSession env $ \session -> do
    updateSessionD session upd 4
    assertNoErrors session

    updateSessionD session
                   (updateRelativeIncludes ["test/FFI"])
                   4
    assertNoErrors session

    updateSessionD session (updateTargets (TargetsInclude ["test/FFI/Main2.hs"])) 3
    assertNoErrors session

    updateSessionD session (updateTargets (TargetsInclude ["test/FFI/Main3.hs"])) 4
    assertNoErrors session

    let m = "Main"
        upd2 = buildExe [] [(T.pack m, "Main3.hs")]
    updateSessionD session upd2 3
    distDir <- getDistDir session
    buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
    assertEqual "buildStderr empty" "" buildStderr
    exeOut <- readProcess (distDir </> "build" </> m </> m) [] []
    assertEqual "FFI exe output" "84\n" exeOut
    runActionsExe <- runExe session m
    (outExe, statusExe) <- runWaitAll runActionsExe
    assertEqual "Output from runExe"
                "84\n"
                outExe
    assertEqual "after runExe" ExitSuccess statusExe
  where
    upd = mconcat [
        updateCodeGeneration True
      , updateTargets (TargetsInclude ["test/FFI/Main.hs"])
      , updateSourceFileFromFile "test/FFI/Main.hs"
      , updateSourceFileFromFile "test/FFI/Main2.hs"
      , updateSourceFileFromFile "test/FFI/Main3.hs"
      , updateSourceFileFromFile "test/FFI/A.hs"
      , updateSourceFileFromFile "test/FFI/ffiles/life.c"
      , updateSourceFileFromFile "test/FFI/ffiles/life.h"
      , updateSourceFileFromFile "test/FFI/ffiles/local.h"
      ]

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

assertCheckWarns :: S.ByteString -> Assertion
assertCheckWarns checkWarns =
  assertEqual "checkWarns for dotCabal" (filterCheckWarns checkWarns) (filterCheckWarns "These warnings may cause trouble when distributing the package:\n* No 'category' field.\n\n* No 'maintainer' field.\n\nThe following errors will cause portability problems on other environments:\n* The package is missing a Setup.hs or Setup.lhs script.\n\n* No 'synopsis' or 'description' field.\n\n* The 'license' field is missing or specified as AllRightsReserved.\n\nHackage would reject this package.\n")

filterCheckWarns :: S.ByteString -> S.ByteString
filterCheckWarns s =
  let (bs1, rest1) = S.breakSubstring "The following warnings are likely affect your build negatively" s
      (_,   bs2)   = S.breakSubstring "These warnings may cause trouble" rest1
  in S.append bs1 bs2
