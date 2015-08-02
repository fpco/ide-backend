module TestSuite.Tests.Packages (testGroupPackages) where

import Prelude hiding (span, mod)
import Data.Maybe
import Data.Monoid
import System.Exit
import System.Process
import System.FilePath
import Test.Tasty
import Test.HUnit
import qualified Data.ByteString.Lazy.UTF8  as L
import qualified Data.ByteString.Lazy.Char8 as L (unlines)
import qualified Data.Text                  as T

import IdeSession
import TestSuite.State
import TestSuite.Session
import TestSuite.Assertions

testGroupPackages :: TestSuiteEnv -> TestTree
testGroupPackages env = testGroup "Packages" $ [
    stdTest env "Package dependencies"                                                            test_PackageDependencies
  , stdTest env "Register a package, don't restart session, don't see the package"                test_Register_NoRestart
  , stdTest env "Register a package, restart session, see the package and check for cabal macros" test_Register_Restart
  , stdTest env "Make sure package DB is passed to ghc (configGenerateModInfo False)"             test_PackageDB_ModInfoFalse
  , stdTest env "Make sure package DB is passed to ghc (configGenerateModInfo True)"              test_PackageDB_ModInfoTrue
  , stdTest env "Make sure package DB is passed to ghc after restartSession"                      test_PackageDB_AfterRestart
  , stdTest env "Module name visible from 2 packages --- picked from mtl (expected failure)"      test_ModuleIn2Pkgs_2
  , stdTest env "Hiding and unhiding a package (#185-2)"                                          test_HideUnhide
  , stdTest env "Unhiding and hiding a package (#185-3; converse of #185-2)"                      test_UnhideHide
  , stdTest env "Trusting and distrusting packages (#185-4)"                                      test_TrustDistrust
  , stdTest env "Using something from a different package (no \"Loading package\" msg)"           test_UseFromDifferentPackage
  ] ++ docTests env [
    stdTest env "Consistency of multiple modules of the same name"                                test_Consistency
  , withOK  env "Consistency of multiple modules of the same name: PackageImports"                test_Consistency_PackageImports
  , stdTest env "Module name visible from 2 packages --- picked from monads-tf"                   test_ModuleIn2Pkgs_1
  ]

test_PackageDependencies :: TestSuiteEnv -> Assertion
test_PackageDependencies env = withAvailableSession' env (withGhcOpts ["-hide-package monads-tf"]) $ \session -> do
    updateSessionD session upd 3
    assertNoErrors session

    deps <- getPkgDeps session
    assertEqual "" (ignoreVersions "Just [base-4.5.1.0,ghc-prim-0.2.0.0,integer-gmp-0.4.0.0]") (ignoreVersions $ show (deps "A"))
    assertEqual "" (ignoreVersions "Just [parallel-3.2.0.4,base-4.5.1.0,ghc-prim-0.2.0.0,integer-gmp-0.4.0.0]") (ignoreVersions $ show (deps "B"))
    assertEqual "" (ignoreVersions "Just [mtl-2.1.2,base-4.5.1.0,ghc-prim-0.2.0.0,integer-gmp-0.4.0.0,transformers-0.3.0.0]") (ignoreVersions $ show (deps "C"))
  where
    upd = (updateSourceFile "A.hs" . L.unlines $
            [ "module A where"
            ])
       <> (updateSourceFile "B.hs" . L.unlines $
            [ "module B where"
            , "import Control.Parallel"
            ])
       <> (updateSourceFile "C.hs" . L.unlines $
            [ "module C where"
            , "import Control.Monad.Cont" -- from mtl
            ])

test_Register_NoRestart :: TestSuiteEnv -> Assertion
test_Register_NoRestart env = withAvailableSession env $ \session -> do
    -- Session is started successfully, because we haven't referenced the
    -- package yet.

    withInstalledPackage env "TestSuite/inputs/simple-lib17" $ do
      -- Package is now installed, but the session not restarted, so we cannot
      -- reference the new package
      updateSessionD session upd 1
      assertSomeErrors session
  where
    upd = updateGhcOpts ["-package simple-lib17"]
       <> (updateSourceFile "Main.hs" . L.unlines $
            [ "module Main where"
            , "import SimpleLib (simpleLib)"
            , "main = print simpleLib"
            ])

test_Register_Restart :: TestSuiteEnv -> Assertion
test_Register_Restart env = withAvailableSession env $ \session -> do
    -- Session is started successfully, because we haven't referenced the
    -- package yet.

    withInstalledPackage env "TestSuite/inputs/simple-lib17" $ do
      -- After restarting the session the new package should be visible
      restartSession session
      updateSessionD session upd 1
      assertNoErrors session

      ifTestingExe env $ do
         let m = "Main"
             upd2 = buildExe [] [(T.pack m, "Main.hs")]
         updateSessionD session upd2 2
         distDir <- getDistDir session
         out <- readProcess (distDir </> "build" </> m </> m) [] []
         assertEqual "DB exe output"
                     "42\n"
                     out
         runActionsExe <- runExe session m
         (outExe, statusExe) <- runWaitAll runActionsExe
         assertEqual "Output from runExe"
                     "42\n"
                     outExe
         assertEqual "after runExe" ExitSuccess statusExe

  where
    upd = updateGhcOpts ["-package simple-lib17", "-XCPP"]
       <> (updateSourceFile "Main.hs" . L.unlines $
            [ "module Main where"
            , "import SimpleLib (simpleLib)"
            , "#if MIN_VERSION_simple_lib17(0,1,0)"
            , "main = print simpleLib"
            , "#else"
            , "terrible error"
            , "#endif"
            ])

test_PackageDB_ModInfoFalse :: TestSuiteEnv -> Assertion
test_PackageDB_ModInfoFalse env = withAvailableSession' env setup $ \session -> do
    -- We expect an error because 'ide-backend-rts' and/or 'parallel'
    -- are not (usually?) installed in the global package DB.
    updateSessionD session upd 1
    assertSourceErrors session [[
        (Nothing, expected1)
      , (Nothing, expected2)
      ]]
  where
    packageOpts = ["-package parallel"]
    setup       = withModInfo True
                . withDBStack [GlobalPackageDB]
                . withGhcOpts packageOpts

    upd = (updateSourceFile "A.hs" . L.unlines $
            [ "module A where"
            , "import Control.Parallel"
            ])

    expected1 = "cannot satisfy -package ide-backend-rts"
    expected2 = "cannot satisfy -package parallel"

test_PackageDB_ModInfoTrue :: TestSuiteEnv -> Assertion
test_PackageDB_ModInfoTrue env = withAvailableSession' env setup $ \session -> do
     -- We expect an error because 'ide-backend-rts' and/or 'parallel'
     -- are not (usually?) installed in the global package DB.
     updateSessionD session upd 1
     assertSourceErrors session [[
         (Nothing, expected1)
       , (Nothing, expected2)
       ]]
  where
    packageOpts = ["-package parallel"]
    setup       = withModInfo True
                . withDBStack [GlobalPackageDB]
                . withGhcOpts packageOpts

    upd = (updateSourceFile "A.hs" . L.unlines $
            [ "module A where"
            , "import Control.Parallel"
            ])

    expected1 = "cannot satisfy -package ide-backend-rts"
    expected2 = "<command line>: cannot satisfy -package parallel"

test_PackageDB_AfterRestart :: TestSuiteEnv -> Assertion
test_PackageDB_AfterRestart env = withAvailableSession' env setup $ \session -> do
    restartSession session
    -- We expect an error because 'ide-backend-rts' and/or 'parallel'
    -- are not (usually?) installed in the global package DB.
    updateSessionD session upd 1
    assertSourceErrors session [[
        (Nothing, expected1)
      , (Nothing, expected2)
      ]]
  where
    packageOpts = ["-package parallel"]
    setup       = withModInfo True
                . withDBStack [GlobalPackageDB]
                . withGhcOpts packageOpts

    upd = (updateSourceFile "A.hs" . L.unlines $
            [ "module A where"
            , "import Control.Parallel"
            ])

    expected1 = "cannot satisfy -package ide-backend-rts"
    expected2 = "<command line>: cannot satisfy -package parallel"

{-
18:45 < mikolaj> from http://www.haskell.org/ghc/docs/7.4.2/html/users_guide/packages.html#package-overlaps
18:45 < mikolaj> It is possible that by using packages you might end up with a program that contains two modules with the same name: perhaps you used a package P that has a hidden module M, and there is also a module M in your program.
                 Or perhaps the dependencies of packages that you used contain some overlapping modules. Perhaps the program even contains multiple versions of a certain package, due to dependencies from other packages.
18:45 < mikolaj> None of these scenarios gives rise to an error on its own[8], but they may have some interesting consequences. For instance, if you have a type M.T from version 1 of package P, then this is not the same as the type M.T
                 from version 2 of package P, and GHC will report an error if you try to use one where the other is expected.
18:46 < mikolaj> so it seems it's unspecified which module will be used --- it just happens that our idInfo code picks a different package than GHC API in this case
-}
test_Consistency :: TestSuiteEnv -> Assertion
test_Consistency env = withAvailableSession env $ \sess -> do
    let cb     = \_ -> return ()
        update = flip (updateSession sess) cb
        updMod = \mod code -> updateSourceFile mod (L.fromString code)

    update $ updMod "Control/Parallel.hs" $ unlines [
        "module Control.Parallel where"
      , ""
      , "import Bar"
      , ""
      , "foo = bar >> bar"
      , ""
      , "foobar = putStrLn \"Baz\""
      ]

    update $ updMod "Bar.hs" $ unlines [
        "module Bar where"
      , ""
      , "bar = putStrLn \"Hello, world!\""
      ]

    update $ updMod "Baz.hs" $ unlines [
        "module Baz where"
      , ""
      , "import Control.Parallel"
      , "import Bar"
      , ""
      , "baz = foobar"
      ]

    assertNoErrors sess
    assertIdInfo sess "Bar" (3, 7, 3, 15) "putStrLn" VarName "String -> IO ()" "base-4.5.1.0:System.IO" "<no location info>" "base-4.5.1.0:System.IO" "imported from base-4.5.1.0:Prelude at Bar.hs@1:8-1:11"
-- w fail:            assertIdInfo gif "Baz" (6, 8, 6, 9) "foobar" VarName "IO ()" "main:Control.Parallel" "Control/Parallel.hs@7:1-7:7" "" "imported from main:Control.Parallel at Baz.hs@3:1-3:24"

    update $ updMod "Baz.hs" $ unlines [
        "module Baz where"
      , ""
      , "import Control.Parallel"
      , "import Bar"
      , ""
      , "baz = foobar >>>> foo >> bar"
      ]
    assertOneError sess
    _ <- getSpanInfo sess -- TODO: Is this call useful?
    assertIdInfo sess "Bar" (3, 7, 3, 15) "putStrLn" VarName "String -> IO ()" "base-4.5.1.0:System.IO" "<no location info>" "base-4.5.1.0:System.IO" "imported from base-4.5.1.0:Prelude at Bar.hs@1:8-1:11"
    -- Baz is broken at this point

    update $ updMod "Baz.hs" $ unlines [
        "module Baz where"
      , ""
      , "import Control.Parallel"
      , "import Bar"
      , ""
      , "baz = foobar >> foo >> bar"
      ]

    assertNoErrors sess
    assertIdInfo sess "Bar" (3, 7, 3, 15) "putStrLn" VarName "String -> IO ()" "base-4.5.1.0:System.IO" "<no location info>" "base-4.5.1.0:System.IO" "imported from base-4.5.1.0:Prelude at Bar.hs@1:8-1:11"
-- would fail:           assertIdInfo gif "Baz" (6, 8, 6, 9) "foobar" VarName "IO ()" "main:Control.Parallel" "Control/Parallel.hs@7:1-7:7" "" "imported from main:Control/Parallel at Baz.hs@3:1-3:24"

test_Consistency_PackageImports :: TestSuiteEnv -> IO String
test_Consistency_PackageImports env = withAvailableSession env $ \sess -> do
    let cb     = \_ -> return ()
        update = flip (updateSession sess) cb
        updMod = \mod code -> updateSourceFile mod (L.fromString code)

    update $ updMod "Control/Parallel.hs" $ unlines [
        "module Control.Parallel where"
      , ""
      , "import Bar"
      , ""
      , "foo = bar >> bar"
      , ""
      , "par = putStrLn \"Baz\""
      ]

    update $ updMod "Bar.hs" $ unlines [
        "module Bar where"
      , ""
      , "bar = putStrLn \"Hello, world!\""
      ]

    update $ updMod "Baz.hs" $ unlines [
        "{-# LANGUAGE PackageImports #-}"
      , "module Baz where"
      , ""
      , "import \"parallel\" Control.Parallel"
      , "import Bar"
      , ""
      , "baz = par"
      ]

    assertNoErrors sess
    assertIdInfo sess "Bar" (3, 7, 3, 15) "putStrLn" VarName "String -> IO ()" "base-4.5.1.0:System.IO" "<no location info>" "base-4.5.1.0:System.IO" "imported from base-4.5.1.0:Prelude at Bar.hs@1:8-1:11"
    fixme sess "#254" $ assertIdInfo sess "Baz" (7, 7, 7, 10) "par" VarName "a1 -> b1 -> b1" "parallel-X.Y.Z:Control.Parallel" "<no location info>" "parallel-X.Y.Z:Control.Parallel" "imported from parallel-X.Y.Z:Control.Parallel at Baz.hs@4:1-4:35"

test_ModuleIn2Pkgs_1 :: TestSuiteEnv -> Assertion
test_ModuleIn2Pkgs_1 env = withAvailableSession' env (withGhcOpts packageOpts) $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session

    imports <- getImports session
    let base mod = ModuleId {
            moduleName    = T.pack mod
          , modulePackage = PackageId {
                packageName    = "base"
              , packageVersion = Just "X.Y.Z"
              , packageKey     = "base" -- ignoreVersions sets key == name
              }
          }
        monads_tf mod = ModuleId {
            moduleName    = T.pack mod
          , modulePackage = PackageId {
                packageName    = "monads-tf"
              , packageVersion = Just "X.Y.Z"
              , packageKey     = "monads-tf"
              }
          }
    assertSameSet "imports: " (ignoreVersions . fromJust . imports $ "A") $ [
        Import {
            importModule    = base "Prelude"
          , importPackage   = Nothing
          , importQualified = False
          , importImplicit  = True
          , importAs        = Nothing
          , importEntities  = ImportAll
          }
      , Import {
            importModule    = monads_tf "Control.Monad.Cont"
          , importPackage   = Just "monads-tf"
          , importQualified = False
          , importImplicit  = False
          , importAs        = Nothing
          , importEntities  = ImportAll
          }
      ]

    -- FIXME: We expect the scope "imported from monads-tf-X.Y.Z:Control.Monad.Cont at A.hs@3:1-3:38"
    -- but we cannot guarantee it (#95)
    -- assertIdInfo' session "A" (4,5,4,12) (4,5,4,12) "runCont" VarName (allVersions "Cont r1 a1 -> (a1 -> r1) -> r1") (allVersions "transformers-X.Y.Z:Control.Monad.Trans.Cont") (allVersions "<no location info>") (allVersions "monads-tf-X.Y.Z:Control.Monad.Cont") []
  where
    packageOpts = [ "-hide-all-packages"
                  , "-package base"
                  , "-package monads-tf"
                  , "-package mtl"
                  ]

    upd = (updateSourceFile "A.hs" . L.unlines $
            [ "{-# LANGUAGE PackageImports #-}"
            , "module A where"
            , "import \"monads-tf\" Control.Monad.Cont"
            , "f = runCont"
            ])

test_ModuleIn2Pkgs_2 :: TestSuiteEnv -> Assertion
test_ModuleIn2Pkgs_2 env = withAvailableSession' env (withGhcOpts packageOpts) $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session

    imports <- getImports session
    let base mod = ModuleId {
            moduleName    = T.pack mod
          , modulePackage = PackageId {
                packageName    = "base"
              , packageVersion = Just "X.Y.Z"
              , packageKey     = "base" -- ignoreVersions sets key == name
              }
          }
        mtl mod = ModuleId {
            moduleName    = T.pack mod
          , modulePackage = PackageId {
                packageName    = "mtl"
              , packageVersion = Just "X.Y.Z"
              , packageKey     = "mtl"
              }
          }
    assertSameSet "imports: " (ignoreVersions . fromJust . imports $ "A") $ [
        Import {
            importModule    = base "Prelude"
          , importPackage   = Nothing
          , importQualified = False
          , importImplicit  = True
          , importAs        = Nothing
          , importEntities  = ImportAll
          }
      , Import {
            importModule    = mtl "Control.Monad.Cont"
          , importPackage   = Just "mtl"
          , importQualified = False
          , importImplicit  = False
          , importAs        = Nothing
          , importEntities  = ImportAll
          }
      ]

    -- FIXME: We expect the scope "imported from mtl-X.Y.Z:Control.Monad.Cont at A.hs@3:1-3:38"
    -- but we cannot guarantee it (#95)
    -- assertIdInfo' session "A" (4,5,4,12) (4,5,4,12) "runCont" VarName (allVersions "Cont r1 a1 -> (a1 -> r1) -> r1") (allVersions "transformers-X.Y.Z:Control.Monad.Trans.Cont") (allVersions "<no location info>") (allVersions "mtl-X.Y.Z:Control.Monad.Cont") []
  where
    packageOpts = [ "-hide-all-packages"
                  , "-package base"
                  , "-package monads-tf"
                  , "-package mtl"
                  ]

    upd = (updateSourceFile "A.hs" . L.unlines $
            [ "{-# LANGUAGE PackageImports #-}"
            , "module A where"
            , "import \"mtl\" Control.Monad.Cont"
            , "f = runCont"
            ])

-- We want to test that:
--
-- 1. The package flags work at all
-- 2. Transitions
-- 3. Statelessness of updateGhcOpts
test_HideUnhide :: TestSuiteEnv -> Assertion
test_HideUnhide env = withAvailableSession env $ \session -> do
    let runCode = do
          runActions <- runStmt session "A" "test"
          (output, result) <- runWaitAll runActions
          assertEqual "" RunOk result
          assertEqual "" "9\n" output

    -- First, check that we can import stuff from the unix package
    do let upd = (updateSourceFile "A.hs" $ L.unlines [
                     "module A (test) where"
                   , "import System.Posix"
                   , "test :: IO ()"
                   , "test = print sigKILL"
                   ])
              <> (updateCodeGeneration True)
       updateSessionD session upd 1
       assertNoErrors session
       runCode

    -- Hide the package
    do let upd = updateGhcOpts ["-hide-package unix"]
       updateSessionD session upd 1
       assertOneError session

    -- Reveal the package again with an explicit flag
    do let upd = updateGhcOpts ["-package unix"]
       updateSessionD session upd 1
       assertNoErrors session
       runCode

    -- Hide once more
    do let upd = updateGhcOpts ["-hide-package unix"]
       updateSessionD session upd 1
       assertOneError session

    -- Reveal it again by using the default package flags
    -- (statelessness of updateGhcOpts)
    do let upd = updateGhcOpts []
       updateSessionD session upd 1
       assertNoErrors session
       runCode

test_UnhideHide :: TestSuiteEnv -> Assertion
test_UnhideHide env = withAvailableSession env $ \session -> do
    let runCode = do
          runActions <- runStmt session "A" "test"
          (output, result) <- runWaitAll runActions
          assertEqual "" RunOk result
          case testSuiteEnvGhcVersion env of
            GHC_7_4  -> assertEqual "" "7.4.\n" output
            GHC_7_8  -> assertEqual "" "7.8.\n" output
            GHC_7_10 -> assertEqual "" "7.10\n" output

    -- First, check that we cannot import from the ghc package
    do let upd = (updateSourceFile "A.hs" $ L.unlines [
                     "module A (test) where"
                   , "import Config"
                   , "test :: IO ()"
                   , "test = putStrLn (take 4 cProjectVersion)"
                   ])
              <> (updateCodeGeneration True)
       updateSessionD session upd 1
       assertOneError session

    -- Reveal the package with an explicit flag
    do let upd = updateGhcOpts ["-package ghc"]
       updateSessionD session upd 1
       assertNoErrors session
       runCode

    -- Hide the package
    do let upd = updateGhcOpts ["-hide-package ghc"]
       updateSessionD session upd 1
       assertOneError session

    -- Reveal once more
    do let upd = updateGhcOpts ["-package ghc"]
       updateSessionD session upd 1
       assertNoErrors session
       runCode

    -- Hide it again by using the default package flags
    -- (statelessness of updateGhcOpts)
    do let upd = updateGhcOpts []
       updateSessionD session upd 1
       assertOneError session

test_TrustDistrust :: TestSuiteEnv -> Assertion
test_TrustDistrust env = withAvailableSession env $ \session -> do
    let runCode = do
          runActions <- runStmt session "A" "test"
          (output, result) <- runWaitAll runActions
          assertEqual "" RunOk result
          assertEqual "" "Hello\n" output

    -- First, check that base is untrusted
    do let upd = (updateGhcOpts ["-XSafe", "-fpackage-trust"])
              <> (updateSourceFile "A.hs" $ L.unlines [
                     "module A (test) where"
                   , "test :: IO ()"
                   , "test = putStrLn \"Hello\""
                   ])
              <> (updateCodeGeneration True)
       updateSessionD session upd 1
       assertOneError session

    -- Trust base
    do let upd = updateGhcOpts ["-XSafe", "-fpackage-trust", "-trust base"]
       updateSessionD session upd 1
       assertNoErrors session
       runCode

    -- Untrust it
    do let upd = updateGhcOpts ["-XSafe", "-fpackage-trust", "-distrust base"]
       updateSessionD session upd 1
       assertOneError session

    -- Trust it once more
    do let upd = updateGhcOpts ["-XSafe", "-fpackage-trust", "-trust base"]
       updateSessionD session upd 1
       assertNoErrors session
       runCode

    -- Untrust it again by using the default package flags
    -- (statelessness of updateGhcOpts)
    do let upd = updateGhcOpts ["-XSafe", "-fpackage-trust"]
       updateSessionD session upd 1
       assertOneError session

-- We pick something from the haskell platform but that doesn't come with ghc itself
-- https://github.com/haskell/haskell-platform/blob/2012.4.0.0/haskell-platform.cabal
test_UseFromDifferentPackage :: TestSuiteEnv -> Assertion
test_UseFromDifferentPackage env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    runActions <- runStmt session "M" "hello"
    (output, result) <- runWaitAll runActions
    assertEqual "" RunOk result
    assertEqual "" "5\n" output
  where
    upd = (updateCodeGeneration True)
       <> (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "import Control.Monad.IO.Class" -- From transformers
            , "hello :: IO ()"
            , "hello = liftIO $ print 5"
            ])
