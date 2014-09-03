module TestSuite.Tests.Autocompletion (testGroupAutocompletion) where

import Prelude hiding (mod, span)
import Data.Maybe
import Data.Monoid
import Test.Tasty
import Test.HUnit
import qualified Data.ByteString.Lazy.Char8 as L (unlines)
import qualified Data.Text                  as T

import IdeSession
import TestSuite.State
import TestSuite.Session
import TestSuite.Assertions

-- TODO: Autocomplete test that checks import errors:
--
-- * Explicitly importing something that wasn't exported
-- * Explicitly hiding something that wasn't exported
-- * Use of PackageImports without the flag
testGroupAutocompletion :: TestSuiteEnv -> TestTree
testGroupAutocompletion env = testGroup "Autocompletion" $ [
    stdTest env "Imports for partial module"                          test_PartialModule
  , stdTest env "Recompute after recompilation"                       test_Recompute
  , stdTest env "fpco issue #2518"                                    test_2518
  ] ++ docTests env [
    stdTest env "Autocompletion entries should have home module info" test_HomeModuleInfo
  ]

test_PartialModule :: TestSuiteEnv -> Assertion
test_PartialModule env = withAvailableSession' env (withGhcOpts ["-XPackageImports"]) $ \session -> do
    updateSessionD session upd 1
    assertSourceErrors' session ["parse error"]
    imports <- getImports session

    assertSameSet "imports: " (ignoreVersions . fromJust . imports $ "M") $ [
        Import {
            importModule    = base "Prelude"
          , importPackage   = Nothing
          , importQualified = False
          , importImplicit  = True
          , importAs        = Nothing
          , importEntities  = ImportAll
          }
      , Import {
            importModule    = base "Control.Monad"
          , importPackage   = Nothing
          , importQualified = False
          , importImplicit  = False
          , importAs        = Nothing
          , importEntities  = ImportAll
          }
      , Import {
            importModule    = base "Control.Category"
          , importPackage   = Nothing
          , importQualified = False
          , importImplicit  = False
          , importAs        = Nothing
          , importEntities  = ImportHiding ["id"]
          }
      , Import {
            importModule     = base "Control.Arrow"
          , importPackage    = Nothing
          , importQualified  = True
          , importImplicit   = False
          , importAs         = Just "A"
          , importEntities   = ImportOnly ["second"]
          }
      , Import {
            importModule    = base "Data.List"
          , importPackage   = Just "base"
          , importQualified = True
          , importImplicit  = False
          , importAs        = Nothing
          , importEntities  = ImportAll
          }
      , Import {
            importModule    = par "Control.Parallel"
          , importPackage   = Nothing
          , importQualified = False
          , importImplicit  = False
          , importAs        = Nothing
          , importEntities  = ImportAll
          }
      ]
    autocomplete <- getAutocompletion session
    let completeFo = autocomplete "M" "fo"
    assertSameSet "fo: " (map idInfoQN completeFo) [
        "foldM"
      , "foldM_"
      , "forM"
      , "forM_"
      , "forever"
      , "Data.List.foldl'"
      , "Data.List.foldl1"
      , "Data.List.foldl1'"
      , "Data.List.foldr"
      , "Data.List.foldl"
      , "Data.List.foldr1"
      ]
    let completeControlMonadFo = autocomplete "M" "Data.List.fo"
    assertSameSet "Data.List.fo: " (map idInfoQN completeControlMonadFo) [
        "Data.List.foldl'"
      , "Data.List.foldl1"
      , "Data.List.foldl1'"
      , "Data.List.foldr"
      , "Data.List.foldl"
      , "Data.List.foldr1"
      ]
    let completeSec = autocomplete "M" "sec"
    assertSameSet "sec: " (map idInfoQN completeSec) [
        "A.second"
      ]
  where
    upd = (updateSourceFile "M.hs" . L.unlines $
      [ "module M where"
      , "import Control.Monad"
      , "import Control.Category hiding (id)"
      , "import qualified Control.Arrow as A (second)"
      , "import qualified \"base\" Data.List"
      , "import Control.Parallel"
      , "foo ="
      ])

    base mod = ModuleId {
        moduleName    = T.pack mod
      , modulePackage = PackageId {
            packageName    = "base"
          , packageVersion = Just "X.Y.Z"
          }
      }
    par mod = ModuleId {
        moduleName    = T.pack mod
      , modulePackage = PackageId {
            packageName    = "parallel"
          , packageVersion = Just "X.Y.Z"
          }
      }

test_Recompute :: TestSuiteEnv -> Assertion
test_Recompute env = withAvailableSession env $ \session -> do
    updateSessionD session upd 2
    assertNoErrors session

    -- First check, for sanity
    do autocomplete <- getAutocompletion session
       let completeFoob = autocomplete "B" "foob"
       assertEqual "" "[foobar (VarName) :: Bool -> Bool defined in main:A at A.hs@3:1-3:7 (imported from main:A at B.hs@2:1-2:9)]" (show completeFoob)

    -- Change A, but not B. The type reported in the autocompletion for B
    -- should now be changed, too
    -- This will trigger recompilation of B
    updateSessionD session upd' 2
    assertNoErrors session

    do autocomplete <- getAutocompletion session
       let completeFoob = autocomplete "B" "foob"
       let expected = "[foobar (VarName) :: Int -> Int defined in main:A at A.hs@3:1-3:7 (imported from main:A at B.hs@2:1-2:9),foobar' (VarName) :: () -> () defined in main:A at A.hs@5:1-5:8 (imported from main:A at B.hs@2:1-2:9)]"
       assertEqual "" expected (show completeFoob)
  where
    upd = (updateSourceFile "A.hs" . L.unlines $
            [ "module A where"
            , "foobar :: Bool -> Bool"
            , "foobar = id"
            ])
       <> (updateSourceFile "B.hs" . L.unlines $
            [ "module B where"
            , "import A"
            ])

    upd' = (updateSourceFile "A.hs" . L.unlines $
             [ "module A where"
             , "foobar :: Int -> Int"
             , "foobar = id"
             , "foobar' :: () -> ()"
             , "foobar' = id"
             ])

test_HomeModuleInfo :: TestSuiteEnv -> Assertion
test_HomeModuleInfo env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session

    autocomplete <- getAutocompletion session
    let completeTru = autocomplete "A" "Tru"
    assertEqual "" (ignoreVersions "[True (DataName) defined in ghc-prim-0.2.0.0:GHC.Types at <wired into compiler> (home base-4.5.1.0:Data.Bool) (wired in to the compiler)]") (ignoreVersions $ show completeTru)
  where
    upd = (updateSourceFile "A.hs" . L.unlines $
            [ "module A where"
            ])


test_2518 :: TestSuiteEnv -> Assertion
test_2518 env = withAvailableSession' env (withGhcOpts ["-XPackageImports"]) $ \session -> do
    updateSessionD session upd 1
    assertSourceErrors' session ["Not in scope: `toC'"]
    autocomplete <- getAutocompletion session
    let complete_toC = autocomplete "M" "toC"
    assertSameSet "" (map idInfoQN complete_toC) [
        "B.toChunks"
      ]
  where
    upd = (updateSourceFile "M.hs" . L.unlines $
      [ "module M where"
      , "import qualified Data.ByteString.Lazy as B"
      , "foo = toC"
      ])
