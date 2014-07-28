module TestSuite.Tests.TypeInformation (testGroupTypeInformation) where

import Prelude hiding (span, mod)
import Data.Monoid
import Test.Tasty
import Test.HUnit
import qualified Data.ByteString.Lazy.UTF8  as L
import qualified Data.ByteString.Lazy.Char8 as L (unlines)

import IdeSession
import TestSuite.State
import TestSuite.Session
import TestSuite.Assertions

testGroupTypeInformation :: TestSuiteEnv -> TestTree
testGroupTypeInformation env = testGroup "Type Information" [
    stdTest env "Test internal consistency of local id markers"                              test_Consistency_Local
  , stdTest env "Test internal consistency of imported id markers"                           test_Consistency_Imported
  , docTest env "Local identifiers and Prelude"                                              testLocalIdentifiersAndPrelude
  , docTest env "Simple ADTs"                                                                testSimpleADTs
  , docTest env "Polymorphism"                                                               testPolymorphism
  , docTest env "Multiple modules"                                                           testMultipleModules
  , docTest env "External packages, type sigs, scoped type vars, kind sigs"                  testExternalPkgs
  , docTest env "Reusing type variables"                                                     testReusingTypeVariables
  , docTest env "Qualified imports"                                                          testQualifiedImports
  , docTest env "Imprecise source spans"                                                     testImpreciseSourceSpans
  , docTest env "Quasi-quotation (QQ in own package)"                                        testQuasiOwnPackage
  , docTest env "Quasi-quotation (QQ in separate package, check home module info)"           testQuasiSeperatePackage
  , docTest env "Template Haskell"                                                           testTemplateHaskell
  , docTest env "Take advantage of scope (1)"                                                testScope1
  , docTest env "Take advantage of scope (2)"                                                testScope2
  , docTest env "Take advantage of scope (3)"                                                testScope3
  , docTest env "Take advantage of scope (4)"                                                testScope4
  , docTest env "Other constructs"                                                           testOtherConstructs
  , docTest env "FFI"                                                                        testFFI
  , docTest env "GADTs"                                                                      testGADTs
  , docTest env "Other types"                                                                testOtherTypes
  , docTest env "Default methods"                                                            testDefaultMethods
  , docTest env "Updated session (#142)"                                                     testUpdatedSession
  , docTest env "spanInfo vs expTypes (#3043)"                                               testSpanInfoVsExpTypes
  , docTest env "Consistency of IdMap/explicit sharing cache through multiple updates (#88)" test_StateOfCacheThroughoutUpdates
    -- TODO: Should these we docTest instead?
  , stdTest env "Subexpression types 1: Simple expressions"                                  test_SubExp_Simple
  , stdTest env "Subexpression types 2: TH and QQ"                                           test_SubExp_TH
  , stdTest env "Subexpression types 3: Type families (fpco #2609)"                          test_SubExp_TypeFamilies
  , stdTest env "Subexpression types 4: Higher rank types (fpco #2635)"                      test_SubExp_HigherRank
  , stdTest env "Subexpression types 5: Sections of functions with 3 or more args"           test_SubExp_Sections
  , stdTest env "Use sites 1: Global values"                                                 test_UseSites_GlobalValues
  , stdTest env "Use sites 2: Types"                                                         test_UseSites_Types
  , stdTest env "Use sites 3: Local identifiers"                                             test_UseSites_Local
  ]

test_Consistency_Local :: TestSuiteEnv -> Assertion
test_Consistency_Local env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertOneError session
  where
    upd = (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "import qualified Text.PrettyPrint as Disp"
            , "class Text a where"
            , "  disp  :: a -> Disp.Doc"
            , "display :: Text a => a -> String"
            , "display = Disp.renderStyle style . disp"
            , "  where style = Disp.Style {}"
            ])

test_Consistency_Imported :: TestSuiteEnv -> Assertion
test_Consistency_Imported env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
  where
    upd = (updateSourceFile "M.hs" . L.unlines $
            [ "module M where"
            , "import qualified Text.PrettyPrint as Disp"
            , "class Text a where"
            , "  disp  :: a -> Disp.Doc"
            , "display :: Text a => a -> String"
            , "display = Disp.renderStyle astyle . disp"
            , "  where astyle = Disp.Style {"
            , "          Disp.mode            = Disp.PageMode,"
            , "          Disp.lineLength      = 79,"
            , "          Disp.ribbonsPerLine  = 1.0"
            , "        }"
            ])

testLocalIdentifiersAndPrelude :: TestSuiteEnv -> Assertion
testLocalIdentifiersAndPrelude env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    assertIdInfo session "A" (2,1,2,2) "a" VarName "Int" "main:A" "A.hs@2:1-2:2" "" "binding occurrence"
    assertIdInfo session "A" (3,1,3,2) "b" VarName "Int" "main:A" "A.hs@3:1-3:2" "" "binding occurrence"
    assertIdInfo session "A" (3,5,3,6) "a" VarName "Int" "main:A" "A.hs@2:1-2:2" "" "defined locally"
    assertIdInfo session "A" (3,7,3,8) "+" VarName "Num a => a -> a -> a" "base-4.5.1.0:GHC.Num" "<no location info>" "base-4.5.1.0:Prelude" "imported from base-4.5.1.0:Prelude at A.hs@1:8-1:9"
    assertIdInfo session "A" (4,1,4,2) "c" VarName "Bool" "main:A" "A.hs@4:1-4:2" "" "binding occurrence"
    assertIdInfo session "A" (4,5,4,9) "True" DataName "" "ghc-prim-0.2.0.0:GHC.Types" "<wired into compiler>" "base-4.5.1.0:Data.Bool" "wired in to the compiler"
    assertIdInfo session "A" (5,1,5,2) "d" VarName "(a -> b -> b) -> b -> [a] -> b" "main:A" "A.hs@5:1-5:2" "" "binding occurrence"
    assertIdInfo session "A" (5,5,5,10) "foldr" VarName "(a1 -> b1 -> b1) -> b1 -> [a1] -> b1" "base-4.5.1.0:GHC.Base" "<no location info>" "base-4.5.1.0:Data.List" "imported from base-4.5.1.0:Prelude at A.hs@1:8-1:9"

    {- TODO: reenable
    assertEqual "Haddock link for A.b should be correct"
                "main/latest/doc/html/A.html#v:b" $
                haddockLink (idMapToMap idMapB Map.! SourceSpan "B.hs" 5 8 5 9)
    -}
  where
    upd = updateSourceFile "A.hs" . L.unlines $
            [ "module A where"
            , "a = (5 :: Int)"
            , "b = a + 6"
            , "c = True"
            , "d = foldr"
            ]

testSimpleADTs :: TestSuiteEnv -> Assertion
testSimpleADTs env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    assertIdInfo' session "A" (2,6,2,7) (2,6,2,7) "T" TcClsName [] "main:A" [(GHC742, "A.hs@2:6-2:7"), (GHC78, "A.hs@2:1-2:13")] "" (allVersions "binding occurrence")
    assertIdInfo session "A" (2,10,2,13) "MkT" DataName "T" "main:A" "A.hs@2:10-2:13" "" "binding occurrence"
  where
    upd = updateSourceFile "A.hs" . L.unlines $
            [ "module A where"
            , "data T = MkT"
            ]

testPolymorphism :: TestSuiteEnv -> Assertion
testPolymorphism env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    assertIdInfo' session "A" (2,6,2,12) (2,6,2,12) "TMaybe" TcClsName [] "main:A" [(GHC742, "A.hs@2:6-2:12"), (GHC78, "A.hs@2:1-2:35")] "" (allVersions "binding occurrence")
    assertIdInfo session "A" (2,13,2,14) "a" TvName "" "main:A" "A.hs@2:13-2:14" "" "binding occurrence"
    assertIdInfo session "A" (2,17,2,25) "TNothing" DataName "TMaybe a" "main:A" "A.hs@2:17-2:25" "" "binding occurrence"
    assertIdInfo' session "A" (2,28,2,33) (2,28,2,33) "TJust" DataName (allVersions "a -> TMaybe a") "main:A" [(GHC742, "A.hs@2:28-2:33"), (GHC78, "A.hs@2:28-2:35")] "" (allVersions "binding occurrence")
    assertIdInfo session "A" (2,34,2,35) "a" TvName "" "main:A" "A.hs@2:13-2:14" "" "defined locally"
    assertIdInfo session "A" (4,1,4,3) "f1" VarName "t -> t" "main:A" "A.hs@4:1-4:3" "" "binding occurrence"
    assertIdInfo session "A" (4,4,4,5) "x" VarName "t" "main:A" "A.hs@4:4-4:5" "" "binding occurrence"
    assertIdInfo session "A" (4,8,4,9) "x" VarName "t" "main:A" "A.hs@4:4-4:5" "" "defined locally"
    assertIdInfo session "A" (5,1,5,3) "f2" VarName "t -> t" "main:A" "A.hs@5:1-5:3" "" "binding occurrence"
    assertIdInfo session "A" (5,7,5,8) "x" VarName "t" "main:A" "A.hs@5:7-5:8" "" "binding occurrence"
    assertIdInfo session "A" (5,12,5,13) "x" VarName "t" "main:A" "A.hs@5:7-5:8" "" "defined locally"
    assertIdInfo session "A" (7,1,7,3) "g1" VarName "t -> t1 -> t" "main:A" "A.hs@7:1-7:3" "" "binding occurrence"
    assertIdInfo session "A" (7,4,7,5) "x" VarName "t" "main:A" "A.hs@7:4-7:5" "" "binding occurrence"
    assertIdInfo session "A" (7,6,7,7) "y" VarName "t1" "main:A" "A.hs@7:6-7:7" "" "binding occurrence"
    assertIdInfo session "A" (7,10,7,11) "x" VarName "t" "main:A" "A.hs@7:4-7:5" "" "defined locally"
    assertIdInfo session "A" (8,1,8,3) "g2" VarName "t -> t1 -> t" "main:A" "A.hs@8:1-8:3" "" "binding occurrence"
    assertIdInfo session "A" (8,7,8,8) "x" VarName "t" "main:A" "A.hs@8:7-8:8" "" "binding occurrence"
    assertIdInfo session "A" (8,9,8,10) "y" VarName "t1" "main:A" "A.hs@8:9-8:10" "" "binding occurrence"
    assertIdInfo session "A" (8,14,8,15) "x" VarName "t" "main:A" "A.hs@8:7-8:8" "" "defined locally"
    assertIdInfo session "A" (10,1,10,3) "h1" VarName "Bool" "main:A" "A.hs@10:1-10:3" "" "binding occurrence"
    assertIdInfo session "A" (10,6,10,10) "h1go" VarName "t -> t1 -> t" "main:A" "A.hs@12:5-12:9" "" "defined locally"
    assertIdInfo session "A" (10,11,10,15) "True" DataName "" "ghc-prim-0.2.0.0:GHC.Types" "<wired into compiler>" "base-4.5.1.0:Data.Bool" "wired in to the compiler"
    assertIdInfo session "A" (10,16,10,21) "False" DataName "" "ghc-prim-0.2.0.0:GHC.Types" "<wired into compiler>" "base-4.5.1.0:Data.Bool" "wired in to the compiler"
    assertIdInfo session "A" (12,5,12,9) "h1go" VarName "t -> t1 -> t" "main:A" "A.hs@12:5-12:9" "" "binding occurrence"
    assertIdInfo session "A" (12,10,12,11) "x" VarName "t" "main:A" "A.hs@12:10-12:11" "" "binding occurrence"
    assertIdInfo session "A" (12,12,12,13) "y" VarName "t1" "main:A" "A.hs@12:12-12:13" "" "binding occurrence"
    assertIdInfo session "A" (12,16,12,17) "x" VarName "t" "main:A" "A.hs@12:10-12:11" "" "defined locally"
    assertIdInfo session "A" (14,1,14,3) "h2" VarName "Bool" "main:A" "A.hs@14:1-14:3" "" "binding occurrence"
    assertIdInfo session "A" (14,6,14,10) "h2go" VarName "t -> t1 -> t" "main:A" "A.hs@16:5-16:9" "" "defined locally"
    assertIdInfo session "A" (14,11,14,15) "True" DataName "" "ghc-prim-0.2.0.0:GHC.Types" "<wired into compiler>" "base-4.5.1.0:Data.Bool" "wired in to the compiler"
    assertIdInfo session "A" (14,16,14,21) "False" (DataName) "" "ghc-prim-0.2.0.0:GHC.Types" "<wired into compiler>" "base-4.5.1.0:Data.Bool" "wired in to the compiler"
    assertIdInfo session "A" (16,5,16,9) "h2go" VarName "t -> t1 -> t" "main:A" "A.hs@16:5-16:9" "" "binding occurrence"
    assertIdInfo session "A" (16,13,16,14) "x" VarName "t" "main:A" "A.hs@16:13-16:14" "" "binding occurrence"
    assertIdInfo session "A" (16,15,16,16) "y" VarName "t1" "main:A" "A.hs@16:15-16:16" "" "binding occurrence"
    assertIdInfo session "A" (16,20,16,21) "x" VarName "t" "main:A" "A.hs@16:13-16:14" "" "defined locally"
  where
    upd = updateSourceFile "A.hs" . L.unlines $
            [ "module A where"
            , "data TMaybe a = TNothing | TJust a"
            , ""
            , "f1 x = x"
            , "f2 = \\x -> x"
            , ""
            , "g1 x y = x"
            , "g2 = \\x y -> x"
            , ""
            , "h1 = h1go True False"
            , "  where"
            , "    h1go x y = x"
            , ""
            , "h2 = h2go True False"
            , "  where"
            , "    h2go = \\x y -> x"
            ]

testMultipleModules :: TestSuiteEnv -> Assertion
testMultipleModules env = withAvailableSession env $ \session -> do
    updateSessionD session upd 2
    assertNoErrors session
    assertIdInfo' session "A" (2,6,2,7) (2,6,2,7) "T" TcClsName [] "main:A" [(GHC742, "A.hs@2:6-2:7"), (GHC78, "A.hs@2:1-2:13")] "" (allVersions "binding occurrence")
    assertIdInfo session "A" (2,10,2,13) "MkT" DataName "T" "main:A" "A.hs@2:10-2:13" "" "binding occurrence"
    assertIdInfo session "B" (3,1,3,4) "foo" VarName "T" "main:B" "B.hs@3:1-3:4" "" "binding occurrence"
    assertIdInfo session "B" (3,7,3,10) "MkT" DataName "T" "main:A" "A.hs@2:10-2:13" "" "imported from main:A at B.hs@2:1-2:9"
  where
    upd = (updateSourceFile "A.hs" . L.unlines $
            [ "module A where"
            , "data T = MkT"
            ])
            -- Make sure that an occurrence of MkT in a second module
            -- doesn't cause us to lose type information we learned
            -- while processing the first
       <> (updateSourceFile "B.hs" . L.unlines $
            [ "module B where"
            , "import A"
            , "foo = MkT"
            ])

testExternalPkgs :: TestSuiteEnv -> Assertion
testExternalPkgs env = withAvailableSession' env (withDynOpts opts) $ \session -> do
    updateSessionD session upd 2
    assertNoErrors session
    assertIdInfo session "A" (3,1,3,2) "e" VarName "Bool" "main:A" "A.hs@3:1-3:2" "" "binding occurrence"
    assertIdInfo session "A" (3,5,3,9) "True" DataName "" "ghc-prim-0.2.0.0:GHC.Types" "<wired into compiler>" "base-4.5.1.0:Data.Bool" "wired in to the compiler"
    assertIdInfo session "A" (3,10,3,16) "pseq" VarName "a -> b -> b" "parallel-3.2.0.3:Control.Parallel" "<no location info>" "parallel-3.2.0.3:Control.Parallel" "imported from parallel-3.2.0.3:Control.Parallel at A.hs@2:1-2:24"
    assertIdInfo session "A" (3,17,3,22) "False" DataName "" "ghc-prim-0.2.0.0:GHC.Types" "<wired into compiler>" "base-4.5.1.0:Data.Bool" "wired in to the compiler"
    assertIdInfo session "A" (4,1,4,2) "f" VarName "a -> a" "main:A" "A.hs@5:1-5:2" "" "defined locally"
    assertIdInfo' session "A" (4,6,4,7) (4,6,4,7) "a" TvName [] "main:A" [(GHC742, "A.hs@4:6-4:7"), (GHC78, "A.hs@4:6-4:12")] "" (allVersions "defined locally")
    assertIdInfo' session "A" (4,11,4,12) (4,11,4,12) "a" TvName [] "main:A" [(GHC742, "A.hs@4:6-4:7"), (GHC78, "A.hs@4:6-4:12")] "" (allVersions "defined locally")
    assertIdInfo session "A" (5,1,5,2) "f" VarName "a -> a" "main:A" "A.hs@5:1-5:2" "" "binding occurrence"
    assertIdInfo session "A" (5,3,5,4) "x" VarName "a" "main:A" "A.hs@5:3-5:4" "" "binding occurrence"
    assertIdInfo session "A" (5,7,5,8) "x" VarName "a" "main:A" "A.hs@5:3-5:4" "" "defined locally"
    assertIdInfo session "A" (6,1,6,2) "g" VarName "a -> a" "main:A" "A.hs@7:1-7:2" "" "defined locally"
    assertIdInfo session "A" (6,13,6,14) "a" TvName "" "main:A" "A.hs@6:13-6:14" "" "binding occurrence"
    assertIdInfo session "A" (6,16,6,17) "a" TvName "" "main:A" "A.hs@6:13-6:14" "" "defined locally"
    assertIdInfo session "A" (6,21,6,22) "a" TvName "" "main:A" "A.hs@6:13-6:14" "" "defined locally"
    assertIdInfo session "A" (7,1,7,2) "g" VarName "a -> a" "main:A" "A.hs@7:1-7:2" "" "binding occurrence"
    assertIdInfo session "A" (7,3,7,4) "x" VarName "a" "main:A" "A.hs@7:3-7:4" "" "binding occurrence"
    assertIdInfo session "A" (7,7,7,8) "x" VarName "a" "main:A" "A.hs@7:3-7:4" "" "defined locally"
    assertIdInfo session "A" (8,1,8,2) "h" VarName "a -> a" "main:A" "A.hs@9:1-9:2" "" "defined locally"
    assertIdInfo session "A" (8,13,8,14) "a" TvName "" "main:A" "A.hs@8:13-8:14" "" "binding occurrence"
    assertIdInfo session "A" (8,16,8,17) "a" TvName "" "main:A" "A.hs@8:13-8:14" "" "defined locally"
    assertIdInfo session "A" (8,21,8,22) "a" TvName "" "main:A" "A.hs@8:13-8:14" "" "defined locally"
    assertIdInfo session "A" (9,1,9,2) "h" VarName "a -> a" "main:A" "A.hs@9:1-9:2" "" "binding occurrence"
    assertIdInfo session "A" (9,3,9,4) "x" VarName "a" "main:A" "A.hs@9:3-9:4" "" "binding occurrence"
    assertIdInfo session "A" (9,7,9,8) "y" VarName "a" "main:A" "A.hs@12:5-12:6" "" "defined locally"
    assertIdInfo session "A" (11,5,11,6) "y" VarName "a" "main:A" "A.hs@12:5-12:6" "" "defined locally"
    assertIdInfo session "A" (11,5,11,6) "y" VarName "a" "main:A" "A.hs@12:5-12:6" "" "defined locally"
    assertIdInfo session "A" (11,10,11,11) "a" TvName "" "main:A" "A.hs@8:13-8:14" "" "defined locally"
    assertIdInfo session "A" (11,10,11,11) "a" TvName "" "main:A" "A.hs@8:13-8:14" "" "defined locally"
    assertIdInfo session "A" (12,5,12,6) "y" VarName "a" "main:A" "A.hs@12:5-12:6" "" "binding occurrence"
    assertIdInfo session "A" (12,9,12,10) "x" VarName "a" "main:A" "A.hs@9:3-9:4" "" "defined locally"
    assertIdInfo session "A" (13,1,13,2) "i" VarName "t a -> t a" "main:A" "A.hs@14:1-14:2" "" "defined locally"
    assertIdInfo session "A" (13,13,13,26) "t" TvName "" "main:A" "A.hs@13:13-13:26" "" "binding occurrence"
    assertIdInfo session "A" (13,27,13,28) "a" TvName "" "main:A" "A.hs@13:27-13:28" "" "binding occurrence"
    assertIdInfo session "A" (13,30,13,31) "t" TvName "" "main:A" "A.hs@13:13-13:26" "" "defined locally"
    assertIdInfo session "A" (13,32,13,33) "a" TvName "" "main:A" "A.hs@13:27-13:28" "" "defined locally"
    assertIdInfo session "A" (13,37,13,38) "t" TvName "" "main:A" "A.hs@13:13-13:26" "" "defined locally"
    assertIdInfo session "A" (13,39,13,40) "a" TvName "" "main:A" "A.hs@13:27-13:28" "" "defined locally"
    assertIdInfo session "A" (14,1,14,2) "i" VarName "t a -> t a" "main:A" "A.hs@14:1-14:2" "" "binding occurrence"
    assertIdInfo session "A" (14,3,14,4) "x" VarName "t a" "main:A" "A.hs@14:3-14:4" "" "binding occurrence"
    assertIdInfo session "A" (14,7,14,8) "x" VarName "t a" "main:A" "A.hs@14:3-14:4" "" "defined locally"
  where
    opts = [ "-XScopedTypeVariables"
           , "-XKindSignatures"
           ]

    upd = updateSourceFile "A.hs" . L.unlines $
            [ "module A where"

            , "import Control.Parallel"

            , "e = True `pseq` False"

            , "f :: a -> a"
            , "f x = x"

            , "g :: forall a. a -> a"
            , "g x = x"

            , "h :: forall a. a -> a"
            , "h x = y"
            , "  where"
            , "    y :: a"
            , "    y = x"

            , "i :: forall (t :: * -> *) a. t a -> t a"
            , "i x = x"
            ]

testReusingTypeVariables :: TestSuiteEnv -> Assertion
testReusingTypeVariables env = withAvailableSession' env (withDynOpts ["-XScopedTypeVariables"]) $ \session -> do
    updateSessionD session upd 2
    assertNoErrors session
    assertIdInfo session "A" (2,1,2,3) "f1" VarName "(t, t1) -> t" "main:A" "A.hs@2:1-2:3" "" "binding occurrence"
    assertIdInfo session "A" (2,5,2,6) "x" VarName "t" "main:A" "A.hs@2:5-2:6" "" "binding occurrence"
    assertIdInfo session "A" (2,8,2,9) "y" VarName "t1" "main:A" "A.hs@2:8-2:9" "" "binding occurrence"
    assertIdInfo session "A" (2,13,2,14) "x" VarName "t" "main:A" "A.hs@2:5-2:6" "" "defined locally"
    assertIdInfo session "A" (3,1,3,3) "f2" VarName "(t, t1) -> t" "main:A" "A.hs@3:1-3:3" "" "binding occurrence"
    assertIdInfo session "A" (3,5,3,6) "x" VarName "t" "main:A" "A.hs@3:5-3:6" "" "binding occurrence"
    assertIdInfo session "A" (3,8,3,9) "y" VarName "t1" "main:A" "A.hs@3:8-3:9" "" "binding occurrence"
    assertIdInfo session "A" (3,13,3,14) "x" VarName "t" "main:A" "A.hs@3:5-3:6" "" "defined locally"
    assertIdInfo session "A" (4,1,4,3) "f3" VarName "(t, t1) -> t" "main:A" "A.hs@4:1-4:3" "" "binding occurrence"
    assertIdInfo session "A" (4,5,4,6) "x" VarName "t" "main:A" "A.hs@4:5-4:6" "" "binding occurrence"
    assertIdInfo session "A" (4,8,4,9) "y" VarName "t1" "main:A" "A.hs@4:8-4:9" "" "binding occurrence"
    assertIdInfo session "A" (4,13,4,15) "f4" VarName "(t2, t3) -> t2" "main:A" "A.hs@6:5-6:7" "" "defined locally"
    assertIdInfo session "A" (4,17,4,18) "x" VarName "t" "main:A" "A.hs@4:5-4:6" "" "defined locally"
    assertIdInfo session "A" (4,20,4,21) "y" VarName "t1" "main:A" "A.hs@4:8-4:9" "" "defined locally"
    assertIdInfo session "A" (6,5,6,7) "f4" VarName "(t2, t3) -> t2" "main:A" "A.hs@6:5-6:7" "" "binding occurrence"
    assertIdInfo session "A" (6,9,6,10) "x" VarName "t2" "main:A" "A.hs@6:9-6:10" "" "binding occurrence"
    assertIdInfo session "A" (6,12,6,13) "y" VarName "t3" "main:A" "A.hs@6:12-6:13" "" "binding occurrence"
    assertIdInfo session "A" (6,17,6,18) "x" VarName "t2" "main:A" "A.hs@6:9-6:10" "" "defined locally"
    assertIdInfo session "A" (7,1,7,3) "f5" VarName "(t, t1) -> t" "main:A" "A.hs@8:1-8:3" "" "defined locally"
    assertIdInfo session "A" (7,14,7,15) "t" TvName "" "main:A" "A.hs@7:14-7:15" "" "binding occurrence"
    assertIdInfo session "A" (7,16,7,18) "t1" TvName "" "main:A" "A.hs@7:16-7:18" "" "binding occurrence"
    assertIdInfo session "A" (7,21,7,22) "t" TvName "" "main:A" "A.hs@7:14-7:15" "" "defined locally"
    assertIdInfo session "A" (7,24,7,26) "t1" TvName "" "main:A" "A.hs@7:16-7:18" "" "defined locally"
    assertIdInfo session "A" (7,31,7,32) "t" TvName "" "main:A" "A.hs@7:14-7:15" "" "defined locally"
    assertIdInfo session "A" (8,1,8,3) "f5" VarName "(t, t1) -> t" "main:A" "A.hs@8:1-8:3" "" "binding occurrence"
    assertIdInfo session "A" (8,5,8,6) "x" VarName "t" "main:A" "A.hs@8:5-8:6" "" "binding occurrence"
    assertIdInfo session "A" (8,8,8,9) "y" VarName "t1" "main:A" "A.hs@8:8-8:9" "" "binding occurrence"
    assertIdInfo session "A" (8,13,8,15) "f6" VarName "(t, t2) -> t" "main:A" "A.hs@11:5-11:7" "" "defined locally"
    assertIdInfo session "A" (8,17,8,18) "x" VarName "t" "main:A" "A.hs@8:5-8:6" "" "defined locally"
    assertIdInfo session "A" (8,20,8,21) "y" VarName "t1" "main:A" "A.hs@8:8-8:9" "" "defined locally"
    assertIdInfo session "A" (10,5,10,7) "f6" VarName "(t, t2) -> t" "main:A" "A.hs@11:5-11:7" "" "defined locally"
    assertIdInfo session "A" (10,5,10,7) "f6" VarName "(t, t2) -> t" "main:A" "A.hs@11:5-11:7" "" "defined locally"
    assertIdInfo session "A" (10,18,10,20) "t2" TvName "" "main:A" "A.hs@10:18-10:20" "" "binding occurrence"
    assertIdInfo session "A" (10,18,10,20) "t2" TvName "" "main:A" "A.hs@10:18-10:20" "" "binding occurrence"
    assertIdInfo session "A" (10,23,10,24) "t" TvName "" "main:A" "A.hs@7:14-7:15" "" "defined locally"
    assertIdInfo session "A" (10,23,10,24) "t" TvName "" "main:A" "A.hs@7:14-7:15" "" "defined locally"
    assertIdInfo session "A" (10,26,10,28) "t2" TvName "" "main:A" "A.hs@10:18-10:20" "" "defined locally"
    assertIdInfo session "A" (10,26,10,28) "t2" TvName "" "main:A" "A.hs@10:18-10:20" "" "defined locally"
    assertIdInfo session "A" (10,33,10,34) "t" TvName "" "main:A" "A.hs@7:14-7:15" "" "defined locally"
    assertIdInfo session "A" (10,33,10,34) "t" TvName "" "main:A" "A.hs@7:14-7:15" "" "defined locally"
    assertIdInfo session "A" (11,5,11,7) "f6" VarName "(t, t2) -> t" "main:A" "A.hs@11:5-11:7" "" "binding occurrence"
    assertIdInfo session "A" (11,9,11,10) "x" VarName "t" "main:A" "A.hs@11:9-11:10" "" "binding occurrence"
    assertIdInfo session "A" (11,12,11,13) "y" VarName "t2" "main:A" "A.hs@11:12-11:13" "" "binding occurrence"
    assertIdInfo session "A" (11,17,11,18) "x" VarName "t" "main:A" "A.hs@11:9-11:10" "" "defined locally"
  where
    upd = updateSourceFile "A.hs" . L.unlines $
            [ "module A where"

            , "f1 (x, y) = x"
            , "f2 (x, y) = x"

            , "f3 (x, y) = f4 (x, y)"
            , "  where"
            , "    f4 (x, y) = x"

            , "f5 :: forall t t1. (t, t1) -> t"
            , "f5 (x, y) = f6 (x, y)"
            , "  where"
            , "    f6 :: forall t2. (t, t2) -> t"
            , "    f6 (x, y) = x"
            ]

testQualifiedImports :: TestSuiteEnv -> Assertion
testQualifiedImports env = withAvailableSession env $ \session -> do
    updateSessionD session upd 2
    assertNoErrors session
    assertIdInfo session "A" (5,1,5,4) "foo" VarName "(Maybe a -> a, [Bool] -> Bool, (b -> b -> c) -> (a1 -> b) -> a1 -> a1 -> c)" "main:A" "A.hs@5:1-5:4" "" "binding occurrence"
    assertIdInfo session "A" (5,8,5,16) "fromJust" VarName "Maybe a2 -> a2" "base-4.5.1.0:Data.Maybe" "<no location info>" "base-4.5.1.0:Data.Maybe" "imported from base-4.5.1.0:Data.Maybe at A.hs@2:1-2:18"
    assertIdInfo session "A" (5,18,5,31) "and" VarName "[Bool] -> Bool" "base-4.5.1.0:GHC.List" "<no location info>" "base-4.5.1.0:Data.List" "imported from base-4.5.1.0:Data.List as 'Data.List.' at A.hs@3:1-3:27"
    assertIdInfo session "A" (5,33,5,37) "on" VarName "(b1 -> b1 -> c1) -> (a2 -> b1) -> a2 -> a2 -> c1" "base-4.5.1.0:Data.Function" "<no location info>" "base-4.5.1.0:Data.Function" "imported from base-4.5.1.0:Data.Function as 'F.' at A.hs@4:1-4:36"
  where
    upd = updateSourceFile "A.hs" . L.unlines $
            [ "module A where"
            , "import Data.Maybe"
            , "import qualified Data.List"
            , "import qualified Data.Function as F"
            , "foo = (fromJust, Data.List.and, F.on)"
            ]

testImpreciseSourceSpans :: TestSuiteEnv -> Assertion
testImpreciseSourceSpans env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session

    let checkPrint span = assertIdInfo' session "A" span (2, 8, 2, 13) "print" VarName (allVersions "Show a => a -> IO ()") "base-4.5.1.0:System.IO" (allVersions "<no location info>") "base-4.5.1.0:System.IO" (allVersions "imported from base-4.5.1.0:Prelude at A.hs@1:8-1:9")

    checkPrint (2,8,2,13)
    checkPrint (2,8,2,8)
    checkPrint (2,8,2,9)
    checkPrint (2,9,2,9)
    checkPrint (2,9,2,10)
    checkPrint (2,9,2,13)
  where
    upd = updateSourceFile "A.hs" . L.unlines $
            [ "module A where"
            , "main = print True"
            ]

testQuasiOwnPackage :: TestSuiteEnv -> Assertion
testQuasiOwnPackage env = withAvailableSession env $ \session -> do
    updateSessionD session upd 2
    assertNoErrors session
    {-
    let span l c = SourceSpan { spanFilePath   = "B.hs"
                              , spanFromLine   = l
                              , spanFromColumn = c
                              , spanToLine     = l
                              , spanToColumn   = c
                              }
    print (idInfo (Text.pack "B") (span 4 11))
    print (idInfo (Text.pack "B") (span 5 11))
    print (idInfo (Text.pack "B") (span 6 11))
    print (idInfo (Text.pack "B") (span 7 11))
    -}
    assertIdInfo session "B" (4,7,4,14) "qq" VarName "QuasiQuoter" "main:A" "A.hs@4:1-4:3" "" "imported from main:A at B.hs@3:1-3:9"
    assertIdInfo session "B" (5,7,5,14) "qq" VarName "QuasiQuoter" "main:A" "A.hs@4:1-4:3" "" "imported from main:A at B.hs@3:1-3:9"
    assertIdInfo session "B" (6,7,6,14) "qq" VarName "QuasiQuoter" "main:A" "A.hs@4:1-4:3" "" "imported from main:A at B.hs@3:1-3:9"
    assertIdInfo session "B" (7,7,7,14) "qq" VarName "QuasiQuoter" "main:A" "A.hs@4:1-4:3" "" "imported from main:A at B.hs@3:1-3:9"
  where
    upd = updateCodeGeneration True
       <> (updateSourceFile "A.hs" . L.unlines $
            [ "{-# LANGUAGE TemplateHaskell #-}"
            , "module A where"
            , "import Language.Haskell.TH.Quote"
            , "qq = QuasiQuoter {"
            , "         quoteExp  = \\str -> case str of"
            , "                                \"a\" -> [| True |]"
            , "                                \"b\" -> [| id True |]"
            , "                                \"c\" -> [| True || False |]"
            , "                                \"d\" -> [| False |]"
            , "       , quotePat  = undefined"
            , "       , quoteType = undefined"
            , "       , quoteDec  = undefined"
            , "       }"
            ])
       <> (updateSourceFile "B.hs" . L.unlines $
            [ "{-# LANGUAGE QuasiQuotes #-}"
            , "module B where"
            , "import A"
            -- 1234567890123
            , "ex1 = [qq|a|]"
            , "ex2 = [qq|b|]"
            , "ex3 = [qq|c|]"
            , "ex4 = [qq|d|]"
            ])

testQuasiSeperatePackage :: TestSuiteEnv -> Assertion
testQuasiSeperatePackage env = withAvailableSession env $ \session -> do
    updateSessionD session upd 2

    errs <- getSourceErrors session
    case errs of
      [] -> do
        -- TODO: why don't we get type information here?
        assertIdInfo session "Main" (6,19,8,3) "parseRoutes" VarName "" "yesod-routes-1.2.0.1:Yesod.Routes.Parse" "<no location info>" "yesod-core-1.2.2:Yesod.Core.Dispatch" "imported from yesod-1.2.1:Yesod at Main.hs@3:1-3:13"
        assertIdInfo session "Main" (9,26,11,5) "whamlet" VarName "" "yesod-core-1.2.2:Yesod.Core.Widget" "<no location info>" "yesod-core-1.2.2:Yesod.Core.Widget" "imported from yesod-1.2.1:Yesod at Main.hs@3:1-3:13"
      _ ->
        skipTest "Probably yesod package not installed"
  where
    upd = updateCodeGeneration True
       <> (updateSourceFile "Main.hs" . L.unlines $
            [ "{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,"
            , "             TemplateHaskell, OverloadedStrings #-}"
            , "import Yesod"

            , "data Piggies = Piggies"

            , "instance Yesod Piggies"

            , "mkYesod \"Piggies\" [parseRoutes|"
            , "  / HomeR GET"
            , "|]"

            , "getHomeR = defaultLayout [whamlet|"
            , "  Welcome to the Pigsty!"
            , "  |]"

            , "main = warpEnv Piggies"
            ])

testTemplateHaskell :: TestSuiteEnv -> Assertion
testTemplateHaskell env = withAvailableSession env $ \session -> do
    updateSessionD session upd 2
    assertNoErrors session
    assertIdInfo session "A" (4,1,4,4) "ex1" VarName "Q Exp" "main:A" "A.hs@5:1-5:4" "" "defined locally"
    assertIdInfo session "A" (4,8,4,9) "Q" TcClsName "" "template-haskell-2.7.0.0:Language.Haskell.TH.Syntax" "<no location info>" "template-haskell-2.7.0.0:Language.Haskell.TH.Syntax" "imported from template-haskell-2.7.0.0:Language.Haskell.TH at A.hs@3:1-3:27"
    assertIdInfo session "A" (4,10,4,13) "Exp" TcClsName "" "template-haskell-2.7.0.0:Language.Haskell.TH.Syntax" "<no location info>" "template-haskell-2.7.0.0:Language.Haskell.TH.Syntax" "imported from template-haskell-2.7.0.0:Language.Haskell.TH at A.hs@3:1-3:27"
    assertIdInfo session "A" (5,1,5,4) "ex1" VarName "Q Exp" "main:A" "A.hs@5:1-5:4" "" "binding occurrence"
    assertIdInfo session "A" (5,11,5,12) "x" VarName "" "main:A" "A.hs@5:11-5:12" "" "binding occurrence"
    assertIdInfo session "A" (5,16,5,17) "x" VarName "" "main:A" "A.hs@5:11-5:12" "" "defined locally"
    assertIdInfo session "A" (6,1,6,4) "ex2" VarName "Q Type" "main:A" "A.hs@7:1-7:4" "" "defined locally"
    assertIdInfo session "A" (6,8,6,9) "Q" TcClsName "" "template-haskell-2.7.0.0:Language.Haskell.TH.Syntax" "<no location info>" "template-haskell-2.7.0.0:Language.Haskell.TH.Syntax" "imported from template-haskell-2.7.0.0:Language.Haskell.TH at A.hs@3:1-3:27"
    assertIdInfo session "A" (6,10,6,14) "Type" TcClsName "" "template-haskell-2.7.0.0:Language.Haskell.TH.Syntax" "<no location info>" "template-haskell-2.7.0.0:Language.Haskell.TH.Syntax" "imported from template-haskell-2.7.0.0:Language.Haskell.TH at A.hs@3:1-3:27"
    assertIdInfo session "A" (7,1,7,4) "ex2" VarName "Q Type" "main:A" "A.hs@7:1-7:4" "" "binding occurrence"
    assertIdInfo session "A" (7,11,7,17) "String" TcClsName "" "base-4.5.1.0:GHC.Base" "<no location info>" "base-4.5.1.0:Data.String" "imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9"
    assertIdInfo session "A" (7,21,7,27) "String" TcClsName "" "base-4.5.1.0:GHC.Base" "<no location info>" "base-4.5.1.0:Data.String" "imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9"
    assertIdInfo session "B" (4,1,4,4) "ex5" VarName "String -> String" "main:B" "B.hs@5:1-5:4" "" "defined locally"
    assertIdInfo session "B" (4,8,4,12) "ex2" VarName "Q Type" "main:A" "A.hs@7:1-7:4" "" "imported from main:A at B.hs@3:1-3:9"
    assertIdInfo session "B" (5,1,5,4) "ex5" VarName "String -> String" "main:B" "B.hs@5:1-5:4" "" "binding occurrence"
    assertIdInfo session "B" (5,7,5,11) "ex1" VarName "Q Exp" "main:A" "A.hs@5:1-5:4" "" "imported from main:A at B.hs@3:1-3:9"

    assertIdInfo session "B" (6,21,6,24) "ex2" VarName "Q Type" "main:A" "A.hs@7:1-7:4" "" "imported from main:A at B.hs@3:1-3:9"
    assertIdInfo session "B" (7,9,7,12) "ex1" VarName "Q Exp" "main:A" "A.hs@5:1-5:4" "" "imported from main:A at B.hs@3:1-3:9"
    assertIdInfo session "B" (8,1,8,5) "ex3" VarName "Q [Dec]" "main:A" "A.hs@9:1-9:4" "" "imported from main:A at B.hs@3:1-3:9"
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
            , "module B where"
            , "import A"
              -- Types and expressions
            , "ex5 :: $ex2"
            , "ex5 = $ex1"
              -- Just to test slightly larger expressions
            , "ex6 :: $(return =<< ex2)"
            , "ex6 = $(ex1 >>= return)"
              -- Declarations
            , "$ex3"
            ])

testScope1 :: TestSuiteEnv -> Assertion
testScope1 env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    assertIdInfo session "A" (2,8,2,13) "print" VarName "Show a => a -> IO ()" "base-4.5.1.0:System.IO" "<no location info>" "base-4.5.1.0:System.IO" "imported from base-4.5.1.0:Prelude at A.hs@1:8-1:9"
  where
    upd = updateSourceFile "A.hs" . L.unlines $
            [ "module A where"
            , "main = print True"
            ]

testScope2 :: TestSuiteEnv -> Assertion
testScope2 env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    assertIdInfo session "A" (3,7,3,13) "append" VarName "Data.ByteString.Internal.ByteString -> Data.ByteString.Internal.ByteString -> Data.ByteString.Internal.ByteString" "bytestring-0.9.2.1:Data.ByteString" "<no location info>" "bytestring-0.9.2.1:Data.ByteString" "imported from bytestring-0.9.2.1:Data.ByteString at A.hs@2:25-2:31"
  where
    upd = updateSourceFile "A.hs" . L.unlines $
            [ "module A where"
            , "import Data.ByteString (append)"
            , "foo = append"
            ]

testScope3 :: TestSuiteEnv -> Assertion
testScope3 env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    assertIdInfo session "A" (3,7,3,13) "append" VarName "ByteString -> ByteString -> ByteString" "bytestring-0.9.2.1:Data.ByteString" "<no location info>" "bytestring-0.9.2.1:Data.ByteString" "imported from bytestring-0.9.2.1:Data.ByteString at A.hs@2:1-2:23"
  where
    upd = updateSourceFile "A.hs" . L.unlines $
            [ "module A where"
            , "import Data.ByteString"
            , "foo = append"
            ]

testScope4 :: TestSuiteEnv -> Assertion
testScope4 env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    assertIdInfo session "A" (4,7,4,13) "append" VarName "BS.ByteString -> BS.ByteString -> BS.ByteString" "bytestring-0.9.2.1:Data.ByteString" "<no location info>" "bytestring-0.9.2.1:Data.ByteString" "imported from bytestring-0.9.2.1:Data.ByteString as 'BS.' at A.hs@3:1-3:39"
  where
    upd = updateSourceFile "A.hs" . L.unlines $
            [ "module A where"
            , "import Data.ByteString (append)"
            , "import qualified Data.ByteString as BS"
            , "foo = append"
            ]

testOtherConstructs :: TestSuiteEnv -> Assertion
testOtherConstructs env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    assertIdInfo session "A" (4,10,4,12) "Eq" TcClsName "" "ghc-prim-0.2.0.0:GHC.Classes" "<no location info>" "base-4.5.1.0:Data.Eq" "imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9"
    assertIdInfo session "A" (5,18,5,23) "const" VarName "a -> b -> a" "base-4.5.1.0:GHC.Base" "<no location info>" "base-4.5.1.0:Prelude" "imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9"
    assertIdInfo session "A" (6,19,6,23) "Show" TcClsName "" "base-4.5.1.0:GHC.Show" "<no location info>" "base-4.5.1.0:Text.Show" "imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9"
    assertIdInfo' session "A" (6,24,6,27) (6,24,6,27) "MkT" TcClsName [] "main:A" [(GHC742, "A.hs@3:6-3:9"), (GHC78, "A.hs@3:1-3:15")]  "" (allVersions "defined locally")
    assertIdInfo session "A" (8,10,8,13) "+++" VarName "[a] -> [a] -> [a]" "main:A" "A.hs@7:1-7:6" "" "defined locally"
    assertIdInfo session "A" (9,10,9,13) "Int" TcClsName "" "ghc-prim-0.2.0.0:GHC.Types" "<wired into compiler>" "base-4.5.1.0:Data.Int" "wired in to the compiler"
    assertIdInfo session "A" (17,13,17,14) "x" VarName "Int" "main:A" "A.hs@17:3-17:4" "" "defined locally"
    assertIdInfo session "A" (17,21,17,22) "x" VarName "Int" "main:A" "A.hs@17:3-17:4" "" "defined locally"
    assertIdInfo session "A" (17,24,17,25) "y" VarName "Int" "main:A" "A.hs@17:5-17:6" "" "defined locally"
    assertIdInfo session "A" (17,31,17,32) "x" VarName "Int" "main:A" "A.hs@17:3-17:4" "" "defined locally"
    assertIdInfo session "A" (17,36,17,37) "z" VarName "Int" "main:A" "A.hs@17:7-17:8" "" "defined locally"
    assertIdInfo session "A" (17,41,17,42) "x" VarName "Int" "main:A" "A.hs@17:3-17:4" "" "defined locally"
    assertIdInfo session "A" (17,44,17,45) "y" VarName "Int" "main:A" "A.hs@17:5-17:6" "" "defined locally"
    assertIdInfo session "A" (17,49,17,50) "z" VarName "Int" "main:A" "A.hs@17:7-17:8" "" "defined locally"
    assertIdInfo session "A" (18,19,18,21) "xs" VarName "[Int]" "main:A" "A.hs@18:19-18:21" "" "binding occurrence"
    assertIdInfo session "A" (18,25,18,29) "Just" DataName "" "base-4.5.1.0:Data.Maybe" "<no location info>" "base-4.5.1.0:Data.Maybe" "imported from base-4.5.1.0:Prelude at A.hs@2:8-2:9"
    assertIdInfo session "A" (18,35,18,37) "xs" VarName "[Int]" "main:A" "A.hs@18:19-18:21" "" "defined locally"
  where
    langPragma = case testSuiteEnvGhcVersion env of
      GHC742 -> "{-# LANGUAGE StandaloneDeriving, DoRec #-}"
      GHC78  -> "{-# LANGUAGE StandaloneDeriving, RecursiveDo #-}"

    upd = updateSourceFile "A.hs" . L.unlines $
            [ {-  1 -} langPragma
            , {-  2 -} "module A where"

            , {-  3 -} "data MkT = MkT"

            , {-  4 -} "instance Eq MkT where"
            , {-  5 -} "  (==) = const $ const True"

            , {-  6 -} "deriving instance Show MkT"

            , {-  7 -} "(+++) = (++)"
            , {-  8 -} "infixr 5 +++"

            , {-  9 -} "default (Int)"

            , {- 10 -} "{-# DEPRECATED f \"This is completely obsolete\" #-}"
            , {- 11 -} "f :: Int"
            , {- 12 -} "f = 1"

            , {- 13 -} "{-# WARNING g \"You really shouldn't be using g!\" #-}"
            , {- 14 -} "g :: Int"
            , {- 15 -} "g = 2"

            , {- 16 -} "h :: Int -> Int -> Int -> ([Int], [Int], [Int], [Int])"
            , {- 17 -} "h x y z = ([x ..], [x, y..], [x .. z], [x, y .. z])"

            , {- 18 -} "justOnes = do rec xs <- Just (1 : xs)"
            , {- 19 -} "              return (map negate xs)"
            ]

testFFI :: TestSuiteEnv -> Assertion
testFFI env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    assertIdInfo' session "A" (5,28,5,33) (5,28,5,33) "c_sin" VarName (allVersions "CDouble -> CDouble") "main:A" [(GHC742, "A.hs@5:28-5:33"), (GHC78, "A.hs@5:1-5:55")] "" (allVersions "binding occurrence")
    assertIdInfo session "A" (5,37,5,44) "CDouble" TcClsName "" "base-4.5.1.0:Foreign.C.Types" "<no location info>" "base-4.5.1.0:Foreign.C.Types" "imported from base-4.5.1.0:Foreign.C at A.hs@4:1-4:17"
    assertIdInfo session "A" (10,22,10,29) "andBack" VarName "CDouble -> CDouble" "main:A" "A.hs@9:1-9:8" "" "defined locally"
    assertIdInfo session "A" (10,33,10,40) "CDouble" TcClsName "" "base-4.5.1.0:Foreign.C.Types" "<no location info>" "base-4.5.1.0:Foreign.C.Types" "imported from base-4.5.1.0:Foreign.C at A.hs@4:1-4:17"
  where
    upd = updateSourceFile "A.hs" . L.unlines $
            [ "{-# LANGUAGE ForeignFunctionInterface #-}"
            , "module A where"

            , "import Prelude hiding (sin)"
            , "import Foreign.C"

            , "foreign import ccall \"sin\" c_sin :: CDouble -> CDouble"

            , "sin :: Double -> Double"
            , "sin d = realToFrac (c_sin (realToFrac d))"

            , "andBack :: CDouble -> CDouble"
            , "andBack = realToFrac . sin . realToFrac"

            , "foreign export ccall andBack :: CDouble -> CDouble"
            ]

testGADTs :: TestSuiteEnv -> Assertion
testGADTs env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    -- TODO: These types should not contain explicit coercions (#68)
    assertIdInfo' session "A" (4,3,4,6) (4,3,4,6) "Num" DataName [(GHC742, "GHC.Prim.~# * ($a) Int -> Int -> Expr ($a)"), (GHC78, "($a GHC.Prim.~# Int) -> Int -> Expr $a")] "main:A" [(GHC742, "A.hs@4:3-4:6"), (GHC78, "A.hs@4:3-4:26")] "" (allVersions "binding occurrence")
    assertIdInfo session "A" (4,23,4,26) "Int" TcClsName "" "ghc-prim-0.2.0.0:GHC.Types" "<wired into compiler>" "base-4.5.1.0:Data.Int" "wired in to the compiler"
    assertIdInfo' session "A" (7,3,7,7) (7,3,7,7) "Cond" DataName (allVersions "Expr Bool -> Expr a -> Expr a -> Expr a") "main:A" [(GHC742, "A.hs@7:3-7:7"), (GHC78, "A.hs@7:3-7:60")] "" (allVersions "binding occurrence")
    assertIdInfo session "A" (7,18,7,19) "a" TvName "" "main:A" "A.hs@7:18-7:19" "" "binding occurrence"
    assertIdInfo' session "A" (7,54,7,58) (7,54,7,58) "Expr" TcClsName [] "main:A" [(GHC742, "A.hs@3:6-3:10"), (GHC78, "A.hs@3:1-7:60")] "" (allVersions "defined locally")
    assertIdInfo session "A" (7,59,7,60) "a" TvName "" "main:A" "A.hs@7:18-7:19" "" "defined locally"
  where
    upd = updateSourceFile "A.hs" . L.unlines $
            [ "{-# LANGUAGE GADTs, KindSignatures, RankNTypes #-}"
            , "module A where"

            , "data Expr :: * -> * where"
            , "  Num  :: Int -> Expr Int"
            , "  Bool :: Bool -> Expr Bool"
            , "  Add  :: Expr Int -> Expr Int -> Expr Int"
            , "  Cond :: forall a. Expr Bool -> Expr a -> Expr a -> Expr a"
            ]

testOtherTypes :: TestSuiteEnv -> Assertion
testOtherTypes env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    -- TODO: we don't get location info for the fundeps
    -- (this is missing from GHC's AST)
    assertIdInfo' session "A" (3,7,3,8) (3,7,3,8) "C" TcClsName [] "main:A" [(GHC742, "A.hs@3:7-3:8"), (GHC78, "A.hs@3:1-4:16")] "" (allVersions "binding occurrence")
    assertIdInfo session "A" (3,9,3,10) "a" TvName "" "main:A" "A.hs@3:9-3:10" "" "binding occurrence"
    assertIdInfo' session "A" (4,3,4,4) (4,3,4,4) "f" VarName [] "main:A" [(GHC742, "A.hs@4:3-4:4"), (GHC78, "A.hs@4:3-4:16")] "" (allVersions "defined locally")
    assertIdInfo session "A" (4,8,4,11) "Int" TcClsName "" "ghc-prim-0.2.0.0:GHC.Types" "<wired into compiler>" "base-4.5.1.0:Data.Int" "wired in to the compiler"
    assertIdInfo session "A" (4,15,4,16) "a" TvName "" "main:A" "A.hs@3:9-3:10" "" "defined locally"
    assertIdInfo' session "A" (5,7,5,8) (5,7,5,8) "D" TcClsName [] "main:A" [(GHC742, "A.hs@5:7-5:8"), (GHC78, "A.hs@5:1-6:14")] "" (allVersions "binding occurrence")
    assertIdInfo session "A" (5,9,5,10) "a" TvName "" "main:A" "A.hs@5:9-5:10" "" "binding occurrence"
    assertIdInfo session "A" (5,11,5,12) "b" TvName "" "main:A" "A.hs@5:11-5:12" "" "binding occurrence"
    assertIdInfo' session "A" (6,3,6,4) (6,3,6,4) "g" VarName [] "main:A" [(GHC742, "A.hs@6:3-6:4"), (GHC78, "A.hs@6:3-6:14")] "" (allVersions "defined locally")
    assertIdInfo session "A" (6,8,6,9) "a" TvName "" "main:A" "A.hs@5:9-5:10" "" "defined locally"
    assertIdInfo session "A" (6,13,6,14) "b" TvName "" "main:A" "A.hs@5:11-5:12" "" "defined locally"
    assertIdInfo' session "A" (7,6,7,9) (7,6,7,9) "Foo" TcClsName [] "main:A" [(GHC742, "A.hs@7:6-7:9"), (GHC78, "A.hs@7:1-7:15")] "" (allVersions "binding occurrence")
    assertIdInfo session "A" (7,12,7,15) "Int" TcClsName "" "ghc-prim-0.2.0.0:GHC.Types" "<wired into compiler>" "base-4.5.1.0:Data.Int" "wired in to the compiler"
    assertIdInfo' session "A" (8,13,8,16) (8,13,8,16) "Bar" TcClsName [] "main:A" [(GHC742, "A.hs@8:13-8:16"), (GHC78, "A.hs@8:1-8:18")] "" (allVersions "binding occurrence")
    assertIdInfo session "A" (8,17,8,18) "a" TvName "" "main:A" "A.hs@8:17-8:18" "" "binding occurrence"
    assertIdInfo' session "A" (9,15,9,18) (9,15,9,18) "Bar" TcClsName [] "main:A" [(GHC742, "A.hs@8:13-8:16"), (GHC78, "A.hs@8:1-8:18")] "" (allVersions "defined locally")
    assertIdInfo session "A" (9,19,9,22) "Int" TcClsName "" "ghc-prim-0.2.0.0:GHC.Types" "<wired into compiler>" "base-4.5.1.0:Data.Int" "wired in to the compiler"
    assertIdInfo session "A" (9,25,9,29) "Bool" TcClsName "" "ghc-prim-0.2.0.0:GHC.Types" "<wired into compiler>" "base-4.5.1.0:Data.Bool" "wired in to the compiler"
  where
    upd = updateSourceFile "A.hs" . L.unlines $
            [ {-  1 -} "{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, TypeFamilies #-}"
            , {-  2 -} "module A where"

            , {-  3 -} "class C a where"
            , {-  4 -} "  f :: Int -> a"

            , {-  5 -} "class D a b | a -> b where"
            , {-  6 -} "  g :: a -> b"

            , {-  7 -} "type Foo = Int"

            , {-  8 -} "type family Bar a"
            , {-  9 -} "type instance Bar Int = Bool"
            ]

testDefaultMethods :: TestSuiteEnv -> Assertion
testDefaultMethods env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session
    assertIdInfo session "A" (4,11,4,15) "succ" VarName "Enum a1 => a1 -> a1" "base-4.5.1.0:GHC.Enum" "<no location info>" "base-4.5.1.0:Prelude" "imported from base-4.5.1.0:Prelude at A.hs@1:8-1:9"
  where
    upd = updateSourceFile "A.hs" . L.unlines $
            [ "module A where"
            , "class Foo a where"
            , "  foo :: a -> Int"
            , "  foo _ = succ 1"
            ]

testUpdatedSession :: TestSuiteEnv -> Assertion
testUpdatedSession env = withAvailableSession env $ \session -> do
    updateSessionD session upd1 1
    assertNoErrors session
    assertIdInfo session "Main" (1,14,1,17) "foo" VarName "Integer" "main:Main" "Main.hs@2:1-2:4" "" "defined locally"

    updateSessionD session upd2 1
    assertNoErrors session
    assertIdInfo session "Main" (1,14,1,17) "foo" VarName "Integer" "main:Main" "Main.hs@3:1-3:4" "" "defined locally"
  where
    upd1 = updateSourceFile "Main.hs" "main = print foo\nfoo = 5"
    upd2 = updateSourceFile "Main.hs" "main = print foo\n\nfoo = 5"

testSpanInfoVsExpTypes :: TestSuiteEnv -> Assertion
testSpanInfoVsExpTypes env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    errs <- getSourceErrors session

    case errs of
      [] -> do
        -- Check the polymorphic type of getNum
        assertIdInfo session "A" (6,  8, 6, 14) "getNum" VarName "Stream s m2 Char => s -> m2 (Either ParseError Char)" "main:A" "A.hs@7:9-7:15" "" "defined locally"
        assertIdInfo session "A" (6, 31, 6, 37) "getNum" VarName "Stream s m2 Char => s -> m2 (Either ParseError Char)" "main:A" "A.hs@7:9-7:15" "" "defined locally"
        assertIdInfo session "A" (7,  9, 7, 15) "getNum" VarName "Stream s m2 Char => s -> m2 (Either ParseError Char)" "main:A" "A.hs@7:9-7:15" "" "binding occurrence"

        -- Check the monomorphic (local) type of getNum
        expTypes <- getExpTypes session
        assertExpTypes expTypes "A" (6,  8, 6, 14) [
            (6,  7, 6, 58, "(m (Either ParseError Char), m1 (Either ParseError Char))")
          , (6,  8, 6, 14, "String -> m (Either ParseError Char)")
          , (6,  8, 6, 14, "Stream s m2 Char => s -> m2 (Either ParseError Char)")
          , (6,  8, 6, 30, "m (Either ParseError Char)")
          ]
        assertExpTypes expTypes "A" (6, 31, 6, 37) [
            (6,  7, 6, 58, "(m (Either ParseError Char), m1 (Either ParseError Char))")
          , (6, 31, 6, 37, "ByteString -> m1 (Either ParseError Char)")
          , (6, 31, 6, 37, "Stream s m2 Char => s -> m2 (Either ParseError Char)")
          , (6, 31, 6, 57, "m1 (Either ParseError Char)")
          ]
        assertExpTypes expTypes "A" (7,  9, 7, 15) [
            (7,  9, 7, 15, "s -> m2 (Either ParseError Char)")
          ]

        -- For completeness' sake, check polymorphic type of foo
        assertIdInfo session "A" (6,  1, 6, 4) "foo" VarName "(Monad m1, Monad m) => (m (Either ParseError Char), m1 (Either ParseError Char))" "main:A" "A.hs@6:1-6:4" "" "binding occurrence"
      _ ->
        skipTest "Probably parsec package not installed"
  where
    upd = updateSourceFile "A.hs" . L.unlines $ [
        "{-# LANGUAGE NoMonomorphismRestriction #-}"
      , "{-# LANGUAGE OverloadedStrings #-}"
      , "module A where"
      , "import Data.ByteString"
      , "import Text.Parsec"
      --                  1           2         3          4          5         6
      --         123456789012345 67 890123456789012345678 90 123456789012345678901
      , {- 6 -} "foo = (getNum (\"x\" :: String),getNum (\"x\" :: ByteString))"
      --                  1         2         3          4
      --         1234567890123456789012345678901234567 890123 4
      , {- 7 -} "  where getNum = runParserT digit () \"x.txt\""
      ]

test_StateOfCacheThroughoutUpdates :: TestSuiteEnv -> Assertion
test_StateOfCacheThroughoutUpdates env = withAvailableSession' env (withDynOpts packageOpts) $ \sess -> do
    let cb     = \_ -> return ()
        update = flip (updateSession sess) cb
        updMod = \mod code -> updateSourceFile mod (L.fromString code)

    update $ updateCodeGeneration True
    update $ updateStdoutBufferMode (RunLineBuffering Nothing)
    update $ updateStderrBufferMode (RunLineBuffering Nothing)

    update $ updMod "Foo.hs" $ unlines [
        "module Foo where"
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
    assertIdInfo sess "Bar" (3, 7, 3, 15) "putStrLn" VarName "String -> IO ()" "base-4.5.1.0:System.IO" "<no location info>" "base-4.5.1.0:System.IO" "imported from base-4.5.1.0:Prelude at Bar.hs@1:8-1:11"

    update $ updMod "Baz.hs" $ unlines [
        "module Baz where"
      , ""
      , "import Foo"
      , "import Bar"
      , ""
      , "baz = foobar"
      ]

    assertNoErrors sess
    assertIdInfo sess "Bar" (3, 7, 3, 15) "putStrLn" VarName "String -> IO ()" "base-4.5.1.0:System.IO" "<no location info>" "base-4.5.1.0:System.IO" "imported from base-4.5.1.0:Prelude at Bar.hs@1:8-1:11"
    assertIdInfo sess "Baz" (6, 7, 6, 13) "foobar" VarName "IO ()" "main:Foo" "Foo.hs@7:1-7:7" "" "imported from main:Foo at Baz.hs@3:1-3:11"

    update $ updMod "Baz.hs" $ unlines [
        "module Baz where"
      , ""
      , "import Foo"
      , "import Bar"
      , ""
      , "baz = foobar >>>> foo >> bar"
      ]

    assertOneError sess
    assertIdInfo sess "Bar" (3, 7, 3, 15) "putStrLn" VarName "String -> IO ()" "base-4.5.1.0:System.IO" "<no location info>" "base-4.5.1.0:System.IO" "imported from base-4.5.1.0:Prelude at Bar.hs@1:8-1:11"
    -- Baz is broken at this point

    update $ updMod "Baz.hs" $ unlines [
        "module Baz where"
      , ""
      , "import Foo"
      , "import Bar"
      , ""
      , "baz = foobar >> foo >> bar"
      ]

    assertNoErrors sess
    assertIdInfo sess "Bar" (3, 7, 3, 15) "putStrLn" VarName "String -> IO ()" "base-4.5.1.0:System.IO" "<no location info>" "base-4.5.1.0:System.IO" "imported from base-4.5.1.0:Prelude at Bar.hs@1:8-1:11"
    assertIdInfo sess "Baz" (6, 7, 6, 13) "foobar" VarName "IO ()" "main:Foo" "Foo.hs@7:1-7:7" "" "imported from main:Foo at Baz.hs@3:1-3:11"
  where
    packageOpts = [ "-hide-all-packages"
                  , "-package base"
                  ]

test_SubExp_Simple :: TestSuiteEnv -> Assertion
test_SubExp_Simple env = withAvailableSession' env (withDynOpts ["-XNoMonomorphismRestriction"]) $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session

    -- TODO: the order of these "dominators" seems rather random!
    expTypes <- getExpTypes session
    assertExpTypes expTypes "A" (2, 6, 2, 6) [
        (2,  6, 2,  8, "Bool -> Bool")
      , (2,  6, 2,  8, "a -> a")
      , (2,  6, 2, 13, "Bool")
      ]
    assertExpTypes expTypes "A" (2, 9, 2, 13) [
        (2, 6, 2, 13, "Bool")
      , (2, 9, 2, 13, "Bool")
      ]
    assertExpTypes expTypes "A" (3, 7, 3, 7) [
        (3,  6, 3, 38, "Float")
      , (3,  7, 3, 19, "Int -> Float")
      , (3,  7, 3, 19, "(Integral a, Num b) => a -> b")
      ]
    assertExpTypes expTypes "A" (3, 37, 3, 37) [
        (3,  6, 3, 38, "Float")
      , (3, 37, 3, 38, "Int")
      ]
    assertExpTypes expTypes "A" (4, 7, 4, 7) [
        (4,  6, 4, 13, "t -> t")
      , (4,  7, 4,  8, "t")
      ]
    assertExpTypes expTypes "A" (5, 25, 5, 25) [
        (5,  6, 5, 32, "()")
      , (5, 24, 5, 27, "Bool -> ()")
      , (5, 24, 5, 27, "t -> ()")
      , (5, 24, 5, 32, "()")
      ]
    assertExpTypes expTypes "A" (6, 12, 6, 13) [
        (6, 12, 6, 13, "t")
      ]
    assertExpTypes expTypes "A" (7, 12, 7, 13) [
        (7, 12, 7, 13, "t1")
      ]
    assertExpTypes expTypes "A" (8, 12, 8, 13) [
        (8, 12, 8, 13, "t2")
      ]
    assertExpTypes expTypes "A" (9, 30, 9, 31) [
        (9, 16, 9, 50, "(t, t)")
      , (9, 17, 9, 32, "t")
      , (9, 24, 9, 31, "t")
      , (9, 30, 9, 31, "t2")
      ]
    assertExpTypes expTypes "A" (9, 35, 9, 36) [
        (9, 16, 9, 50, "(t, t)")
      , (9, 34, 9, 49, "t")
      , (9, 35, 9, 36, "t1")
      , (9, 35, 9, 42, "t")
      ]
    assertExpTypes expTypes "A" (10, 8, 10, 11) [
        (10, 7, 10, 19, "(Char, [Char])")
      , (10, 8, 10, 11, "Char")
      ]
    assertExpTypes expTypes "A" (10, 13, 10, 18) [
        (10,  7, 10, 19, "(Char, [Char])")
      , (10, 13, 10, 18, "[Char]")
      ]
    assertExpTypes expTypes "A" (11, 10, 11, 14) [
        (11,  7, 11, 48, "IO Int")
      , (11, 10, 11, 14, "String")
      ]
    assertExpTypes expTypes "A" (11, 36, 11, 42) [
        (11,  7, 11, 48, "IO Int")
      , (11, 28, 11, 48, "IO Int")
      , (11, 36, 11, 42, "[Char] -> Int")
      , (11, 36, 11, 42, "[a] -> Int")
      , (11, 36, 11, 47, "Int")
      ]
    assertExpTypes expTypes "A" (12, 8, 12, 12) [
        (12, 7, 12, 20, "[Bool]")
      , (12, 8, 12, 12, "Bool")
      ]
    assertExpTypes expTypes "A" (13, 8, 13, 9) [
        (13, 7, 13, 13, "[t]")
      , (13, 8, 13,  9, "t")
      ]
    assertExpTypes expTypes "A" (18, 7, 18, 18) [
        (18, 7, 18, 18, "Bool -> Int -> MyRecord")
      , (18, 7, 18, 38, "MyRecord")
      ]
    assertExpTypes expTypes "A" (18, 21, 18, 22) [
        (18,  7, 18, 38, "MyRecord")
      , (18, 21, 18, 22, "Bool")
      ]
    assertExpTypes expTypes "A" (18, 35, 18, 36) [
        (18,  7, 18, 38, "MyRecord")
      , (18, 35, 18, 36, "Int")
      ]
    assertExpTypes expTypes "A" (19, 13, 19, 14) [
        (19,  7, 19, 20, "MyRecord")
      , (19, 13, 19, 14, "Int")
      ]
    assertExpTypes expTypes "A" (21, 29, 21, 32) [
        (21,  7, 21, 34, "MyPolyRecord Char")
      , (21, 29, 21, 32, "Char")
      ]
    assertExpTypes expTypes "A" (21, 7, 21, 22) [
        (21,  7, 21, 22, "Char -> MyPolyRecord Char")
      , (21,  7, 21, 22, "a -> MyPolyRecord a")
      , (21,  7, 21, 34, "MyPolyRecord Char")
      ]
    assertExpTypes expTypes "A" (22, 13, 22, 14) [
        (22,  7, 22, 22, "MyPolyRecord Char")
      , (22, 13, 22, 14, "Char")
      ]
    -- the "MyRecordCon" apparently is not really recorded as such in the
    -- AST, we don't get type information about it
    assertExpTypes expTypes "A" (23, 19, 23, 30) [
        (23, 7, 23, 39, "Int")
      ]
    assertExpTypes expTypes "A" (24, 35, 24, 36) [
        (24,  7, 24, 41, "Char")
      , (24, 35, 24, 36, "Char")
      ]
    assertExpTypes expTypes "A" (25, 24, 25, 25) [
        (25,  9, 26, 36, "Maybe a")
      , (25, 24, 25, 25, "t")
      ]
    assertExpTypes expTypes "A" (26, 25, 26, 26) [
        (25,  9, 26, 36, "Maybe a")
      , (26, 25, 26, 26, "a")
      ]
    assertExpTypes expTypes "A" (27, 23, 27, 24) [
        (27, 13, 27, 31, "t")
      , (27, 23, 27, 24, "t")
      ]
    assertExpTypes expTypes "A" (28, 8, 28, 13) [
        (28, 8, 28, 13, "a -> [a]")
      , (28, 8, 28, 13, "Enum a1 => a1 -> [a1]")
      , (28, 8, 28, 16, "Int -> a")
      ]
    -- [a1] -> Int -> [a1] is the polymorphic type of (!!),
    -- [a] -> Int -> [a] is the "monomorphic" instance (with type vars)
    assertExpTypes expTypes "A" (29, 8, 29, 10) [
        (29, 8, 29, 10, "[a] -> Int -> a")
      , (29, 8, 29, 10, "[a1] -> Int -> a1")
      , (29, 8, 29, 12, "[a] -> a")
      ]
    -- The 'negation' isn't a proper operator and therefore doesn't get its
    -- own type
    assertExpTypes expTypes "A" (30, 9, 30, 10) [
        (30, 9, 30, 12, "a")
      ]
  where
    upd = (updateSourceFile "A.hs" . L.unlines $
            [ "module A where"
              -- Single type var inst, boolean literal
              --        123456789012
            , {-  2 -} "t2 = id True"
              -- Double type var inst, double evidence app, overloaded lit
              --        1234567890123456789012345678901234567
            , {-  3 -} "t3 = (fromIntegral :: Int -> Float) 1"
              -- Lambda
              --        12345 6789012
            , {-  4 -} "t4 = \\x -> x"
              -- Let
              --        1234567890123456789012345678901
            , {-  5 -} "t5 = let foo x = () in foo True"
              -- Type variables
              --        123456789012
            , {-  6 -} "t6 x y z = x"
            , {-  7 -} "t7 x y z = y"
            , {-  8 -} "t8 x y z = z"
              -- Brackets, tuples, operators
              --       0         1         2         3         4
              --        1234567890123456789012345678901234567890123456789
            , {-  9 -} "t9 f g x y z = (x `f` (y `g` z), (x `f` y) `g` z)"
              -- Literals
              --        123456789012 3456 78
            , {- 10 -} "t10 = ('a', \"foo\")"
              -- Do expression
              --       0         1         2         3         4
              --        12345678901234567890123456789012345678901234567
            , {- 11 -} "t11 = do line <- getLine ; return (length line)"
              -- Lists
              --        1234567890123456789
            , {- 12 -} "t12 = [True, False]"
            , {- 13 -} "t13 = [1, 2]"
              -- Records
            , "data MyRecord = MyRecordCon {"
            , "    a :: Bool"
            , "  , b :: Int"
            , "  }"
              --       0         1         2         3         4
              --        1234567890123456789012345678901234567
            , {- 18 -} "t18 = MyRecordCon { a = True, b = 5 }"
              -- Record update
            , {- 19 -} "t19 = t18 { b = 6 }"
              -- Polymorphic records
            , "data MyPolyRecord a = MyPolyRecordCon { c :: a }"
              --       0         1         2         3
              --        123456789012345678901234567890123
            , {- 21 -} "t21 = MyPolyRecordCon { c = 'a' }"
            , {- 22 -} "t22 = t21 { c = 'b' }"
              -- Case statements, underscore
              --       0         1         2         3         4
              --        1234567890123456789012345678901234567890
            , {- 23 -} "t23 = case t18 of MyRecordCon a b -> b"
            , {- 24 -} "t24 = case t21 of MyPolyRecordCon c -> c"
            , {- 25 -} "t25 x = case x of Left _  -> Nothing"
            , {- 26 -} "                  Right y -> Just y"
              -- If statement
              --       0         1         2         3
              --        123456789012345678901234567890
            , {- 27 -} "t27 x y z = if x then y else z"
              -- Sections, arithmetic sequences
              --        1234567890123456
            , {- 28 -} "t28 = ([1..] !!)"
            , {- 29 -} "t29 = (!! 0)"
              -- Negation
              --        12345678901
            , {- 30 -} "t30 x = - x"
            ])

test_SubExp_TH :: TestSuiteEnv -> Assertion
test_SubExp_TH env = withAvailableSession' env (withDynOpts ["-XNoMonomorphismRestriction", "-XTemplateHaskell"]) $ \session -> do
    updateSessionD session upd 2
    assertNoErrors session

    expTypes <- getExpTypes session
    -- The AST doesn't really give us a means to extract the type of
    -- a bracket expression :( And we get no info about the lifted vars
    assertExpTypes expTypes "A" (3, 9, 3, 20) []
    assertExpTypes expTypes "A" (4, 9, 4, 20) []
    assertExpTypes expTypes "A" (5, 17, 5, 18) []
    -- We don't return types for types, so check the expr splice only
    assertExpTypes expTypes "B" (3, 6, 3, 12) [
        (3, 6, 3, 12, "Bool")
      ]
    -- The typechecked tree contains the expanded splice, but the location
    -- of every term in the splice is set to the location of the entire
    -- splice
    assertExpTypes expTypes "B" (4, 8, 4, 13) [
        (4, 8, 4, 22, "[Char]")
      , (4, 8, 4, 22, "[Char]")
      , (4, 8, 4, 22, "Char -> [Char] -> [Char]")
      , (4, 8, 4, 22, "a -> [a] -> [a]")
      , (4, 8, 4, 22, "Char")
      ]
  where
    upd = (updateSourceFile "A.hs" . L.unlines $
            [ {-  1 -} "module A where"
            , {-  2 -} "import Language.Haskell.TH.Syntax"
              -- Quotations (expressions, types)
              --        1234567890123456789
            , {-  3 -} "qTrue = [|  True |]"
            , {-  4 -} "qBool = [t| Bool |]"
              --        1234567890123456789012345
            , {-  5 -} "qComp x xs = [| x : xs |]"
            ])
       <> (updateSourceFile "B.hs" . L.unlines $
            [ {-  1 -} "module B where"
            , {-  2 -} "import A"
              -- Splices (expressions, types)
              --        123456789012345678901
            , {-  3 -} "t1 = $qTrue :: $qBool"
              --        12345678901234567 890 12
            , {-  4 -} "t2 = $(qComp 'a' \"bc\")"
            ])
       <> (updateCodeGeneration True)

test_SubExp_TypeFamilies :: TestSuiteEnv -> Assertion
test_SubExp_TypeFamilies env = withAvailableSession' env (withDynOpts ["-XTypeFamilies"]) $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session

    expTypes <- getExpTypes session
    assertExpTypes expTypes "A" (5, 6, 5, 10) [
        (5, 6, 5, 10, "TestTypeFamily ()")
      , (5, 6, 5, 10, "Bool")
      ]
    assertExpTypes expTypes "A" (8, 6, 8, 13) [
        (8, 6, 8, 13, "TestTypeFamily [a]")
      , (8, 6, 8, 13, "Maybe a")
      , (8, 6, 8, 13, "Maybe a1")
      ]
  where
    upd = (updateSourceFile "A.hs" . L.unlines $ [
               {- 1 -} "module A where"
             , {- 2 -} "type family TestTypeFamily a"
             -- Monomorphic instance
             , {- 3 -} "type instance TestTypeFamily () = Bool"
             , {- 4 -} "t1 :: TestTypeFamily ()"
               --       123456789
             , {- 5 -} "t1 = True"
             -- Polymorphic instance
             , {- 6 -} "type instance TestTypeFamily [a] = Maybe a"
             , {- 7 -} "t2 :: TestTypeFamily [a]"
               --       123456789012
             , {- 8 -} "t2 = Nothing"
             ])

test_SubExp_HigherRank :: TestSuiteEnv -> Assertion
test_SubExp_HigherRank env = withAvailableSession' env (withDynOpts ["-XRank2Types"]) $ \session -> do
    -- Note: intentionally using (==) in this test rather than (<=) so that
    -- the "definition type" is different from the "usage type"
    -- (forall a. Eq a => a -> a -> Bool) vs (forall a. Ord a => a -> a -> Bool)

    updateSessionD session upd 1
    assertNoErrors session

    expTypes <- getExpTypes session
    assertExpTypes expTypes "A" (3, 12, 3, 14) [
        (3,  7, 3, 15, "T")
      , (3, 11, 3, 15, "Ord a => a -> a -> Bool")
      , (3, 11, 3, 15, "a -> a -> Bool")
      , (3, 11, 3, 15, "Eq a => a -> a -> Bool")
      ]
  where
    upd = (updateSourceFile "A.hs" . L.unlines $ [
               {- 1 -} "module A where"
             , {- 2 -} "newtype T = MkT (forall a. Ord a => a -> a -> Bool)"
               --       12345678901234
             , {- 3 -} "teq = MkT (==)"
             ])

test_SubExp_Sections :: TestSuiteEnv -> Assertion
test_SubExp_Sections env = withAvailableSession env $ \session -> do
    updateSessionD session upd 1
    assertNoErrors session

    expTypes <- getExpTypes session
    assertExpTypes expTypes "A" (4, 11, 4, 12) [
        (4, 10, 4, 13, "Int -> Bool -> Char -> ()")
      , (4, 10, 4, 18, "Int -> Char -> ()")
      ]
    assertExpTypes expTypes "A" (5, 13, 5, 14) [
        (5, 10, 5, 15, "Bool -> Char -> ()")
      , (5, 12, 5, 15, "Int -> Bool -> Char -> ()")
      ]
  where
    upd = (updateSourceFile "A.hs" . L.unlines $ [
               {- 1 -} "module A where"
             , {- 2 -} "f :: Int -> Bool -> Char -> ()"
             , {- 3 -} "f = undefined"
               --       1234567890123456789
             , {- 4 -} "test1 = (`f` True)"
             , {- 5 -} "test2 = (1 `f`)"
             ])

test_UseSites_GlobalValues :: TestSuiteEnv -> Assertion
test_UseSites_GlobalValues env = withAvailableSession env $ \session -> do
    updateSessionD session upd1 2
    assertNoErrors session

    do useSites <- getUseSites session
       assertUseSites useSites "A" (3,  1, 3,  2) "f" uses_f
       assertUseSites useSites "A" (3,  6, 3,  7) "+" uses_add
       assertUseSites useSites "A" (5,  1, 5,  2) "g" uses_g
       assertUseSites useSites "B" (4,  1, 4,  2) "h" uses_h
       assertUseSites useSites "B" (4,  6, 4,  7) "+" uses_add
       assertUseSites useSites "B" (4, 13, 4, 14) "f" uses_f
       assertUseSites useSites "B" (4, 17, 4, 18) "g" uses_g

    -- Update B, swap positions of g and f

    updateSessionD session upd2 1
    assertNoErrors session

    do useSites <- getUseSites session
       assertUseSites useSites "A" (3,  1, 3,  2) "f" uses_f2
       assertUseSites useSites "A" (3,  6, 3,  7) "+" uses_add
       assertUseSites useSites "A" (5,  1, 5,  2) "g" uses_g2
       assertUseSites useSites "B" (4,  1, 4,  2) "h" uses_h
       assertUseSites useSites "B" (4,  6, 4,  7) "+" uses_add
       assertUseSites useSites "B" (4, 13, 4, 14) "g" uses_g2
       assertUseSites useSites "B" (4, 17, 4, 18) "f" uses_f2

    -- Update A, remove one internal use of f, and shift the other

    updateSessionD session upd3 2
    assertNoErrors session

    do useSites <- getUseSites session
       assertUseSites useSites "A" (3,  1, 3,  2) "f" uses_f3
       assertUseSites useSites "A" (3,  6, 3,  7) "+" uses_add3
       assertUseSites useSites "A" (5,  1, 5,  2) "g" uses_g2
       assertUseSites useSites "B" (4,  1, 4,  2) "h" uses_h
       assertUseSites useSites "B" (4,  6, 4,  7) "+" uses_add3
       assertUseSites useSites "B" (4, 13, 4, 14) "g" uses_g2
       assertUseSites useSites "B" (4, 17, 4, 18) "f" uses_f3
  where
    upd1 = (updateSourceFile "A.hs" . L.unlines $ [
                {- 1 -} "module A where"
                      -- 123456789012345
              , {- 2 -} "f :: Int -> Int"
              , {- 3 -} "f = (+ 1)"
              , {- 4 -} "g :: Int -> Int"
              , {- 5 -} "g = f . f"
              ])
        <> (updateSourceFile "B.hs" . L.unlines $ [
                {- 1 -} "module B where"
              , {- 2 -} "import A"
              , {- 3 -} "h :: Int -> Int"
              , {- 4 -} "h = (+ 1) . f . g"
              ])
    upd2 = (updateSourceFile "B.hs" . L.unlines $ [
                {- 1 -} "module B where"
              , {- 2 -} "import A"
              , {- 3 -} "h :: Int -> Int"
              , {- 4 -} "h = (+ 1) . g . f"
              ])
    upd3 = (updateSourceFile "A.hs" . L.unlines $ [
                {- 1 -} "module A where"
                      -- 123456789012345
              , {- 2 -} "f :: Int -> Int"
              , {- 3 -} "f = (+ 1)"
              , {- 4 -} "g :: Int -> Int"
              , {- 5 -} "g = (+ 1) . f"
              ])


    uses_f = [
        "A.hs@5:9-5:10"
      , "A.hs@5:5-5:6"
      , "B.hs@4:13-4:14"
      ]
    uses_add = [ -- "+"
        "A.hs@3:6-3:7"
      , "B.hs@4:6-4:7"
      ]
    uses_g = [
        "B.hs@4:17-4:18"
      ]
    uses_h = [
      ]

    uses_f2 = [
        "A.hs@5:9-5:10"
      , "A.hs@5:5-5:6"
      , "B.hs@4:17-4:18"
      ]
    uses_g2 = [
        "B.hs@4:13-4:14"
      ]

    uses_f3 = [
        "A.hs@5:13-5:14"
      , "B.hs@4:17-4:18"
      ]
    uses_add3 = [ -- "+"
        "A.hs@5:6-5:7"
      , "A.hs@3:6-3:7"
      , "B.hs@4:6-4:7"
      ]

test_UseSites_Types :: TestSuiteEnv -> Assertion
test_UseSites_Types env = withAvailableSession env $ \session -> do
    -- This test follows the same structure as "Use sites 1", but
    -- tests types rather than values

    updateSessionD session upd1 2
    assertNoErrors session

    do useSites <- getUseSites session
       assertUseSites useSites "A" (2,  6, 2,  7) "F"   uses_F
       assertUseSites useSites "A" (2, 14, 2, 17) "Int" uses_Int
       assertUseSites useSites "A" (3,  6, 3,  7) "G"   uses_G
       assertUseSites useSites "B" (3,  6, 3,  7) "H"   uses_H
       assertUseSites useSites "B" (3, 14, 3, 17) "Int" uses_Int
       assertUseSites useSites "B" (3, 18, 3, 19) "F"   uses_F
       assertUseSites useSites "B" (3, 20, 3, 21) "G"   uses_G

    -- Update B, swap positions of g and f

    updateSessionD session upd2 1
    assertNoErrors session

    do useSites <- getUseSites session
       assertUseSites useSites "A" (2,  6, 2,  7) "F"   uses_F2
       assertUseSites useSites "A" (2, 14, 2, 17) "Int" uses_Int
       assertUseSites useSites "A" (3,  6, 3,  7) "G"   uses_G2
       assertUseSites useSites "B" (3,  6, 3,  7) "H"   uses_H
       assertUseSites useSites "B" (3, 14, 3, 17) "Int" uses_Int
       assertUseSites useSites "B" (3, 18, 3, 19) "G"   uses_G2
       assertUseSites useSites "B" (3, 20, 3, 21) "F"   uses_F2

    -- Update A, remove one internal use of f, and shift the other

    updateSessionD session upd3 2
    assertNoErrors session

    do useSites <- getUseSites session
       assertUseSites useSites "A" (2,  6, 2,  7) "F"   uses_F3
       assertUseSites useSites "A" (2, 14, 2, 17) "Int" uses_Int3
       assertUseSites useSites "A" (3,  6, 3,  7) "G"   uses_G2
       assertUseSites useSites "B" (3,  6, 3,  7) "H"   uses_H
       assertUseSites useSites "B" (3, 14, 3, 17) "Int" uses_Int3
       assertUseSites useSites "B" (3, 18, 3, 19) "G"   uses_G2
       assertUseSites useSites "B" (3, 20, 3, 21) "F"   uses_F3
  where
    upd1 = (updateSourceFile "A.hs" . L.unlines $ [
                {- 1 -} "module A where"
                      -- 1234567890123456
              , {- 2 -} "data F = MkF Int"
              , {- 3 -} "data G = MkG F F"
              ])
        <> (updateSourceFile "B.hs" . L.unlines $ [
                {- 1 -} "module B where"
              , {- 2 -} "import A"
                      -- 12345678901234567890
              , {- 3 -} "data H = MkH Int F G"
              ])
    upd2 = (updateSourceFile "B.hs" . L.unlines $ [
                {- 1 -} "module B where"
              , {- 2 -} "import A"
                      -- 12345678901234567890
              , {- 3 -} "data H = MkH Int G F"
              ])
    upd3 = (updateSourceFile "A.hs" . L.unlines $ [
                {- 1 -} "module A where"
                      -- 123456789012345678
              , {- 2 -} "data F = MkF Int"
              , {- 3 -} "data G = MkG Int F"
              ])

    uses_F = [
        "A.hs@3:16-3:17"
      , "A.hs@3:14-3:15"
      , "B.hs@3:18-3:19"
      ]
    uses_Int = [
        "A.hs@2:14-2:17"
      , "B.hs@3:14-3:17"
      ]
    uses_G = [
        "B.hs@3:20-3:21"
      ]
    uses_H = [
      ]

    uses_F2 = [
        "A.hs@3:16-3:17"
      , "A.hs@3:14-3:15"
      , "B.hs@3:20-3:21"
      ]
    uses_G2 = [
        "B.hs@3:18-3:19"
      ]

    uses_F3 = [
        "A.hs@3:18-3:19"
      , "B.hs@3:20-3:21"
      ]
    uses_Int3 = [
        "A.hs@3:14-3:17"
      , "A.hs@2:14-2:17"
      , "B.hs@3:14-3:17"
      ]

test_UseSites_Local :: TestSuiteEnv -> Assertion
test_UseSites_Local env = withAvailableSession' env (withDynOpts ["-XScopedTypeVariables"]) $ \session -> do
    updateSessionD session upd1 2
    assertNoErrors session
    useSites <- getUseSites session

    -- where-bound
    do let uses_f = [
               "A.hs@7:14-7:15"
             , "A.hs@7:10-7:11"
             , "A.hs@2:16-2:17"
             ]
           uses_add = [ -- "+"
               "A.hs@5:11-5:12"
             , "A.hs@2:9-2:10"
             ]
           uses_g = [
               "A.hs@2:20-2:21"
             ]

       assertUseSites useSites "A" (5,  6, 5,  7) "f" uses_f
       assertUseSites useSites "A" (5, 11, 5, 12) "+" uses_add
       assertUseSites useSites "A" (7,  6, 7,  7) "g" uses_g

    -- function args, lambda bound, let bound, case bound
    do let uses_f = ["A.hs@8:22-8:23"]
           uses_x = ["A.hs@8:24-8:25"]
           uses_a = ["A.hs@9:31-9:32"]
           uses_b = ["A.hs@9:39-9:40","A.hs@9:35-9:36"]
           uses_c = ["A.hs@10:30-10:31","A.hs@10:26-10:27"]

       assertUseSites useSites "A" (8,  7, 8,  8) "f" uses_f
       assertUseSites useSites "A" (8, 12, 8, 13) "x" uses_x
       assertUseSites useSites "A" (9, 14, 9, 15) "a" uses_a
       assertUseSites useSites "A" (9, 35, 9, 36) "b" uses_b -- using def site for variety
       assertUseSites useSites "A" (9, 27, 9, 28) "c" uses_c

    -- type args
    do let uses_a = ["A.hs@11:20-11:21","A.hs@11:18-11:19"]
           uses_b = ["A.hs@11:22-11:23"]

       assertUseSites useSites "A" (11, 18, 11, 19) "a" uses_a
       assertUseSites useSites "A" (11, 10, 11, 11) "b" uses_b

    -- implicit forall
    do let uses_a = ["A.hs@12:16-12:17","A.hs@12:6-12:7"]
           uses_b = ["A.hs@12:11-12:12"]

       assertUseSites useSites "A" (12,  6, 12,  7) "a" uses_a
       assertUseSites useSites "A" (12, 11, 12, 12) "b" uses_b

    -- explicit forall
    do let uses_a = ["A.hs@14:28-14:29","A.hs@14:18-14:19"]
           uses_b = ["A.hs@14:23-14:24"]

       assertUseSites useSites "A" (14, 13, 14, 14) "a" uses_a
       assertUseSites useSites "A" (14, 23, 14, 24) "b" uses_b
  where
    upd1 = (updateSourceFile "A.hs" . L.unlines $ [
                {-  1 -} "module A where"
                      -- 123456789012345
              , {-  2 -} "test = (+ 1) . f . g"
              , {-  3 -} "  where"
              , {-  4 -} "     f :: Int -> Int"
              , {-  5 -} "     f = (+ 1)"
              , {-  6 -} "     g :: Int -> Int"
              , {-  7 -} "     g = f . f"
                -- Function args, lambda bound, let bound, case bound
                       --          1         2         3
                       -- 1234567890 12345678901234567
              , {-  8 -} "test2 f = \\x -> case f x of"
                       -- 123456789012345678901234567890123456789
              , {-  9 -} "            (a, b) -> let c = a * b * b"
              , {- 10 -} "                      in c * c"
                -- Type arguments
                       -- 1234567890123456789012
              , {- 11 -} "data T a b = MkT a a b"
                -- Implicit forall
                       -- 1234567890123456
              , {- 12 -} "f :: a -> b -> a"
              , {- 13 -} "f x y = x"
                -- Explicit forall
                       -- 1234567890123456789012345678
              , {- 14 -} "g :: forall a b. a -> b -> a"
              , {- 15 -} "g x y = x"
              ])
