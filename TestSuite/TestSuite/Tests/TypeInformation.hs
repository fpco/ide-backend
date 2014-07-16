module TestSuite.Tests.TypeInformation (testGroupTypeInformation) where

import Test.Tasty
import Test.HUnit
import qualified Data.ByteString.Lazy.Char8 as L (unlines)

import IdeSession
import TestSuite.State
import TestSuite.Session
import TestSuite.Assertions

testGroupTypeInformation :: TestSuiteEnv -> TestTree
testGroupTypeInformation env = testGroup "Type Information" [
    docTest env "Local identifiers and Prelude" testLocalIdentifiersAndPrelude
  ]

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
