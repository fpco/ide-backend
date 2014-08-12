module TestSuite.Tests.Debugger (testGroupDebugger) where

import Prelude hiding (span, mod)
import Control.Monad
import Data.List (sort)
import Data.Monoid
import Test.HUnit
import Test.Tasty
import qualified Data.ByteString.Lazy.Char8 as L (unlines)
import qualified Data.ByteString.Lazy.UTF8  as L
import qualified Data.ByteString.Lazy       as L
import qualified Data.Text                  as T

import IdeSession
import TestSuite.State
import TestSuite.Session
import TestSuite.Assertions

testGroupDebugger :: TestSuiteEnv -> TestTree
testGroupDebugger env = testGroup "Debugger" [
    stdTest env "Setting and clearing breakpoints"        testSettingAndClearing
  , stdTest env "Running until breakpoint, then resuming" testResuming
  , stdTest env "Printing and forcing"                    testPrintingAndForcing
  ]

testSettingAndClearing :: TestSuiteEnv -> Assertion
testSettingAndClearing env = withAvailableSession env $ \session -> do
    skipTest "Debugger API known broken"

    updateSessionD session qsort 1
    assertNoErrors session

    expTypes <- getExpTypes session
    let (modName, mouseSpan) = mkSpan "Main" (2, 16, 2, 16)
        fullSpan = fst . last $ expTypes modName mouseSpan
        (_, wrongSpan) = mkSpan "Main" (0, 0, 0, 0)

    r1 <- setBreakpoint session modName fullSpan True
    r2 <- setBreakpoint session modName fullSpan False
    r3 <- setBreakpoint session modName fullSpan False
    r4 <- setBreakpoint session modName wrongSpan True
    assertEqual "original value of breakpoint"  (Just False) r1
    assertEqual "breakpoint successfully set"   (Just True)  r2
    assertEqual "breakpoint successfully unset" (Just False) r3
    assertEqual "invalid breakpoint"            Nothing      r4

testResuming :: TestSuiteEnv -> Assertion
testResuming env = withAvailableSession env $ \session -> do
    skipTest "Debugger API known broken"

    updateSessionD session qsort 1
    assertNoErrors session

    expTypes <- getExpTypes session
    let (modName, mouseSpan) = mkSpan "Main" (2, 16, 2, 16)
        fullSpan = fst . last $ expTypes modName mouseSpan

    Just False <- setBreakpoint session modName fullSpan True

    let inputList :: [Integer]
        inputList = [8, 4, 0, 3, 1, 23, 11, 18]

    outputs <- forM (mark inputList) $ \(i, isFirst, _isLast) -> do
       runActions <- if isFirst then runStmt session "Main" "main"
                                else resume session
       (output, RunBreak) <- runWaitAll runActions
       assertBreak session "Main"
                           "Main.hs@2:16-2:48"
                           "[a]"
                           [ ("_result" , "[Integer]" , "_")
                           , ("a"       , "Integer"   , show i)
                           , ("left"    , "[Integer]" , "_")
                           , ("right"   , "[Integer]" , "_")
                           ]
       return output

    do runActions <- resume session
       (finalOutput, finalResult) <- runWaitAll runActions
       let output = L.concat $ outputs ++ [finalOutput]
       assertEqual "" RunOk finalResult
       assertEqual "" (show (sort inputList) ++ "\n") (L.toString output)
       mBreakInfo <- getBreakInfo session
       assertEqual "" Nothing mBreakInfo

testPrintingAndForcing :: TestSuiteEnv -> Assertion
testPrintingAndForcing env = withAvailableSession env $ \session -> do
    skipTest "Debugger API known broken"

    updateSessionD session qsort 1
    assertNoErrors session

    expTypes <- getExpTypes session
    let (modName, mouseSpan) = mkSpan "Main" (2, 16, 2, 16)
        fullSpan = fst . last $ expTypes modName mouseSpan

    Just False <- setBreakpoint session modName fullSpan True
    runActions <- runStmt session "Main" "main"
    (_output, RunBreak) <- runWaitAll runActions

    printed <- printVar session "left" True False
    forced  <- printVar session "left" True True
    assertEqual "" [("left", "[Integer]", "(_t1::[Integer])")] printed
    assertEqual "" [("left", "[Integer]", "[4, 0, 3, 1]")] forced

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

qsort :: IdeSessionUpdate
qsort = (updateSourceFile "Main.hs" . L.unlines $ [
          --          1         2         3         4         5
          -- 12345678901234567890123456789012345678901234567890123456
            "qsort [] = [] "
          , "qsort (a:as) = qsort left ++ [a] ++ qsort right"
          , "  where (left,right) = (filter (<=a) as, filter (>a) as)"
          , ""
          , "main = print (qsort [8, 4, 0, 3, 1, 23, 11, 18])"
          ])
     <> (updateCodeGeneration True)

-- | Mark each element of the list with booleans indicating whether it's the
-- first and/or the last element in the list
mark :: [a] -> [(a, Bool, Bool)]
mark []     = []
mark [x]    = [(x, True, True)]
mark (x:xs) = (x, True, False) : aux xs
  where
    aux []     = error "impossible"
    aux [y]    = [(y, False, True)]
    aux (y:ys) = (y, False, False) : aux ys

assertBreak :: IdeSession
            -> String                     -- ^ Module
            -> String                     -- ^ Location
            -> String                     -- ^ Result type
            -> [(String, String, String)] -- ^ Var, type, value
            -> Assertion
assertBreak session mod loc resTy vars = do
  Just BreakInfo{..} <- getBreakInfo session
  assertEqual      "module name" mod   (T.unpack breakInfoModule)
  assertEqual      "location"    loc   (show breakInfoSpan)
  assertAlphaEquiv "result type" resTy (T.unpack breakInfoResultType)
  assertEqual      "number of local vars" (length vars) (length breakInfoVariableEnv)
  forM_ (zip vars breakInfoVariableEnv) $ \((var, typ, val), (var', typ', val')) -> do
    assertEqual      "var name" var (T.unpack var')
    assertAlphaEquiv "var type" typ (T.unpack typ')
    assertEqual      "var val"  val (T.unpack val')
