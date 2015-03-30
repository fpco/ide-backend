-- | Maintained state in the sesssion
module TestSuite.Tests.SessionState (testGroupSessionState) where

import Test.HUnit
import Test.Tasty

import TestSuite.State
import TestSuite.Assertions
import TestSuite.Session

testGroupSessionState :: TestSuiteEnv -> TestTree
testGroupSessionState env = testGroup "Session state" [
    stdTest env "Maintain list of compiled modules 1" testListCompiledModules1
  , stdTest env "Maintain list of compiled modules 2" testListCompiledModules2
  , stdTest env "Maintain list of compiled modules 3" testListCompiledModules3
  ]

testListCompiledModules1 :: TestSuiteEnv -> Assertion
testListCompiledModules1 env = withAvailableSession env $ \session -> do
  updateSessionD session (loadModule "XXX.hs" "a = 5") 1
  assertLoadedModules session "XXX" ["XXX"]
  updateSessionD session (loadModule "A.hs" "a = 5") 1
  assertLoadedModules session "[m1]" ["A", "XXX"]
  updateSessionD session (loadModule "A2.hs" "import A\na2 = A.a") 1
  assertLoadedModules session "[m1, m2]" ["A", "A2", "XXX"]
  updateSessionD session (loadModule "A3.hs" "") 1
  assertLoadedModules session "[m1, m2, m3]" ["A", "A2", "A3", "XXX"]
  updateSessionD session (loadModule "Wrong.hs" "import A4\na2 = A4.a + 1") 1
  assertLoadedModules session "wrong1" ["A", "A2", "A3", "XXX"]
  updateSessionD session (loadModule "Wrong.hs" "import A\na2 = A.a + c") 1
  assertLoadedModules session "wrong2" ["A", "A2", "A3", "XXX"]
  updateSessionD session (loadModule "A.hs" "a = c") 1
  -- Module "A" is compiled before "Wrong", fails, so it's invalidated
  -- and all modules that depend on it are invalidated. Module "Wrong"
  -- is never compiled.
  assertLoadedModules session "wrong3" ["A3", "XXX"]

testListCompiledModules2 :: TestSuiteEnv -> Assertion
testListCompiledModules2 env = withAvailableSession env $ \session -> do
  updateSessionD session (loadModule "XXX.hs" "a = 5") 1
  assertLoadedModules session "XXX" ["XXX"]
  updateSessionD session (loadModule "A.hs" "a = 5") 1
  assertLoadedModules session "[m1]" ["A", "XXX"]
  updateSessionD session (loadModule "A2.hs" "import A\na2 = A.a") 1
  assertLoadedModules session "[m1, m2]" ["A", "A2", "XXX"]
  updateSessionD session (loadModule "A3.hs" "") 1
  assertLoadedModules session "[m1, m2, m3]" ["A", "A2", "A3", "XXX"]
  updateSessionD session (loadModule "Wrong.hs" "import A4\na2 = A4.a + 1") 1
  assertLoadedModules session "wrong1" ["A", "A2", "A3", "XXX"]
  -- This has to be disabled to get the different outcome below:
    -- updateSessionD session (loadModule m4 "import A\na2 = A.a + c") 1
    -- assertLoadedModules session "wrong2" [m1, m2, m3, xxx]
  -- We get this differemnt outcome both in original 7.4.2
  -- and after the GHC#7231 fix. It's probably caused by target
  -- Wrong place before or after target "A" depending on what kind
  -- of error Wrong had. This is strange, but not incorrect.
  updateSessionD session (loadModule "A.hs" "a = c") 1
  -- Module "Wrong" is compiled first here, fails, so module "A"
  -- is never comipiled, so it's not invalidated.
  assertLoadedModules session "wrong3" ["A", "A2", "A3", "XXX"]

testListCompiledModules3 :: TestSuiteEnv -> Assertion
testListCompiledModules3 env = withAvailableSession env $ \session -> do
  updateSessionD session (loadModule "A.hs" "a = 5") 1
  assertLoadedModules session "1 [A]" ["A"]
  updateSessionD session (loadModule "A.hs" "a = 5 + True") 1
  assertLoadedModules session "1 []" []
  updateSessionD session (loadModule "A.hs" "a = 5") 1
  assertLoadedModules session "2 [A]" ["A"]
  updateSessionD session (loadModule "A.hs" "a = 5 + wrong") 1
  assertLoadedModules session "2 []" []
  updateSessionD session (loadModule "A.hs" "a = 5") 1
  assertLoadedModules session "3 [A]" ["A"]
  updateSessionD session (loadModule "A.hs" "import WRONG\na = 5") 1
  assertLoadedModules session "3 [A]; wrong imports do not unload old modules" ["A"]
  updateSessionD session (loadModule "A.hs" "a = 5 + True") 1
  assertLoadedModules session "3 []" []
