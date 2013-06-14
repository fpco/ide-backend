{-
  Assumptions:

    for testpkg-A

      cabal install --prefix=/Users/dev/.cabal --global

    for testpkg-B

      cabal install

    for testpkg-C

      ghc-pkg init test-packages/db1

      cabal install --prefix=/Users/dev/.cabal \
        --package-db=/Users/dev/wt/projects/fpco/ide-backend/test-packages/db1

    for testpkg-D

      ghc-pkg init test-packages/db2

      cabal install --prefix=/Users/dev/.cabal \
        --package-db=/Users/dev/wt/projects/fpco/ide-backend/test-packages/db2
-}

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertBool, assertEqual, assertFailure, (@?=))
import Data.Monoid (mconcat, mempty, (<>))
import qualified Data.ByteString.Lazy.Char8 as BSLC (pack, unpack, null)
import Data.Maybe (catMaybes)
import Data.List (subsequences)
import Control.Concurrent (threadDelay)
import qualified Control.Exception as Ex

import IdeSession

data PackageUsage =
    DontUse
  | UseAndLoadDB
  | UseWithoutDB
  deriving Show

type Configuration = [(String, PackageUsage)]

configToImports :: Configuration -> [String]
configToImports = catMaybes . map aux
  where
    aux (pkg, DontUse) = Nothing
    aux (pkg, _)       = Just $ "import Testing.TestPkg" ++ pkg

configToPackageDBStack :: Configuration -> PackageDBStack
configToPackageDBStack = catMaybes . map aux
  where
    aux ("A", UseAndLoadDB) = Just GlobalPackageDB
    aux ("B", UseAndLoadDB) = Just UserPackageDB
    aux ("C", UseAndLoadDB) = Just $ SpecificPackageDB "test-packages/db1"
    aux ("D", UseAndLoadDB) = Just $ SpecificPackageDB "test-packages/db2"
    aux _                   = Nothing

verifyErrors :: Configuration -> [SourceError] -> Assertion
verifyErrors _ errs = assertBool ("Unexpected errors: " ++ show errs) (null errs)

-- TODO: would it be useful to move this to IdeSession?
withSession :: SessionConfig -> (IdeSession -> IO a) -> IO a
withSession config = Ex.bracket (initSession config) shutdownSession

testGhc :: Configuration -> Assertion
testGhc cfg = withSession config $ \session -> do
    let upd = (updateCodeGeneration True)
           <> (updateModule "Main.hs" . BSLC.pack . unlines $
                 configToImports cfg ++ [
                   "main = putStrLn \"hi\""
                 ])

    updateSession session upd (\_ -> return ())

    errs <- getSourceErrors session
    verifyErrors cfg errs

    runActions <- runStmt session "Main" "main"
    (output, result) <- runWaitAll runActions
    case result of
      RunOk _ -> assertEqual "" (BSLC.pack "hi\n") output
      _       -> assertFailure $ "Unexpected run result: " ++ show result
  where
    config = defaultSessionConfig {
        configPackageDBStack = configToPackageDBStack cfg
      }

configs :: [Configuration]
configs = concatMap aux [["A", "B", "C", "D"]]
  where
    aux :: [String] -> [Configuration]
    aux [] = return []
    aux (pkg : pkgs) = do
      usage  <- [DontUse, UseAndLoadDB, UseWithoutDB]
      config <- aux pkgs
      return $ (pkg, usage) : config

testCaseGhc :: Configuration -> Test
testCaseGhc cfg =
  testCase (show cfg) (testGhc cfg `Ex.finally` threadDelay 1000000)

tests :: [Test]
tests = [
    testGroup "GHC" $ map testCaseGhc configs
  , testGroup "Cabal" [
      ]
  ]


main :: IO ()
main = defaultMain tests
