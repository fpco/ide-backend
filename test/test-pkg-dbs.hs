{-
  Assumptions:

    Since we will always have the global package DB (otherwise we get an
    internal error from ghc) install the ide-backend-rts into the global DB.

    for testpkg-A

      cabal install --prefix=/Users/dev/.cabal --global

    for testpkg-B

      cabal install

    for testpkg-C

      ghc-pkg init /Users/dev/.cabal/db1

      cabal install --prefix=/Users/dev/.cabal \
        --package-db=/Users/dev/.cabal/db1

    for testpkg-D

      ghc-pkg init /Users/dev/.cabal/db2

      cabal install --prefix=/Users/dev/.cabal \
        --package-db=/Users/dev/.cabal/db2

    similar progression for testpkg-E-{0.1,0.2,0.3,0.4}
-}

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertBool, assertEqual, assertFailure, (@?=))
import Data.Monoid (mconcat, mempty, (<>))
import qualified Data.ByteString.Lazy.Char8 as BSLC (pack, unpack, null)
import Data.Maybe (catMaybes)
import Data.List (subsequences, permutations, elemIndices, isInfixOf)
import Control.Concurrent (threadDelay)
import qualified Control.Exception as Ex
import System.Directory(getHomeDirectory)
import System.FilePath ((</>))
import Control.Monad (when, unless)
import qualified Data.Text as Text

import IdeSession

type LoadDB        = Bool
type PackageName   = String
type Configuration = [(PackageName, LoadDB)]

configToImports :: Configuration -> [String]
configToImports = map aux
  where
    aux (pkg, _) = "import Testing.TestPkg" ++ pkg

configToPackageDBStack :: FilePath -> Configuration -> PackageDBStack
configToPackageDBStack homeDir = catMaybes . map aux
  where
    aux ("A", True) = Just GlobalPackageDB
    aux ("B", True) = Just UserPackageDB
    aux ("C", True) = Just $ SpecificPackageDB $ homeDir </> ".cabal/db1"
    aux ("D", True) = Just $ SpecificPackageDB $ homeDir </> ".cabal/db2"
    aux _           = Nothing


verifyErrors :: Configuration -> [SourceError] -> Bool
verifyErrors cfg actualErrors =
       null expectedErrors == null actualErrors
    && all isExpectedError actualErrors
  where
    isExpectedError :: SourceError -> Bool
    isExpectedError actual = any (\expected -> expected `isInfixOf` Text.unpack (errorMsg actual)) expectedErrors

    expectedErrors :: [String]
    expectedErrors = catMaybes (map expectedError cfg)

    expectedError :: (PackageName, Bool) -> Maybe String
    expectedError (pkg, False) = Just $ "Could not find module `Testing.TestPkg" ++ pkg ++ "'"
    expectedError (_pkg, True) = Nothing

-- Find the first element satisfying the given the predicate, and return that
-- element and the list with the element extracted
find :: (a -> Bool) -> [a] -> Maybe (a, [a])
find p = go []
  where
    go _acc []                 = Nothing
    go  acc (x:xs) | p x       = Just (x, reverse acc ++ xs)
                   | otherwise = go (x:acc) xs

-- TODO: would it be useful to move this to IdeSession?
withSession :: SessionConfig -> (IdeSession -> IO a) -> IO a
withSession config = Ex.bracket (initSession config) shutdownSession

testGhc :: Configuration -> Assertion
testGhc cfg = do
  homeDirectory <- getHomeDirectory

  let config = defaultSessionConfig {
           configPackageDBStack = configToPackageDBStack homeDirectory cfg
           -- TODO: although we are not interested in mod info in this test,
           -- and hence it makes sense to set configGenerateModInfo to False,
           -- in fact the tests *fail* when we don't! That should not be the case.
         , configGenerateModInfo = False
         }

  withSession config $ \session -> do
    let prog = unlines $
                 configToImports cfg ++ [
                   "main = putStrLn \"hi\""
                 ]
    let upd = (updateCodeGeneration True)
           <> (updateModule "Main.hs" . BSLC.pack $ prog)

    updateSession session upd (\_ -> return ())

    errs <- getSourceErrors session
    unless (verifyErrors cfg errs) $ do
      putStrLn $ " - Program:  " ++ show prog
      putStrLn $ " - DB stack: " ++ show (configPackageDBStack config)
      putStrLn $ " - Errors:   " ++ show errs
      assertFailure "Unexpected errors"

    when (null errs) $ do
      runActions <- runStmt session "Main" "main"
      (output, result) <- runWaitAll runActions
      case result of
        RunOk _ -> assertEqual "" (BSLC.pack "hi\n") output
        _       -> assertFailure $ "Unexpected run result: " ++ show result

configs :: (Configuration -> Bool) -> [Configuration]
configs validCfg = filter validCfg $ concatMap aux packages
  where
    aux :: [PackageName] -> [Configuration]
    aux [] = return []
    aux (pkg : pkgs) = do
      loadDB <- [True, False]
      config <- aux pkgs
      return $ (pkg, loadDB) : config

    packages :: [[PackageName]]
    packages = concatMap permutations $ subsequences ["A", "B", "C", "D"]

validGhcCfg :: Configuration -> Bool
validGhcCfg cfg =
       not (null dbStack)
    && elemIndices GlobalPackageDB dbStack == [0]
    && elemIndices UserPackageDB dbStack `elem` [[], [1]]
  where
    dbStack = configToPackageDBStack (error "homedir") cfg

testCaseGhc :: Configuration -> Test
testCaseGhc cfg =
  testCase (show cfg) (testGhc cfg {- `Ex.finally` threadDelay 1000000 -})

tests :: [Test]
tests = [
    testGroup "GHC" $ map testCaseGhc (configs validGhcCfg)
  , testGroup "Cabal" [
      ]
  ]

main :: IO ()
main = defaultMain tests
