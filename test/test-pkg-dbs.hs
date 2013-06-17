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
import Test.HUnit (Assertion, assertEqual, assertFailure)
import Data.Monoid ((<>))
import qualified Data.ByteString.Lazy.Char8 as BSLC (pack, unpack)
import Data.Maybe (catMaybes)
import Data.List (subsequences, permutations, elemIndices, isInfixOf)
import qualified Data.Text as Text
import qualified Control.Exception as Ex
import Control.Monad (when, unless, guard)
import System.Directory(getHomeDirectory)
import System.FilePath ((</>))
import System.Process (readProcess)
import System.Exit (ExitCode (..))

import IdeSession

{------------------------------------------------------------------------------
  Test package meta data

  We abstract from "real" package DBs here because they really on knowing
  the user's home directory
------------------------------------------------------------------------------}

data Pkg        = A | B | C | D | E1 | E2 | E3 | E4 deriving Show
data PkgDB      = Global | User | DB1 | DB2         deriving (Show, Eq)
type PkgDBStack = [PkgDB]

pkgName :: Pkg -> String
pkgName A  = "Testing.TestPkgA"
pkgName B  = "Testing.TestPkgB"
pkgName C  = "Testing.TestPkgC"
pkgName D  = "Testing.TestPkgD"
pkgName E1 = "Testing.TestPkgE"
pkgName E2 = "Testing.TestPkgE"
pkgName E3 = "Testing.TestPkgE"
pkgName E4 = "Testing.TestPkgE"

pkgDB :: Pkg -> PkgDB
pkgDB A  = Global
pkgDB B  = User
pkgDB C  = DB1
pkgDB D  = DB2
pkgDB E1 = Global
pkgDB E2 = User
pkgDB E3 = DB1
pkgDB E4 = DB2

dbE :: PkgDB -> Pkg
dbE Global = E1
dbE User   = E2
dbE DB1    = E3
dbE DB2    = E4

db :: FilePath -> PkgDB -> PackageDB
db _home Global = GlobalPackageDB
db _home User   = UserPackageDB
db  home DB1    = SpecificPackageDB $ home </> ".cabal/db1"
db  home DB2    = SpecificPackageDB $ home </> ".cabal/db2"

dbStack :: FilePath -> PkgDBStack -> PackageDBStack
dbStack = map . db

pkgOut :: Pkg -> String
pkgOut A  = "This is test package A\n"
pkgOut B  = "This is test package B\n"
pkgOut C  = "This is test package C\n"
pkgOut D  = "This is test package D\n"
pkgOut E1 = "This is test package E-0.1\n"
pkgOut E2 = "This is test package E-0.2\n"
pkgOut E3 = "This is test package E-0.3\n"
pkgOut E4 = "This is test package E-0.4\n"

{------------------------------------------------------------------------------
  Test configuration (which packages and which package DBs do we load?)
------------------------------------------------------------------------------}

type LoadDB        = Bool
type Configuration = [(Pkg, LoadDB)]

configToImports :: Configuration -> [String]
configToImports = map aux
  where
    aux (pkg, _) = "import " ++ pkgName pkg

configToPkgDBStack :: Configuration -> PkgDBStack
configToPkgDBStack = catMaybes . map aux
  where
    aux :: (Pkg, Bool) -> Maybe PkgDB
    aux (pkg, loadDB) = guard loadDB >> return (pkgDB pkg)

{------------------------------------------------------------------------------
  Test which configurations are valid

  This test focusses exclusively on the packages A, B, C, D, which are all
  installed in different packages DBs. We import a subset of {A,B,C,D}, and
  specify a subset of global, user, DB1 and DB2 databases, and check that we
  get the expected compile errors. This also checks which combination of
  packages DBs is actually supported.
------------------------------------------------------------------------------}

verifyErrors :: Configuration -> [SourceError] -> Bool
verifyErrors cfg actualErrors =
       null expectedErrors == null actualErrors
    && all isExpectedError actualErrors
  where
    isExpectedError :: SourceError -> Bool
    isExpectedError actual = any (\expected -> expected `isInfixOf` Text.unpack (errorMsg actual)) expectedErrors

    expectedErrors :: [String]
    expectedErrors = catMaybes (map expectedError cfg)

    expectedError :: (Pkg, Bool) -> Maybe String
    expectedError (pkg, False) = Just $ "Could not find module `" ++ pkgName pkg ++ "'"
    expectedError (_pkg, True) = Nothing

testDBsGhc :: Configuration -> Assertion
testDBsGhc cfg = do
  home <- getHomeDirectory

  let config = defaultSessionConfig {
           configPackageDBStack  = dbStack home $ configToPkgDBStack cfg
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
    aux :: [Pkg] -> [Configuration]
    aux [] = return []
    aux (pkg : pkgs) = do
      loadDB <- [True, False]
      config <- aux pkgs
      return $ (pkg, loadDB) : config

    packages :: [[Pkg]]
    packages = concatMap permutations $ subsequences [A, B, C, D]

validPkgDBStack :: PkgDBStack -> Bool
validPkgDBStack stack =
     not (null stack)
  && elemIndices Global stack == [0]
  && elemIndices User stack `elem` [[], [1]]

validGhcCfg :: Configuration -> Bool
validGhcCfg cfg = validPkgDBStack (configToPkgDBStack cfg)

{------------------------------------------------------------------------------
  Test package DB order

  In this test we focus on package E, which has version 0.1 installed in the
  global DB, 0.2 in the user DB, 0.3 in db1 and 0.4 in db2. We always import
  "package E" and print testPkgE (which is a simple string that includes the
  package version), and specify a permutation of a non-empty subsequence of
  {global, user, DB1, DB2} and check that we get the right version.
------------------------------------------------------------------------------}

testOrderGhc :: PkgDBStack -> Assertion
testOrderGhc stack = do
  home <- getHomeDirectory

  let config = defaultSessionConfig {
           configPackageDBStack  = dbStack home stack
         , configGenerateModInfo = False
         }

  withSession config $ \session -> do
    let prog = unlines $ [
                   "import Testing.TestPkgE"
                 , "main = putStrLn testPkgE"
                 ]
    let upd = (updateCodeGeneration True)
           <> (updateModule "Main.hs" . BSLC.pack $ prog)

    updateSession session upd (\_ -> return ())

    errs <- getSourceErrors session
    unless (null errs) $ do
      putStrLn $ " - Program:  " ++ show prog
      putStrLn $ " - DB stack: " ++ show stack
      putStrLn $ " - Errors:   " ++ show errs
      assertFailure "Unexpected errors"

    when (null errs) $ do
      runActions <- runStmt session "Main" "main"
      (output, result) <- runWaitAll runActions
      case result of
        RunOk _ -> assertEqual "" (pkgOut . dbE . last $ stack) (BSLC.unpack output)
        _       -> assertFailure $ "Unexpected run result: " ++ show result

testOrderCabal :: PkgDBStack -> Assertion
testOrderCabal stack = do
  home <- getHomeDirectory

  let config = defaultSessionConfig {
           configPackageDBStack  = dbStack home stack
         , configGenerateModInfo = False
         , configStaticOpts      = ["-package base", "-package testpkg-E"]
         }

  withSession config $ \session -> do
    let prog = unlines $ [
                   "import Testing.TestPkgE"
                 , "main = putStrLn testPkgE"
                 ]
    let upd = (updateCodeGeneration False)
           <> (updateModule "Main.hs" . BSLC.pack $ prog)

    updateSession session upd (\_ -> return ())

    errs <- getSourceErrors session
    unless (null errs) $ do
      putStrLn $ " - Program:  " ++ show prog
      putStrLn $ " - DB stack: " ++ show stack
      putStrLn $ " - Errors:   " ++ show errs
      assertFailure "Unexpected errors"

    updateSession session (buildExe [(Text.pack "Main", "Main.hs")]) (\_ -> return ())
    status <- getBuildExeStatus session
    assertEqual "" (Just ExitSuccess) status

    distDir <- getDistDir session
    out     <- readProcess (distDir </> "build" </> "Main" </> "Main") [] []
    assertEqual "" (pkgOut . dbE . last $ stack) out

orderStacks :: [PkgDBStack]
orderStacks = filter validPkgDBStack
            $ concatMap permutations
            $ filter (not . null)
            $ subsequences
            $ [Global, User, DB1, DB2]

{------------------------------------------------------------------------------
  Driver
------------------------------------------------------------------------------}

-- TODO: would it be useful to move this to IdeSession?
withSession :: SessionConfig -> (IdeSession -> IO a) -> IO a
withSession config = Ex.bracket (initSession config) shutdownSession

tests :: [Test]
tests = [
    testGroup "Valid package DB stacks" [
        testGroup "GHC" $ map testCaseDBsGhc (configs validGhcCfg)
      , testGroup "Cabal" [
          ]
      ]
  , testGroup "Check package DB order" [
        testGroup "GHC"   $ map testCaseOrderGhc   orderStacks
      , testGroup "Cabal" $ map testCaseOrderCabal orderStacks
      ]
  ]
  where
    testCaseDBsGhc     cfg   = testCase (show cfg)   (testDBsGhc     cfg)
    testCaseOrderGhc   stack = testCase (show stack) (testOrderGhc   stack)
    testCaseOrderCabal stack = testCase (show stack) (testOrderCabal stack)

main :: IO ()
main = defaultMain tests
