{-
  Assumptions:

    Since we will always have the global package DB (otherwise we get an
    internal error from ghc) install the ide-backend-rts into the global DB.

    Create two custom databases:

      ghc-pkg init /Users/dev/.cabal/db1
      ghc-pkg init /Users/dev/.cabal/db2

    install A, B, C, D:

      testpkg-A      cabal install --prefix=/Users/dev/.cabal --global
      testpkg-B      cabal install
      testpkg-C      cabal install --prefix=/Users/dev/.cabal --package-db=/Users/dev/.cabal/db1
      testpkg-D      cabal install --prefix=/Users/dev/.cabal --package-db=/Users/dev/.cabal/db2

    similar progression for testpkg-E-{0.1,0.2,0.3,0.4}

    for testpkg-F the installation is a tad more complicated because we cannot
    install all versions to the same prefix, because then they would all
    overwrite each other

      mkdir ~/.cabal/f
      mkdir ~/.cabal/f/{a,b,c,d}

      testpkg-F-A    cabal install --prefix=/Users/dev/.cabal/f/a --global
      testpkg-F-B    cabal install --prefix=/Users/dev/.cabal/f/b
      testpkg-F-C    cabal install --prefix=/Users/dev/.cabal/f/c --package-db=/Users/dev/.cabal/db1
      testpkg-F-D    cabal install --prefix=/Users/dev/.cabal/f/d --package-db=/Users/dev/.cabal/db2

    for testpkg-G we test what happens when one DB contains multiple versions
    of the same package: 0.1 in User, {0.2,0.3} in Global, and {0.3,0.4} in DB1

      testpkg-G-0.1  cabal install --prefix=/Users/dev/.cabal --global
      testpkg-G-0.2  cabal install --prefix=/Users/dev/.cabal
      testpkg-G-0.3  cabal install --prefix=/Users/dev/.cabal
      testpkg-G-0.3  cabal install --prefix=/Users/dev/.cabal --package-db=/Users/dev/.cabal/db1
      testpkg-G-0.4  cabal install --prefix=/Users/dev/.cabal --package-db=/Users/dev/.cabal/db1
-}

import Prelude hiding (exp)
import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual, assertFailure, assertBool)
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

data Pkg = A  | B  | C  | D
         | E1 | E2 | E3 | E4
         | FA | FB | FC | FD
         | G1 | G2 | G3 | G4
  deriving (Show, Eq, Ord, Enum)

data PkgDB = Global | User | DB1 | DB2
  deriving (Show, Eq)

type PkgDBStack = [PkgDB]

pkgMod :: Pkg -> String
pkgMod A  = "Testing.TestPkgA"
pkgMod B  = "Testing.TestPkgB"
pkgMod C  = "Testing.TestPkgC"
pkgMod D  = "Testing.TestPkgD"
pkgMod E1 = "Testing.TestPkgE"
pkgMod E2 = "Testing.TestPkgE"
pkgMod E3 = "Testing.TestPkgE"
pkgMod E4 = "Testing.TestPkgE"
pkgMod FA = "Testing.TestPkgF"
pkgMod FB = "Testing.TestPkgF"
pkgMod FC = "Testing.TestPkgF"
pkgMod FD = "Testing.TestPkgF"
pkgMod G1 = "Testing.TestPkgG"
pkgMod G2 = "Testing.TestPkgG"
pkgMod G3 = "Testing.TestPkgG"
pkgMod G4 = "Testing.TestPkgG"

pkgName :: Pkg -> String
pkgName A  = "testpkg-A"
pkgName B  = "testpkg-B"
pkgName C  = "testpkg-C"
pkgName D  = "testpkg-D"
pkgName E1 = "testpkg-E"
pkgName E2 = "testpkg-E"
pkgName E3 = "testpkg-E"
pkgName E4 = "testpkg-E"
pkgName FA = "testpkg-F"
pkgName FB = "testpkg-F"
pkgName FC = "testpkg-F"
pkgName FD = "testpkg-F"
pkgName G1 = "testpkg-G"
pkgName G2 = "testpkg-G"
pkgName G3 = "testpkg-G"
pkgName G4 = "testpkg-G"

pkgFun :: Pkg -> String
pkgFun A  = "testPkgA"
pkgFun B  = "testPkgB"
pkgFun C  = "testPkgC"
pkgFun D  = "testPkgD"
pkgFun E1 = "testPkgE"
pkgFun E2 = "testPkgE"
pkgFun E3 = "testPkgE"
pkgFun E4 = "testPkgE"
pkgFun FA = "testPkgF"
pkgFun FB = "testPkgF"
pkgFun FC = "testPkgF"
pkgFun FD = "testPkgF"
pkgFun G1 = "testPkgG"
pkgFun G2 = "testPkgG"
pkgFun G3 = "testPkgG"
pkgFun G4 = "testPkgG"

pkgDB :: Pkg -> PkgDB
pkgDB A  = Global
pkgDB B  = User
pkgDB C  = DB1
pkgDB D  = DB2
pkgDB E1 = Global
pkgDB E2 = User
pkgDB E3 = DB1
pkgDB E4 = DB2
pkgDB FA = Global
pkgDB FB = User
pkgDB FC = DB1
pkgDB FD = DB2

dbE :: PkgDB -> Pkg
dbE Global = E1
dbE User   = E2
dbE DB1    = E3
dbE DB2    = E4

dbF :: PkgDB -> Pkg
dbF Global = FA
dbF User   = FB
dbF DB1    = FC
dbF DB2    = FD

pkgOut :: Pkg -> String
pkgOut A  = "This is test package A\n"
pkgOut B  = "This is test package B\n"
pkgOut C  = "This is test package C\n"
pkgOut D  = "This is test package D\n"
pkgOut E1 = "This is test package E-0.1\n"
pkgOut E2 = "This is test package E-0.2\n"
pkgOut E3 = "This is test package E-0.3\n"
pkgOut E4 = "This is test package E-0.4\n"
pkgOut FA = "This is test package F-A\n"
pkgOut FB = "This is test package F-B\n"
pkgOut FC = "This is test package F-C\n"
pkgOut FD = "This is test package F-D\n"
pkgOut G1 = "This is test package G-0.1\n"
pkgOut G2 = "This is test package G-0.2\n"
pkgOut G3 = "This is test package G-0.3\n"
pkgOut G4 = "This is test package G-0.4\n"

db :: FilePath -> PkgDB -> PackageDB
db _home Global = GlobalPackageDB
db _home User   = UserPackageDB
db  home DB1    = SpecificPackageDB $ home </> ".cabal/db1"
db  home DB2    = SpecificPackageDB $ home </> ".cabal/db2"

dbStack :: FilePath -> PkgDBStack -> PackageDBStack
dbStack = map . db

{------------------------------------------------------------------------------
  Test configuration (which packages and which package DBs do we load?)
------------------------------------------------------------------------------}

type LoadDB        = Bool
type Configuration = [(Pkg, LoadDB)]

configToImports :: Configuration -> [String]
configToImports = map aux
  where
    aux (pkg, _) = "import " ++ pkgMod pkg

configToPkgDBStack :: Configuration -> PkgDBStack
configToPkgDBStack = catMaybes . map aux
  where
    aux :: (Pkg, Bool) -> Maybe PkgDB
    aux (pkg, loadDB) = guard loadDB >> return (pkgDB pkg)

configToProg :: Configuration -> [String]
configToProg = map aux
  where
    aux (pkg, _) = "    putStrLn " ++ pkgFun pkg

configToOutput :: Configuration -> String
configToOutput = concatMap aux
  where
    aux (pkg, _) = pkgOut pkg

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
    expectedError (pkg, False) = Just $ "Could not find module `" ++ pkgMod pkg ++ "'"
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
                    configToImports cfg
                 ++ ["main = do"]
                 ++ configToProg cfg
    let upd = (updateCodeGeneration True)
           <> (updateModule "Main.hs" . BSLC.pack $ prog)

    updateSession session upd (\_ -> return ())

    errs <- getSourceErrors session
    unless (verifyErrors cfg errs) $ do
      putStrLn $ " - Program:  " ++ show prog
      putStrLn $ " - DB stack: " ++ show (configPackageDBStack config)
      putStrLn $ " - Errors:   " ++ show errs
      assertFailure "Unexpected errors"

    -- We only assertFailure above ^^ if there are *unexpected* errors
    when (null errs) $ do
      runActions <- runStmt session "Main" "main"
      (output, result) <- runWaitAll runActions
      case result of
        RunOk _ -> assertEqual "" (BSLC.pack $ configToOutput cfg) output
        _       -> assertFailure $ "Unexpected run result: " ++ show result

testDBsCabal :: Configuration -> Assertion
testDBsCabal cfg = do
  home <- getHomeDirectory

  let config = defaultSessionConfig {
           configPackageDBStack  = dbStack home $ configToPkgDBStack cfg
         , configGenerateModInfo = True  -- see #86
         }

  withSession config $ \session -> do
    let prog = unlines $
                    configToImports cfg
                 ++ ["main = do"]
                 ++ configToProg cfg
    let upd = (updateCodeGeneration True)
           <> (updateModule "Main.hs" . BSLC.pack $ prog)

    updateSession session upd (\_ -> return ())

    errs <- getSourceErrors session
    assertBool "Unexpected errors" (null errs)

    updateSession session (buildExe [(Text.pack "Main", "Main.hs")]) (\_ -> return ())
    status <- getBuildExeStatus session
    -- assertEqual "" (Just ExitSuccess) status

    distDir <- getDistDir session
    out     <- readProcess (distDir </> "build" </> "Main" </> "Main") [] []
    assertEqual "" (configToOutput cfg) out

configs :: (Configuration -> Bool) -> [Configuration]
configs isValid = filter isValid $ concatMap aux packages
  where
    aux :: [Pkg] -> [Configuration]
    aux [] = return []
    aux (pkg : pkgs) = do
      loadDB <- [True, False]
      config <- aux pkgs
      return $ (pkg, loadDB) : config

    packages :: [[Pkg]]
    packages = concatMap permutations $ subsequences [A .. D]

validPkgDBStack :: PkgDBStack -> Bool
validPkgDBStack stack =
     not (null stack)
  && elemIndices Global stack == [0]
  && elemIndices User stack `elem` [[], [1]]

validCfg :: Configuration -> Bool
validCfg cfg = validPkgDBStack (configToPkgDBStack cfg)
            && and (map snd cfg)

{------------------------------------------------------------------------------
  Test package DB order

  In this test we focus on package E, which has version 0.1 installed in the
  global DB, 0.2 in the user DB, 0.3 in db1 and 0.4 in db2. We always import
  "package E" and print testPkgE (which is a simple string that includes the
  package version), and specify a permutation of a non-empty subsequence of
  {global, user, DB1, DB2} and check that we get the right version.
------------------------------------------------------------------------------}

testOrderGhc :: String -> (PkgDBStack -> String) -> PkgDBStack -> Assertion
testOrderGhc pkg expectedOutput stack = do
  home <- getHomeDirectory

  let config = defaultSessionConfig {
           configPackageDBStack  = dbStack home stack
         , configGenerateModInfo = False
         }

  withSession config $ \session -> do
    let prog = unlines $ [
                   "import Testing.TestPkg" ++ pkg
                 , "main = do putStrLn testPkg" ++ pkg
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

    runActions <- runStmt session "Main" "main"
    (output, result) <- runWaitAll runActions
    case result of
      RunOk _ -> assertEqual "" (expectedOutput stack) (BSLC.unpack output)
      _       -> assertFailure $ "Unexpected run result: " ++ show result

testOrderCabal :: String -> (PkgDBStack -> String) -> PkgDBStack -> Assertion
testOrderCabal pkg expectedOutput stack = do
  home <- getHomeDirectory

  let config = defaultSessionConfig {
           configPackageDBStack  = dbStack home stack
         , configGenerateModInfo = True  -- see #86
         }

  withSession config $ \session -> do
    let prog = unlines $ [
                   "import Testing.TestPkg" ++ pkg
                 , "main = putStrLn testPkg" ++ pkg
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
    assertEqual "" (expectedOutput stack) out

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
        testGroup "GHC"   $ map testCaseDBsGhc (configs (validPkgDBStack . configToPkgDBStack))
        -- We don't want to test Cabal against packages in unknown DBs,
        -- because we already test that ghc complains about those and we
        -- require that ghc has loaded and type checked the modules that we
        -- compile.
      , testGroup "Cabal" $ map testCaseDBsCabal (configs validCfg)
      ]
  , testGroup "Check package DB order (different versions in different DBs)" [
        testGroup "GHC"   $ map (testCaseOrderGhc   "E" expectedOutputE) orderStacks
      , testGroup "Cabal" $ map (testCaseOrderCabal "E" expectedOutputE) orderStacks
      ]
  , testGroup "Check package DB order (same version in all DBs)" [
        testGroup "GHC"   $ map (testCaseOrderGhc   "F" expectedOutputF) orderStacks
      , testGroup "Cabal" $ map (testCaseOrderCabal "F" expectedOutputF) orderStacks
      ]
  , testGroup "Check package DB order (multiple versions per DB)" [
       testGroup "GHC" [
           testCaseOrderGhc "G" (\_ -> pkgOut G1) [Global]
         , testCaseOrderGhc "G" (\_ -> pkgOut G3) [Global,User]
         , testCaseOrderGhc "G" (\_ -> pkgOut G4) [Global,DB1]
         , testCaseOrderGhc "G" (\_ -> pkgOut G4) [Global,User,DB1]
         ]
     , testGroup "Cabal" [
           testCaseOrderCabal "G" (\_ -> pkgOut G1) [Global]
         , testCaseOrderCabal "G" (\_ -> pkgOut G3) [Global,User]
         , testCaseOrderCabal "G" (\_ -> pkgOut G4) [Global,DB1]
         , testCaseOrderCabal "G" (\_ -> pkgOut G4) [Global,User,DB1]
         ]
     ]
  ]
  where
    testCaseDBsGhc     cfg   = testCase (show cfg)   (testDBsGhc     cfg)
    testCaseDBsCabal   cfg   = testCase (show cfg)   (testDBsCabal   cfg)
    testCaseOrderGhc   pkg exp stack = testCase (show stack) (testOrderGhc   pkg exp stack)
    testCaseOrderCabal pkg exp stack = testCase (show stack) (testOrderCabal pkg exp stack)

    expectedOutputE, expectedOutputF :: PkgDBStack -> String
    expectedOutputE = pkgOut . maximum . map dbE
    expectedOutputF = pkgOut . last    . map dbF

main :: IO ()
main = defaultMain tests
