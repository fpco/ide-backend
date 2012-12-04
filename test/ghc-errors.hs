module Main where

import System.Environment
import System.FilePath ((</>), takeExtension)
import System.Directory
import System.Unix.Directory (withTemporaryDirectory)
import qualified Data.List as List
import Data.Monoid ((<>), mconcat)
import qualified Data.ByteString.Lazy.Char8 as BS
import System.IO (hFlush, stdout, stderr)

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import IdeSession
import GhcServer
import Progress
import Common
import TestTools

-- Tests using various functions of the IdeSession API
-- and a variety of small test Haskell projects.

testAll :: [String] -> FilePath -> IO ()
testAll opts originalSourcesDir =
  withTemporaryDirectory "ide-backend-test" $ check opts originalSourcesDir

putFlush :: String -> IO ()
putFlush msg = do
  putStrLn msg
  hFlush stdout

check :: [String] -> FilePath -> FilePath -> IO ()
check opts originalSourcesDir configSourcesDir = do
  -- Init session.
  let sessionConfig = SessionConfig{ configSourcesDir
                                   , configWorkingDir = configSourcesDir
                                   , configDataDir    = configSourcesDir
                                   , configTempDir    = "."
                                   , configStaticOpts = opts
                                   }
  putStrLn "----- 1 ------"
  sP <- initSession sessionConfig
  putStrLn "----- 2 ------"
  -- Copy some source files from 'originalSourcesDir' to 'configSourcesDir'.
  -- HACK: here we fake module names, guessing them from file names.
  cnts <- getDirectoryContents originalSourcesDir
  putStrLn "----- 3 -----"
  let originalFiles = filter ((`elem` cpExtentions) . takeExtension) cnts
      originalModules =
        map (\ f -> (ModuleName f, f)) originalFiles
      upd (m, f) = updateModule $ ModuleSource m $ originalSourcesDir </> f
      originalUpdate = updateModule (ChangeCodeGeneration False)
                       <> (mconcat $ map upd originalModules)
      displayCounter :: PCounter -> IO ()
      displayCounter n = putStr (show n)
  s0 <- updateSession sP originalUpdate (progressWaitConsume displayCounter)
  putStrLn "----- 4 ------"
  msgs0 <- getSourceErrors s0
  putStrLn "----- 5 ------"
  -- Overwrite some copied files.
  let overName = case originalModules of
        [] -> ModuleName "testEmptyDirModule"
        (m, _) : _ -> m
      update1 =
        updateModule (ChangeCodeGeneration False)
        <> (updateModule $ ModulePut overName (BS.pack "module M where\n2"))
        <> (updateModule $ ModulePut overName (BS.pack "module M where\nx = a2"))
      update2 =
        updateModule (ChangeCodeGeneration True)
        <> (updateModule $ ModulePut overName (BS.pack "module M where\n4"))
        <> (updateModule $ ModulePut overName (BS.pack "module M where\nx = a4"))
  -- Test the computations.
  s2 <- updateSession s0 update1 (progressWaitConsume displayCounter)
  putStrLn "----- 6 ------"
  msgs2 <- getSourceErrors s2
  putStrLn "----- 7 ------"
  s4 <- updateSession s2 update2 (progressWaitConsume displayCounter)
  putStrLn "----- 8 ------"
  msgs4 <- getSourceErrors s4
  putStrLn "----- 9 ------"
  msgs2' <- getSourceErrors s2
  putStrLn "----- 10 ------"
  s5 <- updateSession s4 originalUpdate (progressWaitConsume displayCounter)
  putStrLn "----- 11 ------"
  let update6 = updateModule (ChangeCodeGeneration True)
  s6 <- updateSession s5 update6 (progressWaitConsume displayCounter)
  putStrLn "----- 12 ------"
  (errs, resOrEx) <- runStmt s6 "Main" "main"
  putStrLn "----- 13 ------"
  assertRaises "updateSession s2 update1 (progressWaitConsume displayCounter)"
               (== userError "Invalid session token 2 /= 5")
               (updateSession s2 update1 (progressWaitConsume displayCounter))
  putStrLn "----- 14 ------"
  shutdownSession s6
  putStrLn "----- 15 ------"
  assertRaises "initSession sessionConfig"
               (== userError
                 ("Directory " ++ configSourcesDir ++ " is not empty"))
               (initSession sessionConfig)
  putStrLn "----- 16 ------"
  -- Remove file from the source directory to satisfy the precondition
  -- of initSession.
  mapM_ removeFile $ map (configSourcesDir </>) originalFiles
  putStrLn "----- 17 ------"
  -- Init another session. It strarts a new process with GHC,
  -- so the old state does not interfere.
  s9 <- initSession sessionConfig
  putStrLn "----- 18 ------"
  assertRaises "getSourceErrors s9"
               (== userError "This session state does not admit queries.")
               (getSourceErrors s9)
  putStrLn "----- 19 ------"
  shutdownSession s9
  putStrLn "----- 20 ------"
  s10 <- initSession sessionConfig
  putStrLn "----- 21 ------"
  let punOpts = opts ++ [ "-XNamedFieldPuns", "-XRecordWildCards"]
      optionsUpdate = originalUpdate
                      <> updateModule (ChangeOptions $ Just punOpts)
  s11 <- updateSession s10 optionsUpdate (progressWaitConsume displayCounter)
  putStrLn "----- 22 ------"
  msgs11 <- getSourceErrors s11
  putStrLn "----- 23 ------"
  let update12 = updateModule (ChangeCodeGeneration True)
  s12 <- updateSession s11 update12 (progressWaitConsume displayCounter)
  putStrLn "----- 24 ------"
  msgs12 <- getSourceErrors s12
  putStrLn "----- 25 ------"
  (errs12, resOrEx12) <- runStmt s12 "Main" "main"
  putStrLn "----- 26 ------"
  assertRaises "shutdownSession s11"
               (== userError "Invalid session token 1 /= 2")
               (shutdownSession s11)
  putStrLn "----- 27 ------"
  shutdownSession s12
  putStrLn "----- 28 ------"


-- Driver

defOpts :: [String]
defOpts = [ "-no-user-package-conf" ]

tests :: [Test]
tests =
  [ testGroup "Full integration tests"
    [ testCase "A depends on B, no errors"  $ testAll defOpts "test/ABnoError"
    , testCase "A depends on B, error in A" $ testAll defOpts "test/AerrorB"
    , testCase "A depends on B, error in B" $ testAll defOpts "test/ABerror"
    , testCase "Our own code, package 'ghc' missing"
      $ testAll [] "."
    , testCase "A subdirectory of Cabal code"
      $ testAll defOpts "test/Cabal.Distribution.PackageDescription"
    , testCase "A file requiring -XNamedFieldPuns"
      $ testAll [ "-hide-all-packages"
                , "-package mtl"
                , "-package base"
                , "-package array"
                , "-package bytestring"
                , "-package containers"
                , "-package binary"
                ]
                "test/Puns"
    ]
  ]

main :: IO ()
main = defaultMain tests
