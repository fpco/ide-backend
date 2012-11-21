module Main where

import System.Environment
import System.FilePath ((</>), takeExtension, dropExtension)
import System.Directory
import System.Unix.Directory (withTemporaryDirectory)
import qualified Data.List as List
import Data.Monoid ((<>), mempty, mconcat)
import qualified Data.ByteString.Lazy.Char8 as BS

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)

import IdeSession
import GhcServer
import Progress
import Common
import TestTools

testAll :: [String] -> FilePath -> IO ()
testAll opts originalSourcesDir =
  withTemporaryDirectory "ide-backend-test" $ check opts originalSourcesDir

check :: [String] -> FilePath -> FilePath -> IO ()
check opts originalSourcesDir configSourcesDir = do
  putStrLn $ "\n\nCopying files from: " ++ originalSourcesDir ++ "\n"
          ++ "to a temporary test directory at: " ++ configSourcesDir ++ "\n"
  -- Init session.
  let sessionConfig = SessionConfig{ configSourcesDir
                                   , configWorkingDir = configSourcesDir
                                   , configDataDir    = configSourcesDir
                                   , configTempDir    = "."
                                   , configStaticOpts = opts
                                   }
  sP <- initSession sessionConfig
  -- Copy some source files from 'originalSourcesDir' to 'configSourcesDir'.
  -- HACK: here we fake module names, guessing them from file names.
  cnts <- getDirectoryContents originalSourcesDir
  let originalFiles = filter ((`elem` [".hs"]) . takeExtension) cnts
      originalModules =
        map (\ f -> (ModuleName $ dropExtension f, f)) originalFiles
      upd (m, f) = updateModule $ ModuleSource m $ originalSourcesDir </> f
      originalUpdate = mconcat $ map upd originalModules
      displayCounter :: PCounter -> IO ()
      displayCounter n = putStr (show n)
  s0 <- updateSession sP originalUpdate (progressWaitConsume displayCounter)
  msgs0 <- getSourceErrors s0
  putStrLn $ "Error 0:\n" ++ List.intercalate "\n\n"
    (map formatSourceError msgs0) ++ "\n"
  -- Overwrite some copied files.
  let overName = case originalModules of
        [] -> ModuleName "testEmptyDirModule"
        (m, _) : _ -> m
      update1 =
        (updateModule $ ModulePut overName (BS.pack "module M where\n2"))
        <> (updateModule $ ModulePut overName (BS.pack "module M where\nx = a2"))
      update2 =
        (updateModule $ ModulePut overName (BS.pack "module M where\n4"))
        <> (updateModule $ ModulePut overName (BS.pack "module M where\nx = a4"))
  -- Test the computations.
  s2 <- updateSession s0 update1 (progressWaitConsume displayCounter)
  msgs2 <- getSourceErrors s2
  putStrLn $ "Error 2:\n" ++ List.intercalate "\n\n"
    (map formatSourceError msgs2) ++ "\n"
  assertRaises "updateSession s0 update2 (progressWaitConsume displayCounter)"
               (userError "Invalid session token 1 /= 2")
               (updateSession s0 update2 (progressWaitConsume displayCounter))
  s4 <- updateSession s2 update2 (progressWaitConsume displayCounter)
  msgs4 <- getSourceErrors s4
  putStrLn $ "Error 4:\n" ++ List.intercalate "\n\n"
    (map formatSourceError msgs4) ++ "\n"
  msgs2' <- getSourceErrors s2
  putStrLn $ "Error 2 again:\n" ++ List.intercalate "\n\n"
    (map formatSourceError msgs2') ++ "\n"
  shutdownSession s4
  assertRaises "initSession sessionConfig"
               (userError
                $ "Directory " ++ configSourcesDir ++ " is not empty")
               (initSession sessionConfig)
  -- Remove file from the source directory to satisfy the precondition
  -- of initSession.
  mapM_ removeFile $ map (configSourcesDir </>) originalFiles
  -- Init another session. It strarts a new process with GHC,
  -- so the old state does not interfere.
  s10 <- initSession sessionConfig
  assertRaises "getSourceErrors s10"
               (userError "This session state does not admit queries.")
               (getSourceErrors s10)
  let optionsUpdate = originalUpdate
                      <> updateModule (OptionsSet $ Just ["-XNamedFieldPuns"])
  s11 <- updateSession s10 optionsUpdate (progressWaitConsume displayCounter)
  msgs11 <- getSourceErrors s11
  putStrLn $ "Error 11:\n" ++ List.intercalate "\n\n"
    (map formatSourceError msgs11) ++ "\n"
  assertRaises "shutdownSession s10"
               (userError "Invalid session token 0 /= 1")
               (shutdownSession s10)
  s12 <- updateSession s11 mempty (progressWaitConsume displayCounter)
  msgs12 <- getSourceErrors s12
  putStrLn $ "Error 12:\n" ++ List.intercalate "\n\n"
    (map formatSourceError msgs12) ++ "\n"
  shutdownSession s12

-- Driver

tests :: [Test]
tests =
  [ testGroup "Full integration tests"
    [ testCase "A depends on B, no errors"  $ testAll [] "test/ABnoError"
    , testCase "A depends on B, error in A" $ testAll [] "test/AerrorB"
    , testCase "A depends on B, error in B" $ testAll [] "test/ABerror"
    , testCase "Our own code, package 'ghc' missing" $ testAll [] "."
    , testCase "A subdirectory of Cabal code"
      $ testAll [] "test/Cabal.Distribution.PackageDescription"
    , testCase "A file requiring -XNamedFieldPuns" $ testAll [] "test/Puns"
    ]
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    "--server" : opts -> createGhcServer opts  -- @opts@ are GHC static flags
    _ -> defaultMain tests
