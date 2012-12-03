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
  hFlush stdout
  hFlush stderr
  putFlush $ "\nCopying files from: " ++ originalSourcesDir ++ "\n"
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
  let originalFiles = filter ((`elem` cpExtentions) . takeExtension) cnts
      originalModules =
        map (\ f -> (ModuleName f, f)) originalFiles
      upd (m, f) = updateModule $ ModuleSource m $ originalSourcesDir </> f
      originalUpdate = mconcat $ map upd originalModules
      displayCounter :: PCounter -> IO ()
      displayCounter n = putStr (show n)
  s0 <- updateSession sP originalUpdate (progressWaitConsume displayCounter)
  msgs0 <- getSourceErrors s0
  putFlush $ "Error 0:\n" ++ List.intercalate "\n\n"
    (map formatSourceError msgs0) ++ "\n"
  -- Overwrite some copied files.
  let overName = case originalModules of
        [] -> ModuleName "testEmptyDirModule"
        (m, _) : _ -> m
      update1 =
        (updateModule $ ModulePut overName (BS.pack "module M where\n2"))
        <> (updateModule $ ModulePut overName (BS.pack "module M where\nx = a2"))
      update2 =
        updateModule (ChangeCodeGeneration True)
        <> (updateModule $ ModulePut overName (BS.pack "module M where\n4"))
        <> (updateModule $ ModulePut overName (BS.pack "module M where\nx = a4"))
  -- Test the computations.
  s2 <- updateSession s0 update1 (progressWaitConsume displayCounter)
  msgs2 <- getSourceErrors s2
  putFlush $ "Error 2:\n" ++ List.intercalate "\n\n"
    (map formatSourceError msgs2) ++ "\n"
  s4 <- updateSession s2 update2 (progressWaitConsume displayCounter)
  msgs4 <- getSourceErrors s4
  putFlush $ "Error 4:\n" ++ List.intercalate "\n\n"
    (map formatSourceError msgs4) ++ "\n"
  msgs2' <- getSourceErrors s2
  putFlush $ "Error 2 again:\n" ++ List.intercalate "\n\n"
    (map formatSourceError msgs2') ++ "\n"
  assertRaises "updateSession s2 update1 (progressWaitConsume displayCounter)"
               (== userError "Invalid session token 2 /= 3")
               (updateSession s2 update1 (progressWaitConsume displayCounter))
  shutdownSession s4
  assertRaises "initSession sessionConfig"
               (== userError
                 ("Directory " ++ configSourcesDir ++ " is not empty"))
               (initSession sessionConfig)
  -- Remove file from the source directory to satisfy the precondition
  -- of initSession.
  mapM_ removeFile $ map (configSourcesDir </>) originalFiles
  -- Init another session. It strarts a new process with GHC,
  -- so the old state does not interfere.
  s9 <- initSession sessionConfig
  assertRaises "getSourceErrors s9"
               (== userError "This session state does not admit queries.")
               (getSourceErrors s9)
  shutdownSession s9
  s10 <- initSession sessionConfig
  let punOpts = opts ++ [ "-XNamedFieldPuns", "-XRecordWildCards"]
      optionsUpdate = originalUpdate
                      <> updateModule (ChangeOptions $ Just punOpts)
  s11 <- updateSession s10 optionsUpdate (progressWaitConsume displayCounter)
  msgs11 <- getSourceErrors s11
  putFlush $ "Error 11:\n" ++ List.intercalate "\n\n"
    (map formatSourceError msgs11) ++ "\n"
  let update12 = updateModule (ChangeCodeGeneration True)
  s12 <- updateSession s11 update12 (progressWaitConsume displayCounter)
  msgs12 <- getSourceErrors s12
  putFlush $ "Error 12:\n" ++ List.intercalate "\n\n"
    (map formatSourceError msgs12) ++ "\n"
  (errs, resOrEx) <- runStmt s12 "Main" "main"
  putFlush $ "\nErrors and warnings:\n"
             ++ List.intercalate "\n" (map formatSourceError errs)
             ++ "\n"
  putFlush $ "Run result: "
             ++ case resOrEx of
                  Just (Left ident) -> ident
                  Just (Right ex)   -> ex
                  Nothing           -> "Run failed."
             ++ "\n"
  assertRaises "shutdownSession s11"
               (== userError "Invalid session token 1 /= 2")
               (shutdownSession s11)
  shutdownSession s12


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
main = do
  args <- getArgs
  case args of
    "--server" : opts -> ghcServer opts  -- @opts@ are GHC static flags
    _ -> defaultMain tests
