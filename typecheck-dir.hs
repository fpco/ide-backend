module Main where

import System.Environment
import System.FilePath ((</>), takeExtension, dropExtension)
import System.Directory
import System.Unix.Directory (withTemporaryDirectory)
import qualified Data.List as List
import Data.Monoid (mconcat)

import IdeSession
import GhcServer
import Progress
import Common

--- A sample program using the library. It type-checks all files
--- in the given directory and prints out the list of errors
--- in JSON format.

main :: IO ()
main = do
  args <- getArgs
  case args of
    "--server" : opts -> createGhcServer opts  -- @opts@ are GHC static flags
    _ -> do
      let (originalSourcesDir, opts) = case args of
            ["--help"] -> error "usage: typecheck-dir [source-dir]"
            dir : optsArg -> (dir, optsArg)
            [] -> ("test/Cabal.Distribution.PackageDescription", [])
      withTemporaryDirectory "typecheck-dir" $ check opts originalSourcesDir

check :: [String] -> FilePath -> FilePath -> IO ()
check opts originalSourcesDir configSourcesDir = do
  putStrLn $ "Copying files from: " ++ originalSourcesDir ++ "\n"
          ++ "to a temporary directory at: " ++ configSourcesDir ++ "\n"
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
      len = show $ length originalFiles
      displayCounter :: PCounter -> IO ()
      displayCounter n = putStrLn ("[" ++ show n ++ "/" ++ len ++ "]")
  s0 <- updateSession sP originalUpdate (progressWaitConsume displayCounter)
  msgs0 <- getSourceErrors s0
  putStrLn $ "\nErrors and warnings:\n" ++ List.intercalate "\n"
    (map formatSourceError msgs0) ++ "\n"
  shutdownSession s0
