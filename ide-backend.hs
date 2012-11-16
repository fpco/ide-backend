module Main where

import Control.Monad
import qualified Control.Exception as Ex
import System.Environment
import System.FilePath ((</>), takeExtension, dropExtension)
import System.Directory
import System.Unix.Directory (withTemporaryDirectory)
import qualified Data.List as List
import Data.Monoid (mconcat)
import System.IO
  ( stderr
  , hSetBuffering
  , BufferMode(LineBuffering)
  )

import IdeSession
import GhcServer
import Progress

--- A sample program using the library. It type-checks all files
--- in the given directory and prints out the list of errors
--- in JSON format.

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  args <- getArgs
  case args of
    "--server" : opts -> createGhcServer opts  -- @opts@ are GHC static flags
    _ -> do
      let originalSourcesDir = case args of
            [dir] -> dir
            [] -> "."
            _ -> fail "usage: programName [source-dir]"
      withTemporaryDirectory "ide-backend-test" $ check originalSourcesDir

check :: FilePath -> FilePath -> IO ()
check originalSourcesDir configSourcesDir = do
  putStrLn $ "Copying files from: " ++ originalSourcesDir ++ "\n\n"
          ++ "Temporary directory: " ++ configSourcesDir ++ "\n\n"
  -- Init session.
  let sessionConfig = SessionConfig{ configSourcesDir
                                   , configWorkingDir = configSourcesDir
                                   , configDataDir    = configSourcesDir
                                   , configTempDir    = "."
                                   , configStaticOpts = []
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
      displayCounter n = putStr (show n)  -- or just putStr "#"
  s0 <- updateSession sP originalUpdate (progressWaitConsume displayCounter)
  msgs0 <- getSourceErrors s0
  putStrLn $ "Errors :\n" ++ List.intercalate "\n\n"
    (map formatErrorMessage msgs0) ++ "\n"
  shutdownSession s0

formatErrorMessage :: SourceError -> String
formatErrorMessage = show
