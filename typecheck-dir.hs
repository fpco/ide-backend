module Main where

import qualified Data.List as List
import Data.Monoid (mconcat)
import System.Directory
import System.Environment
import System.FilePath (dropExtension, takeExtension, (</>))
import System.Unix.Directory (withTemporaryDirectory)

import Common
import GhcServer
import IdeSession
import qualified ModuleName as MN

--- A sample program using the library. It type-checks all files
--- in the given directory and prints out the list of errors.

-- | Some common extensions, etc. (please fill in).
-- Curiously "-XTypeFamilies" causes the type-checking of the default
-- test file to fail. The same file type-checks OK with the ghc-errors
-- test program (with no GHC extensions set).
defOpts :: [String]
defOpts = [ "-no-user-package-conf"
          , "-XCPP"
          , "-XTemplateHaskell"
          , "-XBangPatterns"
          , "-XRecordWildCards"
          , "-XNamedFieldPuns"
          , "-XPatternGuards"
          , "-XScopedTypeVariables"
          , "-XMultiParamTypeClasses"
          , "-XRankNTypes"
          , "-XTypeFamilies"
          ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    "--server" : opts -> ghcServer opts  -- @opts@ are GHC static flags
    _ -> do
      let (originalSourcesDir, opts) = case args of
            ["--help"] ->
              error "usage: typecheck-dir [source-dir [ghc-options]]"
            [dir] -> (dir, defOpts)
            dir : optsArg -> (dir, optsArg)
            [] -> ("test/Cabal.Distribution.PackageDescription",
                   defOpts)
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
  session <- initSession sessionConfig
  -- Copy some source files from 'originalSourcesDir' to 'configSourcesDir'.
  -- HACK: here we fake module names, guessing them from file names.
  cnts <- getDirectoryContents originalSourcesDir
  let originalFiles = filter ((`elem` hsExtentions) . takeExtension) cnts
      originalModules =
        map (\ f -> (MN.fromString $ dropExtension f, f)) originalFiles
      upd (m, f) = updateModuleFromFile m $ originalSourcesDir </> f
      originalUpdate = mconcat $ map upd originalModules
      len = show $ length originalFiles
      displayCounter :: PCounter -> IO ()
      displayCounter n = putStrLn ("[" ++ show n ++ "/" ++ len ++ "]")
  updateSession session originalUpdate displayCounter
  msgs0 <- getSourceErrors session
  putStrLn $ "\nErrors and warnings:\n" ++ List.intercalate "\n"
    (map formatSourceError msgs0) ++ "\n"
  shutdownSession session
