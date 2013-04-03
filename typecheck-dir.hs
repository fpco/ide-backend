module Main where

import Control.Monad (liftM, unless)
import Data.Monoid (mconcat)
import System.Directory
import System.Environment
import System.FilePath.Find (always, extension, find)
import System.IO.Temp (withTempDirectory)

import Common
import GhcServer
import IdeSession

--- A sample program using the library. It type-checks all files
--- in the given directory and prints out the list of errors.

-- | Some common extensions, etc. (please fill in).
-- Curiously "-XTypeFamilies" causes the type-checking of the default
-- test file to fail. The same file type-checks OK with the ghc-errors
-- test program (with no GHC extensions set).
defOpts :: [String]
defOpts = [ "-hide-all-packages"
          , "-XCPP"
          , "-XNoTemplateHaskell"  -- TH not available when profiling
          , "-XBangPatterns"
          , "-XRecordWildCards"
          , "-XNamedFieldPuns"
          , "-XPatternGuards"
          , "-XScopedTypeVariables"
          , "-XMultiParamTypeClasses"
          , "-XRankNTypes"
-- causes problems with the Cabal code:          , "-XTypeFamilies"
          , "-XForeignFunctionInterface"
          , "-XDeriveDataTypeable"
          , "-package template-haskell"
          , "-package old-time"
          , "-package parallel"
          , "-package base"
          , "-package deepseq"
          , "-package filepath"
          , "-package directory"
          , "-package process"
          , "-package time"
          , "-package containers"
          , "-package array"
          , "-package pretty"
          , "-package bytestring"
          , "-package unix"
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
            [] -> ("test/Cabal",
                   defOpts)
      slashTmp <- getTemporaryDirectory
      withTempDirectory slashTmp "typecheck-dir."
        $ check opts originalSourcesDir

check :: [String] -> FilePath -> FilePath -> IO ()
check opts what configDir = do
  putStrLn $ "Copying files from: " ++ what ++ "\n"
          ++ "to a temporary directory at: " ++ configDir ++ "\n"
  -- Init session.
  let sessionConfig = SessionConfig{ configDir
                                   , configStaticOpts = opts
                                   , configInProcess  = True
                                   }
  session     <- initSession sessionConfig
  isFile      <- doesFileExist      what
  isDirectory <- doesDirectoryExist what

  unless (isFile || isDirectory) $ fail ("invalid argument " ++ what)

  modules <- if isFile
    then return [what]
    else find always ((`elem` hsExtensions) `liftM` extension) what

  print modules

  let len = show $ length modules

      displayCounter :: Progress -> IO ()
      displayCounter n = putStrLn ("[" ++ show n ++ "/" ++ len ++ "]")

      update = mconcat (map updateModuleFromFile modules)

  updateSession session update displayCounter
  errs <- getSourceErrors session
  putStrLn $ "\nErrors and warnings:\n" ++ unlines (map show errs)
  shutdownSession session
