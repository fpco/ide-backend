module Main where

import qualified Control.Exception as Ex
import Control.Monad (liftM)
import qualified Data.List as List
import Data.Maybe (catMaybes)
import Data.Monoid (mconcat)
import System.Environment
import System.FilePath (dropExtension, makeRelative)
import System.FilePath.Find (always, extension, find)
import System.IO.Temp (withTempDirectory)
import System.Directory

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
            [] -> ("test/Cabal",
                   defOpts)
      slashTmp <- getTemporaryDirectory
      withTempDirectory slashTmp "typecheck-dir."
        $ check opts originalSourcesDir

check :: [String] -> FilePath -> FilePath -> IO ()
check opts originalSourcesDir configDir = do
  putStrLn $ "Copying files from: " ++ originalSourcesDir ++ "\n"
          ++ "to a temporary directory at: " ++ configDir ++ "\n"
  -- Init session.
  let sessionConfig = SessionConfig{ configDir
                                   , configStaticOpts = opts
                                   }
  session <- initSession sessionConfig
  -- Copy some source files from 'originalSourcesDir' to 'configSourcesDir'.
  originalFiles <- find always
                        ((`elem` hsExtentions) `liftM` extension)
                        originalSourcesDir
  -- HACK: here we fake module names, guessing them from file names.
  let tryFromPath f p = do
        mex <- Ex.try $ Ex.evaluate $ MN.fromFilePath p
        return $ case mex of
          Right n  -> Just (n, f)
          Left _ex -> let _ = _ex :: Ex.ErrorCall in Nothing
  triedModules <-
     mapM (\ f -> tryFromPath f
                  $ dropExtension $ makeRelative originalSourcesDir f)
          originalFiles
  let originalModules = catMaybes triedModules
      upd (m, f) = updateModuleFromFile m f
      originalUpdate = mconcat $ map upd originalModules
      len = show $ length originalFiles
      displayCounter :: PCounter -> IO ()
      displayCounter n = putStrLn ("[" ++ show n ++ "/" ++ len ++ "]")
  updateSession session originalUpdate displayCounter
  msgs0 <- getSourceErrors session
  putStrLn $ "\nErrors and warnings:\n" ++ List.intercalate "\n"
    (map formatSourceError msgs0) ++ "\n"
  shutdownSession session
