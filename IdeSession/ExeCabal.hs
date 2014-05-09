{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, TupleSections #-}

-- | Invoke the executable that calls cabal functions.
module IdeSession.ExeCabal (
    invokeExeCabal
  ) where

import System.Exit (ExitCode)
import System.Process (readProcessWithExitCode)

import Distribution.Verbosity (normal)
import Distribution.Simple.Program.Find (
    ProgramSearchPath
  , findProgramOnSearchPath
  , ProgramSearchPathEntry(..)
  )

import IdeSession.Config
import IdeSession.Cabal

-- | Invoke the executable that processes our custom functions that use
-- the machinery of the cabal library.
invokeExeCabal :: SessionConfig -> ExeArgs -> IO ExitCode
invokeExeCabal SessionConfig{..} args = do
  mLoc <- findProgramOnSearchPath normal searchPath "ide-backend-exe-cabal"
  case mLoc of
    Nothing ->
      fail $ "Could not find ide-backend-exe-cabal"
    Just prog -> do
      (exitCode, _, _) <- readProcessWithExitCode prog [show args] ""
      return exitCode
  where
    searchPath :: ProgramSearchPath
    searchPath = ProgramSearchPathDefault
               : map ProgramSearchPathDir configExtraPathDirs
