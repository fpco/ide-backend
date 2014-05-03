module Main where

import System.Environment (getArgs)
import System.Exit (exitWith)

import IdeSession.Cabal (configureAndBuild, configureAndHaddock, runComponentCc)

main :: IO ()
main = getArgs >>= ide_backend_exe_cabal

-- | Parse arguments and run the cabal functions inside the executable.
ide_backend_exe_cabal :: [String] -> IO ()
ide_backend_exe_cabal ["configureAndBuild", buildExeArgs, modArgs] = do
  exitCode <- configureAndBuild (read buildExeArgs) (read modArgs)
  exitWith exitCode
ide_backend_exe_cabal ["configureAndHaddock", buildExeArgs] = do
  exitCode <- configureAndHaddock (read buildExeArgs)
  exitWith exitCode
ide_backend_exe_cabal ["runComponentCc", runCcArgs] = do
  exitCode <- runComponentCc (read runCcArgs)
  exitWith exitCode
ide_backend_exe_cabal _ = fail "ide-backend-exe-cabal: wrong arguments"
