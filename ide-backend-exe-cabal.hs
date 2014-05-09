module Main where

import System.Environment (getArgs)
import System.Exit (exitWith)

import IdeSession.Cabal

main :: IO ()
main = getArgs >>= ide_backend_exe_cabal

-- | Parse arguments and run the cabal functions inside the executable.
ide_backend_exe_cabal :: [String] -> IO ()
ide_backend_exe_cabal [s] = do
  let args = read s :: ExeArgs
  case args of
    ExeBuild buildExeArgs modArgs -> do
      exitCode <- configureAndBuild buildExeArgs modArgs
      exitWith exitCode
    ExeDoc buildExeArgs -> do
      exitCode <- configureAndHaddock buildExeArgs
      exitWith exitCode
    ExeCc runCcArgs -> do
      exitCode <- runComponentCc runCcArgs
      exitWith exitCode
ide_backend_exe_cabal _ = fail "ide-backend-exe-cabal: wrong arguments"
