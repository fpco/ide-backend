module Main where

import IdeSession (ghcServer)
import System.Environment (getArgs)

main = do
  args <- getArgs
  ghcServer args
