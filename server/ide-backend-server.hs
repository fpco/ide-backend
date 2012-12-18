module Main where

import IdeSession
import System.Environment

main = do
  args <- getArgs
  ghcServer args
