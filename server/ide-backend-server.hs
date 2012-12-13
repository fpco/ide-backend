module Main where

import GhcServer
import System.Environment

main = do
  args <- getArgs
  ghcServer args
