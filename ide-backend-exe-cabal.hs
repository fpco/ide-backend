module Main where

import System.Environment (getArgs)

import IdeSession.ExeCabalServer (exeCabalEngine)
import IdeSession.RPC.Server

main :: IO ()
main = do
  args <- getArgs
  rpcServer exeCabalEngine args
