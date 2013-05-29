module Main where

import System.Environment (getArgs)
import Server (ghcServer)

main :: IO ()
main = getArgs >>= ghcServer
