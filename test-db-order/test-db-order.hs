module Main where

import Testing.TestPkgE
import Testing.TestPkgF

main :: IO ()
main = do putStrLn testPkgE
          putStrLn testPkgF
