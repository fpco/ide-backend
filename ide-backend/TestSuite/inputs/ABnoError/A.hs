module Main where

import B

main :: IO ()
main = do
  print B.string
  error "A.hs throws exception"
