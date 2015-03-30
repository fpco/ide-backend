{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

foreign import ccall meaningOfLife :: IO Int

main :: IO ()
main = print =<< meaningOfLife
