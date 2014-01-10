{-# LANGUAGE ForeignFunctionInterface, TemplateHaskell, CPP #-}
module Main where

import Language.Haskell.TH

import A (ex1, ex2)

foreign import ccall meaningOfLife :: IO Int

v3 :: IO Int
v3 =
#if !MIN_VERSION_base(999,0,0)
  meaningOfLife
#else
  "terrible error"
#endif

main :: IO ()
main = do
  i2 <- $ex2
  i3 <- v3
  $ex1 (return $ i2 + i3)
