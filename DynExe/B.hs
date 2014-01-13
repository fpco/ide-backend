{-# LANGUAGE TemplateHaskell #-}
module Main where
import A
ex5 :: $ex2
ex5 = $ex1
ex6 :: $(return =<< ex2)
ex6 = $(ex1 >>= return)
$ex3
main :: IO ()
main = print $ $ex1 42
