{-# LANGUAGE TemplateHaskell #-}
module A where
import Language.Haskell.TH
ex1 :: Q Exp
ex1 = [| \x -> x |]
ex2 :: Q Type
ex2 = [t| String -> String |]
ex3 :: Q [Dec]
ex3 = [d| foo x = x |]
