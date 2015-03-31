module GenMakefile where

import Data.List

data DB = Gbl | Usr | Db1 | Db2 deriving Eq
type Stack = [DB]

instance Show DB where
  show Gbl = "G"
  show Usr = "U"
  show Db1 = "1"
  show Db2 = "2"

stacks :: [Stack]
stacks = filter (Gbl `elem`)
       $ concatMap permutations
       $ subsequences [Gbl, Usr, Db1, Db2]

showStack :: Stack -> String
showStack = concatMap show

pkgPath :: Stack -> String
pkgPath = concat . intersperse ":" . map aux
  where
    aux db = "${DB" ++ show db ++ "}"

rule :: Stack -> String
rule stack =
       exe stack ++ ":\n"
    ++ "\tGHC_PACKAGE_PATH=" ++ pkgPath stack ++ " ${GHC} -o " ++ exe stack ++ " test-db-order.hs\n"

exe :: Stack -> String
exe stack = "test-db-order-" ++ showStack stack

rules :: String
rules = unlines $ map rule stacks

makefile :: String
makefile =
     "all: " ++ concatMap (\s -> exe s ++ " ") stacks ++ "\n\n"
  ++ "run: all\n"
  ++ unlines (map (\s -> "\t./" ++ exe s) stacks)
  ++ "\n"
  ++ rules

main :: IO ()
main = appendFile "Makefile" makefile
