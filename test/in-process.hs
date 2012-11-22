-- | Simple in-prcess execution of GHC API calls, for testing, to easily
-- see stdout, stderr, exception, etc.. Uses "GhcRun" directly, in the same
-- process, without the RPC layer.
module Main where

import System.Environment
import qualified Data.List as List

import Common
import GhcRun

main :: IO ()
main = do
  args <- getArgs
  target <-
    case args of
      [f] -> return f
      []  -> return "test/MainModule/ParFib.hs"
      _   -> fail "usage: in-process [file.hs]"

  putStrLn ""
  errs <- checkModule [target] (optsToDynFlags []) (Just "Main.main") 2
                      putStrLn putStrLn
  putStrLn $ "\nErrors and warnings:\n" ++ List.intercalate "\n"
    (map formatSourceError errs) ++ "\n"
