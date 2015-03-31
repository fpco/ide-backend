-------------------------------------------------------------------------------
-- A parallel implementation of fib in Haskell using semi-explicit
-- parallelism expressed with `par` and `pseq`

module ParFib.Main (main) where

import Control.Parallel

-------------------------------------------------------------------------------
-- A purely sequential implementaiton of fib.

seqFib :: Int -> Integer
seqFib 0 = 1
seqFib 1 = 1
seqFib n = seqFib (n-1) + seqFib (n-2)

-------------------------------------------------------------------------------
-- A thresh-hold value below which the parallel implementation of fib
-- reverts to sequential implementation.

threshHold :: Int
threshHold = 25

-------------------------------------------------------------------------------
-- A parallel implementation of fib.

parFib :: Int -> Integer
parFib n
  = if n < threshHold then
      seqFib n
    else
      r `par` (l `pseq` l + r)
    where
    l  = parFib (n-1)
    r  = parFib (n-2)

-------------------------------------------------------------------------------

result :: Integer
result = parFib 24

-------------------------------------------------------------------------------

main :: IO String
main
  = do pseq result (return ())
       putStrLn $ "running 'A single file with a code to run in parallel' from MainModule/ParFib, which says fib 24 = " ++ show result
       return $ show result

-------------------------------------------------------------------------------
