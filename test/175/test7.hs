module Main where
import Control.Concurrent
import Numeric.LinearAlgebra
main = do
  mv <- newEmptyMVar
  let actionMv = do
        a <- let s = show $ randomVector 77777 Uniform 10 in print ("asasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasfdfasdf1" ++ (take 1600 (drop 1 s)))
        putMVar mv a
  forkIO actionMv
  takeMVar mv
  return ()
