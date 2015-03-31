module TH.TH where

import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar)
import TH.BlockingOps (modifyMVar, modifyMVar_, putMVar, readMVar, takeMVar)

main = do
  mv <- newEmptyMVar
  mv2 <- newEmptyMVar
  $putMVar mv mv2
  $putMVar mv2 42
  mv3 <- $takeMVar mv
  $putMVar mv mv3
  $modifyMVar_ mv $ \mv3 -> $modifyMVar_ mv3 (\i -> return $ i + 1)
                            >> return mv2
  mv4 <- $takeMVar mv
  i <- $readMVar mv3
  print $ (mv2 == mv4, i)
