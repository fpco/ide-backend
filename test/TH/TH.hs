{-# LANGUAGE TemplateHaskell #-}
module TH where

import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar)
import BlockingOps (modifyMVar, modifyMVar_, putMVar, readChan, readMVar)

main = do
  mv <- newEmptyMVar
  $putMVar mv mv
  mv <- $readMVar mv
  $putMVar mv mv
  $modifyMVar_ (\mv -> $modifyMVar_ (const undefined) mv >> return mv) mv
