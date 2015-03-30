{-# LANGUAGE ScopedTypeVariables #-}
-- | MVar that always evaluates its argument to WHNF
module IdeSession.Strict.MVar (
    StrictMVar -- Abstract
  , newEmptyMVar
  , newMVar
  , takeMVar
  , putMVar
  , readMVar
  , swapMVar
  , tryTakeMVar
  , tryPutMVar
  , isEmptyMVar
  , withMVar
  , modifyMVar_
  , modifyMVar
  ) where

import Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import Control.Applicative ((<$>))
import Control.Exception (evaluate)
import Control.Monad ((>=>))

newtype StrictMVar a = StrictMVar (MVar a)

newEmptyMVar :: IO (StrictMVar a)
newEmptyMVar = StrictMVar <$> MVar.newEmptyMVar

newMVar :: a -> IO (StrictMVar a)
newMVar x = StrictMVar <$> (evaluate x >>= MVar.newMVar)

takeMVar :: StrictMVar a -> IO a
takeMVar (StrictMVar v) = MVar.takeMVar v

putMVar :: StrictMVar a -> a -> IO ()
putMVar (StrictMVar v) x = evaluate x >>= MVar.putMVar v

readMVar :: StrictMVar a -> IO a
readMVar (StrictMVar v) = MVar.readMVar v

swapMVar :: StrictMVar a -> a -> IO a
swapMVar (StrictMVar v) x = evaluate x >>= MVar.swapMVar v

tryTakeMVar :: StrictMVar a -> IO (Maybe a)
tryTakeMVar (StrictMVar v) = MVar.tryTakeMVar v

tryPutMVar :: StrictMVar a -> a -> IO Bool
tryPutMVar (StrictMVar v) x = evaluate x >>= MVar.tryPutMVar v

isEmptyMVar :: StrictMVar a -> IO Bool
isEmptyMVar (StrictMVar v) = MVar.isEmptyMVar v

withMVar :: StrictMVar a -> (a -> IO b) -> IO b
withMVar (StrictMVar v) f = MVar.withMVar v f

modifyMVar_ :: StrictMVar a -> (a -> IO a) -> IO ()
modifyMVar_ (StrictMVar v) f = MVar.modifyMVar_ v (f >=> evaluate)

modifyMVar :: forall a b. StrictMVar a -> (a -> IO (a, b)) -> IO b
modifyMVar (StrictMVar v) f = MVar.modifyMVar v aux
  where
    aux :: a -> IO (a, b)
    aux x = do (a, b) <- f x
               a' <- evaluate a
               return (a', b)
