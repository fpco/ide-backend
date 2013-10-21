module TraceMonad where

import qualified Debug.Trace

import GHC
import MonadUtils

class Monad m => TraceMonad m where
  trace :: String -> m ()

traceBlock :: TraceMonad m => String -> m a -> m a
traceBlock str act = do
  trace $ "BKB " ++ str
  result <- act
  trace $ "BKA " ++ str
  return result

traceBlock' :: TraceMonad m => String -> ((m a -> m a) -> m a) -> m a
traceBlock' str act = traceBlock str $ act (traceBlock "-")

instance TraceMonad IO where
  trace = Debug.Trace.traceEventIO

instance TraceMonad Ghc where
  trace = liftIO . trace
