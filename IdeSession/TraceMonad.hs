module IdeSession.TraceMonad where

import qualified Debug.Trace

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
