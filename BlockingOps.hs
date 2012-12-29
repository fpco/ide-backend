{-# LANGUAGE CPP, TemplateHaskell, NamedFieldPuns #-}
-- | Blocking operations such as
--
-- > readMVar v
--
-- may throw an exception such as
--
-- > thread blocked indefinitely in an MVar operation
--
-- Unfortunately, this exception does not give any information of _where_ in
-- the code we are blocked indefinitely. Compiling with profiling info and
-- running with +RTC -xc can address this to some extent, but (1) it requires
-- that all profiling libraries are installed and (2) when we are running
-- multithreaded code the resulting stack trace is often difficult to read
-- (and still does not include line numbers). With this module you can replace
-- the above code with
--
-- > $readMVar v
--
-- and the exception that will be thrown is
--
-- > YourModule:lineNumber: thread blocked indefinitely in an MVar operation
--
-- which is a lot more informative. When the CPP flag DEBUGGING is turned off
-- then @$readMVar@ just turns into @readMVar@.
--
-- NOTE: The type of the exception changes when using DEBUGGING mode -- in order
-- to be able to add the line number, all exceptions are turned into
-- IOExceptions.
module BlockingOps (
    -- * Generic debugging utilities
    lineNumber
  , traceOnException
  , mapExceptionIO
  , mapExceptionShow
    -- * Blocking MVar ops
  , putMVar
  , takeMVar
  , modifyMVar
  , modifyMVar_
  , withMVar
  , readMVar
    -- * Blocking Chan ops
  , readChan
    -- * Blocking Async ops
  , wait
  , waitCatch
  , waitAnyCatchCancel
  ) where

import Language.Haskell.TH
import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as Async
import System.IO (hPutStrLn, stderr)
import qualified Control.Exception as Ex

lineNumber :: ExpQ
lineNumber = do
  Loc{loc_module, loc_start=(line, _)} <- location
  [| loc_module ++ ":" ++ show (line :: Int) |]

mapExceptionIO :: (Ex.Exception e1, Ex.Exception e2)
               => (e1 -> e2) -> IO a -> IO a
mapExceptionIO f io = Ex.catch io (Ex.throwIO . f)

mapExceptionShow :: (String -> String) -> IO a -> IO a
mapExceptionShow f = mapExceptionIO (userError . f . showSomeException)
  where
    showSomeException :: Ex.SomeException -> String
    showSomeException = show

traceOnException :: String -> IO a -> IO a
traceOnException str io = Ex.catch io $ \e -> do
  hPutStrLn stderr (str ++ ": " ++ show e)
  Ex.throwIO (e :: Ex.SomeException)

#define DEBUGGING 0

#if DEBUGGING == 1

rethrowWithLineNumber1 :: ExpQ -> ExpQ
rethrowWithLineNumber1 expr =
  [| \arg1 -> mapExceptionShow (\e -> $lineNumber ++ ": " ++ e)
                               ($expr arg1)
   |]

rethrowWithLineNumber2 :: ExpQ -> ExpQ
rethrowWithLineNumber2 expr =
  [| \arg1 arg2 -> mapExceptionShow (\e -> $lineNumber ++ ": " ++ e)
                                    ($expr arg1 arg2)
   |]

takeMVar :: ExpQ
takeMVar = rethrowWithLineNumber1 [| C.takeMVar |]

putMVar :: ExpQ
putMVar = rethrowWithLineNumber2 [| C.putMVar |]

readMVar :: ExpQ
readMVar = rethrowWithLineNumber1 [| C.readMVar |]

modifyMVar :: ExpQ
modifyMVar = rethrowWithLineNumber2 [| C.modifyMVar |]

modifyMVar_ :: ExpQ
modifyMVar_ = rethrowWithLineNumber2 [| C.modifyMVar_ |]

withMVar :: ExpQ
withMVar = rethrowWithLineNumber2 [| C.withMVar |]

readChan :: ExpQ
readChan = rethrowWithLineNumber1 [| C.readChan |]

wait :: ExpQ
wait = rethrowWithLineNumber1 [| Async.wait |]

waitCatch :: ExpQ
waitCatch = rethrowWithLineNumber1 [| Async.waitCatch |]

waitAnyCatchCancel :: ExpQ
waitAnyCatchCancel = rethrowWithLineNumber1 [| Async.waitAnyCatchCancel |]

#else

takeMVar :: ExpQ
takeMVar = [| C.takeMVar |]

putMVar :: ExpQ
putMVar = [| C.putMVar |]

readMVar :: ExpQ
readMVar = [| C.readMVar |]

modifyMVar :: ExpQ
modifyMVar = [| C.modifyMVar |]

modifyMVar_ :: ExpQ
modifyMVar_ = [| C.modifyMVar_ |]

withMVar :: ExpQ
withMVar = [| C.withMVar |]

readChan :: ExpQ
readChan = [| C.readChan |]

wait :: ExpQ
wait = [| Async.wait |]

waitCatch :: ExpQ
waitCatch = [| Async.waitCatch |]

waitAnyCatchCancel :: ExpQ
waitAnyCatchCancel = [| Async.waitAnyCatchCancel |]

#endif
