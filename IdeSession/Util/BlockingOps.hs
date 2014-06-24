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
module IdeSession.Util.BlockingOps (
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
  , swapMVar
    -- * Same for strict MVars
  , putStrictMVar
  , takeStrictMVar
  , modifyStrictMVar
  , modifyStrictMVar_
  , withStrictMVar
  , readStrictMVar
  , swapStrictMVar
    -- * Blocking Chan ops
  , readChan
    -- * Blocking Async ops
  , wait
  , waitCatch
  , waitAny
  , waitAnyCatchCancel
  ) where

import Language.Haskell.TH
import qualified Control.Concurrent as C
import qualified Control.Concurrent.Async as Async
import System.IO (hPutStrLn, stderr)
import qualified Control.Exception as Ex

import qualified IdeSession.Strict.MVar as StrictMVar

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

{-------------------------------------------------------------------------------
  MVar
-------------------------------------------------------------------------------}

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

swapMVar :: ExpQ
swapMVar = rethrowWithLineNumber2 [| C.swapMVar |]

{-------------------------------------------------------------------------------
  StrictMVar
-------------------------------------------------------------------------------}

takeStrictMVar :: ExpQ
takeStrictMVar = rethrowWithLineNumber1 [| StrictMVar.takeMVar |]

putStrictMVar :: ExpQ
putStrictMVar = rethrowWithLineNumber2 [| StrictMVar.putMVar |]

readStrictMVar :: ExpQ
readStrictMVar = rethrowWithLineNumber1 [| StrictMVar.readMVar |]

modifyStrictMVar :: ExpQ
modifyStrictMVar = rethrowWithLineNumber2 [| StrictMVar.modifyMVar |]

modifyStrictMVar_ :: ExpQ
modifyStrictMVar_ = rethrowWithLineNumber2 [| StrictMVar.modifyMVar_ |]

withStrictMVar :: ExpQ
withStrictMVar = rethrowWithLineNumber2 [| StrictMVar.withMVar |]

swapStrictMVar :: ExpQ
swapStrictMVar = rethrowWithLineNumber2 [| StrictMVar.swapMVar |]

{-------------------------------------------------------------------------------
  Chan
-------------------------------------------------------------------------------}

readChan :: ExpQ
readChan = rethrowWithLineNumber1 [| C.readChan |]

{-------------------------------------------------------------------------------
  Async
-------------------------------------------------------------------------------}

wait :: ExpQ
wait = rethrowWithLineNumber1 [| Async.wait |]

waitCatch :: ExpQ
waitCatch = rethrowWithLineNumber1 [| Async.waitCatch |]

waitAny :: ExpQ
waitAny = rethrowWithLineNumber1 [| Async.waitAny |]

waitAnyCatchCancel :: ExpQ
waitAnyCatchCancel = rethrowWithLineNumber1 [| Async.waitAnyCatchCancel |]

#else

{-------------------------------------------------------------------------------
  MVar
-------------------------------------------------------------------------------}

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

swapMVar :: ExpQ
swapMVar = [| C.swapMVar |]

{-------------------------------------------------------------------------------
  StrictMVar
-------------------------------------------------------------------------------}

takeStrictMVar :: ExpQ
takeStrictMVar = [| StrictMVar.takeMVar |]

putStrictMVar :: ExpQ
putStrictMVar = [| StrictMVar.putMVar |]

readStrictMVar :: ExpQ
readStrictMVar = [| StrictMVar.readMVar |]

modifyStrictMVar :: ExpQ
modifyStrictMVar = [| StrictMVar.modifyMVar |]

modifyStrictMVar_ :: ExpQ
modifyStrictMVar_ = [| StrictMVar.modifyMVar_ |]

withStrictMVar :: ExpQ
withStrictMVar = [| StrictMVar.withMVar |]

swapStrictMVar :: ExpQ
swapStrictMVar = [| StrictMVar.swapMVar |]

{-------------------------------------------------------------------------------
  Chan
-------------------------------------------------------------------------------}

readChan :: ExpQ
readChan = [| C.readChan |]

{-------------------------------------------------------------------------------
  Async
-------------------------------------------------------------------------------}

wait :: ExpQ
wait = [| Async.wait |]

waitCatch :: ExpQ
waitCatch = [| Async.waitCatch |]

waitAny :: ExpQ
waitAny = [| Async.waitAny |]

waitAnyCatchCancel :: ExpQ
waitAnyCatchCancel = [| Async.waitAnyCatchCancel |]

#endif
