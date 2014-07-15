module IdeBackendRTS (
    run
  , RunBufferMode(..)
  , Maybe(..)
  ) where

import Control.Concurrent (forkIO, threadDelay, killThread)
import Control.Concurrent.MVar (MVar, takeMVar, putMVar)
import qualified System.IO as IO
import qualified Control.Exception as Ex
import Control.Monad (forever)

import GHC.IO.Handle.Types (
    Handle(FileHandle)
  , HandleType(ClosedHandle, ReadHandle, WriteHandle)
  , nativeNewlineMode
  , Handle__
  , haType
  )
import GHC.IO.Handle.Internals (
    mkHandle
  , closeTextCodecs
  , ioe_finalizedHandle
  , flushWriteBuffer
  )
import qualified GHC.IO.FD as FD

run :: RunBufferMode -> RunBufferMode -> IO a -> IO ()
run outBMode errBMode io = do
  let resetHandles = do
        resetStdin  IO.utf8
        resetStdout IO.utf8
        resetStderr IO.utf8
  let io' = do _result <- io ; return () -- Throw away any snippet result
  withBuffering IO.stdout outBMode (withBuffering IO.stderr errBMode io')
    `Ex.finally` resetHandles

{-------------------------------------------------------------------------------
  Buffer modes
-------------------------------------------------------------------------------}

data RunBufferMode =
    RunNoBuffering
  | RunLineBuffering (Maybe Int)
  | RunBlockBuffering (Maybe Int) (Maybe Int)
  deriving Read

withBuffering :: IO.Handle -> RunBufferMode -> IO a -> IO a
withBuffering h mode io = do
  IO.hSetBuffering h (bufferMode mode)
  result <- withBufferTimeout h (bufferTimeout mode) io
  ignoreIOExceptions $ IO.hFlush h
  return result

ignoreIOExceptions :: IO () -> IO ()
ignoreIOExceptions = let handler :: Ex.IOException -> IO ()
                         handler _ = return ()
                     in Ex.handle handler

bufferMode :: RunBufferMode -> IO.BufferMode
bufferMode RunNoBuffering           = IO.NoBuffering
bufferMode (RunLineBuffering _)     = IO.LineBuffering
bufferMode (RunBlockBuffering sz _) = IO.BlockBuffering sz

bufferTimeout :: RunBufferMode -> Maybe Int
bufferTimeout RunNoBuffering          = Nothing
bufferTimeout (RunLineBuffering t)    = t
bufferTimeout (RunBlockBuffering _ t) = t

withBufferTimeout :: IO.Handle -> Maybe Int -> IO a -> IO a
withBufferTimeout _ Nothing  io = io
withBufferTimeout h (Just n) io = do
  tid <- forkIO . ignoreIOExceptions . forever $ threadDelay n >> IO.hFlush h
  result <- io
  killThread tid
  return result

swapFileHandles :: Handle -> Handle -> IO ()
swapFileHandles (FileHandle _ h1) (FileHandle _ h2) = Ex.mask_ $ do
  h1' <- takeMVar h1
  h2' <- takeMVar h2
  putMVar h1 h2'
  putMVar h2 h1'
swapFileHandles _ _ =
  Ex.throwIO (userError "swapFileHandles: unsupported handles")

{-------------------------------------------------------------------------------
  To reset the handle we duplicate the implementation of 'stdin', 'stdout' or
  'stderr' and then use 'swapFileHandles' to swap the MVar contents of the real
  Handle
-------------------------------------------------------------------------------}

resetStdin :: IO.TextEncoding -> IO ()
resetStdin enc = do
  new <- mkHandle FD.stdin "<stdin>" ReadHandle True (Just enc)
           nativeNewlineMode{-translate newlines-}
           (Just stdHandleFinalizer) Nothing
  swapFileHandles new IO.stdin

resetStdout :: IO.TextEncoding -> IO ()
resetStdout enc = do
  new <- mkHandle FD.stdout "<stdout>" WriteHandle True (Just enc)
           nativeNewlineMode{-translate newlines-}
           (Just stdHandleFinalizer) Nothing
  swapFileHandles new IO.stdout

resetStderr :: IO.TextEncoding -> IO ()
resetStderr enc = do
  new <- mkHandle FD.stderr "<stderr>" WriteHandle False{-stderr is unbuffered-}
           (Just enc)
           nativeNewlineMode{-translate newlines-}
           (Just stdHandleFinalizer) Nothing
  swapFileHandles new IO.stderr

{-------------------------------------------------------------------------------
  Taken directly from the GHC.IO.Handle.FD (not exported)
-------------------------------------------------------------------------------}

stdHandleFinalizer :: FilePath -> MVar Handle__ -> IO ()
stdHandleFinalizer fp m = do
  h_ <- takeMVar m
  flushWriteBuffer h_
  case haType h_ of
      ClosedHandle -> return ()
      _other       -> closeTextCodecs h_
  putMVar m (ioe_finalizedHandle fp)
