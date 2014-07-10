{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, DeriveDataTypeable, RankNTypes, GADTs #-}
{-# OPTIONS_GHC -Wall #-}
module IdeSession.RPC.Server
  ( rpcServer
  , concurrentConversation
  , RpcConversation(..)
  ) where

import Prelude hiding (take)
import System.IO
  ( Handle
  , hSetBinaryMode
  , hSetBuffering
  , BufferMode(BlockBuffering)
  )
import System.Posix.Types (Fd)
import System.Posix.IO (closeFd, fdToHandle)
import Control.Monad (void)
import qualified Control.Exception as Ex
import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan (Chan, newChan, writeChan)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Control.Concurrent.Async (Async, async)
import Data.Binary (encode, decode)

import IdeSession.Util.BlockingOps (readChan, wait, waitAny)
import IdeSession.RPC.API
import IdeSession.RPC.Stream

--------------------------------------------------------------------------------
-- Server-side API                                                            --
--------------------------------------------------------------------------------

-- Start the RPC server. For an explanation of the command line arguments, see
-- 'forkRpcServer'. This function does not return until the client requests
-- termination of the RPC conversation (or there is an error).
rpcServer :: (RpcConversation -> IO ()) -- ^ Request server
          -> [String]                   -- ^ Command line args
          -> IO ()
rpcServer handler fds = do
  let readFd :: String -> Fd
      readFd fd = fromIntegral (read fd :: Int)

  let [requestR, requestW, responseR, responseW, errorsR, errorsW] = map readFd fds

  closeFd requestW
  closeFd responseR
  closeFd errorsR
  requestR'  <- fdToHandle requestR
  responseW' <- fdToHandle responseW
  errorsW'   <- fdToHandle errorsW

  rpcServer' requestR' responseW' errorsW' handler

-- | Start a concurrent conversation.
concurrentConversation :: FilePath -- ^ stdin named pipe
                       -> FilePath -- ^ stdout named pipe
                       -> FilePath -- ^ stderr named pipe
                       -> (RpcConversation -> IO ())
                       -> IO ()
concurrentConversation requestR responseW errorsW server = do
    hin  <- openPipeForReading requestR  timeout
    hout <- openPipeForWriting responseW timeout
    herr <- openPipeForWriting errorsW   timeout
    rpcServer' hin hout herr server
  where
    timeout :: Int
    timeout = maxBound

-- | Start the RPC server
rpcServer' :: Handle                     -- ^ Input
           -> Handle                     -- ^ Output
           -> Handle                     -- ^ Errors
           -> (RpcConversation -> IO ()) -- ^ The request server
           -> IO ()
rpcServer' hin hout herr server = do
    requests   <- newChan :: IO (Chan BSL.ByteString)
    responses  <- newChan :: IO (Chan (Maybe BSL.ByteString))

    setBinaryBlockBuffered [hin, hout, herr]

    -- Each thread installs it own exception handler before unmasking
    -- asynchronous exceptions. This way when an exception occurs we can
    -- identify it (by looking at which ServerEvent was returned).
    (reader, writer, handler) <- Ex.mask $ \restore -> do
      reader  <- async $ readRequests   restore hin requests
      writer  <- async $ writeResponses restore responses hout
      handler <- async $ channelHandler restore requests responses server
      return (reader, writer, handler)

    (_thread, ev) <- $waitAny [reader, writer, handler]
    case ev of
      -- If we lose connection with the client, just terminate.
      -- See #194 (in particular, https://github.com/fpco/ide-backend/issues/194#issuecomment-44210412)
      LostConnection ex ->
        tryShowException (Just ex)

      -- If the client requests termination, we simply terminate immediately.
      -- It is the client's responsibility to have a proper shutdown protocol
      -- with the server thread
      ReaderThreadTerminated ->
        return ()

      -- The writer thread should never terminate normally unless we request
      -- it; this is a logical impossibility :)
      WriterThreadTerminated ->
        error "The impossible happened"

      -- When the main server thread terminates we ask the writer thread to
      -- terminate so that we make sure to send any pending messages
      ServerThreadTerminated ->
        tryShowException =<< flushResponses responses writer

      -- When the main server thread aborts, we still attempt to flush any
      -- remaining messages, but the exception that we record is the one from
      -- the server (the writer thread might terminate with a further exception)
      ServerThreadAborted ex -> do
        tryShowException (Just ex)
        void $ flushResponses responses writer

    threadDelay 100000
  where
    tryShowException :: Maybe Ex.SomeException -> IO ()
    tryShowException (Just ex) =
      -- We don't want to throw an exception showing the previous exception
      ignoreIOExceptions $ hPutFlush herr . BSL.pack . show $ ex
    tryShowException Nothing =
      return ()

--------------------------------------------------------------------------------
-- Internal                                                                   --
--------------------------------------------------------------------------------

-- | We record the reason why the various threads are terminating, so that we
-- can take the appropriate action
data ServerEvent =
    -- | The reader thread terminates when the client sends a 'RequestShutdown'
    -- message
    ReaderThreadTerminated

    -- | After the main server thread terminates, we wait for the writer thread
    -- to terminate to make sure there are no pending unsent messages
  | WriterThreadTerminated

    -- | Termination of the main server thread
  | ServerThreadTerminated

    -- | Main server thread threw an exception
  | ServerThreadAborted Ex.SomeException

    -- | The reader thread and writer threads terminate with 'LostConnection'
    -- if an exception occurs
  | LostConnection Ex.SomeException

-- | Decode messages from a handle and forward them to a channel.
-- The boolean result indicates whether the shutdown is forced.
readRequests :: Restore -> Handle -> Chan BSL.ByteString -> IO ServerEvent
readRequests restore h ch =
    Ex.handle (return . LostConnection)
              (restore (newStream h >>= go))
  where
    go :: Stream Request -> IO ServerEvent
    go input = do
      req <- nextInStream input
      case req of
        Request req'         -> writeChan ch (unIncBS req') >> go input
        RequestShutdown      -> return ReaderThreadTerminated

-- | Encode messages from a channel and forward them on a handle
--
-- Terminates on 'Nothing'.
writeResponses :: Restore -> Chan (Maybe BSL.ByteString) -> Handle -> IO ServerEvent
writeResponses restore ch h =
    Ex.handle (return . LostConnection)
              (restore go)
  where
    go :: IO ServerEvent
    go = do
      mbs <- $readChan ch
      case mbs of
        Just bs -> do hPutFlush h $ encode (Response (IncBS bs)) ; go
        Nothing -> return WriterThreadTerminated

-- | Ask the writer thread to terminate and wait for all remaining messages to
-- have been sent. Returns 'Nothing' if the writer thread terminated normally,
-- or the exception if it didn't.
flushResponses :: Chan (Maybe BSL.ByteString) -> Async ServerEvent -> IO (Maybe Ex.SomeException)
flushResponses responses writer = do
  writeChan responses Nothing
  ev <- $wait writer
  case ev of
    WriterThreadTerminated ->
      return Nothing
    LostConnection ex ->
      return (Just ex)
    _ ->
      error "the impossible happened"

-- | Run a handler repeatedly, given input and output channels
channelHandler :: Restore
               -> Chan BSL.ByteString
               -> Chan (Maybe BSL.ByteString)
               -> (RpcConversation -> IO ())
               -> IO ServerEvent
channelHandler restore requests responses server =
    Ex.handle (return . ServerThreadAborted)
              (restore go)
  where
    go :: IO ServerEvent
    go = do
      server RpcConversation {
          get = $readChan requests >>= Ex.evaluate . decode
        , put = writeChan responses . Just . encode
        }
      return ServerThreadTerminated

--------------------------------------------------------------------------------
-- Auxiliary                                                                  --
--------------------------------------------------------------------------------

type Restore = forall a. IO a -> IO a

-- | Set all the specified handles to binary mode and block buffering
setBinaryBlockBuffered :: [Handle] -> IO ()
setBinaryBlockBuffered =
  mapM_ $ \h -> do hSetBinaryMode h True
                   hSetBuffering  h (BlockBuffering Nothing)
