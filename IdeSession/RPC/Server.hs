{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, DeriveDataTypeable, RankNTypes, GADTs #-}
{-# OPTIONS_GHC -Wall #-}
module IdeSession.RPC.Server
  ( rpcServer
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
import System.Posix.Process (exitImmediately)
import System.Exit (ExitCode(ExitFailure))
import Data.Typeable (Typeable, typeOf)
import Control.Monad (forever)
import qualified Control.Exception as Ex
import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan (Chan, newChan, writeChan)
import qualified Data.ByteString.Lazy.Char8 as BSL
import Control.Concurrent.Async (async)
import Data.Binary (encode, decode)

import IdeSession.BlockingOps (readChan, waitAnyCatchCancel, waitCatch)
import IdeSession.RPC.API
import IdeSession.RPC.Stream
import TraceMonad

--------------------------------------------------------------------------------
-- Server-side API                                                            --
--------------------------------------------------------------------------------

-- Start the RPC server. For an explanation of the command line arguments, see
-- 'forkRpcServer'. This function does not return unless there is an error.
rpcServer :: [String]                   -- ^ Command line args
          -> (RpcConversation -> IO ()) -- ^ Request server
          -> IO ()
rpcServer fds handler = do
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

-- | Start the RPC server
rpcServer' :: Handle                     -- ^ Input
           -> Handle                     -- ^ Output
           -> Handle                     -- ^ Errors
           -> (RpcConversation -> IO ()) -- ^ The request server
           -> IO ()
rpcServer' hin hout herr server = do
    requests   <- newChan :: IO (Chan BSL.ByteString)
    responses  <- newChan :: IO (Chan (String, BSL.ByteString))

    setBinaryBlockBuffered [hin, hout, herr]

    reader  <- async $ readRequests hin requests
    writer  <- async $ writeResponses responses hout
    handler <- async $ channelHandler requests responses server

    $waitAnyCatchCancel [reader, writer, handler] >>= tryShowException . snd
    mapM_ $waitCatch [reader, writer, handler]
    threadDelay 100000
  where
    tryShowException :: Either Ex.SomeException () -> IO ()
    tryShowException (Left ex) =
      -- We don't want to throw an exception showing the previous exception
      ignoreIOExceptions $ hPutFlush herr . BSL.pack . show $ ex
    tryShowException (Right ()) =
      return ()

--------------------------------------------------------------------------------
-- Internal                                                                   --
--------------------------------------------------------------------------------

-- | Decode messages from a handle and forward them to a channel.
-- The boolean result indicates whether the shutdown is forced.
readRequests :: Handle -> Chan BSL.ByteString -> IO ()
readRequests h ch = newStream h >>= go
  where
    go :: Stream Request -> IO ()
    go input = do
      req <- nextInStream input
      case req of
        Request req'         -> writeChan ch (unIncBS req') >> go input
        RequestShutdown      -> return ()
        RequestForceShutdown -> exitImmediately (ExitFailure 1)

-- | Encode messages from a channel and forward them on a handle
writeResponses :: Chan (String, BSL.ByteString) -> Handle -> IO ()
writeResponses ch h = forever $ do
  (label, bs) <- $readChan ch
  traceBlock ("Encoding and sending " ++ label) $
    hPutFlush h $ encode (Response (IncBS bs))

-- | Run a handler repeatedly, given input and output channels
channelHandler :: Chan BSL.ByteString
               -> Chan (String, BSL.ByteString)
               -> (RpcConversation -> IO ())
               -> IO ()
channelHandler requests responses server =
  server RpcConversation {
      get = $readChan requests >>= Ex.evaluate . decode
    , put = \resp -> do writeChan responses (show (typeOf resp), encode resp)
    }

-- | Set all the specified handles to binary mode and block buffering
setBinaryBlockBuffered :: [Handle] -> IO ()
setBinaryBlockBuffered =
  mapM_ $ \h -> do hSetBinaryMode h True
                   hSetBuffering  h (BlockBuffering Nothing)

