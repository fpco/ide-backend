{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}
module RpcServer
  ( -- * Server-side
    rpcServer
  , RpcServerActions(..)
    -- * Client-side
  , RpcServer
  , forkRpcServer
  , rpc
  , rpcWithProgress
  , rpcWithProgressCallback
  , shutdown
  , ExternalException(..)
  , underconsumptionException
  , overconsumptionException
  , serverKilledException
  ) where

import Prelude hiding (take)
import System.IO
  ( Handle
  , hSetBinaryMode
  , hSetBuffering
  , BufferMode(BlockBuffering, BlockBuffering)
  , hFlush
  , hClose
  )
import System.Process
  ( createProcess
  , proc
  , ProcessHandle
  , waitForProcess
  )
import System.Posix.Types (Fd)
import System.Posix.IO (createPipe, closeFd, fdToHandle)
import Data.Typeable (Typeable)
import Data.Aeson
  ( FromJSON
  , ToJSON
  , encode
  , json'
  , fromJSON
  , Result(Success, Error)
  )
import Data.Aeson.TH (deriveJSON)
import Control.Applicative ((<$>))
import Control.Monad (forever, void)
import qualified Control.Exception as Ex
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar
  ( MVar
  , newMVar
  , newEmptyMVar
  , putMVar
  , readMVar
  , modifyMVar
  , takeMVar
  )
import Data.ByteString.Lazy (ByteString, hPut, hGetContents, take)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Attoparsec.ByteString.Lazy (parse, Result(Done, Fail))

import Progress

--------------------------------------------------------------------------------
-- Exceptions thrown by the RPC server are retrown locally as                 --
-- 'ExternalException's                                                       --
--------------------------------------------------------------------------------

-- | Exceptions thrown by the remote server
data ExternalException = ExternalException {
     -- | The output from the server on stderr
     externalStdErr    :: String
     -- | The local exception that was thrown and alerted us to the problem
   , externalException :: Maybe Ex.IOException
   }
  deriving (Eq, Typeable)

instance Show ExternalException where
  show (ExternalException err Nothing) =
    "External exception: " ++ err
  show (ExternalException err (Just ex)) =
    "External exception: " ++ err ++ ". Local exception: " ++ show ex

instance Ex.Exception ExternalException

-- | Generic exception thrown if the server gets killed for unknown reason
serverKilledException :: Maybe Ex.IOException -> ExternalException
serverKilledException ex = ExternalException "Server killed" ex

--------------------------------------------------------------------------------
-- Internal data types                                                        --
--                                                                            --
-- We wrap the requests and responses to and from the RPC server in a custom  --
-- data type for two reasons. First, top-level JSON values must be objects or --
-- arrays; by wrapping them the user does not have to worry about this.       --
-- Second, it allows us to send control messages back and worth when we need  --
-- to.                                                                        --
--------------------------------------------------------------------------------

data Request  a = Request { _request :: a }
data Response a = FinalResponse        { _response :: a }
                | IntermediateResponse { _response :: a }

$(deriveJSON tail ''Request)
$(deriveJSON tail ''Response)

--------------------------------------------------------------------------------
-- Server-side API                                                            --
--------------------------------------------------------------------------------

-- Start the RPC server. For an explanation of the command line arguments, see
-- 'forkRpcServer'. This function does not return unless there is an error.
rpcServer :: (FromJSON req, ToJSON resp)
          => [String]                                   -- ^ Command line args
          -> (RpcServerActions req resp resp -> IO ())  -- ^ Request server
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

data RpcServerActions req prog resp
   = RpcServerActions {
       getRequest   :: IO req,
       putProgress  :: prog -> IO (),
       putResponse  :: resp -> IO ()
     }

-- | Start the RPC server
rpcServer' :: (FromJSON req, ToJSON resp)
           => Handle                           -- ^ Input
           -> Handle                           -- ^ Output
           -> Handle                           -- ^ Errors
           -> (RpcServerActions req resp resp -> IO ()) -- ^ The request server
           -> IO ()
rpcServer' hin hout herr server = do
  requests  <- newChan
  responses <- newChan
  exception <- newEmptyMVar :: IO (MVar Ex.SomeException)

  setBinaryBlockBuffered [hin, hout, herr]

  let forkCatch p = forkIO $ Ex.catch p (putMVar exception)

  tid1 <- forkCatch $ readRequests hin requests
  tid2 <- forkCatch $ writeResponses responses hout
  tid3 <- forkCatch $ channelHandler requests responses server

  ex <- readMVar exception
  hPutFlush herr (pack (show ex))
  mapM_ killThread [tid1, tid2, tid3]

--------------------------------------------------------------------------------
-- Client-side API                                                            --
--------------------------------------------------------------------------------

-- | Abstract data type representing RPC servers
data RpcServer req resp = RpcServer {
    -- | Handle to write requests to
    rpcRequestW  :: Handle
    -- | Handle to read server errors from
  , rpcErrorsR   :: Handle
    -- | Handle on the server process itself
  , rpcProc :: ProcessHandle
    -- | Server state
  , rpcState :: MVar (RpcState req resp)
  }

-- | RPC server state
data RpcState req resp =
    -- | The server is running. We record the server's unconsumed output.
    RpcRunning ByteString
    -- | The server was stopped, either manually or because of an exception
  | RpcStopped Ex.SomeException

-- | Fork an RPC server as a separate process
--
-- @forkRpcServer exec args@ starts executable @exec@ with arguments
-- @args ++ args'@ where @args'@ are internal arguments generated by
-- 'forkRpcServer'. These internal arguments should be passed as arguments
-- to 'rpcServer'.
--
-- As a typical example, you might pass @["--server"]@ as @args@, and the
-- 'main' function of @exec@ might look like
--
-- > main = do
-- >   args <- getArgs
-- >   case args of
-- >     "--server" : args' ->
-- >       rpcServer args' <<your request handler>>
-- >     _ ->
-- >       <<deal with other cases>>
forkRpcServer :: FilePath  -- ^ Filename of the executable
              -> [String]  -- ^ Arguments
              -> IO (RpcServer req resp)
forkRpcServer path args = do
  (requestR,  requestW)  <- createPipe
  (responseR, responseW) <- createPipe
  (errorsR,   errorsW)   <- createPipe

  -- We create a file for the server to store exceptions in
  let showFd :: Fd -> String
      showFd fd = show (fromIntegral fd :: Int)

  let args' = args ++ map showFd [ requestR,  requestW
                                 , responseR, responseW
                                 , errorsR,   errorsW
                                 ]
  (Nothing, Nothing, Nothing, ph) <- createProcess $ proc path args'

  -- Close the ends of the pipes that we're not using, and convert the rest
  -- to handles
  closeFd requestR
  closeFd responseW
  closeFd errorsW
  requestW'  <- fdToHandle requestW
  responseR' <- fdToHandle responseR
  errorsR'   <- fdToHandle errorsR

  requestStream <- hGetContents responseR'

  st <- newMVar $ RpcRunning requestStream
  return RpcServer {
      rpcRequestW  = requestW'
    , rpcErrorsR   = errorsR'
    , rpcProc      = ph
    , rpcState     = st
    }

-- | Do an RPC call
--
-- Throws an exception if the server returns a malformed result or the server
-- has shut down, or if the server returns more than one message (see
-- 'rpcWithProgress' instead)
rpc :: (ToJSON req, FromJSON resp)
    => RpcServer req resp  -- ^ RPC server
    -> req                 -- ^ Request
    -> IO resp             -- ^ Response
rpc server req = rpcWithProgress server req $ \p -> do
  resp <- progressWait p
  case resp of
    Left  r -> return r
    Right _ -> Ex.throwIO (userError "rpc: Unexpected intermediate response")

-- | Like 'rpc', but with support for receiving multiple replies for the
-- same request
rpcWithProgress :: forall req resp b. (ToJSON req, FromJSON resp)
                => RpcServer req resp           -- ^ RPC server
                -> req                          -- ^ Request
                -> (Progress resp resp -> IO b) -- ^ Handler
                -> IO b
rpcWithProgress server req handler = withRpcServer server $ \st ->
  case st of
    RpcRunning out -> do
      mapIOToExternal server $
        hPutFlush (rpcRequestW server) (encode $ Request req)

      -- We maintain the state of the progress in an MVar holding an
      -- 'Either ByteString ByteString'. This ByteString is the parser state
      -- (unconsumed input). As long as it's Right, more messages are available;
      -- after the last message the state becomes Left. This means that
      --
      -- 1. We can get the final state of the input after the handler returns
      -- 2. We can throw an exception if the user does not consume all input
      -- 3. We can throw an exception if the user consumes too much input
      -- 4. We can detect if the Progress object escapes the scope of
      --    rpcWithServer (by changing the state after the handler returns).
      progressState <- newMVar (Right out)

      -- Call the handler, update the state, and return the result
      b <- handler $ progress progressState

      -- At this point the handler has returned, but we have not yet changed
      -- the 'progressState' to make sure it's Right. So potentially the
      -- Progress object *might* escape from the handler, and then be invoked
      -- until we change the state below. Although strange (and unlikely), it
      -- won't cause any problems: we don't release the lock on the RPC server
      -- state until we modify the 'progressState', below, and as soon as we do
      -- change the 'progressState' the escaped 'Progress' object becomes
      -- useless.

      out' <- modifyMVar progressState $ \mOut ->
        case mOut of
          Left out' -> return (Left out', out')
          Right _   -> Ex.throwIO underconsumptionException

      return (RpcRunning out', b)
    RpcStopped ex ->
      Ex.throwIO ex
  where
    progress :: MVar (Either ByteString ByteString)
             -> Progress resp resp
    progress progressState = Progress $
      modifyMVar progressState $ \mOut ->
        case mOut of
          Right out -> do
            (out', value) <- mapIOToExternal server $ case parse json' out of
                               Fail _ _ err  -> Ex.throwIO $ parseError err out
                               Done out' val -> return (out', val)
            case fromJSON value of
              Success (FinalResponse resp) ->
                return (Left out', Left resp)
              Success (IntermediateResponse resp) ->
                return (Right out', Right (resp, progress progressState))
              Error err ->
                Ex.throwIO (userError err)
          Left _ ->
            Ex.throwIO overconsumptionException

    parseError :: String -> ByteString -> Ex.IOException
    parseError err out = userError $
      "Could not parse server response '" ++ unpack (take 80 out) ++ "': " ++ err

underconsumptionException :: Ex.IOException
underconsumptionException =
  userError "rpcWithProgress: Not all messages consumed"

overconsumptionException :: Ex.IOException
overconsumptionException =
  userError "rpcWithProgress: No more messages"

-- | Variation on 'rpcWithProgress' with a callback for intermediate messages
rpcWithProgressCallback :: (ToJSON req, FromJSON resp)
                        => RpcServer req resp  -- ^ RPC server
                        -> req                 -- ^ Request
                        -> (resp -> IO ())     -- ^ Callback
                        -> IO resp
rpcWithProgressCallback server req callback = rpcWithProgress server req handler
  where
    handler p = do
      resp <- progressWait p
      case resp of
        Left lastResponse ->
          return lastResponse
        Right (intermediateResponse, p') -> do
          callback intermediateResponse
          handler p'

-- | Shut down the RPC server
--
-- This simply kills the remote process. If you want to shut down the remote
-- process cleanly you must implement your own termination protocol before
-- calling 'shutdown'.
shutdown :: RpcServer req resp -> IO ()
shutdown server = withRpcServer server $ \_ -> do
  terminate server
  let ex = Ex.toException (userError "Manual shutdown")
  return (RpcStopped ex, ())

-- | Force-terminate the external process
terminate :: RpcServer req resp -> IO ()
terminate server = do
  ignoreIOExceptions $ hClose (rpcRequestW server)
  void $ waitForProcess (rpcProc server)

-- | Like modifyMVar, but terminate the server on exceptions
withRpcServer :: RpcServer req resp
              -> (RpcState req resp -> IO (RpcState req resp, a))
              -> IO a
withRpcServer server io =
  Ex.mask $ \restore -> do
    st <- takeMVar (rpcState server)

    mResult <- Ex.try $ restore (io st)

    case mResult of
      Right (st', a) -> do
        putMVar (rpcState server) st'
        return a
      Left ex -> do
        terminate server
        putMVar (rpcState server) (RpcStopped ex)
        Ex.throwIO ex

--------------------------------------------------------------------------------
-- Internal                                                                   --
--------------------------------------------------------------------------------

-- | Decode messages from a handle and forward them to a channel
readRequests :: forall req. FromJSON req => Handle -> Chan (Request req) -> IO ()
readRequests h ch = hGetContents h >>= go
  where
    go :: ByteString -> IO ()
    go contents =
      case parseJSON contents of
        Right (contents', req) -> writeChan ch req >> go contents'
        Left err               -> Ex.throwIO (userError err)

    parseJSON :: FromJSON a => ByteString -> Either String (ByteString, a)
    parseJSON bs =
      case parse json' bs of
        Fail _ _ err -> Left err
        Done bs' value ->
          case fromJSON value of
            Success req -> Right (bs', req)
            Error err   -> Left err

-- | Encode messages from a channel and forward them on a handle
writeResponses :: ToJSON resp => Chan (Response resp) -> Handle -> IO ()
writeResponses ch h = forever $ readChan ch >>= hPutFlush h . encode

-- | Run a handler repeatedly, given input and output channels
channelHandler :: Chan (Request req)
               -> Chan (Response resp)
               -> (RpcServerActions req resp resp -> IO ())
               -> IO ()
channelHandler inp outp server =
    server RpcServerActions {
      getRequest   = _request <$> readChan inp,
      putProgress  = writeChan outp . IntermediateResponse,
      putResponse  = writeChan outp . FinalResponse
    }

-- | Set all the specified handles to binary mode and block buffering
setBinaryBlockBuffered :: [Handle] -> IO ()
setBinaryBlockBuffered =
  mapM_ $ \h -> do hSetBinaryMode h True
                   hSetBuffering  h (BlockBuffering Nothing)

-- | Map IO exceptions to external exceptions, using the error written
-- by the server (if any)
mapIOToExternal :: RpcServer req resp -> IO a -> IO a
mapIOToExternal server p = Ex.catch p $ \ex -> do
  let _ = ex :: Ex.IOException
  merr <- unpack <$> hGetContents (rpcErrorsR server)
  if null merr
    then Ex.throwIO (serverKilledException (Just ex))
    else Ex.throwIO (ExternalException merr (Just ex))

-- | Write a bytestring to a buffer and flush
hPutFlush :: Handle -> ByteString -> IO ()
hPutFlush h bs = hPut h bs >> hFlush h

-- | Ignore IO exceptions
ignoreIOExceptions :: IO () -> IO ()
ignoreIOExceptions = Ex.handle ignore
  where
    ignore :: Ex.IOException -> IO ()
    ignore _ = return ()
