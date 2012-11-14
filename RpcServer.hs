{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}
module RpcServer
  ( -- * Server-side
    rpcServer
    -- * Client-side
  , RpcServer
  , forkRpcServer
  , rpc
  , rpcWithProgress
  , rpcWithProgressCallback
  , shutdown
  , ExternalException
    -- * Progress
  , Progress(..)
  ) where

import System.IO
  ( Handle
  , hSetBinaryMode
  , hSetBuffering
  , BufferMode(BlockBuffering, NoBuffering)
  , hFlush
  , hIsClosed
  , hClose
  )
import System.Process
  ( CreateProcess(std_in, std_out, std_err)
  , createProcess
  , proc
  , StdStream(CreatePipe)
  , ProcessHandle
  , waitForProcess
  )
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
import Control.Monad (forever)
import qualified Control.Exception as Ex
import Control.Concurrent
  ( ThreadId
  , forkIO
  , killThread
  , threadDelay
  , myThreadId
  , throwTo
  )
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
import Data.ByteString.Lazy (ByteString, hPut, hGetContents)
import Data.Attoparsec.ByteString.Lazy (parse, Result(Done, Fail))

--------------------------------------------------------------------------------
-- Simple "Future" data type                                                  --
--                                                                            --
-- (we should probably move this elsewhere eventually)                        --
--------------------------------------------------------------------------------

newtype Progress p a = Progress {
    progressWait :: IO (Either a (p, Progress p a))
  }

--------------------------------------------------------------------------------
-- Exceptions thrown by the RPC server are retrown locally as                 --
-- 'ExternalException's                                                       --
--------------------------------------------------------------------------------

-- | Exceptions thrown by the remote server
--
-- The record accessor ensures deriveJSON wraps the whole thing in an object
-- so that we can send it as a top-level JSON object
data ExternalException = ExternalException { externalException :: String }
  deriving (Typeable)

instance Show ExternalException where
  show ex = "External exception: " ++ externalException ex

instance Ex.Exception ExternalException

$(deriveJSON id ''ExternalException)

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

-- | Start the RPC server
rpcServer :: (FromJSON req, ToJSON resp)
          => Handle                           -- ^ Input
          -> Handle                           -- ^ Output
          -> Handle                           -- ^ Errors
          -> (req -> IO (Progress resp resp)) -- ^ The request handler
          -> IO ()
rpcServer hin hout herr handler = do
  requests  <- newChan
  responses <- newChan
  exception <- newEmptyMVar :: IO (MVar Ex.SomeException)

  setBinaryBlockBuffered [hin, hout, herr]

  let forkCatch p = forkIO $ Ex.catch p (putMVar exception)

  tid1 <- forkCatch $ readRequests hin requests
  tid2 <- forkCatch $ writeResponses responses hout
  tid3 <- forkCatch $ channelHandler requests responses handler

  ex <- readMVar exception
  hPut herr (encode . ExternalException . show $ ex)
  hFlush herr
  -- TODO: do we need to wait for the client to read the exception?
  mapM_ killThread [tid1, tid2, tid3]

--------------------------------------------------------------------------------
-- Client-side API                                                            --
--------------------------------------------------------------------------------

-- | Abstract data type representing RPC servers
data RpcServer req resp = RpcServer {
    -- | State of the RPC server
    rpcState :: MVar (RpcState req resp)
    -- | When the server throws an exception, we output it on stderr, read the
    -- exception in the client, and then store it here. We don't throw it
    -- asynchronously because this would mean client code would have to worry
    -- about asynchronous exceptions everywhere as long as the RPC server is
    -- running. Instead, we will throw the exception on the next (or current)
    -- call to 'rpc' (and friends)
  , rpcPendingException :: MVar ExternalException
  }

-- | RPC server state
data RpcState req resp =
    RpcRunning {
        -- | The server's 'stdin'
        rpcIn :: Handle
        -- | The server's 'stdout' (this essentially represents the parser state)
      , rpcOut :: ByteString
        -- | Thread that forwards the server's stderr to our stderr
      , rpcErr :: ThreadId
        -- | Handle on the server process itself
      , rpcProc :: ProcessHandle
      }
  | RpcStopped Ex.SomeException

-- | Fork an RPC server as a separate process
forkRpcServer :: FilePath  -- ^ Filename of the executable
              -> [String]  -- ^ Arguments
              -> IO (RpcServer req resp)
forkRpcServer path args = do
  (Just hin, Just hout, Just herr, ph) <- createProcess $ (proc path args) {
      std_in  = CreatePipe
    , std_out = CreatePipe
    , std_err = CreatePipe
    }
  setBinaryBlockBuffered [hin, hout, herr]
  pendingException <- newEmptyMVar
  tid <- forkIO $ forwardExceptions herr pendingException
  contents <- hGetContents hout
  st <- newMVar RpcRunning {
      rpcIn   = hin
    , rpcOut  = contents
    , rpcErr  = tid
    , rpcProc = ph
    }
  return RpcServer {
      rpcState            = st
    , rpcPendingException = pendingException
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
rpcWithProgress :: (ToJSON req, FromJSON resp)
                => RpcServer req resp           -- ^ RPC server
                -> req                          -- ^ Request
                -> (Progress resp resp -> IO b) -- ^ Handler
                -> IO b
rpcWithProgress server req handler = withRpcServer server $ \st ->
  case st of
    RpcRunning {} -> do
      hPut (rpcIn st) $ encode (Request req)
      hFlush (rpcIn st)

      -- We create a local MVar that holds the parser state (unparsed part
      -- of the server input). This MVar is updated every time the handler
      -- waits on the Progress object, so that once the handler returns
      -- we can retrieve the final state. This has the additional advantage
      -- that once we take the MVar at the end the user will no longer be
      -- able to call the handler (that is, we can't stop the user from
      -- calling wait, but it will deadlock and not affect other RPC calls)
      outSt <- newMVar (rpcOut st)

      -- Construct the Progress object
      let go = Progress $ modifyMVar outSt $ \out ->
                 case parseJSON out of
                   Right (out', FinalResponse resp) ->
                     return (out', Left resp)
                   Right (out', IntermediateResponse resp) ->
                     return (out', Right (resp, go))
                   Left err ->
                     Ex.throwIO (userError err)

      -- Call the handler, update the state, and return the result
      b <- handler go
      out' <- takeMVar outSt
      return (st { rpcOut = out' }, b)
    RpcStopped ex ->
      Ex.throwIO (userError $ "rpc: server shutdown (" ++ show ex ++ ")")

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
shutdown server = withRpcServer server $ \st -> do
  ex <- terminate st (Ex.toException (userError "Manual shutdown"))
  return (RpcStopped ex, ())

-- | Force-terminate the external process
terminate :: RpcState req resp -> Ex.SomeException -> IO Ex.SomeException
terminate st@(RpcRunning {}) ex = do
  -- We terminate the thread that listens for exceptions from the server first,
  -- because we don't want the closing of stdin to be reported as an
  -- ExternalException (rather, we want the exception to be "closed manually")
  killThread (rpcErr st)

  -- Close the server's stdin. This will cause the server to exit
  hClose (rpcIn st)

  -- Wait for the server to terminate
  _ <- waitForProcess (rpcProc st)

  return ex
terminate (RpcStopped ex') _ =
  return ex' -- Already stopped

-- | Like modifyMVar, but terminate the server on exceptions
withRpcServer :: RpcServer req resp
              -> (RpcState req resp -> IO (RpcState req resp, a))
              -> IO a
withRpcServer server io =
  Ex.mask $ \restore -> do
    st <- takeMVar (rpcState server)

    mainThread <- myThreadId
    auxThread <- forkIO $ do
      ex <- readMVar (rpcPendingException server)
      throwTo mainThread ex

    mResult <- Ex.try $ restore (io st)

    killThread auxThread

    case mResult of
      Right (st', a) -> do
        putMVar (rpcState server) st'
        return a
      Left ex -> do
        ex' <- terminate st ex
        putMVar (rpcState server) (RpcStopped ex')
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

-- | Parse a JSON value, and return the remainder of the input along with
-- the result (or an error message)
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
writeResponses ch h = forever $ do
  resp <- readChan ch
  hPut h (encode resp)
  hFlush h

-- | Run a handler repeatedly, given input and output channels
channelHandler :: Chan (Request req)
               -> Chan (Response resp)
               -> (req -> IO (Progress resp resp))
               -> IO ()
channelHandler inp outp handler = forever $ do
    Request req <- readChan inp
    handler req >>= go
  where
    go p = do
      resp <- progressWait p
      case resp of
        Left lastResponse ->
          writeChan outp (FinalResponse lastResponse)
        Right (intermediateResponse, p') -> do
          writeChan outp (IntermediateResponse intermediateResponse)
          go p'

-- | Read an exception from the remote server and store it locally
-- (see description of 'RpcServer')
forwardExceptions :: Handle -> MVar ExternalException -> IO ()
forwardExceptions h mvar = do
  contents <- hGetContents h
  case parseJSON contents of
    Right (_, ex) ->
      putMVar mvar ex
    Left err ->
      putMVar mvar (ExternalException $ "Malformed exception: " ++ err)

-- | Set all the specified handles to binary mode and block buffering
setBinaryBlockBuffered :: [Handle] -> IO ()
setBinaryBlockBuffered =
  mapM_ $ \h -> do hSetBinaryMode h True
                   hSetBuffering  h NoBuffering -- (BlockBuffering Nothing)

-- | Wait for a handle to close
--
-- TODO: Is there a better way to do this?
waitUntilClosed :: Handle -> IO ()
waitUntilClosed h = go
  where
    go = do
      closed <- hIsClosed h
      if closed then return ()
                else threadDelay 10000 >> go
