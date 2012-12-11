{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, DeriveDataTypeable, RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
module RpcServer
  ( -- * Server-side
    rpcServer
  , RpcConversation(..)
    -- * Client-side
  , RpcServer
  , forkRpcServer
  , rpc
  , rpcConversation
  , shutdown
  , ExternalException(..)
  , illscopedConversationException
  , serverKilledException
  ) where

import Prelude hiding (take)
import System.IO
  ( Handle
  , hSetBinaryMode
  , hSetBuffering
  , BufferMode(BlockBuffering, BlockBuffering)
  , hFlush
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
  , fromJSON
  , ToJSON(toJSON)
  , encode
  , json'
  , Result(Success, Error)
  , Value
  )
import Data.Aeson.TH (deriveJSON)
import Control.Applicative ((<$>))
import Control.Monad (void, forever)
import qualified Control.Exception as Ex
import Control.Concurrent (forkIOWithUnmask, killThread, ThreadId, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar
  ( MVar
  , newMVar
  , newEmptyMVar
  , putMVar
  , readMVar
  , modifyMVar
  , takeMVar
  , withMVar
  , tryPutMVar
  )
import Data.ByteString.Lazy (ByteString, hPut, hGetContents, take)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Attoparsec.ByteString.Lazy (parse, Result(Done, Fail))

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
                | RequestShutdown
data Response a = Response { _response :: a }

$(deriveJSON tail ''Request)
$(deriveJSON tail ''Response)

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

data RpcConversation = RpcConversation {
    get :: forall a. FromJSON a => IO a
  , put :: forall a. ToJSON a => a -> IO ()
  }

-- | Start the RPC server
rpcServer' :: Handle                     -- ^ Input
           -> Handle                     -- ^ Output
           -> Handle                     -- ^ Errors
           -> (RpcConversation -> IO ()) -- ^ The request server
           -> IO ()
rpcServer' hin hout herr server = do
    requests  <- newChan      :: IO (Chan Value)
    responses <- newChan      :: IO (Chan Value)
    exception <- newEmptyMVar :: IO (MVar (Maybe Ex.SomeException))

    setBinaryBlockBuffered [hin, hout, herr]

    let forkCatch :: IO () -> IO (ThreadId, MVar ())
        forkCatch p = do
          terminated <- newEmptyMVar
          tid <- Ex.mask_ $ forkIOWithUnmask $ \unmask ->
            Ex.catch (unmask (p >> putMVar terminated ())) $ \ex -> do
              void $ tryPutMVar terminated ()
              void $ tryPutMVar exception (Just ex)
          return (tid, terminated)

    readerThread <- forkCatch $ do readRequests hin requests
                                   putMVar exception Nothing
    writerThread <- forkCatch $ writeResponses responses hout
    serverThread <- forkCatch $ channelHandler requests responses server

    readMVar exception >>= tryShowException
    mapM_ (killThread . fst) [readerThread, writerThread, serverThread]
    mapM_ (takeMVar   . snd) [readerThread, writerThread, serverThread]
    -- TODO: Without this threadDelay, many of the unit tests print
    --   <stdout>: hPutChar: resource vanished (Broken pipe)
    -- to stderr, but I don't know why.
    threadDelay 100000
  where
    tryShowException :: Maybe Ex.SomeException -> IO ()
    tryShowException (Just ex) =
      -- We don't want to throw an exception showing the previous exception
      ignoreIOExceptions $ hPutFlush herr . pack . show $ ex
    tryShowException Nothing =
      return ()

--------------------------------------------------------------------------------
-- Client-side API                                                            --
--------------------------------------------------------------------------------

-- | Abstract data type representing RPC servers
data RpcServer = RpcServer {
    -- | Handle to write requests to
    rpcRequestW  :: Handle
    -- | Handle to read server errors from
  , rpcErrorsR   :: Handle
    -- | Handle on the server process itself
  , rpcProc :: ProcessHandle
    -- | Server state
  , rpcState :: MVar RpcClientSideState
  }

-- | RPC server state
data RpcClientSideState =
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
              -> IO RpcServer
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

-- | Specialized form of 'rpcConversation' to do single request and wait for
-- a single response.
rpc :: (ToJSON req, FromJSON resp) => RpcServer -> req -> IO resp
rpc server req = rpcConversation server $ \RpcConversation{..} -> put req >> get

-- | Run an RPC conversation. If the handler throws an exception durin
-- the conversation the server is terminated.
rpcConversation :: RpcServer
                -> (RpcConversation -> IO a)
                -> IO a
rpcConversation server handler = withRpcServer server $ \st ->
  case st of
    RpcRunning out -> do
      -- We maintain the state of the conversation in an MVar holding an 'Maybe
      -- ByteString'. This ByteString is the parser state (unconsumed input).
      -- When the handler returns, we change the state of the MVar to Nothing.
      -- This way we can get the final state of the input after the handler
      -- returns, and moreover avoid the handler calling into the conversation
      -- after it's returned.
      convState <- newMVar (Just out)

      -- Call the handler, update the state, and return the result
      a <- handler $ conversation convState

      -- At this point the handler has returned, but we have not yet changed
      -- the 'progressState' to make sure it's Nothing. So potentially the
      -- Progress object *might* escape from the handler, and then be invoked
      -- until we change the state below. Although strange (and unlikely), it
      -- won't cause any problems: we don't release the lock on the RPC server
      -- state until we modify the 'progressState', below, and as soon as we do
      -- change the 'progressState' the escaped 'Progress' object becomes
      -- useless.
      --
      out' <- modifyMVar convState $ \mOut ->
        case mOut of
          Just out' -> return (Nothing, out')
          Nothing   -> error "The impossible happened"

      return (RpcRunning out', a)
    RpcStopped ex ->
      Ex.throwIO ex
  where
    conversation :: MVar (Maybe ByteString) -> RpcConversation
    conversation convState = RpcConversation {
        put = \req -> withMVar convState $ \state -> case state of
                Just _ ->
                  mapIOToExternal server $
                    hPutFlush (rpcRequestW server) . encode . Request $ req
                Nothing ->
                  Ex.throwIO illscopedConversationException
      , get = modifyMVar convState $ \state -> case state of
                Just out -> do
                  (out', value) <- mapIOToExternal server $
                    case parse json' out of
                      Fail _ _ err  -> Ex.throwIO $ parseError err out
                      Done out' val -> return (out', val)
                  case fromJSON value of
                    Success (Response resp) ->
                      return (Just out', resp)
                    Error err ->
                      Ex.throwIO (userError err)
                Nothing ->
                  Ex.throwIO illscopedConversationException
      }

    parseError :: String -> ByteString -> Ex.IOException
    parseError err out = userError $
      "Could not parse server response '" ++ unpack (take 80 out) ++ "': " ++ err

illscopedConversationException :: Ex.IOException
illscopedConversationException =
  userError "Attempt to use RPC conversation outside its scope"

-- | Shut down the RPC server
--
-- This simply kills the remote process. If you want to shut down the remote
-- process cleanly you must implement your own termination protocol before
-- calling 'shutdown'.
shutdown :: RpcServer -> IO ()
shutdown server = withRpcServer server $ \_ -> do
  terminate server
  let ex = Ex.toException (userError "Manual shutdown")
  return (RpcStopped ex, ())

-- | Force-terminate the external process
terminate :: RpcServer -> IO ()
terminate server = do
    ignoreIOExceptions $ hPutFlush (rpcRequestW server) (encode requestShutdown)
    void $ waitForProcess (rpcProc server)
  where
    requestShutdown :: Request ()
    requestShutdown = RequestShutdown

-- | Like modifyMVar, but terminate the server on exceptions
withRpcServer :: RpcServer
              -> (RpcClientSideState -> IO (RpcClientSideState, a))
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
readRequests :: Handle -> Chan Value -> IO ()
readRequests h ch = hGetContents h >>= go
  where
    go :: ByteString -> IO ()
    go contents = do
      case parseJSON contents of
        Right (contents', req) ->
          case req of
            Request req'    -> writeChan ch req' >> go contents'
            RequestShutdown -> return ()
        Left err ->
          Ex.throwIO (userError err)

    parseJSON :: FromJSON a => ByteString -> Either String (ByteString, a)
    parseJSON bs =
      case parse json' bs of
        Fail _ _ err -> Left err
        Done bs' value ->
          case fromJSON value of
            Success req -> Right (bs', req)
            Error err   -> Left err

-- | Encode messages from a channel and forward them on a handle
writeResponses :: Chan Value -> Handle -> IO ()
writeResponses ch h = forever $ readChan ch >>= hPutFlush h . encode . Response

-- | Run a handler repeatedly, given input and output channels
channelHandler :: Chan Value
               -> Chan Value
               -> (RpcConversation -> IO ())
               -> IO ()
channelHandler requests responses server =
  server RpcConversation {
      get = do value <- readChan requests
               case fromJSON value of
                 Success req -> return req
                 Error err   -> Ex.throwIO (userError err)
    , put = writeChan responses . toJSON
    }

-- | Set all the specified handles to binary mode and block buffering
setBinaryBlockBuffered :: [Handle] -> IO ()
setBinaryBlockBuffered =
  mapM_ $ \h -> do hSetBinaryMode h True
                   hSetBuffering  h (BlockBuffering Nothing)

-- | Map IO exceptions to external exceptions, using the error written
-- by the server (if any)
mapIOToExternal :: RpcServer -> IO a -> IO a
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
