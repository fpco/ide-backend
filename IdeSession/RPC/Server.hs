{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, DeriveDataTypeable, RankNTypes #-}
{-# OPTIONS_GHC -Wall #-}
module IdeSession.RPC.Server
  ( -- * Server-side
    rpcServer
  , RpcConversation(..)
    -- * Client-side
  , RpcServer
  , forkRpcServer
  , rpc
  , rpcConversation
  , shutdown
  , forceShutdown
  , ExternalException(..)
  , illscopedConversationException
  , serverKilledException
  , getRpcExitCode
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
  , CreateProcess(cwd)
  , getProcessExitCode
  )
import System.Exit (ExitCode)
import System.Posix.Types (Fd)
import System.Posix.IO (createPipe, closeFd, fdToHandle)
import System.Posix.Process (exitImmediately)
import System.Exit (ExitCode(ExitFailure))
import System.Directory (canonicalizePath, getPermissions, executable)
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
import Control.Monad (void, forever, unless)
import qualified Control.Exception as Ex
import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan (Chan, newChan, writeChan)
import Control.Concurrent.MVar (MVar, newMVar)
import qualified Data.ByteString.Lazy as BSL (ByteString, hPut, hGetContents)
import qualified Data.ByteString.Lazy.Char8 as BSL (pack, unpack)
import qualified Data.Attoparsec as Attoparsec
import Data.Attoparsec.Lazy (parse, Result(..))
import Data.IORef (IORef, writeIORef, readIORef, newIORef)
import Control.Concurrent.Async (async)

import IdeSession.BlockingOps (putMVar, takeMVar, readChan, waitAnyCatchCancel, waitCatch)

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
                | RequestForceShutdown
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
    requests   <- newChan :: IO (Chan Value)
    responses  <- newChan :: IO (Chan Value)

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
    -- | Parser for the server input
  , rpcParser :: StreamParser Value
    -- | Server state
  , rpcState :: MVar RpcClientSideState
  }

-- | RPC server state
data RpcClientSideState =
    -- | The server is running. We record the server's unconsumed output.
    RpcRunning
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
forkRpcServer :: FilePath        -- ^ Filename of the executable
              -> [String]        -- ^ Arguments
              -> Maybe FilePath  -- ^ Working directory
              -> IO RpcServer
forkRpcServer path args workingDir = do
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


  fullPath <- pathToExecutable path
  (Nothing, Nothing, Nothing, ph) <- createProcess (proc fullPath args') {
                                         cwd = workingDir
                                       }

  -- Close the ends of the pipes that we're not using, and convert the rest
  -- to handles
  closeFd requestR
  closeFd responseW
  closeFd errorsW
  requestW'  <- fdToHandle requestW
  responseR' <- fdToHandle responseR
  errorsR'   <- fdToHandle errorsR

  st     <- newMVar RpcRunning
  input  <- BSL.hGetContents responseR'
  parser <- newStreamParser json' input
  return RpcServer {
      rpcRequestW  = requestW'
    , rpcErrorsR   = errorsR'
    , rpcProc      = ph
    , rpcState     = st
    , rpcParser    = parser
    }
  where
    pathToExecutable :: FilePath -> IO FilePath
    pathToExecutable relPath = do
      fullPath    <- canonicalizePath relPath
      permissions <- getPermissions fullPath
      if executable permissions
        then return fullPath
        else Ex.throwIO . userError $ relPath ++ " not executable"

-- | Specialized form of 'rpcConversation' to do single request and wait for
-- a single response.
rpc :: (ToJSON req, FromJSON resp) => RpcServer -> req -> IO resp
rpc server req = rpcConversation server $ \RpcConversation{..} -> put req >> get

-- | Run an RPC conversation. If the handler throws an exception during
-- the conversation the server is terminated.
rpcConversation :: RpcServer
                -> (RpcConversation -> IO a)
                -> IO a
rpcConversation server handler = withRpcServer server $ \st ->
  case st of
    RpcRunning -> do
      -- We want to be able to detect when a conversation is used out of scope
      inScope <- newIORef True

      -- Call the handler, update the state, and return the result
      a <- handler . conversation $ do isInScope <- readIORef inScope
                                       unless isInScope $
                                         Ex.throwIO illscopedConversationException

      -- Record that the conversation is no longer in scope and return
      writeIORef inScope False
      return (RpcRunning, a)
    RpcStopped ex ->
      Ex.throwIO ex
  where
    conversation :: IO () -> RpcConversation
    conversation verifyScope = RpcConversation {
        put = \req -> do
                 verifyScope
                 mapIOToExternal server $
                   hPutFlush (rpcRequestW server) . encode . Request $ req
      , get = do verifyScope
                 value <- mapIOToExternal server $
                            nextInStream (rpcParser server)
                 case fromJSON value of
                   Success (Response resp) ->
                     return resp
                   Error err ->
                     Ex.throwIO (userError err)
      }

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

-- | Force shutdown. Don't let any thread wait until other threads terminate.
forceShutdown :: RpcServer -> IO ()
forceShutdown server = withRpcServer server $ \_ -> do
  forceTerminate server
  let ex = Ex.toException (userError "Forced manual shutdown")
  return (RpcStopped ex, ())

-- | Terminate the external process
terminate :: RpcServer -> IO ()
terminate server = do
    ignoreIOExceptions $ hPutFlush (rpcRequestW server) (encode requestShutdown)
    void $ waitForProcess (rpcProc server)
  where
    requestShutdown :: Request ()
    requestShutdown = RequestShutdown

-- | Force-terminate the external process
forceTerminate :: RpcServer -> IO ()
forceTerminate server = do
    ignoreIOExceptions $ hPutFlush (rpcRequestW server) (encode requestForceShutdown)
    void $ waitForProcess (rpcProc server)
  where
    requestForceShutdown :: Request ()
    requestForceShutdown = RequestForceShutdown

-- | Like modifyMVar, but terminate the server on exceptions
withRpcServer :: RpcServer
              -> (RpcClientSideState -> IO (RpcClientSideState, a))
              -> IO a
withRpcServer server io =
  Ex.mask $ \restore -> do
    st <- $takeMVar (rpcState server)

    mResult <- Ex.try $ restore (io st)

    case mResult of
      Right (st', a) -> do
        $putMVar (rpcState server) st'
        return a
      Left ex -> do
   --     terminate server
        $putMVar (rpcState server) (RpcStopped ex)
        Ex.throwIO ex

-- | Get the exit code of the RPC server, unless still running.
getRpcExitCode :: RpcServer -> IO (Maybe ExitCode)
getRpcExitCode RpcServer{rpcProc} = getProcessExitCode rpcProc

--------------------------------------------------------------------------------
-- Internal                                                                   --
--------------------------------------------------------------------------------

-- | Decode messages from a handle and forward them to a channel.
-- The boolean result indicates whether the shutdown is forced.
readRequests :: Handle -> Chan Value -> IO ()
readRequests h ch = do
    input  <- BSL.hGetContents h
    parser <- newStreamParser (parseJSON json') input
    go parser
  where
    go :: StreamParser (Request Value) -> IO ()
    go parser = do
      req <- nextInStream parser
      case req of
        Request req'         -> writeChan ch req' >> go parser
        RequestShutdown      -> return ()
        RequestForceShutdown -> exitImmediately (ExitFailure 1)

-- | Encode messages from a channel and forward them on a handle
writeResponses :: Chan Value -> Handle -> IO ()
writeResponses ch h = forever $ $readChan ch >>= hPutFlush h . encode . Response

-- | Run a handler repeatedly, given input and output channels
channelHandler :: Chan Value
               -> Chan Value
               -> (RpcConversation -> IO ())
               -> IO ()
channelHandler requests responses server =
  server RpcConversation {
      get = do value <- $readChan requests
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
  merr <- BSL.unpack <$> BSL.hGetContents (rpcErrorsR server)
  if null merr
    then Ex.throwIO (serverKilledException (Just ex))
    else Ex.throwIO (ExternalException merr (Just ex))

-- | Write a bytestring to a buffer and flush
hPutFlush :: Handle -> BSL.ByteString -> IO ()
hPutFlush h bs = BSL.hPut h bs >> hFlush h

-- | Ignore IO exceptions
ignoreIOExceptions :: IO () -> IO ()
ignoreIOExceptions = Ex.handle ignore
  where
    ignore :: Ex.IOException -> IO ()
    ignore _ = return ()

--------------------------------------------------------------------------------
-- Wrapper around Attoparsec                                                  --
--------------------------------------------------------------------------------

data StreamParser a = StreamParser {
    streamParser    :: Attoparsec.Parser a
  , streamRemainder :: IORef BSL.ByteString
  }

newStreamParser :: Attoparsec.Parser a -> BSL.ByteString -> IO (StreamParser a)
newStreamParser streamParser streamSource = do
  streamRemainder <- newIORef streamSource
  return StreamParser{..}

nextInStream :: forall a. StreamParser a -> IO a
nextInStream StreamParser{..} = do
    bs <- readIORef streamRemainder
    case streamParser `parse` bs of
      Fail _ _ err -> Ex.throwIO (userError err)
      Done bs' r   -> writeIORef streamRemainder bs' >> return r

parseJSON :: FromJSON a => Attoparsec.Parser Value -> Attoparsec.Parser a
parseJSON pvalue = do
  val <- pvalue
  case fromJSON val of
    Success r -> return r
    Error err -> fail err
