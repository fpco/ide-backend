{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, DeriveDataTypeable, RankNTypes, GADTs #-}
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
  , BufferMode(BlockBuffering)
  , hFlush
  )
import System.Process
  ( createProcess
  , proc
  , ProcessHandle
  , waitForProcess
  , CreateProcess(cwd, env)
  , getProcessExitCode
  )
import System.Exit (ExitCode)
import System.Posix.Types (Fd)
import System.Posix.IO (createPipe, closeFd, fdToHandle)
import System.Posix.Process (exitImmediately)
import System.Exit (ExitCode(ExitFailure))
import System.Directory (canonicalizePath, getPermissions, executable)
import Data.Typeable (Typeable)
import Control.Applicative ((<$>))
import Control.Monad (void, forever, unless)
import qualified Control.Exception as Ex
import Control.Concurrent (threadDelay)
import Control.Concurrent.Chan (Chan, newChan, writeChan)
import Control.Concurrent.MVar (MVar, newMVar)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.Internal as BSL
import qualified Data.ByteString as BSS
import Data.IORef (IORef, writeIORef, readIORef, newIORef)
import Control.Concurrent.Async (async)
import Data.Binary (Binary, encode, decode)
import qualified Data.Binary     as Binary
import qualified Data.Binary.Get as Binary

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

-------------------------------------------------------------------------------
-- Lazy bytestring with an incremental Binary instance                       --
--                                                                           --
-- Note only does this avoid loading the entire ByteString into memory when  --
-- serializing stuff, the standard Binary instance for Lazy bytestring is    --
-- actually broken in 0.5 (http://hpaste.org/87401; fixed in 0.7, but even   --
-- there still requires the length of the bytestring upfront).               --
-------------------------------------------------------------------------------

newtype IncBS = IncBS { unIncBS :: BSL.ByteString }

instance Binary IncBS where
  put (IncBS BSL.Empty)        = Binary.putWord8 0
  put (IncBS (BSL.Chunk b bs)) = do Binary.putWord8 1
                                    Binary.put b
                                    Binary.put (IncBS bs)

  get = go []
    where
      go :: [BSS.ByteString] -> Binary.Get IncBS
      go acc = do
        header <- Binary.getWord8
        case header of
          0 -> return . IncBS . BSL.fromChunks . reverse $ acc
          1 -> do b <- Binary.get ; go (b : acc)
          _ -> fail "IncBS.get: invalid header"

instance Show IncBS where
  show = show . unIncBS

-------------------------------------------------------------------------------
-- Internal data types                                                       --
-------------------------------------------------------------------------------

-- TODO: Might want to use a wrapper around BSL.Bytestring which provides
-- incremental framing (i.e., doesn't require the length of the bytestring
-- upfront)

data Request  = Request IncBS
              | RequestShutdown
              | RequestForceShutdown
  deriving Show

newtype Response = Response IncBS

instance Binary Request where
  put (Request bs)         = Binary.putWord8 0 >> Binary.put bs
  put RequestShutdown      = Binary.putWord8 1
  put RequestForceShutdown = Binary.putWord8 2

  get = do
    header <- Binary.getWord8
    case header of
      0 -> Request <$> Binary.get
      1 -> return RequestShutdown
      2 -> return RequestForceShutdown
      _ -> fail "Request.get: invalid header"

instance Binary Response where
  put (Response bs) = Binary.put bs
  get = Response <$> Binary.get

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
    get :: forall a. Binary a => IO a
  , put :: forall a. Binary a => a -> IO ()
  }

-- | Start the RPC server
rpcServer' :: Handle                     -- ^ Input
           -> Handle                     -- ^ Output
           -> Handle                     -- ^ Errors
           -> (RpcConversation -> IO ()) -- ^ The request server
           -> IO ()
rpcServer' hin hout herr server = do
    requests   <- newChan :: IO (Chan BSL.ByteString)
    responses  <- newChan :: IO (Chan BSL.ByteString)

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
    -- | IORef containing the server response stream
  , rpcResponseR :: Stream Response
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
              -> Maybe [(String, String)] -- ^ Environment
              -> IO RpcServer
forkRpcServer path args workingDir menv = do
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
                                         cwd = workingDir,
                                         env = menv
                                       }

  -- Close the ends of the pipes that we're not using, and convert the rest
  -- to handles
  closeFd requestR
  closeFd responseW
  closeFd errorsW
  requestW'  <- fdToHandle requestW
  responseR' <- fdToHandle responseR
  errorsR'   <- fdToHandle errorsR

  st    <- newMVar RpcRunning
  input <- newStream responseR'
  return RpcServer {
      rpcRequestW  = requestW'
    , rpcErrorsR   = errorsR'
    , rpcProc      = ph
    , rpcState     = st
    , rpcResponseR = input
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
rpc :: (Binary req, Binary resp) => RpcServer -> req -> IO resp
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
                 mapIOToExternal server $ do
                   let msg = encode $ Request (IncBS $ encode req)
                   hPutFlush (rpcRequestW server) msg
      , get = do verifyScope
                 mapIOToExternal server $ do
                   Response resp <- nextInStream (rpcResponseR server)
                   Ex.evaluate $ decode (unIncBS resp)
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
    ignoreIOExceptions $ hPutFlush (rpcRequestW server) (encode RequestShutdown)
    void $ waitForProcess (rpcProc server)

-- | Force-terminate the external process
forceTerminate :: RpcServer -> IO ()
forceTerminate server = do
    ignoreIOExceptions $ hPutFlush (rpcRequestW server) (encode RequestForceShutdown)
    void $ waitForProcess (rpcProc server)

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
writeResponses :: Chan BSL.ByteString -> Handle -> IO ()
writeResponses ch h = forever $ do
  bs <- $readChan ch
  hPutFlush h $ encode (Response (IncBS bs))

-- | Run a handler repeatedly, given input and output channels
channelHandler :: Chan BSL.ByteString
               -> Chan BSL.ByteString
               -> (RpcConversation -> IO ())
               -> IO ()
channelHandler requests responses server =
  server RpcConversation {
      get = $readChan requests >>= Ex.evaluate . decode
    , put = writeChan responses . encode
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
-- Wrapper around Binary                                                      --
--------------------------------------------------------------------------------

data Stream a where
  Stream :: Binary a => Handle -> IORef (Binary.Decoder a) -> Stream a

newStream :: Binary a => Handle -> IO (Stream a)
newStream h = do
  st <- newIORef $ Binary.runGetIncremental Binary.get
  return $ Stream h st

nextInStream :: forall a. Stream a -> IO a
nextInStream (Stream h st) = readIORef st >>= go
  where
    go :: Binary.Decoder a -> IO a
    go decoder = case decoder of
      Binary.Fail _ _ err -> do
        writeIORef st decoder
        Ex.throwIO (userError err)
      Binary.Partial k -> do
        mchunk <- Ex.try $ BSS.hGetSome h BSL.defaultChunkSize
        case mchunk of
          Left ex -> do writeIORef st decoder
                        Ex.throwIO (ex :: Ex.SomeException)
          Right chunk | BSS.null chunk -> go . k $ Nothing
                      | otherwise      -> go . k $ Just chunk
      Binary.Done unused _numConsumed a -> do
        writeIORef st $ contDecoder unused
        return a

    contDecoder :: BSS.ByteString -> Binary.Decoder a
    contDecoder = Binary.pushChunk (Binary.runGetIncremental Binary.get)
