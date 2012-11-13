{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module RpcServer 
  ( -- * Server-side
    rpcServer
    -- * Client-side
  , RpcServer
  , forkRpcServer
  , rpc 
  , rpcWithProgress
  , shutdown
    -- * Progress 
  , Progress(..)
    -- * Testing
  , main
  ) where

import System.IO 
  ( Handle
  , hSetBinaryMode
  , stdin
  , stdout
  , stderr
  , hPrint
  , hSetBuffering
  , BufferMode(LineBuffering, NoBuffering)
  )
import System.Process 
  ( CreateProcess(std_in, std_out, std_err)
  , createProcess
  , proc
  , StdStream(CreatePipe)
  , ProcessHandle
  , terminateProcess
  )
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
import Control.Concurrent (ThreadId, forkIO, killThread)
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
import Data.ByteString.Lazy (ByteString, hPut, hGetContents, hPutStr)
import Data.Attoparsec.ByteString.Lazy (parse, Result(Done, Fail))

-- For testing
import System.Environment (getArgs, getProgName)

--------------------------------------------------------------------------------
-- Simple "Future" data type                                                  --
--                                                                            --
-- (we should probably move this elsewhere eventually)                        --
--------------------------------------------------------------------------------

newtype Progress p a = Progress { 
    progressWait :: IO (Either a (p, Progress p a)) 
  }

--------------------------------------------------------------------------------
-- Internal data types                                                        --
--                                                                            --
-- We wrap the requests and responses to and from the RPC server in a custom  --
-- data type for two reasons. First, top-level JSON values must be objects or --
-- arrays; by wrapping them the user does not have to worry about this.       --
-- Second, it allows us to send control messages back and worth when we need  --
-- to.                                                                        -- 
--------------------------------------------------------------------------------

data Request  a = Request a 
data Response a = FinalResponse a | IntermediateResponse a 

$(deriveJSON id ''Request) 
$(deriveJSON id ''Response)

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
  
  let forkCatch p = forkIO $ Ex.catch p (putMVar exception) 

  tid1 <- forkCatch $ readRequests hin requests
  tid2 <- forkCatch $ writeResponses responses hout 
  tid3 <- forkCatch $ channelHandler requests responses handler 

  readMVar exception >>= hPrint herr 
  mapM_ killThread [tid1, tid2, tid3]

--------------------------------------------------------------------------------
-- Client-side API                                                            --
--------------------------------------------------------------------------------

-- | Abstract data type representing RPC servers
newtype RpcServer req resp = RpcServer (MVar (RpcState req resp))

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
  -- Start the server
  (Just hin, Just hout, Just herr, ph) <- createProcess $ (proc path args) { 
      std_in  = CreatePipe
    , std_out = CreatePipe
    , std_err = CreatePipe
    }

  -- Set to binary mode so that 'hGetContents' works
  hSetBinaryMode hin  True
  hSetBinaryMode hout True
  hSetBinaryMode herr True

  -- Make sure requests get sent to the server immediately
  hSetBuffering hin  NoBuffering
  hSetBuffering hout NoBuffering
  hSetBuffering herr LineBuffering

  -- Forward server's stderr to our stderr
  tid <- forkIO $ hGetContents herr >>= hPutStr stderr  

  -- Get server's output as a lazy bytestring
  contents <- hGetContents hout

  -- Done
  st <- newMVar RpcRunning {
      rpcIn   = hin
    , rpcOut  = contents
    , rpcErr  = tid
    , rpcProc = ph
    }
  return (RpcServer st)

-- | Do an RPC call
--
-- Throws an exception if the server returns a mallformed result or the server
-- has shut down, or if the server returns more than one message (see 
-- 'rpcWithProgress' instead)
rpc :: (ToJSON req, FromJSON resp)
    => RpcServer req resp  -- ^ RPC server
    -> req                 -- ^ Request
    -> IO resp             -- ^ Response
rpc server req = withRpcServer server $ \st ->
  case st of
    RpcRunning {} -> do
      hPut (rpcIn st) $ encode (Request req) 
      case parseJSON (rpcOut st) of
        Right (out', FinalResponse resp) -> 
          return (st { rpcOut = out' }, resp)
        Right (_, IntermediateResponse _) ->
          Ex.throwIO (userError "rpc: Unexpected intermediate response")
        Left err -> 
          Ex.throwIO (userError err)
    RpcStopped ex -> 
      Ex.throwIO (userError $ "rpc: server shutdown (" ++ show ex ++ ")")

-- | Like 'rpc', but with support for receiving multiple replies for the 
-- same request
rpcWithProgress :: (ToJSON req, FromJSON resp)
                => RpcServer req resp  -- ^ RPC server
                -> req                 -- ^ Request
                -> (Progress resp resp -> IO b) -- ^ Handler
                -> IO b
rpcWithProgress server req handler = withRpcServer server $ \st ->
  case st of
    RpcRunning {} -> do
      hPut (rpcIn st) $ encode (Request req)

      -- We create a local MVar that holds the parser state (unparsed part
      -- of the server input). This MVar is updated every time the handler
      -- waits on the Progress object, so that once the handler returns
      -- we can retrieve the final state. This has the additional advantage
      -- that once we take the MVar at the end the user will no longer be
      -- able to call the handler (that is, we can't stop the user from
      -- calling wait, but it will deadlock and not affect other RPC calls)
      outSt <- newMVar (rpcOut st)

      -- Construct the Progress object
      let go = Progress $ modifyMVar outSt $ \out -> do
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
                        -> (resp -> IO ())     -- ^ Callback for intermediate messages
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
  terminateProcess (rpcProc st)
  killThread (rpcErr st)
  return ex
terminate (RpcStopped ex') _ = 
  return ex' -- Already stopped

-- | Like modifyMVar, but terminate the server on exceptions 
withRpcServer :: RpcServer req resp 
              -> (RpcState req resp -> IO (RpcState req resp, a))
              -> IO a
withRpcServer (RpcServer server) io = 
  Ex.mask $ \restore -> do
    st <- takeMVar server
    mResult <- Ex.try $ restore (io st)
    case mResult of
      Right (st', a) -> do
        putMVar server st'
        return a
      Left ex -> do
        ex' <- terminate st ex
        putMVar server (RpcStopped ex')
        Ex.throwIO ex

--------------------------------------------------------------------------------
-- Internal                                                                   --
--------------------------------------------------------------------------------

-- | Decode messages from a handle and forward them to a channel 
readRequests :: forall req. FromJSON req => Handle -> Chan (Request req) -> IO ()
readRequests h ch = do 
    hSetBinaryMode h True
    hSetBuffering h NoBuffering
    hGetContents h >>= go
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
writeResponses ch h = do
  hSetBinaryMode h True
  hSetBuffering h NoBuffering
  forever $ readChan ch >>= hPut h . encode 

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

--------------------------------------------------------------------------------
-- For testing                                                                --
--------------------------------------------------------------------------------

data CountRequest  = Increment | Reset | Get | Crash | CountFor Int deriving Show
data CountResponse = Ok | Count Int deriving Show

type CountServer = RpcServer CountRequest CountResponse

$(deriveJSON id ''CountRequest)
$(deriveJSON id ''CountResponse)

countServer :: MVar Int -> CountRequest -> IO (Progress CountResponse CountResponse)
countServer st Increment = return $ Progress $ 
  modifyMVar st $ \i -> return (i + 1, Left Ok)
countServer st Reset = return $ Progress $ 
  modifyMVar st $ \_ -> return (0, Left Ok)
countServer st Get = return $ Progress $ do
  i <- readMVar st
  return (Left (Count i))
countServer _  Crash = return $ Progress $ 
  Ex.throwIO (userError "Server crashed!")
countServer st (CountFor n) = do
    leftToCount <- newMVar n
    return (p leftToCount)
  where
    p leftToCount = Progress $ do 
      shouldCount <- modifyMVar leftToCount $ \m -> return (m - 1, m > 0)
      if shouldCount 
        then modifyMVar st $ \i -> return (i + 1, Right (Count (i + 1), p leftToCount))
        else return (Left Ok)

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  args <- getArgs
  case args of
    ["--server"] -> do
      st <- newMVar 0
      rpcServer stdin stdout stderr (countServer st) 
    _ -> do
      prog   <- getProgName 
      server <- forkRpcServer ("./" ++ prog) ["--server"] :: IO CountServer 
      rpc server Get >>= print 
      rpc server Increment >>= print 
      rpc server Increment >>= print 
      rpc server Get >>= print 
      rpc server Reset >>= print 
      rpc server Get >>= print 
      Ex.catch (rpc server Crash >>= print) (\ex -> print (ex :: Ex.SomeException))
      Ex.catch (rpc server Get >>= print) (\ex -> print (ex :: Ex.SomeException))

      server2 <- forkRpcServer ("./" ++ prog) ["--server"] :: IO CountServer 
      rpc server2 Get >>= print 
      shutdown server2
      Ex.catch (rpc server2 Get >>= print) (\ex -> print (ex :: Ex.SomeException))

      server3 <- forkRpcServer ("./" ++ prog) ["--server"] :: IO CountServer 
      rpcWithProgressCallback server3 (CountFor 5) print >>= print
