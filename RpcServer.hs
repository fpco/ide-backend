{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module RpcServer 
  ( -- * Server-side
    rpcServer
    -- * Client-side
  , RpcServer
  , forkRpcServer
  , rpc 
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
import qualified Control.Exception as Ex (SomeException, throwIO, catch)
import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar 
  ( MVar
  , newMVar
  , newEmptyMVar
  , putMVar
  , readMVar
  , modifyMVar 
  )
import Data.ByteString.Lazy (ByteString, hPut, hGetContents, hPutStr)
import Data.Attoparsec.ByteString.Lazy (parse, Result(Done, Fail))

-- For testing
import System.Environment (getArgs, getProgName)

--------------------------------------------------------------------------------
-- Internal data types                                                        --
--                                                                            --
-- We wrap the requests and responses to and from the RPC server in a custom  --
-- data type for two reasons. First, top-level JSON values must be objects or --
-- arrays; by wrapping them the user does not have to worry about this.       --
-- Second, it allows us to send control messages back and worth when we need  --
-- to.                                                                        -- 
--------------------------------------------------------------------------------

data Request  a = Request  { request  :: a }
data Response a = Response { response :: a }

$(deriveJSON id ''Request) 
$(deriveJSON id ''Response)

--------------------------------------------------------------------------------
-- Server-side API                                                            --
--------------------------------------------------------------------------------

-- | Start the RPC server
rpcServer :: (FromJSON req, ToJSON resp) 
          => Handle           -- ^ Input
          -> Handle           -- ^ Output
          -> (req -> IO resp) -- ^ The request handler 
          -> IO ()
rpcServer inp outp handler = do
  requests  <- newChan
  responses <- newChan
  exception <- newEmptyMVar :: IO (MVar Ex.SomeException)
  
  let forkCatch p = forkIO $ Ex.catch p (putMVar exception) 

  tid1 <- forkCatch $ readRequests inp requests
  tid2 <- forkCatch $ writeResponses responses outp
  tid3 <- forkCatch $ channelHandler requests responses handler 

  readMVar exception >>= hPrint stderr
  mapM_ killThread [tid1, tid2, tid3]

--------------------------------------------------------------------------------
-- Client-side API                                                            --
--------------------------------------------------------------------------------

-- | Abstract data type representing RPC servers
data RpcServer req resp = RpcServer {
    -- | The server's 'stdin'
    rpcIn :: Handle               
    -- | The server's 'stdout' 
    --
    -- This represents the parser state, hence we represent it as an MVar
    -- around a lazy bytestring (the MVar doubles as the lock for the server,
    -- sequentializing concurrent requests)
  , rpcOut :: MVar ByteString  
    -- | Thread that forwards the server's stderr to our stderr
  , rpcErr :: ThreadId
    -- | Handle on the server process itself
  , rpcProc :: ProcessHandle
  }

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
  contents     <- hGetContents hout
  contentsMVar <- newMVar contents

  -- Done
  return RpcServer {
      rpcIn   = hin
    , rpcOut  = contentsMVar
    , rpcErr  = tid
    , rpcProc = ph
    }

-- | Do an RPC call
--
-- Throws an exception if the server returns a mallformed result
--
-- TODO: timeout?
rpc :: (ToJSON req, FromJSON resp)
    => RpcServer req resp  -- ^ RPC server
    -> req                 -- ^ Request
    -> IO resp             -- ^ Response
rpc server req = modifyMVar (rpcOut server) $ \out -> do
  hPut (rpcIn server) $ encode Request { request = req }
  case parseJSON out of
    Right (out', Response resp) -> return (out', resp)
    Left err                    -> Ex.throwIO (userError err)

--------------------------------------------------------------------------------
-- Internal                                                                   --
--------------------------------------------------------------------------------

-- | Decode messages from a handle and forward them to a channel 
readRequests :: forall req. FromJSON req => Handle -> Chan req -> IO ()
readRequests h ch = do 
    hSetBinaryMode h True
    hSetBuffering h NoBuffering
    hGetContents h >>= go
  where
    go :: ByteString -> IO ()
    go contents = 
      case parseJSON contents of
        Right (contents', Request req) -> writeChan ch req >> go contents'
        Left err                       -> Ex.throwIO (userError err)
   
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
writeResponses :: ToJSON resp => Chan resp -> Handle -> IO ()
writeResponses ch h = do
  hSetBinaryMode h True
  hSetBuffering h NoBuffering
  forever $ do
    resp <- readChan ch 
    hPut h $ encode Response { response = resp }

-- | Run a handler repeatedly, given input and output channels
channelHandler :: Chan req -> Chan resp -> (req -> IO resp) -> IO ()
channelHandler inp outp handler = forever $ do
  req  <- readChan inp
  resp <- handler req
  writeChan outp resp

--------------------------------------------------------------------------------
-- For testing                                                                --
--------------------------------------------------------------------------------

data CountRequest  = Increment | Reset | Get | Crash deriving Show
data CountResponse = Ok | Count Int deriving Show

type CountServer = RpcServer CountRequest CountResponse

$(deriveJSON id ''CountRequest)
$(deriveJSON id ''CountResponse)

countServer :: MVar Int -> CountRequest -> IO CountResponse
countServer st Increment = modifyMVar st $ \i -> return (i + 1, Ok)
countServer st Reset     = modifyMVar st $ \_ -> return (0, Ok)
countServer st Get       = readMVar st >>= return . Count
countServer _  Crash     = Ex.throwIO (userError "Server crashed!")

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  args <- getArgs
  case args of
    ["--server"] -> do
      st <- newMVar 0
      rpcServer stdin stdout (countServer st) 
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
      rpc server Get >>= print 
