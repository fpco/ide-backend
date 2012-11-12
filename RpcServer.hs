{-# LANGUAGE ScopedTypeVariables #-}
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

import System.IO (Handle, hSetBinaryMode, stdin, stdout, stderr, hPrint)
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
import Control.Monad (forever)
import qualified Control.Exception as Ex (SomeException, throwIO, catch)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar 
  ( MVar
  , newMVar
  , newEmptyMVar
  , putMVar
  , readMVar
  , modifyMVar 
  )
import qualified Data.ByteString.Lazy as BSL (ByteString, hPut, hGetContents)
import Data.Attoparsec.ByteString.Lazy (parse, Result(Done, Fail))

-- For testing
import System.Environment (getArgs, getProgName)

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

  _ <- forkCatch $ readRequests inp requests
  _ <- forkCatch $ writeResponses responses outp
  _ <- forkCatch $ channelHandler requests responses handler 

  readMVar exception >>= hPrint stderr

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
  , rpcOut :: MVar BSL.ByteString  
    -- | The server's 'stderr'
  , rpcErr :: Handle              
    -- | Handle on the server process itself
  , rpcProc :: ProcessHandle
  }

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
  contents     <- BSL.hGetContents hout
  contentsMVar <- newMVar contents
  return (RpcServer hin contentsMVar herr ph)

-- | Do an RPC call
--
-- Throws an exception if the server returns a mallformed result
rpc :: (ToJSON req, FromJSON resp)
    => RpcServer req resp  -- ^ RPC server
    -> req                 -- ^ Request
    -> IO resp             -- ^ Response
rpc server req = modifyMVar (rpcOut server) $ \out -> do
  BSL.hPut (rpcIn server) (encode req)
  case parseJSON out of
    Right (out', resp) -> return (out', resp)
    Left err           -> Ex.throwIO (userError err)

--------------------------------------------------------------------------------
-- Internal                                                                   --
--------------------------------------------------------------------------------

-- | Decode messages from a handle and forward them to a channel 
readRequests :: forall req. FromJSON req => Handle -> Chan req -> IO ()
readRequests h ch = do 
    hSetBinaryMode h True
    contents <- BSL.hGetContents h
    go contents
  where
    go :: BSL.ByteString -> IO ()
    go contents = 
      case parseJSON contents of
        Right (contents', req) -> do
          writeChan ch req
          go contents'
        Left err ->
          -- Exception will be caught by 'rpcServer'
          Ex.throwIO (userError err)
    
parseJSON :: FromJSON a => BSL.ByteString -> Either String (BSL.ByteString, a) 
parseJSON bs =
  case parse json' bs of
    Fail _ _ err -> Left err
    Done bs' value -> 
      case fromJSON value of
        Success req -> Right (bs', req)
        Error err   -> Left err
    
-- | Encode messages from a channel and forward them on a handle
writeResponses :: ToJSON resp => Chan resp -> Handle -> IO ()
writeResponses ch h = forever $ readChan ch >>= BSL.hPut h .  encode 

-- | Run a handler repeatedly, given input and output channels
channelHandler :: Chan req -> Chan resp -> (req -> IO resp) -> IO ()
channelHandler inp outp handler = forever $ do
  req  <- readChan inp
  resp <- handler req
  writeChan outp resp

--------------------------------------------------------------------------------
-- For testing                                                                --
--------------------------------------------------------------------------------

echoServer :: String -> IO String
echoServer = return

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--server"] -> rpcServer stdin stdout echoServer   
    _ -> do
      prog <- getProgName 
      server <- forkRpcServer prog []
      resp   <- rpc server "Hello server"
      putStrLn resp
