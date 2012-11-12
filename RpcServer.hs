{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module RpcServer where

import System.IO (Handle, hSetBinaryMode, stderr, hPrint)
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
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import qualified Data.ByteString.Lazy as BSL (ByteString, hPut, hGetContents)
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString.Lazy (parse, Result(Done, Fail))

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

data RpcServer req resp

-- | Fork an RPC server as a separate process
forkRpcServer :: FilePath -> IO (RpcServer req resp)
forkRpcServer = undefined

-- | Do an RPC call
rpc :: (FromJSON req, ToJSON resp)
    => RpcServer req resp  -- ^ RPC server
    -> req                 -- ^ Request
    -> IO resp             -- ^ Response
rpc = undefined

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
      case parse parser contents of
        Done contents' req -> do
          writeChan ch req
          go contents'
        Fail _ _ err ->
          -- Exception will be caught by 'rpcServer'
          Ex.throwIO (userError err)
    
    parser :: Parser req
    parser = do
      value <- json'
      case fromJSON value of
        Success req -> return req
        Error err   -> fail err
          
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

main :: IO ()
main = putStrLn "Hi"
