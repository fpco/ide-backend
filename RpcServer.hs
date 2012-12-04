module RpcServer where

import Control.Concurrent

type RpcServer req resp = (ThreadId, Chan req, Chan resp)

data RpcServerActions req prog resp = RpcServerActions {
    getRequest  :: IO req
  , putProgress :: prog -> IO ()
  , putResponse :: resp -> IO ()
  }

forkRpcServer :: (RpcServerActions req resp resp -> IO ()) -> IO (RpcServer req resp)
forkRpcServer server = do
  req  <- newChan
  resp <- newChan
  let getRequest  = readChan req
      putProgress = error "putProgress not implementd"
      putResponse = writeChan resp
  tid <- forkIO $ server RpcServerActions{..}
  return (tid, req, resp)

rpc :: RpcServer req resp  -- ^ RPC server
    -> req                 -- ^ Request
    -> IO resp             -- ^ Response
rpc (_, req, resp) request = do
  writeChan req request
  readChan resp

shutdown :: RpcServer req resp -> IO ()
shutdown (tid, _, _) = killThread tid
