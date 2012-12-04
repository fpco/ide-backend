module RpcServer where

import Control.Concurrent
import Progress

type RpcServer req resp = (ThreadId, Chan req, Chan (Either resp resp))

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
      putProgress = writeChan resp . Left
      putResponse = writeChan resp . Right
  tid <- forkIO $ server RpcServerActions{..}
  return (tid, req, resp)

rpcWithProgress :: RpcServer req resp -> req -> (Progress resp resp -> IO b) -> IO b
rpcWithProgress (_, req, resp) request handler = do
  writeChan req request
  let progress = Progress $ do
        response <- readChan resp
        case response of
          Left intermediate -> return . Right $ (intermediate, progress)
          Right final       -> return . Left $ final
  handler progress

shutdown :: RpcServer req resp -> IO ()
shutdown (tid, _, _) = killThread tid
