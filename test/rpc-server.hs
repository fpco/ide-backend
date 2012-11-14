{-# LANGUAGE TemplateHaskell #-}
module Main where

import System.IO
  ( stdin
  , stdout
  , stderr
  , hSetBuffering
  , BufferMode(LineBuffering)
  )
import Data.Aeson.TH (deriveJSON)
import qualified Control.Exception as Ex
import Control.Concurrent.MVar
  ( MVar
  , newMVar
  , readMVar
  , modifyMVar
  )

import RpcServer
import System.Environment (getArgs)

-- getExecutablePath is in base only for >= 4.6
import System.Environment.Executable (getExecutablePath)

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
      prog   <- getExecutablePath
      server <- forkRpcServer prog ["--server"] :: IO CountServer
      rpc server Get >>= print
      rpc server Increment >>= print
      rpc server Increment >>= print
      rpc server Get >>= print
      rpc server Reset >>= print
      rpc server Get >>= print
      Ex.catch (rpc server Crash >>= print) (\ex -> print (ex :: Ex.SomeException))
      Ex.catch (rpc server Get >>= print) (\ex -> print (ex :: Ex.SomeException))

      server2 <- forkRpcServer prog ["--server"] :: IO CountServer
      rpc server2 Get >>= print
      shutdown server2
      Ex.catch (rpc server2 Get >>= print) (\ex -> print (ex :: Ex.SomeException))

      server3 <- forkRpcServer prog ["--server"] :: IO CountServer
      rpcWithProgressCallback server3 (CountFor 5) print >>= print
