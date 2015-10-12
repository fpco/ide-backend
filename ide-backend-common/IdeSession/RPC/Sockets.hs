{-# LANGUAGE CPP, StandaloneDeriving, DeriveGeneric, DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This module provies an interface for using sockets in RPC communication.
module IdeSession.RPC.Sockets (
    portToString
  , makeSocket
  , connectToPort
  , stringToPort
  , acceptHandle
  , ReadChannel(..)
  , WriteChannel(..)
) where

import System.IO (Handle)

import GHC.Generics (Generic)
import Data.Typeable (Typeable)
import Data.Binary
import Network
import Data.ByteString.Lazy.Char8
import qualified Data.ByteString.Base64.Lazy as Base64
import Network.Socket hiding (close, sClose, accept, socketPort)

#ifdef VERSION_unix
-- import Control.Exception
-- import Control.Concurrent.MVar
import System.IO.Temp (createTempDirectory)
import System.FilePath ((</>))
import System.Directory (getTemporaryDirectory)
#endif

newtype ReadChannel = ReadChannel PortID deriving (Generic, Typeable)
newtype WriteChannel = WriteChannel PortID deriving (Generic, Typeable)

instance Binary ReadChannel
instance Binary WriteChannel

-- Creating a socket with auto generated port (or file in case Unix domain
-- sockets are used)
makeSocket :: IO Socket

#ifdef VERSION_unix
makeSocket = do
  tmpDir <- getTemporaryDirectory
  rpcDir <- createTempDirectory tmpDir "ide-backend-rpc."
  let rpcFile = rpcDir </> "rpc"
  s <- listenOn $ UnixSocket rpcFile
  p <- socketPort s
  return s


#else
{- On non-Unix we use a plain socket -}
makeSocket = listenOn $ PortNumber aNY_PORT
#endif

connectToPort :: PortID -> IO Handle
connectToPort = connectTo "localhost"

acceptHandle :: Socket -> IO Handle
acceptHandle s = do
  (h, _, _) <- accept s
  return h

portToString :: PortID -> String
portToString = unpack . Base64.encode . encode

stringToPort :: String -> PortID
stringToPort = decode . Base64.decodeLenient . pack


{- Orphans -}
deriving instance Generic PortID
deriving instance Generic PortNumber
instance Binary PortID
instance Binary PortNumber
