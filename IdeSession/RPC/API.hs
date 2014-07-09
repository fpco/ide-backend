{-# LANGUAGE DeriveDataTypeable, RankNTypes #-}
module IdeSession.RPC.API (
    -- * External exceptions
    ExternalException(..)
  , serverKilledException
    -- * Client-server communication
  , RpcConversation(..)
  , Request(..)
  , Response(..)
    -- * Lazy bytestring with incremental Binary instance
  , IncBS(..)
    -- * IO utils
  , hPutFlush
  , ignoreIOExceptions
  , openPipeForWriting
  , openPipeForReading
  ) where

import Prelude hiding (take)
import Control.Applicative ((<$>))
import Control.Concurrent (threadDelay)
import Data.Binary (Binary)
import Data.Typeable (Typeable)
import System.IO (Handle, hFlush, openFile, IOMode(..), hPutChar, hGetChar)
import qualified Control.Exception as Ex
import qualified Data.Binary as Binary
import qualified Data.ByteString as BSS
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Lazy.Internal as BSL

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

{------------------------------------------------------------------------------
  Client-server communication
------------------------------------------------------------------------------}

data RpcConversation = RpcConversation {
    get :: forall a. (Typeable a, Binary a) => IO a
  , put :: forall a. (Typeable a, Binary a) => a -> IO ()
  }

data Request = Request IncBS | RequestShutdown
  deriving Show

newtype Response = Response IncBS

instance Binary Request where
  put (Request bs)         = Binary.putWord8 0 >> Binary.put bs
  put RequestShutdown      = Binary.putWord8 1

  get = do
    header <- Binary.getWord8
    case header of
      0 -> Request <$> Binary.get
      1 -> return RequestShutdown
      _ -> fail "Request.get: invalid header"

instance Binary Response where
  put (Response bs) = Binary.put bs
  get = Response <$> Binary.get

{------------------------------------------------------------------------------
  Lazy bytestring with an incremental Binary instance

  Note only does this avoid loading the entire ByteString into memory when
  serializing stuff, the standard Binary instance for Lazy bytestring is
  actually broken in 0.5 (http://hpaste.org/87401; fixed in 0.7, but even there
  still requires the length of the bytestring upfront).
------------------------------------------------------------------------------}

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

{------------------------------------------------------------------------------
  Some IO utils
------------------------------------------------------------------------------}

-- | Write a bytestring to a buffer and flush
hPutFlush :: Handle -> BSL.ByteString -> IO ()
hPutFlush h bs = BSL.hPut h bs >> hFlush h

-- | Ignore IO exceptions
ignoreIOExceptions :: IO () -> IO ()
ignoreIOExceptions = Ex.handle ignore
  where
    ignore :: Ex.IOException -> IO ()
    ignore _ = return ()

-- | Open a pipe for writing
--
-- This is meant to be used together with 'openPipeForReading'
openPipeForWriting :: FilePath -> Int -> IO Handle
openPipeForWriting fp = go
  where
    go :: Int -> IO Handle
    go timeout = do
      -- We cannot open a pipe for writing without a corresponding reader
      mh <- Ex.try $ openFile fp WriteMode
      case mh of
        Left ex ->
          if timeout > delay
            then do threadDelay delay
                    go (timeout - delay)
            else Ex.throwIO (ex :: Ex.IOException)
        Right h -> do
          hPutChar h '!'
          hFlush h
          return h

    delay :: Int
    delay = 10000 -- 10 ms

-- | Open a pipe for reading
--
-- This is meant to be used together with 'openPipeForWriting'
openPipeForReading :: FilePath -> Int -> IO Handle
openPipeForReading fp = \timeout -> do
    -- We _can_ open a pipe for reading without a corresponding writer
    h <- openFile fp ReadMode
    -- But if there is no corresponding writer, then trying to read from the
    -- pipe will report EOF. So we wait.
    go h timeout
    return h
  where
    go :: Handle -> Int -> IO ()
    go h timeout = do
      mc <- Ex.try $ hGetChar h
      case mc of
        Left ex ->
          if timeout > delay
            then do threadDelay delay
                    go h (timeout - delay)
            else Ex.throwIO (ex :: Ex.IOException)
        Right '!' ->
          return ()
        Right c ->
          Ex.throwIO (userError $ "openPipeForReading: Unexpected " ++ show c)

    delay :: Int
    delay = 10000 -- 10 ms
