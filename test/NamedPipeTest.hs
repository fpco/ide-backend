module Main where

import Control.Concurrent
import Data.Binary
import Data.Typeable
import Data.IORef
import System.Directory
import System.IO
import System.Posix
import qualified Control.Exception             as Ex
import qualified Data.Binary                   as Binary
import qualified Data.Binary.Get               as Binary
import qualified Data.ByteString               as BSS
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.ByteString.Lazy.Internal as BSL

{-------------------------------------------------------------------------------
  Types of the messages sent from client to server and back
-------------------------------------------------------------------------------}

data Request = Echo String
             | Inc Int
             | Bye
  deriving Show

data Response = String String
              | Int Int
              | Seeya
  deriving Show

instance Binary Request where
  put (Echo s) = putWord8 0 >> put s
  put (Inc i)  = putWord8 1 >> put i
  put Bye      = putWord8 2

  get = do
    header <- getWord8
    case header of
      0 -> Echo `fmap` get
      1 -> Inc  `fmap` get
      2 -> return Bye
      _ -> fail "Request.get: Invalid header"

instance Binary Response where
  put (String s) = putWord8 0 >> put s
  put (Int i)    = putWord8 1 >> put i
  put Seeya      = putWord8 2

  get = do
    header <- getWord8
    case header of
      0 -> String `fmap` get
      1 -> Int    `fmap` get
      2 -> return Seeya
      _ -> fail "Response.get: Invalid header"

{-------------------------------------------------------------------------------
  Client and server proper

  NOTE: The server and client _synchronize_ on these pipes, so it is important
  that they must open them in the _same_ order or we will get deadlock.
-------------------------------------------------------------------------------}

server :: IO ()
server = labelExceptions "server: " $
  Ex.bracket (openPipeForReading reqPipePath  10000000) hClose $ \hReq ->
  Ex.bracket (openPipeForWriting respPipePath 10000000) hClose $ \hResp -> do
    reqStream <- newStream "server got chunk: " hReq

    let go = do req <- nextInStream reqStream
                case req of
                  Echo s -> do hPutFlush hResp $ encode (String s)
                               go
                  Inc  i -> do hPutFlush hResp $ encode (Int (i + 1))
                               go
                  Bye    -> hPutFlush hResp $ encode Seeya

    go

client :: IO ()
client = labelExceptions "client: " $
  Ex.bracket (openPipeForWriting reqPipePath  10000000) hClose $ \hReq ->
  Ex.bracket (openPipeForReading respPipePath 10000000) hClose $ \hResp -> do
    respStream <- newStream "client got chunk: " hResp

    hPutFlush hReq $ encode (Echo "Hi")
    print =<< (nextInStream respStream :: IO Response)
    hPutFlush hReq $ encode (Inc 1)
    print =<< (nextInStream respStream :: IO Response)
    hPutFlush hReq $ encode Bye
    print =<< (nextInStream respStream :: IO Response)

    return ()

{-------------------------------------------------------------------------------
  Main application driver
-------------------------------------------------------------------------------}

main :: IO ()
main =
  Ex.bracket_ (createNamedPipe reqPipePath  0o600) (removeFile reqPipePath) $
  Ex.bracket_ (createNamedPipe respPipePath 0o600) (removeFile respPipePath) $
  Ex.bracket (forkProcess server) ((>>= print) . getProcessStatus True False) $ \_ ->
    client

reqPipePath, respPipePath :: String
reqPipePath  = "./req"
respPipePath = "./resp"

{------------------------------------------------------------------------------
  IO utils from the RPC server
------------------------------------------------------------------------------}

-- | Write a bytestring to a buffer and flush
hPutFlush :: Handle -> BSL.ByteString -> IO ()
hPutFlush h bs = BSL.hPut h bs >> ignoreIOExceptions (hFlush h)

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
            else Ex.throwIO (RPCPipeNotCreated ex)
        Right h -> do
          hPutChar h '!'
          hFlush h
          return h

    delay :: Int
    delay = 10000 -- 10 ms

data RPCPipeNotCreated = RPCPipeNotCreated Ex.IOException
    deriving Typeable
instance Ex.Exception RPCPipeNotCreated
instance Show RPCPipeNotCreated where
    show (RPCPipeNotCreated e) = "The bidirectional RPC pipe could not be opened. Exception was: " ++ show e

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
            else Ex.throwIO (RPCPipeNotCreated ex)
        Right '!' ->
          return ()
        Right c ->
          Ex.throwIO (userError $ "openPipeForReading: Unexpected " ++ show c)

    delay :: Int
    delay = 10000 -- 10 ms

{-------------------------------------------------------------------------------
  Stream abstraction (also taken from the RPC infrastructure)
-------------------------------------------------------------------------------}

data Stream a where
  Stream :: Binary a => String -> Handle -> IORef (Binary.Decoder a) -> Stream a

newStream :: Binary a => String -> Handle -> IO (Stream a)
newStream label h = do
  st <- newIORef $ Binary.runGetIncremental Binary.get
  return $ Stream label h st

nextInStream :: forall a. Stream a -> IO a
nextInStream (Stream label h st) = readIORef st >>= go
  where
    go :: Binary.Decoder a -> IO a
    go decoder = case decoder of
      Binary.Fail _ _ err -> do
        writeIORef st decoder
        Ex.throwIO (userError err)
      Binary.Partial k -> do
        mchunk <- Ex.try $ BSS.hGetSome h BSL.defaultChunkSize
        putStrLn $ label ++ show mchunk
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

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

labelExceptions :: String -> IO a -> IO a
labelExceptions label = Ex.handle aux
  where
    aux :: Ex.SomeException -> IO a
    aux e = Ex.throwIO (userError (label ++ show e))
