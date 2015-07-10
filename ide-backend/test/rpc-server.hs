{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, ExistentialQuantification, DeriveDataTypeable #-}
-- | Test suite for the RPC infrastructure
module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Monad (forM_, forever, replicateM, replicateM_)
import Data.Binary (Binary)
import Data.Function (on)
import Data.List (isInfixOf)
import Data.Typeable (Typeable)
import System.Environment (getArgs)
import System.Environment.Executable (getExecutablePath)
import System.FilePath ((</>))
import System.IO (hClose)
import System.IO.Temp (withTempDirectory, openTempFile)
-- import System.Posix.Signals (sigKILL)
import qualified Control.Concurrent.Async as Async
import qualified Control.Exception        as Ex
import qualified Data.Binary              as Binary
import qualified System.Directory         as Dir

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual)

import Network
import IdeSession.RPC.Client
import IdeSession.RPC.Server
import IdeSession.RPC.Sockets
import TestTools

--------------------------------------------------------------------------------
-- RPC-specific auxiliary                                                     --
--------------------------------------------------------------------------------

-- | Call the current executable and pass the right arguments
forkTestServer :: String -> IO RpcServer
forkTestServer test = do
  prog <- getExecutablePath
  forkRpcServer prog ["--server", test] Nothing Nothing

-- | Do an RPC call and verify the result
assertRpcEqual :: (Typeable req, Typeable resp, Binary req, Binary resp, Show req, Show resp, Eq resp)
               => RpcServer -- ^ RPC server
               -> req       -- ^ Request
               -> resp      -- ^ Expected response
               -> Assertion
assertRpcEqual server req resp =
  assertRpcEquals server req [resp]

-- | Like 'assertRpcEqual' but verify a number of responses, before throwing
-- the specified exception (if any)
assertRpcEquals :: (Typeable req, Typeable resp, Binary req, Binary resp, Show req, Show resp, Eq resp)
                => RpcServer -- ^ RPC server
                -> req       -- ^ Request
                -> [resp]    -- ^ Expected responses
                -> Assertion
assertRpcEquals server req resps =
  assertRpc server (Put req $ foldr Get Done resps)

data Conversation =
    forall req.  (Show req,  Typeable req,  Binary req) => Put req  Conversation
  | forall resp. (Show resp, Typeable resp, Binary resp, Eq resp) => Get resp Conversation
  | Done

assertRpc :: RpcServer -> Conversation -> Assertion
assertRpc server conversation = do
  rpcConversation server $ \RpcConversation{..} ->
    let checkConversation Done =
          return ()
        checkConversation (Put req conv) = do
          put req
          checkConversation conv
        checkConversation (Get resp conv) = do
          assertEqual "" resp =<< get
          checkConversation conv
    in checkConversation conversation

isServerKilledException :: ExternalException -> Bool
isServerKilledException =
  ((==) `on` externalStdErr) (serverKilledException undefined)

isServerIOException :: String -> ExternalException -> Bool
isServerIOException ex ExternalException{externalStdErr} =
  ex `isInfixOf` externalStdErr

--------------------------------------------------------------------------------
-- Feature tests                                                              --
--------------------------------------------------------------------------------

-- | Simple echo server
testEcho :: RpcServer -> Assertion
testEcho server = assertRpcEqual server "ping" "ping"

testEchoServer :: FilePath -> RpcConversation -> IO ()
testEchoServer _errorLog RpcConversation{..} = forever $ do
  req <- get :: IO String
  put req

-- | Test stateful server
testState :: RpcServer -> Assertion
testState server = forM_ ([0 .. 9] :: [Int]) $ assertRpcEqual server ()

testStateServer :: MVar Int -> FilePath -> RpcConversation -> IO ()
testStateServer st _errorLog RpcConversation{..} = forever $ do
  () <- get
  modifyMVar_ st $ \i -> do
    put i
    return (i + 1)

-- | Test with request and response custom data types
data CountRequest  = Increment | GetCount
  deriving (Show, Typeable)
data CountResponse = DoneCounting | Count Int
  deriving (Eq, Show, Typeable)

instance Binary CountRequest where
  put Increment = Binary.putWord8 0
  put GetCount  = Binary.putWord8 1

  get = do
    header <- Binary.getWord8
    case header of
      0 -> return Increment
      1 -> return GetCount
      _ -> fail "CountRequest.get: invalid header"

instance Binary CountResponse where
  put DoneCounting = Binary.putWord8 0
  put (Count i)    = Binary.putWord8 1 >> Binary.put i

  get = do
    header <- Binary.getWord8
    case header of
      0 -> return DoneCounting
      1 -> Count <$> Binary.get
      _ -> fail "CountResponse.get: invalid header"

testCustom :: RpcServer -> Assertion
testCustom server = do
  assertRpcEqual server GetCount (Count 0)
  assertRpcEqual server Increment DoneCounting
  assertRpcEqual server GetCount (Count 1)

testCustomServer :: MVar Int -> FilePath -> RpcConversation -> IO ()
testCustomServer st _errorLog RpcConversation{..} = forever $ do
  req <- get
  case req of
    Increment -> modifyMVar_ st $ \i -> do
      put DoneCounting
      return (i + 1)
    GetCount -> modifyMVar_ st $ \i -> do
      put (Count i)
      return i

-- | Test progress messages
testProgress :: RpcServer -> Assertion
testProgress server =
  forM_ ([0 .. 9] :: [Int]) $ \i -> assertRpcEquals server i [i, i - 1 .. 0]

testProgressServer :: FilePath -> RpcConversation -> IO ()
testProgressServer _errorLog RpcConversation{..} = forever $ do
  req <- get :: IO Int
  forM_ [req, req - 1 .. 1] $ put
  put (0 :: Int)

-- | Test shutdown
testShutdown :: RpcServer -> Assertion
testShutdown server = do
  assertRpcEqual server "ping" "ping"
  shutdown server
  assertRaises "" (== (userError "Manual shutdown"))
    (rpc server "ping" :: IO String)

-- | Test that stdout is available as usual on the server
testStdout :: RpcServer -> Assertion
testStdout server = do
  assertRpcEqual server "ping" "ping"
  shutdown server
  assertRaises "" (== (userError "Manual shutdown"))
    (rpc server "ping" :: IO String)

testStdoutServer :: FilePath -> RpcConversation -> IO ()
testStdoutServer _errorLog RpcConversation{..} = forever $ do
  req <- get :: IO String
  putStrLn "   vvvv    testStdout intentionally printing to stdout"
  put req

-- | Test that we can do concurrent get and put
testConcurrentGetPut :: RpcServer -> Assertion
testConcurrentGetPut server =
  rpcConversation server $ \RpcConversation{..} -> do
    forkIO $ threadDelay 1000000 >> put (1 :: Int)
    Right 1 <- Ex.try get :: IO (Either Ex.SomeException Int)
    return ()

testConcurrentGetPutServer :: FilePath -> RpcConversation -> IO ()
testConcurrentGetPutServer _errorLog RpcConversation{..} = forever $ do
  i <- get
  put (i :: Int)

--------------------------------------------------------------------------------
-- Test generalized conversations                                             --
--------------------------------------------------------------------------------

data StartGame  = StartGame Int Int | NoMoreGames
  deriving (Show, Typeable)
data Guess = Guess Int | GiveUp | Yay
  deriving (Show, Typeable, Eq)
data GuessReply = GuessCorrect | GuessIncorrect
  deriving (Show, Typeable)

instance Binary StartGame where
  put (StartGame i j) = do Binary.putWord8 0
                           Binary.put i
                           Binary.put j
  put NoMoreGames     = Binary.putWord8 1

  get = do
    header <- Binary.getWord8
    case header of
      0 -> StartGame <$> Binary.get <*> Binary.get
      1 -> return NoMoreGames
      _ -> fail "Binary.get: Invalid header"

instance Binary Guess where
  put (Guess i) = Binary.putWord8 0 >> Binary.put i
  put GiveUp    = Binary.putWord8 1
  put Yay       = Binary.putWord8 2

  get = do
    header <- Binary.getWord8
    case header of
      0 -> Guess <$> Binary.get
      1 -> return GiveUp
      2 -> return Yay
      _ -> fail "Guess.get: invalid header"

instance Binary GuessReply where
  put GuessCorrect   = Binary.putWord8 0
  put GuessIncorrect = Binary.putWord8 1

  get = do
    header <- Binary.getWord8
    case header of
      0 -> return GuessCorrect
      1 -> return GuessIncorrect
      _ -> fail "GuessReply.get: invalid header"

testConversation :: RpcServer -> Assertion
testConversation server = do
  assertRpc server
    $ Put (StartGame 1 10)
    $ Get (Guess 1)
    $ Put GuessIncorrect
    $ Get (Guess 2)
    $ Put GuessIncorrect
    $ Get (Guess 3)
    $ Put GuessCorrect
    $ Get Yay
    $ Done
  assertRpc server
    $ Put (StartGame 1 2)
    $ Get (Guess 1)
    $ Put GuessIncorrect
    $ Get (Guess 2)
    $ Put GuessIncorrect
    $ Get GiveUp
    $ Done
  {- DISABLED. Whether or not we get this exception, or just sit here and wait,
     depends on circumstances. Perhaps it would be nicer if we did tag messages
     with some type information so that can catch these errors properly.
  assertRaises "" typeError $
    assertRpcEqual server GuessCorrect GiveUp
  where
    typeError :: ExternalException -> Bool
    typeError ExternalException{externalStdErr} =
      "not enough bytes" `isInfixOf` externalStdErr
  -}
  assertRpcEqual server NoMoreGames ()

testConversationServer :: FilePath -> RpcConversation -> IO ()
testConversationServer _errorLog RpcConversation{..} = outerLoop
  where
    outerLoop = do
      req <- labelExceptions "testConversationServer outerLoop: " $ get
      case req of
        StartGame n m -> innerLoop n m >> outerLoop
        NoMoreGames   -> put () >> return ()

    innerLoop n m
      | n > m     = put GiveUp
      | otherwise = do put (Guess n)
                       answer <- labelExceptions "testConcurrentServer innerLoop: " $ get
                       case answer of
                         GuessCorrect   -> put Yay
                         GuessIncorrect -> innerLoop (n + 1) m

--------------------------------------------------------------------------------
-- Error handling tests                                                       --
--------------------------------------------------------------------------------

-- | Test crashing server
testCrash :: RpcServer -> Assertion
testCrash server =
  assertRaises "" (isServerIOException crash) $
    assertRpcEqual server () ()

testCrashServer :: FilePath -> RpcConversation -> IO ()
testCrashServer _errorLog RpcConversation{..} = do
  () <- get
  Ex.throwIO (userError crash)

crash :: String
crash = "Intentional crash"

-- | Test server which gets killed during a request
testKill :: RpcServer -> Assertion
testKill server = do
  assertRpcEqual server "ping" "ping"   -- First request goes through
  assertRaises "" isServerKilledException $
    assertRpcEqual server "ping" "ping" -- Second does not

testKillServer :: MVar Bool -> FilePath -> RpcConversation -> IO ()
testKillServer firstRequest _errorLog RpcConversation{..} = forever $ do
  req <- get :: IO String
  modifyMVar_ firstRequest $ \isFirst -> do
    if isFirst
      then put req
      else throwSigKill
    return False

-- | Test server which gets killed between requests
testKillAsync :: RpcServer -> Assertion
testKillAsync server = do
  assertRpcEqual server "ping" "ping"
  threadDelay 500000 -- Wait for server to exit
  assertRaises "" isServerKilledException $
    assertRpcEqual server "ping" "ping"

testKillAsyncServer :: FilePath -> RpcConversation -> IO ()
testKillAsyncServer _errorLog RpcConversation{..} = forever $ do
  req <- get :: IO String
  -- Fork a thread which causes the server to crash 0.5 seconds after the request
  forkIO $ threadDelay 250000 >> throwSigKill
  put req

-- | Test crash during decoding
data TypeWithFaultyDecoder = TypeWithFaultyDecoder
  deriving (Show, Typeable)

instance Binary TypeWithFaultyDecoder where
  put _ = Binary.putWord8 0 -- >> return ()
  get = fail "Faulty decoder"

testFaultyDecoder :: RpcServer -> Assertion
testFaultyDecoder server =
  assertRaises "" (isServerIOException "Faulty decoder") $
    assertRpcEqual server TypeWithFaultyDecoder ()

testFaultyDecoderServer :: FilePath -> RpcConversation -> IO ()
testFaultyDecoderServer _errorLog RpcConversation{..} = forever $ do
  TypeWithFaultyDecoder <- get
  put ()

-- | Test crash during encoding
data TypeWithFaultyEncoder = TypeWithFaultyEncoder
  deriving (Show, Eq, Typeable)

instance Binary TypeWithFaultyEncoder where
  put _ = fail "Faulty encoder"
  get = Binary.getWord8 >> return TypeWithFaultyEncoder

testFaultyEncoder :: RpcServer -> Assertion
testFaultyEncoder server =
  assertRaises "" ((== "Faulty encoder") . externalStdErr) $
    assertRpcEqual server () TypeWithFaultyEncoder

testFaultyEncoderServer :: FilePath -> RpcConversation -> IO ()
testFaultyEncoderServer _errorLog RpcConversation{..} = forever $ do
  () <- get
  put TypeWithFaultyEncoder

--------------------------------------------------------------------------------
-- Test errors during RPC calls with multiple responses                       --
--------------------------------------------------------------------------------

-- | Test server which crashes after sending some intermediate responses
testCrashMulti :: RpcServer -> Assertion
testCrashMulti server =
  assertRaises "" (isServerIOException crash) $
    assertRpcEquals server (3 :: Int) ([3, 2, 1, 0] :: [Int])

testCrashMultiServer :: FilePath -> RpcConversation -> IO ()
testCrashMultiServer _errorLog RpcConversation{..} = forever $ do
  req <- get :: IO Int
  forM_ [req, req - 1 .. 1] $ put
  Ex.throwIO (userError crash)

-- | Like 'CrashMulti', but killed rather than an exception
testKillMulti :: RpcServer -> Assertion
testKillMulti server =
  assertRaises "" isServerKilledException $
    assertRpcEquals server (3 :: Int) ([3, 2, 1, 0] :: [Int])

testKillMultiServer :: FilePath -> RpcConversation -> IO ()
testKillMultiServer _errorLog RpcConversation{..} = forever $ do
  req <- get :: IO Int
  forM_ [req, req - 1 .. 1] $ put
  throwSigKill

-- | Like 'KillMulti', but now the server gets killed *between* messages
testKillAsyncMulti :: RpcServer -> Assertion
testKillAsyncMulti server =
  assertRaises "" isServerKilledException $
    assertRpcEquals server (3 :: Int) ([3, 2, 1, 0] :: [Int])

testKillAsyncMultiServer :: FilePath -> RpcConversation -> IO ()
testKillAsyncMultiServer _errorLog RpcConversation{..} = forever $ do
  req <- get
  forkIO $ threadDelay (250000 + (req - 1) * 50000) >> throwSigKill
  forM_ [req, req - 1 .. 1] $ \i -> threadDelay 50000 >> put i

--------------------------------------------------------------------------------
-- Tests for errors in client code                                            --
--------------------------------------------------------------------------------

-- | Test letting the Progress object escape from the scope
testIllscoped :: RpcServer -> Assertion
testIllscoped server = do
  conversation <- rpcConversation server $ return
  assertRaises "" (== illscopedConversationException) (get conversation :: IO String)
  assertRaises "" (== illscopedConversationException) (put conversation "hi")

-- | Test what happens if the client specifies the wrong request type
--
-- (The actual type is the type of the echo server: RpcServer String String)
testInvalidReqType :: RpcServer -> Assertion
testInvalidReqType server =
    assertRaises "" (isServerIOException parseEx) $
      assertRpcEqual server () "ping"
  where
    parseEx = "not enough bytes"

{- DISABLED: We cannot detect this reliably anymore, since we don't
  encode type information anymore since the move to Binary
-- | Test what happens if the client specifies the wrong response type
--
-- Note that since the decoding error now happens *locally*, the exception
-- is a regular userError, rather than an ExternalException
--
-- (The actual type is the type of the echo server: RpcServer String String)
testInvalidRespType :: RpcServer -> Assertion
testInvalidRespType server =
    assertRaises "" (== userError parseEx) $
      assertRpcEqual server "ping" ()
  where
    -- TODO: do we want to insist on this particular parse error?
    parseEx = "when expecting a (), encountered String instead"
-}

--------------------------------------------------------------------------------
-- Concurrent conversations                                                   --
--                                                                            --
-- It is important that all servers involved with the concurrent conversations--
-- have a proper shutdown sequence or else we will get runtime exceptions when--
-- we close the pipes.                                                        --
--------------------------------------------------------------------------------

data ConcurrentServerRequest =
    ConcurrentServerSanityCheck String
  | ConcurrentServerSpawn
  | ConcurrentServerTerminate
  deriving (Typeable, Show)

instance Binary ConcurrentServerRequest where
  put (ConcurrentServerSanityCheck s) = Binary.putWord8 0 >> Binary.put s
  put ConcurrentServerSpawn           = Binary.putWord8 1
  put ConcurrentServerTerminate       = Binary.putWord8 2

  get = do
    header <- Binary.getWord8
    case header of
      0 -> ConcurrentServerSanityCheck <$> Binary.get
      1 -> return ConcurrentServerSpawn
      2 -> return ConcurrentServerTerminate
      _ -> fail "ConcurrentServerRequest.get: invalid header"

testConcurrentServer :: FilePath -> RpcConversation -> IO ()
testConcurrentServer _errorLog RpcConversation{..} = go
  where
    go = do
      req <- labelExceptions "testConcurrentServer: " $ get
      case req of
        ConcurrentServerSanityCheck str -> do
          put str
          go
        ConcurrentServerSpawn -> do
          pipes <- newEmptyMVar :: IO (MVar (WriteChannel, ReadChannel, String))
          forkIO $ do
            stdin <- makeSocket
            stdout <- makeSocket

            tmpDir <- Dir.getTemporaryDirectory
            (errorLogPath, errorLogHandle) <- openTempFile tmpDir "rpc.log"
            hClose errorLogHandle

            [stdinPort, stdoutPort] <- mapM socketPort [stdin, stdout]
            -- Once we have created the sockets we can tell the client
            putMVar pipes (WriteChannel stdinPort, ReadChannel stdoutPort, errorLogPath)
            concurrentConversation stdin stdout errorLogPath testConversationServer

          (stdinPort, stdoutPort, errorLogPath) <- readMVar pipes
          put (stdinPort, stdoutPort, errorLogPath)
          go
        ConcurrentServerTerminate -> do
          put ()
          return ()

testConcurrent :: RpcServer -> Assertion
testConcurrent server = do
  -- test sequentially: echo, spawn conversation, run that conversation, repeat
  replicateM_ 3 $ do
    -- test we can echo
    assertRpcEqual server (ConcurrentServerSanityCheck "ping") "ping"

    (stdin, stdout, stderr) <- rpc server ConcurrentServerSpawn
    connectToRpcServer stdin stdout stderr $ testConversation

  -- test concurrent execution: spawn a bunch of servers, have conversations
  -- with all of them concurrently, while still communicating on the original
  -- conversation, too (note that the calls to 'rpc' to start the concurrent
  -- conversations will be sequentialized)
  clientConvs <- replicateM 10 $ Async.async $ do
    (stdin, stdout, stderr) <- rpc server ConcurrentServerSpawn
    connectToRpcServer stdin stdout stderr $ testConversation

  -- Echo on the original conversation while we have the other conversations
  replicateM_ 100 $
    assertRpcEqual server (ConcurrentServerSanityCheck "ping") "ping"

  -- Wait for all client conversations to finish
  forM_ clientConvs Async.wait

  -- Shutdown the main server
  assertRpcEqual server ConcurrentServerTerminate ()

--------------------------------------------------------------------------------
-- Driver                                                                     --
--------------------------------------------------------------------------------

tests :: [Test]
tests = [
    testGroup "Features" [
        testRPC "echo"             testEcho
      , testRPC "state"            testState
      , testRPC "custom"           testCustom
      , testRPC "progress"         testProgress
      , testRPC "shutdown"         testShutdown
      , testRPC "stdout"           testStdout
      , testRPC "conversation"     testConversation
      , testRPC "concurrentGetPut" testConcurrentGetPut
      ]
  , testGroup "Error handling" [
        testRPC "crash"            testCrash
      , testRPC "kill"             testKill
      , testRPC "killAsync"        testKillAsync
      , testRPC "faultyDecoder"    testFaultyDecoder
      , testRPC "faultyEncoder"    testFaultyEncoder
      ]
   , testGroup "Error handling during RPC calls with multiple responses" [
         testRPC "crashMulti"       testCrashMulti
       , testRPC "killMulti"        testKillMulti
       , testRPC "killAsyncMulti"   testKillAsyncMulti
       ]
   , testGroup "Client code errors" [
         testRPC "illscoped"        testIllscoped
       , testRPC "invalidReqType"   testInvalidReqType
      --  , testRPC "invalidRespType"  testInvalidRespType
       ]
   , testGroup "Concurrent conversations" [
         testRPC "concurrent" testConcurrent
       ]
  ]
  where
    testRPC :: String -> (RpcServer -> Assertion) -> Test
    testRPC name testWith = testCase name $ do
      server <- forkTestServer name
      testWith server
      shutdown server

main :: IO ()
main = do
  args <- getArgs
  case args of
    "--server" : test : args' -> case test of
      "echo"             -> rpcServer testEchoServer args'
      "state"            -> do st <- newMVar 0
                               rpcServer (testStateServer st) args'
      "custom"           -> do st <- newMVar 0
                               rpcServer (testCustomServer st) args'
      "progress"         -> rpcServer testProgressServer args'
      "shutdown"         -> rpcServer testEchoServer args'
      "stdout"           -> rpcServer testStdoutServer args'
      "conversation"     -> rpcServer testConversationServer args'
      "concurrentGetPut" -> rpcServer testConcurrentGetPutServer args'
      "crash"            -> rpcServer testCrashServer args'
      "kill"             -> do firstRequest <- newMVar True
                               rpcServer (testKillServer firstRequest) args'
      "killAsync"        -> rpcServer testKillAsyncServer args'
      "faultyDecoder"    -> rpcServer testFaultyDecoderServer args'
      "faultyEncoder"    -> rpcServer testFaultyEncoderServer args'
      "illscoped"        -> rpcServer testEchoServer args'
      "underconsumption" -> rpcServer testEchoServer args'
      "overconsumption"  -> rpcServer testEchoServer args'
      "crashMulti"       -> rpcServer testCrashMultiServer args'
      "killMulti"        -> rpcServer testKillMultiServer args'
      "killAsyncMulti"   -> rpcServer testKillAsyncMultiServer args'
      "invalidReqType"   -> rpcServer testEchoServer args'
      "invalidRespType"  -> rpcServer testEchoServer args'
      "concurrent"       -> rpcServer testConcurrentServer args'
      _ -> error $ "Invalid server " ++ show test
    _ -> defaultMain tests

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

labelExceptions :: String -> IO a -> IO a
labelExceptions label = Ex.handle aux
  where
    aux :: Ex.SomeException -> IO a
    aux e = Ex.throwIO (userError (label ++ show e))
