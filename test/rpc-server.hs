{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module Main where

import System.IO (stdin, stdout, stderr)
import System.Environment (getArgs)
import System.Environment.Executable (getExecutablePath)
import System.Posix.Signals (raiseSignal, sigKILL)
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON))
import Data.Aeson.TH (deriveJSON, deriveToJSON, deriveFromJSON)
import Data.Maybe (fromJust)
import Control.Monad (forM_)
import qualified Control.Exception as Ex
import Control.Applicative ((<$>), (<|>))
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertEqual, assertFailure)

import RpcServer

--------------------------------------------------------------------------------
-- Generic auxiliary                                                          --
--------------------------------------------------------------------------------

-- | Check that the given IO action raises the specified exception
assertRaises :: (Ex.Exception e, Eq e, Show e)
             => String     -- ^ Message displayed if assertion fails
             -> e          -- ^ Expected exception
             -> IO a       -- ^ Action to run
             -> Assertion
assertRaises msg ex p = Ex.catch runAction $ \ex' ->
    case Ex.fromException ex' of
      Just ex'' -> assertEqual msg ex ex''
      Nothing   -> assertFailure (msg ++ ": Raised exception of the wrong type " ++ exceptionType ex' ++ ": " ++ show ex')
  where
    runAction = p >> assertFailure (msg ++ ": No exception was raised")

exceptionType :: Ex.SomeException -> String
exceptionType ex = fromJust $
      ((\(_ :: Ex.IOException)    -> "IOException")       <$> Ex.fromException ex)
  <|> ((\(_ :: Ex.AsyncException) -> "AsyncException")    <$> Ex.fromException ex)
  <|> ((\(_ :: ExternalException) -> "ExternalException") <$> Ex.fromException ex)
  <|> Just "Unknown type"

--------------------------------------------------------------------------------
-- RPC-specific auxiliary                                                     --
--------------------------------------------------------------------------------

-- | Specialized version of 'rpcServer' that uses the standard I/O
startTestServer :: (FromJSON req, ToJSON resp)
                => (req -> IO (Progress resp resp))
                -> IO ()
startTestServer = rpcServer stdin stdout stderr

-- | Call the current executable and pass the right arguments
forkTestServer :: String -> IO (RpcServer req resp)
forkTestServer test = do
  prog <- getExecutablePath
  forkRpcServer prog ["--server", test]

-- | Do an RPC call and verify the result
assertRpcEqual :: (ToJSON req, FromJSON resp, Show req, Show resp, Eq resp)
               => RpcServer req resp  -- ^ RPC server
               -> req                 -- ^ Request
               -> resp                -- ^ Expected response
               -> Assertion
assertRpcEqual server req resp = do
  resp' <- rpc server req
  assertEqual ("Request " ++ show req) resp resp'

-- | Like 'assertRpcEqual' but verify a number of responses
assertRpcEquals :: (ToJSON req, FromJSON resp, Show req, Show resp, Eq resp)
                => RpcServer req resp  -- ^ RPC server
                -> req                 -- ^ Request
                -> [resp]              -- ^ Expected responses
                -> Assertion
assertRpcEquals server req = rpcWithProgress server req . handler
  where
    handler [] _ =
      assertFailure $ "Received unexpected messages for request " ++ show req
    handler (r:rs) p = do
      resp <- progressWait p
      case resp of
        Left lastResponse ->
          assertEqual ("Request " ++ show req) (r:rs) [lastResponse]
        Right (intermediateResponse, p') -> do
          assertEqual ("Request " ++ show req) r intermediateResponse
          handler rs p'

assertRpcRaises :: (ToJSON req, FromJSON resp, Show req, Eq e, Ex.Exception e)
                => RpcServer req resp  -- ^ RPC server
                -> req                 -- ^ Request
                -> e                   -- ^ Expected exception
                -> Assertion
assertRpcRaises server req ex =
  assertRaises ("Request " ++ show req) ex (rpc server req)

--------------------------------------------------------------------------------
-- Feature tests                                                              --
--------------------------------------------------------------------------------

-- | Simple echo server
testEcho :: Assertion
testEcho = do
  server <- forkTestServer "echo"
  assertRpcEqual server "ping" "ping"

testEchoServer :: String -> IO (Progress String String)
testEchoServer = return . Progress . return . Left

-- | Test stateful server
testState :: Assertion
testState = do
  server <- forkTestServer "state"
  forM_ ([0 .. 9] :: [Int]) $ assertRpcEqual server ()

testStateServer :: MVar Int -> () -> IO (Progress Int Int)
testStateServer st () = return . Progress . modifyMVar st $ \i ->
  return (i + 1, Left i)

-- | Test with request and response custom data types
data CountRequest  = Increment | GetCount deriving Show
data CountResponse = Done | Count Int deriving (Eq, Show)

$(deriveJSON id ''CountRequest)
$(deriveJSON id ''CountResponse)

testCustomDataTypes :: Assertion
testCustomDataTypes = do
  server <- forkTestServer "customDataTypes"
  assertRpcEqual server GetCount (Count 0)
  assertRpcEqual server Increment Done
  assertRpcEqual server GetCount (Count 1)

testCustomDataTypesServer :: MVar Int
                          -> CountRequest
                          -> IO (Progress CountResponse CountResponse)
testCustomDataTypesServer st Increment = return . Progress $
  modifyMVar st $ \i -> return (i + 1, Left Done)
testCustomDataTypesServer st GetCount = return . Progress $
  modifyMVar st $ \i -> return (i, Left (Count i))

-- | Test progress messages
testProgress :: Assertion
testProgress = do
  server <- forkTestServer "progress" :: IO (RpcServer Int Int)
  forM_ [0 .. 9] $ \i -> assertRpcEquals server i [i, i - 1 .. 0]

testProgressServer :: Int -> IO (Progress Int Int)
testProgressServer n = do
    left <- newMVar n
    return (go left)
  where
    go left = Progress $
      modifyMVar left $ \m ->
        if m == 0
          then return (0, Left 0)
          else return (m - 1, Right (m, go left))

-- | Test shutdown
testShutdown :: Assertion
testShutdown = do
  server <- forkTestServer "echo"
  assertRpcEqual server "ping" "ping"
  shutdown server
  assertRpcRaises server "ping" (userError "Manual shutdown")

--------------------------------------------------------------------------------
-- Error handling tests                                                       --
--------------------------------------------------------------------------------

-- | Test crashing server
testCrash :: Assertion
testCrash = do
  server <- forkTestServer "crash" :: IO (RpcServer () ())
  assertRpcRaises server () $ ExternalException (show crash)

testCrashServer :: () -> IO (Progress () ())
testCrashServer () = return . Progress $ Ex.throwIO crash

crash :: Ex.IOException
crash = userError "Intentional crash"

-- | Test server which gets killed during a request
testKill :: Assertion
testKill = do
  server <- forkTestServer "kill" :: IO (RpcServer String String)
  assertRpcEqual server "ping" "ping" -- First request goes through
  assertRpcRaises server "ping" serverKilledException

testKillServer :: MVar Bool -> String -> IO (Progress String String)
testKillServer firstRequest req = return . Progress $ do
  isFirst <- modifyMVar firstRequest $ \b -> return (False, b)
  if isFirst
    then return (Left req)
    else raiseSignal sigKILL >> undefined

-- | Test server which gets killed between requests
testKillAsync :: Assertion
testKillAsync = do
  server <- forkTestServer "killAsync" :: IO (RpcServer String String)
  assertRpcEqual server "ping" "ping"
  threadDelay 500000 -- Wait for server to exit
  assertRpcRaises server "ping" serverKilledException

testKillAsyncServer :: String -> IO (Progress String String)
testKillAsyncServer req = return . Progress $ do
  -- Fork a thread which causes the server to crash 0.5 seconds after the request
  forkIO $ threadDelay 250000 >> raiseSignal sigKILL
  return (Left req)

-- | Test crash during decoding
data TypeWithFaultyDecoder = TypeWithFaultyDecoder deriving Show

instance FromJSON TypeWithFaultyDecoder where
  parseJSON _ = fail "Faulty decoder"

$(deriveToJSON id ''TypeWithFaultyDecoder)

testFaultyDecoder :: Assertion
testFaultyDecoder = do
  server <- forkTestServer "faultyDecoder" :: IO (RpcServer TypeWithFaultyDecoder ())
  assertRpcRaises server TypeWithFaultyDecoder (ExternalException . show . userError $ "Faulty decoder")

testFaultyDecoderServer :: TypeWithFaultyDecoder -> IO (Progress () ())
testFaultyDecoderServer _ = return . Progress $ return (Left ())

-- | Test crash during encoding
data TypeWithFaultyEncoder = TypeWithFaultyEncoder deriving (Show, Eq)

$(deriveFromJSON id ''TypeWithFaultyEncoder)

instance ToJSON TypeWithFaultyEncoder where
  toJSON _ = error "Faulty encoder"

testFaultyEncoder :: Assertion
testFaultyEncoder = do
  server <- forkTestServer "faultyEncoder" :: IO (RpcServer () TypeWithFaultyEncoder)
  assertRpcRaises server () (ExternalException "Faulty encoder")

testFaultyEncoderServer :: () -> IO (Progress TypeWithFaultyEncoder TypeWithFaultyEncoder)
testFaultyEncoderServer () = return . Progress $
  return (Left TypeWithFaultyEncoder)

--------------------------------------------------------------------------------
-- Driver                                                                     --
--------------------------------------------------------------------------------

tests :: [Test]
tests = [
    testGroup "Features" [
        testCase "echo"            testEcho
      , testCase "state"           testState
      , testCase "customDataTypes" testCustomDataTypes
      , testCase "progress"        testProgress
      , testCase "shutdown"        testShutdown
      ]
  , testGroup "Error handling" [
        testCase "crash"         testCrash
      , testCase "kill"          testKill
      , testCase "killAsync"     testKillAsync
      , testCase "faultyDecoder" testFaultyDecoder
      , testCase "faultyEncoder" testFaultyEncoder
      ]
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--server", "echo"] ->
      startTestServer testEchoServer
    ["--server", "state"] -> do
      st <- newMVar 0
      startTestServer (testStateServer st)
    ["--server", "customDataTypes"] -> do
      st <- newMVar 0
      startTestServer (testCustomDataTypesServer st)
    ["--server", "progress"] ->
      startTestServer testProgressServer
    ["--server", "crash"] ->
      startTestServer testCrashServer
    ["--server", "kill"] -> do
      firstRequest <- newMVar True
      startTestServer (testKillServer firstRequest)
    ["--server", "killAsync"] ->
      startTestServer testKillAsyncServer
    ["--server", "faultyDecoder"] ->
      startTestServer testFaultyDecoderServer
    ["--server", "faultyEncoder"] ->
      startTestServer testFaultyEncoderServer
    _ -> defaultMain tests
