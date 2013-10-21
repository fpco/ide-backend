-- | Client interface to the `ide-backend-server` process
--
-- It is important that none of the types here rely on the GHC library.
{-# LANGUAGE TemplateHaskell #-}
module IdeSession.GHC.Client (
    -- * Starting and stopping the server
    InProcess
  , GhcServer(..)
  , forkGhcServer
  , shutdownGhcServer
  , forceShutdownGhcServer
  , getGhcExitCode
    -- * Interacting with the server
  , RunActions(..)
  , runWaitAll
  , rpcCompile
  , rpcRun
  , rpcCrash
  , rpcSetEnv
  , rpcSetArgs
  ) where

import Control.Concurrent (ThreadId, killThread)
import Control.Monad (unless)
import qualified Data.ByteString.Char8      as BSS
import qualified Data.ByteString.Lazy.Char8 as BSL
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import Control.Concurrent.Chan (Chan, newChan, writeChan)
import Control.Concurrent.Async (async, cancel, withAsync)
import Control.Concurrent.MVar (newMVar)
import qualified Control.Exception as Ex
import System.Exit (ExitCode)

import Paths_ide_backend

import IdeSession.GHC.API
import IdeSession.BlockingOps
import IdeSession.RPC.Client
import IdeSession.Types.Progress
import IdeSession.Util

{------------------------------------------------------------------------------
  Starting and stopping the server
------------------------------------------------------------------------------}

type InProcess = Bool

data GhcServer = OutProcess RpcServer
               | InProcess RpcConversation ThreadId

forkGhcServer :: Bool -> [String] -> Maybe String -> Maybe [(String, String)]
              -> InProcess -> IO GhcServer
forkGhcServer configGenerateModInfo opts workingDir menv False = do
  bindir <- getBinDir
  let prog = bindir </> "ide-backend-server"

  exists <- doesFileExist prog
  unless exists $
    fail $ "The 'ide-backend-server' program was expected to "
        ++ "be at location " ++ prog ++ " but it is not."

  server <- forkRpcServer prog
                          (opts ++ [ "--ghc-opts-end"
                                   , show configGenerateModInfo
                                   , show ideBackendApiVersion
                                   ])
                          workingDir
                          menv
  return (OutProcess server)
forkGhcServer _ _ _ _ True =
  fail "In-process ghc server not currently supported"
{- TODO: Reenable in-process
forkGhcServer configGenerateModInfo opts workingDir True = do
  let conv a b = RpcConversation {
                   get = do bs <- $readChan a
                            case decode' bs of
                              Just x  -> return x
                              Nothing -> fail "JSON failure"
                 , put = writeChan b . encode
                 }
  a   <- newChan
  b   <- newChan
  tid <- forkIO $ ghcServerEngine configGenerateModInfo opts (conv a b)
  return $ InProcess (conv b a) tid
-}

shutdownGhcServer :: GhcServer -> IO ()
shutdownGhcServer (OutProcess server) = shutdown server
shutdownGhcServer (InProcess _ tid)   = killThread tid

forceShutdownGhcServer :: GhcServer -> IO ()
forceShutdownGhcServer (OutProcess server) = forceShutdown server
forceShutdownGhcServer (InProcess _ tid)   = killThread tid

getGhcExitCode :: GhcServer -> IO (Maybe ExitCode)
getGhcExitCode (OutProcess server) =
  getRpcExitCode server
getGhcExitCode (InProcess _ _) =
  fail "getGhcExitCode not supported for in-process server"

{------------------------------------------------------------------------------
  Interacting with the server
------------------------------------------------------------------------------}

-- | Handles to the running code, through which one can interact with the code.
data RunActions = RunActions {
    -- | Wait for the code to output something or terminate
    runWait                     :: IO (Either BSS.ByteString RunResult)
    -- | Send a UserInterrupt exception to the code
    --
    -- A call to 'interrupt' after the snippet has terminated has no effect.
  , interrupt                   :: IO ()
    -- | Make data available on the code's stdin
    --
    -- A call to 'supplyStdin' after the snippet has terminated has no effect.
  , supplyStdin                 :: BSS.ByteString -> IO ()
    -- | Register a callback to be invoked when the program terminates
    -- The callback will only be invoked once.
    --
    -- A call to 'registerTerminationCallback' after the snippet has terminated
    -- has no effect. The termination handler is NOT called when the the
    -- 'RunActions' is 'forceCancel'ed.
  , registerTerminationCallback :: (RunResult -> IO ()) -> IO ()
    -- | Force terminate the runaction
    -- (The server will be useless after this -- for internal use only).
    --
    -- Guranteed not to block.
  , forceCancel                 :: IO ()
  }

-- | Repeatedly call 'runWait' until we receive a 'Right' result, while
-- collecting all 'Left' results
runWaitAll :: RunActions -> IO (BSL.ByteString, RunResult)
runWaitAll RunActions{runWait} = go []
  where
    go :: [BSS.ByteString] -> IO (BSL.ByteString, RunResult)
    go acc = do
      resp <- runWait
      case resp of
        Left  bs        -> go (bs : acc)
        Right runResult -> return (BSL.fromChunks (reverse acc), runResult)

-- | Set the environment
rpcSetEnv :: GhcServer -> [(String, Maybe String)] -> IO ()
rpcSetEnv (OutProcess server) env = rpc server (ReqSetEnv env)
rpcSetEnv (InProcess _ _)     env = setupEnv env

-- | Set command line arguments
rpcSetArgs :: GhcServer -> [String] -> IO ()
rpcSetArgs (OutProcess server) args =
  rpc server (ReqSetArgs args)
rpcSetArgs (InProcess _ _) _ =
  error "rpcSetArgs not supposed for in-process server"

-- | Compile or typecheck
rpcCompile :: GhcServer           -- ^ GHC server
           -> Maybe [String]      -- ^ Options
           -> FilePath            -- ^ Source directory
           -> Bool                -- ^ Should we generate code?
           -> (Progress -> IO ()) -- ^ Progress callback
           -> IO GhcCompileResult
rpcCompile server opts dir genCode callback =
  conversation server $ \RpcConversation{..} -> do
    put (ReqCompile opts dir genCode)

    let go = do response <- get
                case response of
                  GhcCompileProgress pcounter -> callback pcounter >> go
                  GhcCompileDone result       -> return result

    go

-- | Run code
rpcRun :: GhcServer       -- ^ GHC server
       -> String          -- ^ Module
       -> String          -- ^ Function
       -> RunBufferMode   -- ^ Buffer mode for stdout
       -> RunBufferMode   -- ^ Buffer mode for stderr
       -> IO RunActions
rpcRun server m fun outBMode errBMode = do
  runWaitChan <- newChan :: IO (Chan (Either BSS.ByteString RunResult))
  reqChan     <- newChan :: IO (Chan GhcRunRequest)

  conv <- async . Ex.handle (handleExternalException runWaitChan) $
    conversation server $ \RpcConversation{..} -> do
      put (ReqRun m fun outBMode errBMode)
      withAsync (sendRequests put reqChan) $ \sentAck -> do
        let go = do resp <- get
                    case resp of
                      GhcRunDone result -> writeChan runWaitChan (Right result)
                      GhcRunOutp bs     -> writeChan runWaitChan (Left bs) >> go
        go
        $wait sentAck

  -- The runActionState initially is the termination callback to be called
  -- when the snippet terminates. After termination it becomes (Right outcome).
  -- This means that we will only execute the termination callback once, and
  -- the user can safely call runWait after termination and get the same
  -- result.
  let onTermination :: RunResult -> IO ()
      onTermination _ = do writeChan reqChan GhcRunAckDone
                           $wait conv
  runActionsState <- newMVar (Left onTermination)

  return RunActions {
      runWait = $modifyMVar runActionsState $ \st -> case st of
        Right outcome ->
          return (Right outcome, Right outcome)
        Left terminationCallback -> do
          outcome <- $readChan runWaitChan
          case outcome of
            Left bs ->
              return (Left terminationCallback, Left bs)
            Right res@RunForceCancelled ->
              return (Right res, Right res)
            Right res -> do
              terminationCallback res
              return (Right res, Right res)
    , interrupt   = writeChan reqChan GhcRunInterrupt
    , supplyStdin = writeChan reqChan . GhcRunInput
    , registerTerminationCallback = \callback' ->
        $modifyMVar_ runActionsState $ \st -> case st of
          Right outcome ->
            return (Right outcome)
          Left callback ->
            return (Left (\res -> callback res >> callback' res))
    , forceCancel = do
        writeChan runWaitChan (Right RunForceCancelled)
        cancel conv
    }
  where
    sendRequests :: (GhcRunRequest -> IO ()) -> Chan GhcRunRequest -> IO ()
    sendRequests put reqChan =
      let go = do req <- $readChan reqChan
                  put req
                  case req of
                    GhcRunAckDone -> return ()
                    _             -> go
      in go

    -- TODO: should we restart the session when ghc crashes?
    -- Maybe recommend that the session is started on GhcExceptions?
    handleExternalException :: Chan (Either BSS.ByteString RunResult)
                            -> ExternalException
                            -> IO ()
    handleExternalException ch = writeChan ch . Right . RunGhcException . show

-- | Crash the GHC server (for debugging purposes)
rpcCrash :: GhcServer -> Maybe Int -> IO ()
rpcCrash server delay = conversation server $ \RpcConversation{..} ->
  put (ReqCrash delay)

{------------------------------------------------------------------------------
  Internal
------------------------------------------------------------------------------}

conversation :: GhcServer -> (RpcConversation -> IO a) -> IO a
conversation (OutProcess server) = rpcConversation server
conversation (InProcess conv _)  = ($ conv)
