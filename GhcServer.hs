{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
-- | Implementation of the server that controls the long-running GHC instance.
-- This is the place where the GHC-specific part joins the part
-- implementing the general RPC infrastructure.
--
-- The modules importing any GHC internals, as well as the modules
-- implementing the  RPC infrastructure, should be accessible to the rest
-- of the program only indirectly, through the @GhcServer@ module.
module GhcServer
  ( -- * Types involved in the communication
    Progress
    -- * A handle to the server
  , GhcServer
    -- * Server-side operations
  , ghcServer
    -- * Client-side operations
  , forkGhcServer
  , rpcCompile
  , rpcRun
  , rpcSetEnv
  , shutdownGhcServer
  , getRpcExitCode
  ) where

import Control.Concurrent (ThreadId, forkIO, myThreadId, throwTo)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar, modifyMVar)
import qualified Control.Exception as Ex
import Control.Monad (forM, forever, when, void)
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString as BSS (ByteString, hGetSome, hPut, null)
import qualified Data.ByteString.Char8 as BSSC (pack)
import Data.IORef

import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO (hFlush, stdout, Handle)
import System.Posix (Fd)
import System.Posix.Env (setEnv, unsetEnv)
import System.Posix.IO.ByteString

import Common
import GhcRun
import ModuleName (LoadedModules, ModuleName)
import RpcServer
import RunAPI

import Paths_ide_backend

data GhcRequest
  = ReqCompile (Maybe [String]) FilePath Bool
  | ReqRun     ModuleName String
  | ReqSetEnv  [(String, Maybe String)]
  deriving Show
data GhcCompileResponse =
    GhcCompileProgress Progress
  | GhcCompileDone ([SourceError], LoadedModules)
  deriving Show
data GhcRunResponse =
    GhcRunOutp BSS.ByteString
  | GhcRunDone RunResult
  deriving Show
data GhcRunRequest =
    GhcRunInput BSS.ByteString
  | GhcRunInterrupt
  | GhcRunAckDone
  deriving Show

$(deriveJSON id ''GhcRequest)
$(deriveJSON id ''GhcCompileResponse)
$(deriveJSON id ''GhcRunResponse)
$(deriveJSON id ''GhcRunRequest)

type GhcServer = RpcServer

--------------------------------------------------------------------------------
-- Server-side operations                                                     --
--------------------------------------------------------------------------------

-- | Start the RPC server. Used from within the server executable.
ghcServer :: [String] -> IO ()
ghcServer fdsAndOpts = do
  let (opts, markerAndFds) = span (/= "--ghc-opts-end") fdsAndOpts
  rpcServer (tail markerAndFds) (ghcServerEngine opts)

-- | The GHC server engine proper.
--
-- This function runs in end endless loop inside the @Ghc@ monad, making
-- incremental compilation possible.
ghcServerEngine :: [String] -> RpcConversation  -> IO ()
ghcServerEngine staticOpts conv@RpcConversation{..} = do
  -- Submit static opts and get back leftover dynamic opts.
  dOpts <- submitStaticOpts staticOpts

  -- Start handling requests. From this point on we don't leave the GHC monad.
  runFromGhc . forever $ do
    req <- liftIO $ get
    case req of
      ReqCompile opts dir genCode ->
        ghcHandleCompile conv dOpts opts dir genCode
      ReqRun m fun ->
        ghcHandleRun conv m fun
      ReqSetEnv env ->
        ghcHandleSetEnv conv env

-- | Handle a compile or type check request
ghcHandleCompile :: RpcConversation
                 -> DynamicOpts        -- ^ startup dynamic flags
                 -> Maybe [String]     -- ^ new, user-submitted dynamic flags
                 -> FilePath           -- ^ Source directory
                 -> Bool               -- ^ Should we generate code
                 -> Ghc ()
ghcHandleCompile RpcConversation{..} dOpts ideNewOpts configSourcesDir ideGenerateCode = do
    errsRef <- liftIO $ newIORef []
    counter <- liftIO $ newIORef initialProgress
    (errs, context) <-
      surpressGhcStdout $ compileInGhc configSourcesDir
                                       dynOpts
                                       ideGenerateCode
                                       verbosity
                                       errsRef
                                       (progressCallback counter)
                                       (\_ -> return ()) -- TODO: log?
    liftIO $ debug dVerbosity $ "returned from compileInGhc with " ++ show errs
    liftIO $ put $ GhcCompileDone (errs, context)
  where
    dynOpts :: DynamicOpts
    dynOpts = maybe dOpts optsToDynFlags ideNewOpts

    -- Let GHC API print "compiling M ... done." for each module.
    verbosity :: Int
    verbosity = 1

    -- TODO: verify that _ is the "compiling M" message
    progressCallback :: IORef Progress -> String -> IO ()
    progressCallback counter ghcMsg = do
      oldCounter <- readIORef counter
      modifyIORef counter (updateProgress ghcMsg)
      put $ GhcCompileProgress oldCounter

-- | Handle a run request
ghcHandleRun :: RpcConversation
             -> ModuleName        -- ^ Module
             -> String            -- ^ Function
             -> Ghc ()
ghcHandleRun RpcConversation{..} m fun = do
    (stdOutputRd, stdOutputBackup, stdErrorBackup) <- redirectStdout 
    (stdInputWr,  stdInputBackup)                  <- redirectStdin

    ghcThread    <- liftIO $ newEmptyMVar :: Ghc (MVar (Maybe ThreadId))
    stdoutThread <- liftIO $ newEmptyMVar :: Ghc (MVar ())
    reqThread    <- liftIO $ newEmptyMVar :: Ghc (MVar ())

    liftIO . forkIO $ readRunRequests ghcThread stdInputWr reqThread
    liftIO . forkIO $ readStdout stdOutputRd stdoutThread

    -- This is a little tricky. We only want to deliver the UserInterrupt
    -- exceptions when we are running 'runInGhc'. If the UserInterrupt arrives
    -- before we even get a chance to call 'runInGhc' the exception should not
    -- be delivered until we are in a position to catch it; after 'runInGhc'
    -- completes we should just ignore any further 'GhcRunInterrupt' requests.
    --
    -- We achieve this by
    --
    -- 1. The thread ID is stored in an MVar ('ghcThread'). Initially this
    --    MVar is empty, so if a 'GhcRunInterrupt' arrives before we are ready
    --    to deal with it the 'reqThread' will block
    -- 2. We install an exception handler before putting the thread ID into
    --    the MVar
    -- 3. We override the MVar with Nothing before leaving the exception handler
    -- 4. In the 'reqThread' we ignore GhcRunInterrupts once the 'MVar' is
    --    'Nothing'

    runOutcome <- ghandleJust isUserInterrupt return $ do
      liftIO $ myThreadId >>= putMVar ghcThread . Just
      outcome <- runInGhc (m, fun)
      liftIO $ modifyMVar ghcThread $ \_ -> return (Nothing, outcome)

    liftIO $ do
      -- Restore stdin and stdout
      dupTo stdOutputBackup stdOutput >> closeFd stdOutputBackup
      dupTo stdErrorBackup  stdError  >> closeFd stdErrorBackup
      dupTo stdInputBackup  stdInput  >> closeFd stdInputBackup

      -- Closing the write end of the stdout pipe will cause the stdout
      -- thread to terminate after it processed all remaining output;
      -- wait for this to happen
      readMVar stdoutThread

      -- Report the final result
      liftIO $ debug dVerbosity $ "returned from ghcHandleRun with "
                                  ++ show runOutcome
      put $ GhcRunDone runOutcome

      -- Wait for the client to acknowledge the done
      -- (this avoids race conditions)
      readMVar reqThread
  where
    -- Wait for and execute run requests from the client
    readRunRequests :: MVar (Maybe ThreadId) -> Handle -> MVar () -> IO ()
    readRunRequests ghcThread stdInputWr done = 
      let go = do request <- get
                  case request of
                    GhcRunInterrupt -> do
                      mTid <- readMVar ghcThread
                      case mTid of
                        Just tid -> throwTo tid Ex.UserInterrupt
                        Nothing  -> return () -- See above
                      go
                    GhcRunInput bs -> do
                      BSS.hPut stdInputWr bs
                      hFlush stdInputWr
                      go
                    GhcRunAckDone -> 
                      putMVar done () 
      in go

    -- Wait for the process to output something or terminate
    readStdout :: Handle -> MVar () -> IO ()
    readStdout stdOutputRd done = 
      let go = do bs <- BSS.hGetSome stdOutputRd blockSize
                  if BSS.null bs
                    then putMVar done ()
                    else put (GhcRunOutp bs) >> go
      in go

    isUserInterrupt :: Ex.AsyncException -> Maybe RunResult
    isUserInterrupt ex@Ex.UserInterrupt =
      Just . RunProgException . showExWithClass . Ex.toException $ ex
    isUserInterrupt _ =
      Nothing

    -- TODO: What is a good value here?
    blockSize :: Int
    blockSize = 4096
     
    -- Setup loopback pipe so we can capture runStmt's stdout/stderr
    redirectStdout :: Ghc (Handle, Fd, Fd)
    redirectStdout = liftIO $ do
      -- Create pipe
      (stdOutputRd, stdOutputWr) <- liftIO createPipe

      -- Backup stdout, then replace stdout and stderr with the pipe's write end
      stdOutputBackup <- liftIO $ dup stdOutput
      stdErrorBackup  <- liftIO $ dup stdError
      dupTo stdOutputWr stdOutput
      dupTo stdOutputWr stdError
      closeFd stdOutputWr

      -- Convert to the read end to a handle and return
      stdOutputRd' <- fdToHandle stdOutputRd
      return (stdOutputRd', stdOutputBackup, stdErrorBackup)
      
    -- Setup loopback pipe so we can write to runStmt's stdin 
    redirectStdin :: Ghc (Handle, Fd)
    redirectStdin = liftIO $ do
      -- Create pipe
      (stdInputRd, stdInputWr) <- liftIO createPipe

      -- Swizzle stdin
      stdInputBackup <- liftIO $ dup stdInput
      dupTo stdInputRd stdInput
      closeFd stdInputRd

      -- Convert the write end to a handle and return
      stdInputWr' <- fdToHandle stdInputWr
      return (stdInputWr', stdInputBackup)

-- | Handle a set-environment request
ghcHandleSetEnv :: RpcConversation -> [(String, Maybe String)] -> Ghc ()
ghcHandleSetEnv RpcConversation{put} env = liftIO $ do
  forM env $ \(var, mVal) -> case mVal of Just val -> setEnv var val True
                                          Nothing  -> unsetEnv var
  put ()

--------------------------------------------------------------------------------
-- Client-side operations                                                     --
--------------------------------------------------------------------------------

forkGhcServer :: [String] -> Maybe String -> IO GhcServer
forkGhcServer opts workingDir = do
  bindir <- getBinDir
  let prog = bindir </> "ide-backend-server"

  exists <- doesFileExist prog
  when (not exists) $
    fail $ "The 'ide-backend-server' program was expected to "
        ++ "be at location " ++ prog ++ " but it is not."

  forkRpcServer prog (opts ++ ["--ghc-opts-end"]) workingDir

-- | Compile or typecheck
rpcCompile :: GhcServer           -- ^ GHC server
           -> Maybe [String]      -- ^ Options
           -> FilePath            -- ^ Source directory
           -> Bool                -- ^ Should we generate code?
           -> (Progress -> IO ()) -- ^ Progress callback
           -> IO ([SourceError], LoadedModules)
rpcCompile server opts dir genCode callback =
  rpcConversation server $ \RpcConversation{..} -> do
    put (ReqCompile opts dir genCode)

    let go = do response <- get
                case response of
                  GhcCompileProgress pcounter -> callback pcounter >> go
                  GhcCompileDone     res      -> return res

    go

-- | Run code
rpcRun :: GhcServer       -- ^ GHC server
       -> ModuleName      -- ^ Module
       -> String          -- ^ Function
       -> IO RunActions
rpcRun server m fun = do
  runWaitChan <- newChan :: IO (Chan (Either BSS.ByteString RunResult))
  reqChan     <- newChan :: IO (Chan GhcRunRequest)
  sentAck     <- newEmptyMVar :: IO (MVar ())

  forkIO $ rpcConversation server $ \RpcConversation{..} -> do
    put (ReqRun m fun)
    void $ forkIO (sendRequests put reqChan sentAck)
    let go = do resp <- get
                case resp of
                  GhcRunDone runResult -> writeChan runWaitChan (Right runResult)
                  GhcRunOutp bs        -> writeChan runWaitChan (Left bs) >> go
    go
    -- Don't leave RPC conversation scope until ack is sent 
    readMVar sentAck

  return RunActions {
      runWait = do
        outcome <- readChan runWaitChan
        case outcome of
          Left  _ -> return ()
          Right _ -> do writeChan reqChan GhcRunAckDone 
                        readMVar sentAck
        return outcome
    , interrupt   = writeChan reqChan GhcRunInterrupt
    , supplyStdin = writeChan reqChan . GhcRunInput
    }
  where
    sendRequests :: (GhcRunRequest -> IO ()) -> Chan GhcRunRequest -> MVar () -> IO ()
    sendRequests put reqChan done = 
      let go = do req <- readChan reqChan
                  put req
                  case req of
                    GhcRunAckDone -> putMVar done () 
                    _             -> go
      in go

-- | Set the environment
rpcSetEnv :: GhcServer -> [(String, Maybe String)] -> IO ()
rpcSetEnv server env = rpc server (ReqSetEnv env)

shutdownGhcServer :: GhcServer -> IO ()
shutdownGhcServer = shutdown

--------------------------------------------------------------------------------
-- Auxiliary                                                                  --
--------------------------------------------------------------------------------

surpressGhcStdout :: Ghc a -> Ghc a
surpressGhcStdout p = do
  stdOutputBackup <- liftIO $ surpressStdOutput
  x <- p
  liftIO $ restoreStdOutput stdOutputBackup
  return x

type StdOutputBackup = Fd

surpressStdOutput :: IO StdOutputBackup
surpressStdOutput = do
  hFlush stdout
  stdOutputBackup <- dup stdOutput
  closeFd stdOutput
  -- Will use next available file descriptor: that is, stdout
  _ <- openFd (BSSC.pack "/dev/null") WriteOnly Nothing defaultFileFlags
  return stdOutputBackup

restoreStdOutput :: StdOutputBackup -> IO ()
restoreStdOutput stdOutputBackup = do
  hFlush stdout
  closeFd stdOutput
  dup stdOutputBackup
  closeFd stdOutputBackup
