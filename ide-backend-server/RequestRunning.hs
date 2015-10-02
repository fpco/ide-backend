{-# LANGUAGE CPP, TemplateHaskell, ScopedTypeVariables #-}

-- A module that handles request running. This is taken out of Server to allow
-- for conditional compilation on non-Unix platforms.
module RequestRunning (runCommand) where

import IdeSession.RPC.API
import IdeSession.GHC.Requests
import GhcMonad(Ghc(..))

#ifdef VERSION_unix

-- Unix specific imports
import System.Posix (createSession)
import System.Posix.Process (forkProcess, getProcessStatus)
import System.Posix.Terminal (openPseudoTerminal)
#ifdef PTY_SUPPORT
import qualified Posix
#endif

-- General imports
import Network
import Control.Concurrent (ThreadId, throwTo, forkIO, myThreadId)
import Control.Concurrent.Async (async, withAsync)
import Control.Concurrent.MVar (MVar, newEmptyMVar)
import Control.Monad (void, unless, forever)
import Data.Function (fix)
import Foreign.C.Types (CFile)
import Foreign.Ptr (Ptr, nullPtr)
import GHC.IO.Exception (IOException(..), IOErrorType(..))
import System.Environment (withArgs, getEnvironment, setEnv)
import System.IO (Handle, hFlush, hClose, IOMode(..))
import System.IO.Temp (openTempFile)
import System.Mem (performGC)
import qualified Control.Exception as Ex
import qualified Data.ByteString   as BSS
import qualified System.Directory  as Dir

import qualified GHC

import IdeSession.RPC.Server
import IdeSession.GHC.API
import IdeSession.RPC.Sockets
import IdeSession.Types.Private
import IdeSession.Util
import IdeSession.Util.PortableProcess
import IdeSession.Util.PortableIO
import IdeSession.Util.BlockingOps
import Run
import Auxiliary
import Debug

foreign import ccall "fflush" fflush :: Ptr CFile -> IO ()

#else

import qualified Control.Exception as Ex
import Control.Monad.Trans (liftIO)
import IdeSession.Util

#endif


runCommand :: RpcConversation -> [String] -> FilePath -> RunCmd -> Ghc [String]


#ifndef VERSION_unix
runCommand RpcConversation{..} args _ _ = do
  liftIO $ put $ ReqRunUnsupported "Cannot run commands on non-Unix platforms"
  return args
#else
runCommand conv args sessionDir runCmd
  | runCmdPty runCmd = do
#if PTY_SUPPORT
        fds <- liftIO openPseudoTerminal
        conversationTuple <- startConcurrentConversation sessionDir $ \_ _ _ ->
          ghcWithArgs args $ ghcHandleRunPtySlave fds runCmd
        liftIO $ runPtyMaster fds conversationTuple
        rpcConversationTuple conv conversationTuple
        return args
#else
        --TODO: fail more gracefully than this?
        fail "ide-backend-server not build with -DPTY_SUPPORT / pty-support cabal flag"
#endif
  | otherwise = do
        conversationTuple <- startConcurrentConversation sessionDir $
          ghcConcurrentConversation $ \_errorLog' conv' ->
            ghcWithArgs args $ ghcHandleRun conv' runCmd
        rpcConversationTuple conv conversationTuple
        return args

rpcConversationTuple :: RpcConversation -> (Pid, Socket, Socket, FilePath) -> Ghc ()
rpcConversationTuple RpcConversation{..} (pid, stdin, stdout, errorLogPath) = do
  [stdinPort, stdoutPort] <- liftIO $ mapM socketPort [stdin, stdout]
  liftIO $ put $ ReqRunConversation pid (WriteChannel stdinPort) (ReadChannel stdoutPort) errorLogPath

-- | Handle a run request
ghcHandleRun :: RpcConversation -> RunCmd -> Ghc ()
ghcHandleRun RpcConversation{..} runCmd = do
    (stdOutputRd, stdOutputBackup, stdErrorBackup) <- redirectStdout
    (stdInputWr,  stdInputBackup)                  <- redirectStdin

    -- We don't need to keep a reference to the reqThread: when the snippet
    -- terminates, the whole server process terminates with it and hence
    -- so does the reqThread. If we wanted to reuse this server process we
    -- would need to have some sort of handshake so make sure that the client
    -- and the server both agree that further requests are no longer accepted
    -- (we used to do that when we ran snippets inside the main server process).
    ghcThread    <- liftIO newEmptyMVar :: Ghc (MVar (Maybe ThreadId))
    _reqThread   <- liftIO . async $ readRunRequests ghcThread stdInputWr
    stdoutThread <- liftIO . async $ readStdout stdOutputRd

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

    runOutcome <- ghandle ghcException . ghandleJust isUserInterrupt return $
      GHC.gbracket
        (liftIO (myThreadId >>= $putMVar ghcThread . Just))
        (\() -> liftIO $ $modifyMVar_ ghcThread (\_ -> return Nothing))
        (\() -> runInGhc runCmd)

    liftIO $ do
      -- Make sure the C buffers are also flushed before swapping the handles
      fflush nullPtr

      -- Restore stdin and stdout
      dupTo stdOutputBackup stdOutput >> closeFd stdOutputBackup
      dupTo stdErrorBackup  stdError  >> closeFd stdErrorBackup
      dupTo stdInputBackup  stdInput  >> closeFd stdInputBackup

      -- Closing the write end of the stdout pipe will cause the stdout
      -- thread to terminate after it processed all remaining output;
      -- wait for this to happen
      $wait stdoutThread

      -- Report the final result
      liftIO $ debug dVerbosity $ "returned from ghcHandleRun with "
                                  ++ show runOutcome
      put $ GhcRunDone runOutcome
  where
    -- Wait for and execute run requests from the client
    readRunRequests :: MVar (Maybe ThreadId) -> Handle -> IO ()
    readRunRequests ghcThread stdInputWr =
      let go = do request <- get
                  case request of
                    GhcRunInterrupt -> do
                      $withMVar ghcThread $ \mTid -> do
                        case mTid of
                          Just tid -> throwTo tid Ex.UserInterrupt
                          Nothing  -> return () -- See above
                      go
                    GhcRunInput bs -> do
                      BSS.hPut stdInputWr bs
                      hFlush stdInputWr
                      go
      in go

    -- Wait for the process to output something or terminate
    readStdout :: Handle -> IO ()
    readStdout stdOutputRd =
      let go = do
            mbs <- Ex.try $ BSS.hGetSome stdOutputRd blockSize
            case mbs of
              Right bs -> unless (BSS.null bs) $ put (GhcRunOutp bs) >> go
              -- hGetSome might throw some very unpleasant exceptions in case
              -- someone force cancels the operation
              -- (which triggered some weird race conditions in test220 in Issues.hs)
              -- Swallowing everything, since an exception here implies that there
              -- is no more output to read
              Left (_ :: Ex.SomeException) -> return ()
      in go

    -- Turn an asynchronous exception into a RunResult
    isUserInterrupt :: Ex.AsyncException -> Maybe RunResult
    isUserInterrupt ex@Ex.UserInterrupt =
      Just . RunProgException . showExWithClass . Ex.toException $ ex
    isUserInterrupt _ =
      Nothing

    -- Turn a GHC exception into a RunResult
    ghcException :: GhcException -> Ghc RunResult
    ghcException = return . RunGhcException . show

    -- TODO: What is a good value here?
    blockSize :: Int
    blockSize = 4096

    -- Setup loopback pipe so we can capture runStmt's stdout/stderr
    redirectStdout :: Ghc (Handle, FileDescriptor, FileDescriptor)
    redirectStdout = liftIO $ do
      -- Create pipe
      (stdOutputRd, stdOutputWr) <- liftIO createPipe

      -- Backup stdout, then replace stdout and stderr with the pipe's write end
      stdOutputBackup <- liftIO $ dup stdOutput
      stdErrorBackup  <- liftIO $ dup stdError
      _ <- dupTo stdOutputWr stdOutput
      _ <- dupTo stdOutputWr stdError
      closeFd stdOutputWr

      -- Convert to the read end to a handle and return
      stdOutputRd' <- fdToHandle stdOutputRd ReadMode
      return (stdOutputRd', stdOutputBackup, stdErrorBackup)

    -- Setup loopback pipe so we can write to runStmt's stdin
    redirectStdin :: Ghc (Handle, FileDescriptor)
    redirectStdin = liftIO $ do
      -- Create pipe
      (stdInputRd, stdInputWr) <- liftIO createPipe

      -- Swizzle stdin
      stdInputBackup <- liftIO $ dup stdInput
      _ <- dupTo stdInputRd stdInput
      closeFd stdInputRd

      -- Convert the write end to a handle and return
      stdInputWr' <- fdToHandle stdInputWr WriteMode
      return (stdInputWr', stdInputBackup)

#if PTY_SUPPORT

runPtyMaster :: (FileDescriptor, FileDescriptor) -> (Pid, Socket, Socket, FilePath) -> IO ()
runPtyMaster (masterFd, slaveFd) (processId, stdin, stdout, errorLog) = do
  -- Since we're in the master process, close the slave FD.
  closeFd slaveFd

  let readOutput :: RpcConversation -> IO RunResult
      readOutput conv = fix $ \loop -> do
        bs <- Posix.readChunk masterFd `Ex.catch` \ex ->
          -- Ignore HardwareFaults as they seem to always happen when
          -- process exits..
          if ioe_type ex == HardwareFault
            then return BSS.empty
            else Ex.throwIO ex
        if BSS.null bs
          then return RunOk
          else do
            put conv (GhcRunOutp bs)
            loop
      handleRequests :: RpcConversation -> IO ()
      handleRequests conv = forever $ do
        request <- get conv
        case request of
          GhcRunInput bs -> Posix.write masterFd bs
          -- Fork a new thread because this could throw exceptions.
          GhcRunInterrupt -> void $ forkIO $ sigKillProcess processId
      -- Turn a GHC exception into a RunResult
      ghcException :: GhcException -> IO RunResult
      ghcException = return . RunGhcException . show


  void $ forkIO $
    concurrentConversation stdin stdout errorLog $ \_ conv -> do
      result <- Ex.handle ghcException $
        withAsync (handleRequests conv) $ \_ ->
        readOutput conv
      put conv (GhcRunDone result)

ghcHandleRunPtySlave :: (FileDescriptor, FileDescriptor) -> RunCmd -> Ghc ()
ghcHandleRunPtySlave (masterFd, slaveFd) runCmd = do
  liftIO $ do
    -- Since we're in the slave process, close the master FD.
    closeFd masterFd
    -- Create a new session with a controlling terminal.
    void createSession
    Posix.setControllingTerminal slaveFd
    -- Redirect standard IO to the terminal FD.
    void $ dupTo slaveFd stdInput
    void $ dupTo slaveFd stdOutput
    void $ dupTo slaveFd stdError
    closeFd slaveFd
    -- Set TERM env variable
    setEnv "TERM" "xterm-256color"
  --FIXME: Properly pass the run result to the client as a GhcRunDone
  --value. Instead, we write it to standard output, which gets sent to
  --the terminal.
  result <- runInGhc runCmd
  case result of
    -- A successful result will be sent by runPtyMaster - only send
    -- failures.
    RunOk -> return ()
    _ -> liftIO $ putStrLn $ "\r\nProcess done: " ++ show result ++ "\r\n"

#endif

startConcurrentConversation
  :: FilePath
  -> (Socket -> Socket -> FilePath -> Ghc ())
  -> Ghc (Pid, Socket, Socket, FilePath)
startConcurrentConversation sessionDir inner = do
  -- Ideally, we'd have the child process create the temp directory and
  -- communicate the name back to us, so that the child process can remove the
  -- directories again when it's done with it. However, this means we need some
  -- interprocess communication, which is awkward. So we create the temp
  -- directory here; I suppose we could still delegate the responsibility of
  -- deleting the directory to the child, but instead we'll just remove the
  -- directory along with the rest of the session temp dirs on session exit.
  (stdin, stdout, errorLog) <- liftIO $ do
    stdin <- makeSocket
    stdout <- makeSocket

    tmpDir <- Dir.getTemporaryDirectory
    (errorLogPath, errorLogHandle) <- openTempFile tmpDir "rpc-snippet-.log"
    hClose errorLogHandle

    return (stdin, stdout, errorLogPath)

  -- Start the concurrent conversion. We use forkGhcProcess rather than forkGhc
  -- because we need to change global state in the child process; in particular,
  -- we need to redirect stdin, stdout, and stderr (as well as some other global
  -- state, including withArgs).
  liftIO performGC
  processId <- forkGhcProcess $ inner stdin stdout errorLog

  -- We wait for the process to finish in a separate thread so that we do not
  -- accumulate zombies
  --
  -- FIXME(mgs): I didn't write this, and I'm not sure I see the point
  -- of doing this.
  liftIO $ void $ forkIO $
    void $ getProcessStatus True False processId

  return (processId, stdin, stdout, errorLog)
#endif
