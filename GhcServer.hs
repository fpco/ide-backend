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
  , RunActions(..)
-- TODO: a bug in haddock ignores the 3 ops:  , RunActions(interrupt, runWait, supplyStdin)
  , RunResult(..)
  , runWaitAll
  , afterRunActions
  , shutdownGhcServer
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread, myThreadId, throwTo)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, readMVar)
import qualified Control.Exception as Ex
import Control.Monad (forM, forever, when)
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString as BSS (ByteString, hGetSome, hPut, null)
import qualified Data.ByteString.Char8 as BSSC (pack)
import qualified Data.ByteString.Lazy as BSL (ByteString, fromChunks)
import Data.IORef

import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.Posix (Fd)
import System.Posix.Env (setEnv, unsetEnv)
import System.Posix.IO.ByteString

import Common
import GhcRun
import ModuleName (LoadedModules, ModuleName)
import RpcServer

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
  deriving Show

$(deriveJSON id ''GhcRequest)
$(deriveJSON id ''GhcCompileResponse)
$(deriveJSON id ''GhcRunResponse)
$(deriveJSON id ''GhcRunRequest)

type GhcServer = RpcServer

--------------------------------------------------------------------------------
-- Server-side operations                                                     --
--------------------------------------------------------------------------------

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
    -- Setup loopback pipe so we can capture runStmt's stdout/stderr
    (stdOutputRd, stdOutputBackup, stdErrorBackup) <- liftIO $ do
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

    -- Similar procedure for stdin
    (stdInputWr, stdInputBackup) <- liftIO $ do
      -- Create pipe
      (stdInputRd, stdInputWr) <- liftIO createPipe

      -- Swizzle stdin
      stdInputBackup <- liftIO $ dup stdInput
      dupTo stdInputRd stdInput
      closeFd stdInputRd

      -- Convert the write end to a handle and return
      stdInputWr' <- fdToHandle stdInputWr
      return (stdInputWr', stdInputBackup)

    ghcThread    <- liftIO $ myThreadId
    stdoutThread <- liftIO $ newEmptyMVar

    runOutcome <- ghandleJust isUserInterrupt return $ do
      -- Start thread to read 'interrupt' requests from the client
      reqThread <- liftIO . forkIO $ do
        let go = do request <- get
                    case request of
                      GhcRunInterrupt -> do
                        -- We terminate after receiving GhcRunInterrupt
                        throwTo ghcThread Ex.UserInterrupt
                      GhcRunInput bs -> do
                        BSS.hPut stdInputWr bs
                        hFlush stdInputWr
                        go
        go

      -- Start thread to read runStmt's stdout
      liftIO . forkIO $ do
        let go = do bs <- BSS.hGetSome stdOutputRd blockSize
                    if BSS.null bs
                      then putMVar stdoutThread ()
                      else put (GhcRunOutp bs) >> go
        go

      -- Actually start the code
      outcome <- runInGhc (m, fun)

      -- Kill request thread and return the result
      liftIO $ killThread reqThread
      return outcome

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
  where
    isUserInterrupt :: Ex.AsyncException -> Maybe RunResult
    isUserInterrupt ex@Ex.UserInterrupt =
      Just . RunProgException . showExWithClass . Ex.toException $ ex
    isUserInterrupt _ =
      Nothing

    -- TODO: What is a good value here?
    blockSize :: Int
    blockSize = 4096

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

-- TODO: move this and a few other operations to a separate module.
-- Then do not expose GhcServer module in cabal.
-- | Handles to the running code, through which one can interact with the code.
data RunActions = RunActions {
    runWait     :: IO (Either BSS.ByteString RunResult)
  , interrupt   :: IO ()
  , supplyStdin :: BSS.ByteString -> IO ()
  }

-- | Run code
rpcRun :: GhcServer       -- ^ GHC server
       -> ModuleName      -- ^ Module
       -> String          -- ^ Function
       -> IO RunActions
rpcRun server m fun = do
  runWaitChan   <- newChan      :: IO (Chan (Either BSS.ByteString RunResult))
  reqChan       <- newChan      :: IO (Chan GhcRunRequest)
  reqThreadMVar <- newEmptyMVar :: IO (MVar ThreadId)

  forkIO $ rpcConversation server $ \RpcConversation{..} -> do
    put (ReqRun m fun)
    forkIO (forever $ readChan reqChan >>= put) >>= putMVar reqThreadMVar
    let go = do resp <- get
                case resp of
                  GhcRunDone runResult -> writeChan runWaitChan (Right runResult)
                  GhcRunOutp bs        -> writeChan runWaitChan (Left bs) >> go
    go

  reqThreadId <- readMVar reqThreadMVar

  return RunActions {
      runWait = do
        outcome <- readChan runWaitChan
        case outcome of
          Left  _ -> return ()
          Right _ -> killThread reqThreadId
        return outcome
    , interrupt   = writeChan reqChan GhcRunInterrupt
    , supplyStdin = writeChan reqChan . GhcRunInput
    }

-- | Set the environment
rpcSetEnv :: GhcServer -> [(String, Maybe String)] -> IO ()
rpcSetEnv server env = rpc server (ReqSetEnv env)

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

-- | Register a callback to be invoked when the program terminates
afterRunActions :: RunActions -> (RunResult -> IO ()) -> RunActions
afterRunActions runActions callback = runActions {
    runWait = do result <- runWait runActions
                 case result of
                   Left bs -> return (Left bs)
                   Right r -> callback r >> return (Right r)
  }

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
