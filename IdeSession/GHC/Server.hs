{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
-- | Implementation of the server that controls the long-running GHC instance.
-- This is the place where the GHC-specific part joins the part
-- implementing the general RPC infrastructure.
--
-- The modules importing any GHC internals, as well as the modules
-- implementing the  RPC infrastructure, should be accessible to the rest
-- of the program only indirectly, through the @GhcServer@ module.
module IdeSession.GHC.Server
  ( -- * A handle to the server
    GhcServer
    -- * Server-side operations
  , ghcServer
    -- * Client-side operations
  , InProcess
  , forkGhcServer
  , rpcCompile
  , RunActions(..)
  , runWaitAll
  , rpcRun
  , rpcSetEnv
  , shutdownGhcServer
  , forceShutdownGhcServer
  , getGhcExitCode
  ) where

import Control.Arrow (second)
import Control.Concurrent (ThreadId, myThreadId, throwTo, forkIO, killThread)
import Control.Concurrent.Async (async, cancel, withAsync)
import Control.Concurrent.Chan (Chan, newChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar)
import qualified Control.Exception as Ex
import Control.Monad (forM_, forever, unless, when)
import Data.Aeson (decode', encode)
import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString as BSS (ByteString, hGetSome, hPut, null)
import qualified Data.ByteString.Char8 as BSSC (pack)
import qualified Data.ByteString.Lazy as BSL (ByteString, fromChunks)
import Data.IORef
import System.Exit (ExitCode)
import qualified Data.Set as Set
import qualified Data.Text as Text

import System.Directory (doesFileExist)
import System.FilePath ((</>))

import System.IO (Handle, hFlush, stdout)
import System.Posix (Fd)
import System.Posix.Env (setEnv, unsetEnv)
import System.Posix.IO.ByteString

import IdeSession.GHC.HsWalk
import IdeSession.GHC.Run
import IdeSession.RPC.Server
import IdeSession.Types.Private
import IdeSession.Types.Progress
import IdeSession.Debug
import IdeSession.Util
import IdeSession.BlockingOps (modifyMVar, modifyMVar_, putMVar, readChan, readMVar, wait)
import IdeSession.Strict.Container
import qualified IdeSession.Strict.Map    as StrictMap
import qualified IdeSession.Strict.IntMap as StrictIntMap
import qualified IdeSession.Strict.List   as StrictList
import qualified IdeSession.Strict.Trie   as StrictTrie

import Paths_ide_backend

data GhcRequest
  = ReqCompile (Maybe [String]) FilePath Bool
  | ReqRun     String String RunBufferMode RunBufferMode
  | ReqSetEnv  [(String, Maybe String)]
data GhcCompileResponse =
    GhcCompileProgress Progress
  | GhcCompileDone ( Strict [] SourceError
                   , LoadedModules
                   , Strict (Map ModuleName)
                            (Diff ( Strict [] Import
                                  , Strict [] IdInfo))
                   , ExplicitSharingCache
                   )
data GhcRunResponse =
    GhcRunOutp BSS.ByteString
  | GhcRunDone RunResult
data GhcRunRequest =
    GhcRunInput BSS.ByteString
  | GhcRunInterrupt
  | GhcRunAckDone

$(deriveJSON id ''GhcRequest)
$(deriveJSON id ''GhcCompileResponse)
$(deriveJSON id ''GhcRunResponse)
$(deriveJSON id ''GhcRunRequest)

data GhcServer = OutProcess RpcServer
               | InProcess RpcConversation ThreadId

conversation :: GhcServer -> (RpcConversation -> IO a) -> IO a
conversation (OutProcess server) = rpcConversation server
conversation (InProcess conv _)  = ($ conv)

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
ghcServerEngine :: [String] -> RpcConversation -> IO ()
ghcServerEngine staticOpts conv@RpcConversation{..} = do
  -- Submit static opts and get back leftover dynamic opts.
  dOpts <- submitStaticOpts (ideBackendRTSOpts ++ staticOpts)
  -- Set up references for the current session of Ghc monad computations.
  -- TODOs: These should become StrictIORefs to that we don't build up
  -- unnecessary thunks
  pluginRef      <- newIORef StrictMap.empty
  idMapRef       <- newIORef StrictMap.empty
  importsAutoRef <- newIORef StrictMap.empty

  -- Start handling requests. From this point on we don't leave the GHC monad.
  runFromGhc $ do
    -- Initialize the dynamic flags
    dynFlags <- getSessionDynFlags
    let dynFlags' = dynFlags {
          sourcePlugins = extractIdsPlugin pluginRef : sourcePlugins dynFlags
        }
    setSessionDynFlags dynFlags'

    -- Start handling RPC calls
    forever $ do
      req <- liftIO get
      case req of
        ReqCompile opts dir genCode ->
          ghcHandleCompile
            conv dOpts opts pluginRef idMapRef importsAutoRef dir genCode
        ReqRun m fun outBMode errBMode ->
          ghcHandleRun conv m fun outBMode errBMode
        ReqSetEnv env ->
          ghcHandleSetEnv conv env
  where
    ideBackendRTSOpts = [
        -- Just in case the user specified -hide-all-packages
        "-package ide-backend-rts"
      , "-i/Users/dev/wt/projects/fpco/ide-backend/test/Cabal"
      ]

-- | Handle a compile or type check request
ghcHandleCompile :: RpcConversation
                 -> DynamicOpts         -- ^ startup dynamic flags
                 -> Maybe [String]      -- ^ new, user-submitted dynamic flags
                 -> IORef LoadedModules -- ^ ref for newly generated IdMaps
                 -> IORef LoadedModules -- ^ ref for accumulated IdMaps
                 -> IORef (Strict (Map ModuleName) ( Strict [] Import
                                                   , Strict [] IdInfo
                                                   ))
                                        -- ^ ref for previous imports and auto
                 -> FilePath            -- ^ source directory
                 -> Bool                -- ^ should we generate code
                 -> Ghc ()
ghcHandleCompile RpcConversation{..} dOpts ideNewOpts pluginRef idMapRef
                 importsAutoRef configSourcesDir ideGenerateCode = do
    errsRef <- liftIO $ newIORef StrictList.nil
    counter <- liftIO $ newIORef initialProgress
    (  errs          :: Strict [] SourceError
     , loadedModules :: [ModuleName]
     , importsAuto   :: Strict (Map ModuleName) ( Strict [] Import
                                                  , Strict [] IdInfo)) <-
      suppressGhcStdout $ compileInGhc configSourcesDir
                                       dynOpts
                                       ideGenerateCode
                                       verbosity
                                       errsRef
                                       (progressCallback counter)
                                       (\_ -> return ()) -- TODO: log?
    cache <- liftIO $ constructExplicitSharingCache
--    liftIO $ debug dVerbosity $ "returned from compileInGhc with " ++ (unlines $ map (showNormalized cache) errs)
    -- Kyes of @pluginIdMaps@ are the modules changed in this GHC call.
    pluginIdMaps <- liftIO $ readIORef pluginRef
    accIdMaps <- liftIO $ readIORef idMapRef
    let idMaps = pluginIdMaps `StrictMap.union` accIdMaps
        loadedModulesSet = Set.fromList loadedModules
        -- Filter out modules that got unloaded.
        filteredIdMaps =
          StrictMap.filterWithKey (\k _ -> k `Set.member` loadedModulesSet) idMaps
        filteredKeySet = StrictMap.keysSet filteredIdMaps
    liftIO $ writeIORef pluginRef StrictMap.empty
    liftIO $ writeIORef idMapRef filteredIdMaps
    -- Verify the plugin and @compileInGhc@ agree on which modules are loaded.
    when (loadedModulesSet /= filteredKeySet) $ do
      error $ "ghcHandleCompile: loaded modules do not match id info maps: "
              ++ show (loadedModulesSet, filteredKeySet)
    -- Compute and send only diffs (encoded as Maybes) of the map of imports
    -- and autocompletion data. The data for a module does not need to be sent
    -- if the imports of the module are unchanged (even if the module is)
    -- and the modules it imports are not recompiled. (It's not enough
    -- to determine that their exports are unchanged (which would be
    -- reasonably fast using @mi_exp_hash@) because the types
    -- could have changed and we send full idMaps, including types.)
    -- Note that we really don't recompute the unneeded autocompletion
    -- data, because it's generated in a lazy way and we don't force it
    -- (we don't force unneeded elements of @currentImportsAuto@).
    -- TODO: is the above comment still true/relevant now that we use strict types?
    previousImportsAuto <- liftIO $ readIORef importsAutoRef
    let changedModuleSet = StrictMap.keysSet pluginIdMaps

        diff :: ModuleName
             -> (Strict [] Import, Strict [] IdInfo)
             -> Diff (Strict [] Import, Strict [] IdInfo)
        diff m ia = case StrictMap.lookup m previousImportsAuto of
            Nothing -> Insert ia
            Just iaOld
              | m `Set.member` changedModuleSet && fst iaOld /= fst ia
                    -> Insert ia
              | StrictList.any (`Set.member` changedModuleSet) imports
                    -> Insert ia
              | otherwise
                    -> Keep
          where
            imports = StrictList.map importModule $ fst ia

        removedModules = previousImportsAuto StrictMap.\\ importsAuto
        diffMap = StrictMap.mapWithKey diff importsAuto
                  `StrictMap.union` StrictMap.map (const Remove) removedModules
    liftIO $ writeIORef importsAutoRef
           $ applyMapDiff diffMap previousImportsAuto
    -- Ship the results.
    liftIO $ put $ GhcCompileDone (errs, filteredIdMaps, diffMap, cache)
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
             -> String            -- ^ Module
             -> String            -- ^ Function
             -> RunBufferMode     -- ^ Buffer mode for stdout
             -> RunBufferMode     -- ^ Buffer mode for stderr
             -> Ghc ()
ghcHandleRun RpcConversation{..} m fun outBMode errBMode = do
    (stdOutputRd, stdOutputBackup, stdErrorBackup) <- redirectStdout
    (stdInputWr,  stdInputBackup)                  <- redirectStdin

    ghcThread    <- liftIO newEmptyMVar :: Ghc (MVar (Maybe ThreadId))
    reqThread    <- liftIO . async $ readRunRequests ghcThread stdInputWr
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

    runOutcome <- ghandle ghcException . ghandleJust isUserInterrupt return $ do
      liftIO $ myThreadId >>= $putMVar ghcThread . Just
      outcome <- runInGhc (m, fun) outBMode errBMode
      liftIO $ $modifyMVar ghcThread $ \_ -> return (Nothing, outcome)

    liftIO $ do
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

      -- Wait for the client to acknowledge the done
      -- (this avoids race conditions)
      $wait reqThread
  where
    -- Wait for and execute run requests from the client
    readRunRequests :: MVar (Maybe ThreadId) -> Handle -> IO ()
    readRunRequests ghcThread stdInputWr =
      let go = do request <- get
                  case request of
                    GhcRunInterrupt -> do
                      mTid <- $readMVar ghcThread
                      case mTid of
                        Just tid -> throwTo tid Ex.UserInterrupt
                        Nothing  -> return () -- See above
                      go
                    GhcRunInput bs -> do
                      BSS.hPut stdInputWr bs
                      hFlush stdInputWr
                      go
                    GhcRunAckDone ->
                      return ()
      in go

    -- Wait for the process to output something or terminate
    readStdout :: Handle -> IO ()
    readStdout stdOutputRd =
      let go = do bs <- BSS.hGetSome stdOutputRd blockSize
                  unless (BSS.null bs) $ put (GhcRunOutp bs) >> go
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
  setupEnv env
  put ()

setupEnv :: [(String, Maybe String)] -> IO ()
setupEnv env = forM_ env $ \(var, mVal) ->
  case mVal of Just val -> setEnv var val True
               Nothing  -> unsetEnv var


--------------------------------------------------------------------------------
-- Client-side operations                                                     --
--------------------------------------------------------------------------------

type InProcess = Bool

forkGhcServer :: [String] -> Maybe String -> InProcess -> IO GhcServer
forkGhcServer opts workingDir False = do
  bindir <- getBinDir
  let prog = bindir </> "ide-backend-server"

  exists <- doesFileExist prog
  unless exists $
    fail $ "The 'ide-backend-server' program was expected to "
        ++ "be at location " ++ prog ++ " but it is not."

  server <- forkRpcServer prog (opts ++ ["--ghc-opts-end"]) workingDir
  return (OutProcess server)
forkGhcServer opts workingDir True = do
  let conv a b = RpcConversation {
                   get = do bs <- $readChan a
                            case decode' bs of
                              Just x  -> return x
                              Nothing -> fail "JSON failure"
                 , put = writeChan b . encode
                 }
  a   <- newChan
  b   <- newChan
  tid <- forkIO $ ghcServerEngine opts (conv a b)
  return $ InProcess (conv b a) tid

-- | Compile or typecheck
rpcCompile :: GhcServer           -- ^ GHC server
           -> Maybe [String]      -- ^ Options
           -> FilePath            -- ^ Source directory
           -> Bool                -- ^ Should we generate code?
           -> (Progress -> IO ()) -- ^ Progress callback
           -> IO ( Strict [] SourceError
                 , LoadedModules
                 , Strict (Map ModuleName) (Diff ( Strict [] Import
                                                 , Strict Trie (Strict [] IdInfo)
                                                 ))
                 , ExplicitSharingCache
                 )
rpcCompile server opts dir genCode callback =
  conversation server $ \RpcConversation{..} -> do
    put (ReqCompile opts dir genCode)

    let go = do response <- get
                case response of
                  GhcCompileProgress pcounter ->
                    callback pcounter >> go
                  GhcCompileDone (errs, loaded, importsAuto, cache) ->
                    return ( errs
                           , loaded
                           , StrictMap.map (fmap $ second $ constructAuto cache)
                                           importsAuto
                           , cache
                           )
    go

constructAuto :: ExplicitSharingCache -> Strict [] IdInfo -> Strict Trie (Strict [] IdInfo)
constructAuto cache lk = StrictTrie.fromListWith (StrictList.++) $ map aux (toLazyList lk)
  where
    aux :: IdInfo -> (BSS.ByteString, Strict [] IdInfo)
    aux idInfo@IdInfo{idProp = k} =
      let idProp = idPropCache cache StrictIntMap.! idPropPtr k
      in (BSSC.pack . Text.unpack . idName $ idProp, StrictList.singleton idInfo)

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

-- | Set the environment
rpcSetEnv :: GhcServer -> [(String, Maybe String)] -> IO ()
rpcSetEnv (OutProcess server) env = rpc server (ReqSetEnv env)
rpcSetEnv (InProcess _ _)     env = setupEnv env

shutdownGhcServer :: GhcServer -> IO ()
shutdownGhcServer (OutProcess server) = shutdown server
shutdownGhcServer (InProcess _ tid)   = killThread tid

forceShutdownGhcServer :: GhcServer -> IO ()
forceShutdownGhcServer (OutProcess server) = forceShutdown server
forceShutdownGhcServer (InProcess _ tid)   = killThread tid

getGhcExitCode :: GhcServer -> IO (Maybe ExitCode)
getGhcExitCode (OutProcess server) = getRpcExitCode server

--------------------------------------------------------------------------------
-- Auxiliary                                                                  --
--------------------------------------------------------------------------------

 -- Half of a workaround for http://hackage.haskell.org/trac/ghc/ticket/7456.
-- We suppress stdout during compilation to avoid stray messages, e.g. from
-- the linker.
-- TODO: send all suppressed messages to a debug log file.
suppressGhcStdout :: Ghc a -> Ghc a
suppressGhcStdout p = do
  stdOutputBackup <- liftIO suppressStdOutput
  x <- p
  liftIO $ restoreStdOutput stdOutputBackup
  return x

type StdOutputBackup = Fd

suppressStdOutput :: IO StdOutputBackup
suppressStdOutput = do
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
