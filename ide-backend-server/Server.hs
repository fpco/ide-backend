{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, CPP #-}
-- | Implementation of the server that controls the long-running GHC instance.
-- This interacts with the ide-backend library through serialized data only.
module Server (ghcServer) where

import Prelude hiding (mod, span)
import Control.Concurrent (ThreadId, throwTo, forkIO, myThreadId, threadDelay)
import Control.Concurrent.Async (async, withAsync)
import Control.Concurrent.MVar (MVar, newEmptyMVar)
import Control.Monad (void, unless, when, forever)
import Control.Monad.State (StateT, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Accessor (accessor, (.>))
import Data.Accessor.Monad.MTL.State (set)
import Data.Function (on, fix)
import Foreign.C.Types (CFile)
import Foreign.Ptr (Ptr, nullPtr)
import GHC.IO.Exception (IOException(..), IOErrorType(..))
import System.Environment (withArgs, getEnvironment, setEnv)
import System.FilePath ((</>))
import System.IO (Handle, hFlush, hClose)
import System.IO.Temp (createTempDirectory, openTempFile)
import System.Mem (performGC)
import System.Posix (Fd, createSession)
import System.Posix.IO
import System.Posix.Files (createNamedPipe)
import System.Posix.Process (forkProcess, getProcessStatus)
import System.Posix.Terminal (openPseudoTerminal)
import System.Posix.Signals (signalProcess, sigKILL, sigTERM)
import qualified Posix
import System.Posix.Types (ProcessID)
import qualified Control.Exception as Ex
import qualified Data.ByteString   as BSS
import qualified Data.List         as List
import qualified Data.Text         as Text
import qualified System.Directory  as Dir

import IdeSession.GHC.API
import IdeSession.RPC.Server
import IdeSession.Strict.Container
import IdeSession.Strict.IORef
import IdeSession.Types.Private
import IdeSession.Types.Progress
import IdeSession.Util
import IdeSession.Util.BlockingOps
import qualified IdeSession.Strict.List  as StrictList
import qualified IdeSession.Strict.Map   as StrictMap
import qualified IdeSession.Types.Public as Public

import qualified GHC
import GhcMonad(Ghc(..))
import qualified ObjLink as ObjLink
import qualified Linker  as Linker
import Hooks

import Run
import HsWalk
import Debug
import GhcShim

foreign import ccall "fflush" fflush :: Ptr CFile -> IO ()

--------------------------------------------------------------------------------
-- Server-side operations                                                     --
--------------------------------------------------------------------------------

-- | Start the RPC server. Used from within the server executable.
ghcServer :: [String] -> IO ()
ghcServer = rpcServer ghcServerEngine

-- | The GHC server engine proper.
--
-- This function runs in end endless loop inside the @Ghc@ monad, making
-- incremental compilation possible.
ghcServerEngine :: FilePath -> RpcConversation -> IO ()
ghcServerEngine errorLog conv@RpcConversation{..} = do
  -- The initial handshake with the client
  (configGenerateModInfo, initOpts, sessionDir) <- handleInit conv
  let distDir   = ideSessionDistDir   sessionDir
      sourceDir = last initOpts

  -- Submit static opts and get back leftover dynamic opts.
  dOpts <- submitStaticOpts initOpts

  -- Set up references for the current session of Ghc monad computations.
  pluginRef  <- newIORef StrictMap.empty
  importsRef <- newIORef StrictMap.empty
  stRef      <- newIORef initExtractIdsSuspendedState
  errsRef    <- liftIO $ newIORef StrictList.nil

  -- Get environment on server startup so that we can restore it
  initEnv <- getEnvironment

  -- Start handling requests. From this point on we don't leave the GHC monad.
  runFromGhc $ do
    -- Register startup options and perhaps our plugin in dynamic flags.
    initSession distDir configGenerateModInfo dOpts errsRef stRef pluginRef progressCallback

    -- We store the DynFlags _after_ setting the "static" options, so that
    -- we restore to this state before every call to updateDynamicOpts
    -- Note that this happens _after_ we have called setSessionDynFlags
    -- and hence after the package DB has been initialized.
    storeDynFlags

    -- Make sure that the dynamic linker has been initialized. This is done
    -- implicitly deep in the bowels of GhcMake.load, but if we attempt to load
    -- C object files before calling GhcMake.load (i.e., before attempting to
    -- compile any Haskell code) then loading the object files will fail if
    -- they rely on linker flags such as @-lz@ (#214).
    do dflags <- getSessionDynFlags
       liftIO $ Linker.initDynLinker dflags

    -- Start handling RPC calls
    let go args = do
          req <- liftIO get
          args' <- case req of
            ReqCompile genCode targets -> do
              ghcHandleCompile
                conv pluginRef importsRef errsRef sourceDir
                genCode targets configGenerateModInfo
              return args
            ReqRun runCmd
              | runCmdPty runCmd -> do
                fds <- liftIO openPseudoTerminal
                conversationTuple <- startConcurrentConversation sessionDir $ \_ _ _ ->
                  ghcWithArgs args $ ghcHandleRunPtySlave fds runCmd
                liftIO $ runPtyMaster fds conversationTuple
                liftIO $ put conversationTuple
                return args
              | otherwise -> do
                conversationTuple <- startConcurrentConversation sessionDir $
                  ghcConcurrentConversation $ \_errorLog' conv' ->
                    ghcWithArgs args $ ghcHandleRun conv' runCmd
                liftIO $ put conversationTuple
                return args
            ReqSetEnv env -> do
              ghcHandleSetEnv conv initEnv env
              return args
            ReqSetArgs args' -> do
              liftIO $ put ()
              return args'
            ReqBreakpoint mod span value -> do
              ghcHandleBreak conv mod span value
              return args
            ReqPrint vars bind forceEval -> do
              ghcHandlePrint conv vars bind forceEval
              return args
            ReqLoad objects -> do
              ghcHandleLoad errorLog conv objects
              return args
            ReqUnload objects -> do
              ghcHandleUnload conv objects
              return args
            ReqSetGhcOpts opts -> do
              ghcHandleSetOpts conv opts
              return args
            ReqCrash delay -> do
              ghcHandleCrash delay
              return args
          go args'

    go []

  where
    progressCallback :: String -> IO ()
    progressCallback ghcMsg = do
      let ghcMsg' = Text.pack ghcMsg
      case parseProgressMessage ghcMsg' of
        Right (step, numSteps, msg) ->
          put $ GhcCompileProgress $ Progress {
               progressStep      = step
             , progressNumSteps  = numSteps
             , progressParsedMsg = Just msg
             , progressOrigMsg   = Just ghcMsg'
             }
        _ ->
          -- Ignore messages we cannot parse
          return ()

-- Register startup options and perhaps our plugin in dynamic flags.
--
-- This is the only place where the @packageDbArgs@ options are used
-- and indeed, as the first invocation of @setSessionDynFlags@,
-- this is the only place they could take any effect.
-- This also implies that any options specifying package DBs
-- passed via @updateGhcOptions@ won't have any effect in GHC API
initSession :: FilePath
            -> Bool
            -> DynamicOpts
            -> StrictIORef (Strict [] SourceError)
            -> StrictIORef ExtractIdsSuspendedState
            -> StrictIORef (Strict (Map ModuleName) PluginResult)
            -> (String -> IO ())
            -> Ghc ()
initSession distDir modInfo dOpts errsRef stRef pluginRef callback = do
    flags          <- getSessionDynFlags
    (flags', _, _) <- parseDynamicFlags flags $ dOpts ++ dynOpts

    let flags'' = (if modInfo then installHooks else id)
                . installErrorLoggers
                $ flags'

    void $ setSessionDynFlags flags''
  where
    dynOpts :: DynamicOpts
    dynOpts = optsToDynFlags [
        -- Just in case the user specified -hide-all-packages.
        "-package ide-backend-rts"

        -- Include cabal_macros.h
      , "-optP-include"
      , "-optP" ++ cabalMacrosLocation distDir
      ]

    installHooks :: DynFlags -> DynFlags
    installHooks dflags = dflags {
        hooks = (hooks dflags) {
            hscFrontendHook   = Just $ runHscPlugin pluginRef stRef
          , runQuasiQuoteHook = Just $ runHscQQ stRef
          , runRnSpliceHook   = Just $ runRnSplice stRef
          }
      }

    installErrorLoggers :: DynFlags -> DynFlags
    installErrorLoggers dflags = dflags {
#if __GLASGOW_HASKELL__ >= 706
        GHC.log_action = collectSrcError errsRef callback (\_ -> return ()) -- TODO: log?
#else
        GHC.log_action = collectSrcError errsRef callback (\_ -> return ()) dflags
#endif
      }

startConcurrentConversation
  :: FilePath
  -> (FilePath -> FilePath -> FilePath -> Ghc ())
  -> Ghc (ProcessID, FilePath, FilePath, FilePath)
startConcurrentConversation sessionDir inner = do
  -- Ideally, we'd have the child process create the temp directory and
  -- communicate the name back to us, so that the child process can remove the
  -- directories again when it's done with it. However, this means we need some
  -- interprocess communication, which is awkward. So we create the temp
  -- directory here; I suppose we could still delegate the responsibility of
  -- deleting the directory to the child, but instead we'll just remove the
  -- directory along with the rest of the session temp dirs on session exit.
  (stdin, stdout, errorLog) <- liftIO $ do
    tempDir <- createTempDirectory sessionDir "rpc."
    let stdin  = tempDir </> "stdin"
        stdout = tempDir </> "stdout"

    createNamedPipe stdin  0o600
    createNamedPipe stdout 0o600

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

-- | We cache our own "module summaries" in between compile requests
data ModSummary = ModSummary {
    -- | We cache the import lists so that we can check if the import
    -- list for this module has changed, and hence whether we need to recompute
    -- autocompletion info
    modImports   :: !(Strict [] Import)
    -- | We cache the file stamp to see if the file has changed at all, and
    -- hence whether we need to recompute the import list
  , modTimestamp :: !GhcTime
    -- | We cache whether this module was reported as "loaded" before so that
    -- we can see which modules got unloaded
  , modIsLoaded :: !Bool
  }

-- | Client handshake
handleInit :: RpcConversation -> IO (Bool, [String], FilePath)
handleInit RpcConversation{..} = do
  GhcInitRequest{..} <- get

  -- Check API versions
  unless (ghcInitClientApiVersion == ideBackendApiVersion) $
    Ex.throwIO . userError $ "API version mismatch between ide-backend "
                          ++ "(" ++ show ghcInitClientApiVersion ++ ") "
                          ++ "and ide-backend-server "
                          ++ "(" ++ show ideBackendApiVersion ++ ")"

  -- Return initialization result to the client
  put GhcInitResponse {
      ghcInitVersion = ghcGetVersion
    }

  -- Setup parameters for the server
  return ( ghcInitGenerateModInfo
         , ghcInitOpts ++
           packageDBFlags ghcInitUserPackageDB ghcInitSpecificPackageDBs
         , ghcInitSessionDir
         )

-- | Handle a compile or type check request
ghcHandleCompile
  :: RpcConversation
  -> StrictIORef (Strict (Map ModuleName) PluginResult)
                         -- ^ ref where the ExtractIdsT plugin stores its data
                         -- (We clear this at the end of each call)
  -> StrictIORef (Strict (Map ModuleName) ModSummary)
                         -- ^ see doc for 'ModSummary'
  -> StrictIORef (Strict [] SourceError)
                         -- ^ the IORef where GHC stores errors
  -> FilePath            -- ^ source directory
  -> Bool                -- ^ should we generate code
  -> Public.Targets      -- ^ targets
  -> Bool                -- ^ should we generate per-module info
  -> Ghc ()
ghcHandleCompile RpcConversation{..}
                 pluginRef modsRef errsRef configSourcesDir
                 ideGenerateCode targets configGenerateModInfo = do
    -- | Half of a workaround for
    -- http://hackage.haskell.org/trac/ghc/ticket/7456.  We suppress stdout
    -- during compilation to avoid stray messages, e.g. from the linker.
    --
    -- TODO: Should we log the suppressed messages?
    (_suppressed, (errs, loadedModules, fileMap)) <-
      captureGhcOutput $ compileInGhc configSourcesDir
                                      ideGenerateCode
                                      targets
                                      errsRef

    let initialResponse = GhcCompileResult {
            ghcCompileErrors   = errs
          , ghcCompileLoaded   = force $ loadedModules
          , ghcCompileFileMap  = fileMap
          , ghcCompileCache    = error "ghcCompileCache set last"
          -- We construct the diffs incrementally
          , ghcCompileImports  = StrictMap.empty
          , ghcCompileAuto     = StrictMap.empty
          , ghcCompilePkgDeps  = StrictMap.empty
          , ghcCompileSpanInfo = StrictMap.empty
          , ghcCompileExpTypes = StrictMap.empty
          , ghcCompileUseSites = StrictMap.empty
          }

    response <- if not configGenerateModInfo
      then return initialResponse
      else do
        pluginIdMaps <- liftIO $ do
          idMaps <- readIORef pluginRef
          writeIORef pluginRef StrictMap.empty
          return idMaps

        let recompiledModules :: [ModuleName]
            recompiledModules = StrictMap.keys pluginIdMaps

            -- Strictly speaking, this check is not entirely accurate, because
            -- we ignore the package of the imported module. However, I don't
            -- think this can lead to actual problems, because if modules
            -- between packages overlap this will cause trouble elsewhere.
            gotRecompiled :: Import -> Bool
            gotRecompiled imp =
              moduleName (importModule imp) `elem` recompiledModules

            removeOldModule :: ModuleName -> StateT GhcCompileResult Ghc ()
            removeOldModule m = do
              set (importsFor m)  Remove
              set (autoFor m)     Remove
              set (spanInfoFor m) Remove
              set (expTypesFor m) Remove
              set (useSitesFor m) Remove
              set (pkgDepsFor m)  Remove

            addNewModule :: (ModuleName, GHC.ModSummary)
                         -> StateT GhcCompileResult Ghc (ModuleName, ModSummary)
            addNewModule (m, ghcSummary) = do
              imports <- lift $ importList     ghcSummary
              auto    <- lift $ autocompletion ghcSummary
              set (importsFor m) (Insert imports)
              set (autoFor m)    (Insert auto)
              -- Information computed by the plugin set separately

              let newSummary = ModSummary {
                                   modTimestamp = ms_hs_date ghcSummary
                                 , modImports   = imports
                                 , modIsLoaded  = m `elem` loadedModules
                                 }

              return (m, newSummary)

            updateSourceFile :: ModuleName -> ModSummary -> GHC.ModSummary
                         -> StateT GhcCompileResult Ghc (ModuleName, ModSummary)
            updateSourceFile m oldSummary ghcSummary = do
              (imports, importsChanged) <-
                -- We recompute imports when the file changed, rather than when
                -- it got (successfully) recompiled because we provide the
                -- imports even for modules with type errors
                if modTimestamp oldSummary == ms_hs_date ghcSummary
                  then return (modImports oldSummary, False)
                  else do imports <- lift $ importList ghcSummary
                          set (importsFor m) (Insert imports)
                          return (imports, imports /= modImports oldSummary)

              -- We recompute autocompletion info if the imported modules have
              -- been recompiled. TODO: We might be able to optimize this by
              -- checking one of ghc's various hashes to avoid recomputing
              -- autocompletion info even if an imported module got recompiled,
              -- but it's interface did not change (`mi_iface_hash` perhaps?)
              -- TODO: We might also be able to make this check more fine
              -- grained and recompute autocompletion info for some imports,
              -- but not for others.
              when (importsChanged || StrictList.any gotRecompiled imports) $ do
                auto <- lift $ autocompletion ghcSummary
                set (autoFor m) (Insert auto)

              let newSummary = ModSummary {
                                   modTimestamp = ms_hs_date ghcSummary
                                 , modImports   = imports
                                 , modIsLoaded  = m `elem` loadedModules
                                 }

              when (not (modIsLoaded newSummary)) $ do
                set (spanInfoFor m) Remove
                set (pkgDepsFor m)  Remove
                set (expTypesFor m) Remove
                set (useSitesFor m) Remove

              return (m, newSummary)

        let go :: [(ModuleName, ModSummary)]
               -> [(ModuleName, GHC.ModSummary)]
               -> StateT GhcCompileResult Ghc [(ModuleName, ModSummary)]
            go ((m, oldSummary) : old) ((m', ghcSummary) : new) = do
              case compare m m' of
                LT -> do removeOldModule m
                         go old ((m', ghcSummary) : new)
                GT -> do newSummary   <- addNewModule (m', ghcSummary)
                         newSummaries <- go ((m, oldSummary) : old) new
                         return $ newSummary : newSummaries
                EQ -> do newSummary   <- updateSourceFile m oldSummary ghcSummary
                         newSummaries <- go old new
                         return $ newSummary : newSummaries
            go old new = do
              mapM_ removeOldModule (map fst old)
              mapM addNewModule new

        let sendPluginResult :: [(ModuleName, PluginResult)]
                             -> StateT GhcCompileResult Ghc ()
            sendPluginResult = mapM_ $ \(m, PluginResult{..}) -> do
              set (spanInfoFor m) (Insert pluginIdList)
              set (pkgDepsFor m)  (Insert pluginPkgDeps)
              set (expTypesFor m) (Insert pluginExpTypes)
              set (useSitesFor m) (Insert pluginUseSites)

        (newSummaries, finalResponse) <- flip runStateT initialResponse $ do
          sendPluginResult (StrictMap.toList pluginIdMaps)

          graph <- lift $ getModuleGraph
          let name s      = Text.pack (moduleNameString (ms_mod_name s))
              namedGraph  = map (\s -> (name s, s)) graph
              sortedGraph = List.sortBy (compare `on` fst) namedGraph
          oldSummaries <- lift . liftIO $ readIORef modsRef
          go (StrictMap.toList oldSummaries) sortedGraph

        liftIO $ writeIORef modsRef (StrictMap.fromList newSummaries)
        return finalResponse

    cache <- liftIO $ constructExplicitSharingCache
    let fullResponse = response { ghcCompileCache = cache }

    -- TODO: Should we clear the link env caches here?
    liftIO $ put (GhcCompileDone fullResponse)
  where
    -- Various accessors
    allImports  = accessor ghcCompileImports  (\is st -> st { ghcCompileImports  = is })
    allAuto     = accessor ghcCompileAuto     (\as st -> st { ghcCompileAuto     = as })
    allSpanInfo = accessor ghcCompileSpanInfo (\ss st -> st { ghcCompileSpanInfo = ss })
    allPkgDeps  = accessor ghcCompilePkgDeps  (\ds st -> st { ghcCompilePkgDeps  = ds })
    allExpTypes = accessor ghcCompileExpTypes (\ts st -> st { ghcCompileExpTypes = ts })
    allUseSites = accessor ghcCompileUseSites (\us st -> st { ghcCompileUseSites = us })

    importsFor  m = allImports  .> StrictMap.accessorDefault Keep m
    autoFor     m = allAuto     .> StrictMap.accessorDefault Keep m
    spanInfoFor m = allSpanInfo .> StrictMap.accessorDefault Keep m
    pkgDepsFor  m = allPkgDeps  .> StrictMap.accessorDefault Keep m
    expTypesFor m = allExpTypes .> StrictMap.accessorDefault Keep m
    useSitesFor m = allUseSites .> StrictMap.accessorDefault Keep m

-- | Handle a break request
ghcHandleBreak :: RpcConversation -> ModuleName -> Public.SourceSpan -> Bool -> Ghc ()
ghcHandleBreak RpcConversation{..} modName span value = do
  oldValue <- breakFromSpan modName span value
  liftIO $ put oldValue

-- | Handle a print request
ghcHandlePrint :: RpcConversation -> Public.Name -> Bool -> Bool -> Ghc ()
ghcHandlePrint RpcConversation{..} var bind forceEval = do
  vals <- printVars (Text.unpack var) bind forceEval
  liftIO $ put vals

-- | Handle a load object request
ghcHandleLoad :: FilePath -> RpcConversation -> [FilePath] -> Ghc ()
ghcHandleLoad errorLog RpcConversation{..} objects =
  liftIO $ do
    -- If loadObj fails, it fails with a hard crash (not an exception) and
    -- hence we cannot capture the output. Instead, we redirect it to the
    -- error log so that if the crash does happen, the RPC infastructure
    -- will read this log file and use its constents to report an error.
    redirectStderr errorLog $ mapM_ ObjLink.loadObj objects

    -- Although resolveObjs does _not_ fail quite so spectacularly, it still
    -- writes its error messages to stdout.
    (suppressed, success) <- captureOutput $ ObjLink.resolveObjs
    let response :: Maybe String
        response =
          case success of
            GHC.Failed    -> Just suppressed
            GHC.Succeeded -> Nothing
    put response

-- | Handle an unload object request
ghcHandleUnload :: RpcConversation -> [FilePath] -> Ghc ()
ghcHandleUnload RpcConversation{..} objects = liftIO $ do
  mapM_ ObjLink.unloadObj objects
  put ()

runPtyMaster :: (Fd, Fd) -> (ProcessID, FilePath, FilePath, FilePath) -> IO ()
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
          GhcRunInterrupt -> void $ forkIO $ signalProcess sigTERM processId
      -- Turn a GHC exception into a RunResult
      ghcException :: GhcException -> IO RunResult
      ghcException = return . RunGhcException . show
  void $ forkIO $
    concurrentConversation stdin stdout errorLog $ \_ conv -> do
      result <- Ex.handle ghcException $
        withAsync (handleRequests conv) $ \_ ->
        readOutput conv
      put conv (GhcRunDone result)

ghcHandleRunPtySlave :: (Fd, Fd) -> RunCmd -> Ghc ()
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
      _ <- dupTo stdOutputWr stdOutput
      _ <- dupTo stdOutputWr stdError
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
      _ <- dupTo stdInputRd stdInput
      closeFd stdInputRd

      -- Convert the write end to a handle and return
      stdInputWr' <- fdToHandle stdInputWr
      return (stdInputWr', stdInputBackup)

-- | Handle a set-environment request
ghcHandleSetEnv :: RpcConversation -> [(String, String)] -> [(String, Maybe String)] -> Ghc ()
ghcHandleSetEnv RpcConversation{put} initEnv overrides = liftIO $ do
  setupEnv initEnv overrides
  put ()

-- | Set ghc options
ghcHandleSetOpts :: RpcConversation -> [String] -> Ghc ()
ghcHandleSetOpts RpcConversation{put} opts = do
  (leftover, warnings) <- setGhcOptions opts
  liftIO $ put (leftover, warnings)

-- | Handle a crash request (debugging)
ghcHandleCrash :: Maybe Int -> Ghc ()
ghcHandleCrash delay = liftIO $ do
    case delay of
      Nothing -> Ex.throwIO crash
      Just i  -> do tid <- myThreadId
                    void . forkIO $ threadDelay i >> throwTo tid crash
  where
    crash = userError "Intentional crash"

--------------------------------------------------------------------------------
-- Auxiliary                                                                  --
--------------------------------------------------------------------------------

-- | Generalization of captureOutput
captureGhcOutput :: Ghc a -> Ghc (String, a)
captureGhcOutput = unsafeLiftIO captureOutput

-- | Lift operations on `IO` to the `Ghc` monad. This is unsafe as it makes
-- operations possible in the `Ghc` monad that weren't possible before
-- (for instance, @unsafeLiftIO forkIO@ is probably a bad idea!).
unsafeLiftIO :: (IO a -> IO b) -> Ghc a -> Ghc b
unsafeLiftIO f (Ghc ghc) = Ghc $ \session -> f (ghc session)

-- | Generalization of 'unsafeLiftIO'
_unsafeLiftIO1 :: ((c -> IO a) -> IO b) -> (c -> Ghc a) -> Ghc b
_unsafeLiftIO1 f g = Ghc $ \session ->
  f $ \c -> case g c of Ghc ghc -> ghc session

-- | Generalization of 'unsafeLiftIO'
--
-- TODO: Is there a more obvious way to define this progression?
unsafeLiftIO2 :: ((c -> d -> IO a) -> IO b) -> (c -> d -> Ghc a) -> Ghc b
unsafeLiftIO2 f g = Ghc $ \session ->
  f $ \c d -> case g c d of Ghc ghc -> ghc session

-- | Lift `withArgs` to the `Ghc` monad. Relies on `unsafeLiftIO`.
ghcWithArgs :: [String] -> Ghc a -> Ghc a
ghcWithArgs = unsafeLiftIO . withArgs

-- | Fork within the `Ghc` monad. Use with caution.
_forkGhc :: Ghc () -> Ghc ThreadId
_forkGhc = unsafeLiftIO forkIO

-- | forkProcess within the `Ghc` monad. Use with extreme caution.
forkGhcProcess :: Ghc () -> Ghc ProcessID
forkGhcProcess = unsafeLiftIO forkProcess

-- | Lifted version of concurrentConversation
ghcConcurrentConversation :: (FilePath -> RpcConversation -> Ghc ())
                          -> FilePath
                          -> FilePath
                          -> FilePath
                          -> Ghc ()
ghcConcurrentConversation f requestR responseW errorLog =
  unsafeLiftIO2 (concurrentConversation requestR responseW errorLog) f
