{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, CPP #-}
-- | Implementation of the server that controls the long-running GHC instance.
-- This interacts with the ide-backend library through serialized data only.
module Server (ghcServer) where

import Prelude hiding (mod, span)
import Control.Concurrent (ThreadId, throwTo, forkIO, myThreadId, threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.MVar (MVar, newEmptyMVar)
import Control.Monad (void, unless, when)
import Control.Monad.State (StateT, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Accessor (accessor, (.>))
import Data.Accessor.Monad.MTL.State (set)
import Data.Function (on)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.C.Types (CFile)
import System.Environment (withArgs, getEnvironment)
import System.FilePath ((</>))
import System.IO (Handle, hFlush)
import System.IO.Temp (createTempDirectory)
import System.Mem (performGC)
import System.Posix (Fd)
import System.Posix.IO.ByteString
import System.Posix.Files (createNamedPipe)
import System.Posix.Process (forkProcess, getProcessStatus)
import System.Posix.Types (ProcessID)
import qualified Control.Exception as Ex
import qualified Data.ByteString as BSS (hGetSome, hPut, null)
import qualified Data.List as List
import qualified Data.Text as Text

import IdeSession.GHC.API
import IdeSession.RPC.Server
import IdeSession.Strict.Container
import IdeSession.Strict.IORef
import IdeSession.Types.Private
import IdeSession.Types.Progress
import IdeSession.Util
import IdeSession.Util.BlockingOps
import qualified IdeSession.Strict.List as StrictList
import qualified IdeSession.Strict.Map  as StrictMap
import qualified IdeSession.Types.Public as Public

import qualified GHC
import GhcMonad(Ghc(..))
import qualified ObjLink as Linker
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
ghcServerEngine :: RpcConversation -> IO ()
ghcServerEngine conv@RpcConversation{..} = do
  -- The initial handshake with the client
  (configGenerateModInfo, initOpts, sessionDir) <- handleInit conv
  let distDir   = ideSessionDistDir   sessionDir
      sourceDir = ideSessionSourceDir sessionDir

  -- Submit static opts and get back leftover dynamic opts.
  dOpts <- submitStaticOpts initOpts

  -- Set up references for the current session of Ghc monad computations.
  pluginRef  <- newIORef StrictMap.empty
  importsRef <- newIORef StrictMap.empty
  stRef      <- newIORef initExtractIdsSuspendedState

  -- Get environment on server startup so that we can restore it
  initEnv <- getEnvironment

  -- Start handling requests. From this point on we don't leave the GHC monad.
  runFromGhc $ do
    -- Register startup options and perhaps our plugin in dynamic flags.
    -- This is the only place where the @packageDbArgs@ options are used
    -- and indeed, as the first invocation of @setSessionDynFlags@,
    -- this is the only place they could take any effect.
    -- This also implies that any options specifying package DBs
    -- passed via @updateGhcOptions@ won't have any effect in GHC API
    flags0 <- getSessionDynFlags
    let dynOpts :: DynamicOpts
        dynOpts =
          let -- Just in case the user specified -hide-all-packages.
              rtsOpts = ["-package ide-backend-rts"]
              -- Include cabal_macros.h.
              cppOpts = [ "-optP-include"
                        , "-optP" ++ cabalMacrosLocation distDir
                        ]
          in optsToDynFlags (rtsOpts ++ cppOpts)
    (flags05, _, _) <- parseDynamicFlags flags0 $ dOpts ++ dynOpts
    errsRef <- liftIO $ newIORef StrictList.nil
    let flags1 | configGenerateModInfo = flags05 {
          hooks = (hooks flags05) {
              hscFrontendHook   = Just $ runHscPlugin pluginRef stRef
            , runQuasiQuoteHook = Just $ runHscQQ stRef
            , runRnSpliceHook   = Just $ runRnSplice stRef
            }
        }
                 | otherwise = flags05
        dynFlags = flags1
                       {
                         GHC.ghcMode    = GHC.CompManager,
#if __GLASGOW_HASKELL__ >= 706
                         GHC.log_action = collectSrcError errsRef progressCallback (\_ -> return ()) -- TODO: log?
#else
                         GHC.log_action = collectSrcError errsRef progressCallback (\_ -> return ()) dynFlags
#endif
                       }
    void $ setSessionDynFlags dynFlags

    -- We store the DynFlags _after_ setting the "static" options, so that
    -- we restore to this state before every call to updateDynamicOpts
    storeDynFlags

    -- Start handling RPC calls
    let go args = do
          req <- liftIO get
          args' <- case req of
            ReqCompile genCode targets -> do
              ghcHandleCompile
                conv pluginRef importsRef errsRef sourceDir
                genCode targets configGenerateModInfo
              return args
            ReqRun runCmd -> do
              (pid, stdin, stdout, stderr) <- startConcurrentConversation sessionDir $ \conv' -> do
                 ghcWithArgs args $ ghcHandleRun conv' runCmd
              liftIO $ put (pid, stdin, stdout, stderr)
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
            ReqLoad path unload -> do
              ghcHandleLoad conv path unload
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

startConcurrentConversation :: FilePath -> (RpcConversation -> Ghc ()) -> Ghc (ProcessID, FilePath, FilePath, FilePath)
startConcurrentConversation sessionDir server = do
  -- Ideally, we'd have the child process create the temp directory and
  -- communicate the name back to us, so that the child process can remove the
  -- directories again when it's done with it. However, this means we need some
  -- interprocess communication, which is awkward. So we create the temp
  -- directory here; I suppose we could still delegate the responsibility of
  -- deleting the directory to the child, but instead we'll just remove the
  -- directory along with the rest of the session temp dirs on session exit.
  (stdin, stdout, stderr) <- liftIO $ do
    tempDir <- createTempDirectory sessionDir "rpc."
    let stdin  = tempDir </> "stdin"
        stdout = tempDir </> "stdout"
        stderr = tempDir </> "stderr"

    createNamedPipe stdin  0o600
    createNamedPipe stdout 0o600
    createNamedPipe stderr 0o600

    return (stdin, stdout, stderr)

  -- Start the concurrent conversion. We use forkGhcProcess rather than forkGhc
  -- because we need to change global state in the child process; in particular,
  -- we need to redirect stdin, stdout, and stderr (as well as some other global
  -- state, including withArgs).
  liftIO $ performGC
  processId <- forkGhcProcess $ ghcConcurrentConversation stdin stdout stderr server

  -- We wait for the process to finish in a separate thread so that we do not
  -- accumulate zombies
  liftIO $ void $ forkIO $
    void $ getProcessStatus True False processId

  return (processId, stdin, stdout, stderr)

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
    (errs, loadedModules) <-
      suppressGhcStdout $ compileInGhc configSourcesDir
                                       ideGenerateCode
                                       targets
                                       errsRef

    let initialResponse = GhcCompileResult {
            ghcCompileErrors   = errs
          , ghcCompileLoaded   = force $ loadedModules
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
ghcHandleLoad :: RpcConversation -> FilePath -> Bool -> Ghc ()
ghcHandleLoad RpcConversation{..} path unload = do
  liftIO $ if unload then Linker.unloadObj path
                     else Linker.loadObj   path
  liftIO $ put ()

-- | Handle a run request
ghcHandleRun :: RpcConversation -> RunCmd -> Ghc ()
ghcHandleRun RpcConversation{..} runCmd = do
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

-- | Half of a workaround for http://hackage.haskell.org/trac/ghc/ticket/7456.
-- We suppress stdout during compilation to avoid stray messages, e.g. from
-- the linker.
-- TODO: send all suppressed messages to a debug log file.
suppressGhcStdout :: Ghc a -> Ghc a
suppressGhcStdout p = do
  stdOutputBackup <- liftIO suppressStdOutput
  x <- p
  liftIO $ restoreStdOutput stdOutputBackup
  return x

-- | Lift operations on `IO` to the `Ghc` monad. This is unsafe as it makes
-- operations possible in the `Ghc` monad that weren't possible before
-- (for instance, @unsafeLiftIO forkIO@ is probably a bad idea!).
unsafeLiftIO :: (IO a -> IO b) -> Ghc a -> Ghc b
unsafeLiftIO f (Ghc ghc) = Ghc $ \session -> f (ghc session)

-- | Generalization of 'unsafeLiftIO'
unsafeLiftIO' :: ((c -> IO a) -> IO b) -> (c -> Ghc a) -> Ghc b
unsafeLiftIO' f g = Ghc $ \session ->
  f $ \c -> case g c of Ghc ghc -> ghc session

-- | Lift `withArgs` to the `Ghc` monad. Relies on `unsafeLiftIO`.
ghcWithArgs :: [String] -> Ghc a -> Ghc a
ghcWithArgs = unsafeLiftIO . withArgs

-- | Fork within the `Ghc` monad. Use with caution.
forkGhc :: Ghc () -> Ghc ThreadId
forkGhc = unsafeLiftIO forkIO

-- | forkProcess within the `Ghc` monad. Use with extreme caution.
forkGhcProcess :: Ghc () -> Ghc ProcessID
forkGhcProcess = unsafeLiftIO forkProcess

-- | Lifted version of concurrentConversation
ghcConcurrentConversation :: FilePath
                          -> FilePath
                          -> FilePath
                          -> (RpcConversation -> Ghc ())
                          -> Ghc ()
ghcConcurrentConversation requestR responseW errorsW =
  unsafeLiftIO' (concurrentConversation requestR responseW errorsW)
