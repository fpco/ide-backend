{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
-- | Implementation of the server that controls the long-running GHC instance.
-- This is the place where the GHC-specific part joins the parts
-- implementing the general RPC infrastructure and session management.
--
-- The modules importing any GHC internals, as well as the modules
-- implementing the RPC infrastructure, should be accessible to the rest
-- of the program only indirectly, through @IdeSession.GHC.Server@.
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
  , rpcCrash
  , shutdownGhcServer
  , forceShutdownGhcServer
  , getGhcExitCode
  , RunResult(..)
  , RunBufferMode(..)
  ) where

import Control.Concurrent (ThreadId, throwTo, forkIO, killThread, myThreadId, threadDelay)
import Control.Concurrent.Async (async, cancel, withAsync)
import Control.Concurrent.Chan (Chan, newChan, writeChan)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar)
import Control.Applicative ((<$>), (<*>))
import qualified Control.Exception as Ex
import Control.Monad (void, forM_, forever, unless, when)
import Control.Monad.State (StateT, runStateT)
import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString as BSS (ByteString, hGetSome, hPut, null)
import qualified Data.ByteString.Char8 as BSSC (pack)
import qualified Data.ByteString.Lazy as BSL (ByteString, fromChunks)
import System.Exit (ExitCode)
import qualified Data.Text as Text
import Data.Binary (Binary)
import qualified Data.Binary as Binary
import Data.List (sortBy)
import Data.Function (on)
import Data.Accessor (accessor, (.>))
import Data.Accessor.Monad.MTL.State (set)

import System.Directory (doesFileExist)
import System.FilePath ((</>))

import System.IO (Handle, hFlush)
import System.Posix (Fd)
import System.Posix.Env (setEnv, unsetEnv)
import System.Posix.IO.ByteString
import System.Time (ClockTime)

import IdeSession.GHC.HsWalk
import IdeSession.GHC.Run
import IdeSession.RPC.Server
import IdeSession.Types.Private
import IdeSession.Types.Progress
import IdeSession.Debug
import IdeSession.Util
import IdeSession.BlockingOps (modifyMVar, modifyMVar_, readChan, withMVar, wait)
import IdeSession.Strict.IORef
import IdeSession.Strict.Container
import qualified IdeSession.Strict.Map    as StrictMap
import qualified IdeSession.Strict.IntMap as StrictIntMap
import qualified IdeSession.Strict.List   as StrictList
import qualified IdeSession.Strict.Trie   as StrictTrie

import qualified GHC

import Paths_ide_backend

data GhcRequest
  = ReqCompile {
        reqCompileOptions   :: Maybe [String]
      , reqCompileSourceDir :: FilePath
      , reqCompileGenCode   :: Bool
      }
  | ReqRun {
        reqRunModule   :: String
      , reqRunFun      :: String
      , reqRunOutBMode :: RunBufferMode
      , reqRunErrBMode :: RunBufferMode
      }
  | ReqSetEnv {
         reqSetEnv :: [(String, Maybe String)]
       }
    -- | For debugging only! :)
  | ReqCrash {
         reqCrashDelay :: Maybe Int
       }

data GhcCompileResponse =
    GhcCompileProgress {
        ghcCompileProgress :: Progress
      }
  | GhcCompileDone {
        ghcCompileErrors   :: Strict [] SourceError
      , ghcCompileLoaded   :: Strict [] ModuleName
      , ghcCompileImports  :: Strict (Map ModuleName) (Diff (Strict [] Import))
      , ghcCompileAuto     :: Strict (Map ModuleName) (Diff (Strict [] IdInfo))
      , ghcCompileSpanInfo :: Strict (Map ModuleName) (Diff IdList)
      , ghcCompileCache    :: ExplicitSharingCache
      }

data GhcRunResponse =
    GhcRunOutp BSS.ByteString
  | GhcRunDone RunResult

data GhcRunRequest =
    GhcRunInput BSS.ByteString
  | GhcRunInterrupt
  | GhcRunAckDone

instance Binary GhcRequest where
  put ReqCompile{..} = do
    Binary.putWord8 0
    Binary.put reqCompileOptions
    Binary.put reqCompileSourceDir
    Binary.put reqCompileGenCode
  put ReqRun{..} = do
    Binary.putWord8 1
    Binary.put reqRunModule
    Binary.put reqRunFun
    Binary.put reqRunOutBMode
    Binary.put reqRunErrBMode
  put ReqSetEnv{..} = do
    Binary.putWord8 2
    Binary.put reqSetEnv
  put ReqCrash{..} = do
    Binary.putWord8 3
    Binary.put reqCrashDelay

  get = do
    header <- Binary.getWord8
    case header of
      0 -> ReqCompile <$> Binary.get <*> Binary.get <*> Binary.get
      1 -> ReqRun     <$> Binary.get <*> Binary.get <*> Binary.get <*> Binary.get
      2 -> ReqSetEnv  <$> Binary.get
      3 -> ReqCrash   <$> Binary.get
      _ -> fail "GhcRequest.Binary.get: invalid header"

instance Binary GhcCompileResponse where
  put GhcCompileProgress {..} = do
    Binary.putWord8 0
    Binary.put ghcCompileProgress
  put GhcCompileDone {..} = do
    Binary.putWord8 1
    Binary.put ghcCompileErrors
    Binary.put ghcCompileLoaded
    Binary.put ghcCompileImports
    Binary.put ghcCompileAuto
    Binary.put ghcCompileSpanInfo
    Binary.put ghcCompileCache

  get = do
    header <- Binary.getWord8
    case header of
      0 -> GhcCompileProgress <$> Binary.get
      1 -> GhcCompileDone     <$> Binary.get <*> Binary.get <*> Binary.get
                              <*> Binary.get <*> Binary.get <*> Binary.get
      _ -> fail "GhcCompileRespone.Binary.get: invalid header"

instance Binary GhcRunResponse where
  put (GhcRunOutp bs) = Binary.putWord8 0 >> Binary.put bs
  put (GhcRunDone r)  = Binary.putWord8 1 >> Binary.put r

  get = do
    header <- Binary.getWord8
    case header of
      0 -> GhcRunOutp <$> Binary.get
      1 -> GhcRunDone <$> Binary.get
      _ -> fail "GhcRunResponse.get: invalid header"

instance Binary GhcRunRequest where
  put (GhcRunInput bs) = Binary.putWord8 0 >> Binary.put bs
  put GhcRunInterrupt  = Binary.putWord8 1
  put GhcRunAckDone    = Binary.putWord8 2

  get = do
    header <- Binary.getWord8
    case header of
      0 -> GhcRunInput <$> Binary.get
      1 -> return GhcRunInterrupt
      2 -> return GhcRunAckDone
      _ -> fail "GhcRunRequest.get: invalid header"

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
  let (opts, "--ghc-opts-end" : configGenerateModInfo : fds) =
        span (/= "--ghc-opts-end") fdsAndOpts
  rpcServer fds (ghcServerEngine (read configGenerateModInfo) opts)

-- | The GHC server engine proper.
--
-- This function runs in end endless loop inside the @Ghc@ monad, making
-- incremental compilation possible.
ghcServerEngine :: Bool -> [String] -> RpcConversation -> IO ()
ghcServerEngine configGenerateModInfo staticOpts conv@RpcConversation{..} = do
  -- Submit static opts and get back leftover dynamic opts.
  dOpts <- submitStaticOpts (ideBackendRTSOpts ++ staticOpts)
  -- Set up references for the current session of Ghc monad computations.
  pluginRef  <- newIORef StrictMap.empty
  importsRef <- newIORef StrictMap.empty

  -- Start handling requests. From this point on we don't leave the GHC monad.
  runFromGhc $ do
    when configGenerateModInfo $ do
      -- Register our plugin in dynamic flags.
      dynFlags <- getSessionDynFlags
      let dynFlags' = dynFlags {
            sourcePlugins = extractIdsPlugin pluginRef : sourcePlugins dynFlags
          }
      void $ setSessionDynFlags dynFlags'

    -- Start handling RPC calls
    forever $ do
      req <- liftIO get
      case req of
        ReqCompile opts dir genCode ->
          ghcHandleCompile
            conv dOpts opts pluginRef importsRef dir
            genCode configGenerateModInfo
        ReqRun m fun outBMode errBMode ->
          ghcHandleRun conv m fun outBMode errBMode
        ReqSetEnv env ->
          ghcHandleSetEnv conv env
        ReqCrash delay ->
          ghcHandleCrash delay
  where
    ideBackendRTSOpts = [
        -- Just in case the user specified -hide-all-packages
        "-package ide-backend-rts"
      , "-i/Users/dev/wt/projects/fpco/ide-backend/test/Cabal"
      ]

-- | We cache our own "module summaries" in between compile requests
data ModSummary = ModSummary {
    -- | We cache the import lists so that we can check if the import
    -- list for this module has changed, and hence whether we need to recompute
    -- autocompletion info
    modImports   :: !(Strict [] Import)
    -- | We cache the file stamp to see if the file has changed at all, and
    -- hence whether we need to recompute the import list
  , modTimestamp :: !ClockTime
    -- | We cache whether this module was reported as "loaded" before so that
    -- we can see which modules got unloaded
  , modIsLoaded :: !Bool
  }

-- | Handle a compile or type check request
ghcHandleCompile
  :: RpcConversation
  -> DynamicOpts         -- ^ startup dynamic flags
  -> Maybe [String]      -- ^ new, user-submitted dynamic flags
  -> StrictIORef (Strict (Map ModuleName) IdList)
                         -- ^ ref where the ExtractIdsT plugin stores its data
                         -- (We clear this at the end of each call)
  -> StrictIORef (Strict (Map ModuleName) ModSummary)
                         -- ^ see doc for 'ModSummary'
  -> FilePath            -- ^ source directory
  -> Bool                -- ^ should we generate code
  -> Bool                -- ^ should we generate per-module info
  -> Ghc ()
ghcHandleCompile RpcConversation{..} dOpts ideNewOpts
                 pluginRef modsRef configSourcesDir
                 ideGenerateCode configGenerateModInfo = do
    errsRef <- liftIO $ newIORef StrictList.nil
    counter <- liftIO $ newIORef initialProgress
    (errs, loadedModules) <-
      suppressGhcStdout $ compileInGhc configSourcesDir
                                       dynOpts
                                       ideGenerateCode
                                       verbosity
                                       errsRef
                                       (progressCallback counter)
                                       (\_ -> return ()) -- TODO: log?


    let initialResponse = GhcCompileDone {
            ghcCompileErrors   = errs
          , ghcCompileLoaded   = force $ loadedModules
          , ghcCompileCache    = error "ghcCompileCache set last"
          -- We construct the diffs incrementally
          , ghcCompileImports  = StrictMap.empty
          , ghcCompileAuto     = StrictMap.empty
          , ghcCompileSpanInfo = StrictMap.empty
          }

    response <- if not configGenerateModInfo
      then return initialResponse
      else do
        let removeOldModule :: ModuleName -> StateT GhcCompileResponse Ghc ()
            removeOldModule m = do
              set (importsFor m)  Remove
              set (autoFor m)     Remove
              set (spanInfoFor m) Remove

            addNewModule :: (ModuleName, GHC.ModSummary)
                         -> StateT GhcCompileResponse Ghc (ModuleName, ModSummary)
            addNewModule (m, ghcSummary) = do
              imports <- lift $ importList     ghcSummary
              auto    <- lift $ autocompletion ghcSummary
              set (importsFor m) (Insert imports)
              set (autoFor m)    (Insert auto)
              -- spaninfo is set separately

              let newSummary = ModSummary {
                                   modTimestamp = ms_hs_date ghcSummary
                                 , modImports   = imports
                                 , modIsLoaded  = m `elem` loadedModules
                                 }

              return (m, newSummary)

            updateModule :: ModuleName -> ModSummary -> GHC.ModSummary
                         -> StateT GhcCompileResponse Ghc (ModuleName, ModSummary)
            updateModule m oldSummary ghcSummary
              | modTimestamp oldSummary == ms_hs_date ghcSummary =
                  return (m, oldSummary)
              | otherwise = do
                  imports <- lift $ importList ghcSummary
                  set (importsFor m) (Insert imports)

                  when (imports /= modImports oldSummary) $ do
                    auto <- lift $ autocompletion ghcSummary
                    set (autoFor m) (Insert auto)

                  let newSummary = ModSummary {
                                       modTimestamp = ms_hs_date ghcSummary
                                     , modImports   = imports
                                     , modIsLoaded  = m `elem` loadedModules
                                     }

                  when (not (modIsLoaded newSummary)) $
                    set (spanInfoFor m) Remove

                  return (m, newSummary)

        let go :: [(ModuleName, ModSummary)]
               -> [(ModuleName, GHC.ModSummary)]
               -> StateT GhcCompileResponse Ghc [(ModuleName, ModSummary)]
            go ((m, oldSummary) : old) ((m', ghcSummary) : new) = do
              case compare m m' of
                LT -> do removeOldModule m
                         go old ((m', ghcSummary) : new)
                GT -> do newSummary   <- addNewModule (m', ghcSummary)
                         newSummaries <- go ((m, oldSummary) : old) new
                         return $ newSummary : newSummaries
                EQ -> do newSummary   <- updateModule m oldSummary ghcSummary
                         newSummaries <- go old new
                         return $ newSummary : newSummaries
            go old new = do
              mapM_ removeOldModule (map fst old)
              mapM addNewModule new

        let addSpanInfo :: [(ModuleName, IdList)]
                        -> StateT GhcCompileResponse Ghc ()
            addSpanInfo = mapM_ $ \(m, spanInfo) ->
              set (spanInfoFor m) (Insert spanInfo)

        (newSummaries, finalResponse) <- flip runStateT initialResponse $ do
          pluginIdMaps <- lift . liftIO $ do
            idMaps <- readIORef pluginRef
            writeIORef pluginRef StrictMap.empty
            return idMaps
          addSpanInfo (StrictMap.toList pluginIdMaps)

          graph <- lift $ getModuleGraph
          let sortedGraph = sortBy (compare `on` ms_mod_name) graph
              name s      = Text.pack (moduleNameString (ms_mod_name s))
              namedGraph  = map (\s -> (name s, s)) sortedGraph
          oldSummaries <- lift . liftIO $ readIORef modsRef
          go (StrictMap.toList oldSummaries) namedGraph

        liftIO $ writeIORef modsRef (StrictMap.fromList newSummaries)
        return finalResponse

    cache <- liftIO $ constructExplicitSharingCache
    liftIO . put $ response { ghcCompileCache = cache }
  where
    dynOpts :: DynamicOpts
    dynOpts = maybe dOpts optsToDynFlags ideNewOpts

    -- Let GHC API print "compiling M ... done." for each module.
    verbosity :: Int
    verbosity = 1

    -- TODO: verify that _ is the "compiling M" message
    progressCallback :: StrictIORef Progress -> String -> IO ()
    progressCallback counter ghcMsg = do
      oldCounter <- readIORef counter
      modifyIORef counter (updateProgress ghcMsg)
      put $ GhcCompileProgress oldCounter

    -- Various accessors
    allImports  = accessor ghcCompileImports  (\is st -> st { ghcCompileImports  = is })
    allAuto     = accessor ghcCompileAuto     (\as st -> st { ghcCompileAuto     = as })
    allSpanInfo = accessor ghcCompileSpanInfo (\ss st -> st { ghcCompileSpanInfo = ss })

    importsFor  m = allImports  .> StrictMap.accessorDefault Keep m
    autoFor     m = allAuto     .> StrictMap.accessorDefault Keep m
    spanInfoFor m = allSpanInfo .> StrictMap.accessorDefault Keep m

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
      runInGhc (m, fun) outBMode errBMode ghcThread

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
                      $withMVar ghcThread $ \mTid -> do
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
-- Client-side operations                                                     --
--------------------------------------------------------------------------------

type InProcess = Bool

forkGhcServer :: Bool -> [String] -> Maybe String -> InProcess -> IO GhcServer
forkGhcServer configGenerateModInfo opts workingDir False = do
  bindir <- getBinDir
  let prog = bindir </> "ide-backend-server"

  exists <- doesFileExist prog
  unless exists $
    fail $ "The 'ide-backend-server' program was expected to "
        ++ "be at location " ++ prog ++ " but it is not."

  server <- forkRpcServer prog
                          (opts ++ [ "--ghc-opts-end"
                                   , show configGenerateModInfo ])
                          workingDir
  return (OutProcess server)
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

-- | Compile or typecheck
rpcCompile :: GhcServer           -- ^ GHC server
           -> Maybe [String]      -- ^ Options
           -> FilePath            -- ^ Source directory
           -> Bool                -- ^ Should we generate code?
           -> (Progress -> IO ()) -- ^ Progress callback
           -> IO ( Strict [] SourceError
                 , Strict [] ModuleName
                 , Strict (Map ModuleName) (Diff (Strict [] Import))
                 , Strict (Map ModuleName) (Diff (Strict Trie (Strict [] IdInfo)))
                 , Strict (Map ModuleName) (Diff IdList)
                 , ExplicitSharingCache
                 )
rpcCompile server opts dir genCode callback =
  conversation server $ \RpcConversation{..} -> do
    put (ReqCompile opts dir genCode)

    let go = do response <- get
                case response of
                  GhcCompileProgress pcounter ->
                    callback pcounter >> go
                  GhcCompileDone errs loaded imports auto spanInfo cache ->
                    return ( errs
                           , loaded
                           , imports
                           , StrictMap.map (fmap (constructAuto cache)) auto
                           , spanInfo
                           , cache
                           )
    go

constructAuto :: ExplicitSharingCache -> Strict [] IdInfo
              -> Strict Trie (Strict [] IdInfo)
constructAuto cache lk =
  StrictTrie.fromListWith (StrictList.++) $ map aux (toLazyList lk)
  where
    aux :: IdInfo -> (BSS.ByteString, Strict [] IdInfo)
    aux idInfo@IdInfo{idProp = k} =
      let idProp = idPropCache cache StrictIntMap.! idPropPtr k
      in ( BSSC.pack . Text.unpack . idName $ idProp
         , StrictList.singleton idInfo )

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

-- | Crash the GHC server (for debugging purposes)
rpcCrash :: GhcServer -> Maybe Int -> IO ()
rpcCrash server delay = conversation server $ \RpcConversation{..} ->
  put (ReqCrash delay)

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
