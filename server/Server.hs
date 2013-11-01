{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
-- | Implementation of the server that controls the long-running GHC instance.
-- This interacts with the ide-backend library through serialized data only.
module Server (ghcServer) where

import Prelude hiding (mod, span)
import Control.Concurrent (ThreadId, throwTo, forkIO, myThreadId, threadDelay)
import Control.Concurrent.Async (async)
import Control.Concurrent.MVar (MVar, newEmptyMVar)
import qualified Control.Exception as Ex
import Control.Monad (void, unless, when, mplus)
import Control.Monad.State (StateT, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Char (isSpace)
import qualified Data.ByteString as BSS (hGetSome, hPut, null)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List
import Data.Function (on)
import qualified Data.Attoparsec.Text as Att
import Data.Accessor (accessor, (.>))
import Data.Accessor.Monad.MTL.State (set)
import System.IO (Handle, hFlush)
import System.Posix (Fd)
import System.Posix.IO.ByteString
import System.Time (ClockTime)
import System.Environment (withArgs)

import IdeSession.GHC.API
import IdeSession.RPC.Server
import IdeSession.Types.Private
import qualified IdeSession.Types.Public as Public
import IdeSession.Types.Progress
import IdeSession.Util
import IdeSession.Util.BlockingOps (withMVar, wait)
import IdeSession.Strict.IORef
import IdeSession.Strict.Container
import qualified IdeSession.Strict.Map  as StrictMap
import qualified IdeSession.Strict.List as StrictList

import qualified GHC
import GhcMonad(Ghc(..))

import Run
import HsWalk
import Debug

--------------------------------------------------------------------------------
-- Server-side operations                                                     --
--------------------------------------------------------------------------------

-- | Start the RPC server. Used from within the server executable.
ghcServer :: [String] -> IO ()
ghcServer fdsAndOpts = do
  let (opts, "--ghc-opts-end" : configGenerateModInfo : clientApiVersion : fds) =
        List.span (/= "--ghc-opts-end") fdsAndOpts
  rpcServer fds $ ghcServerEngine (read configGenerateModInfo)
                                  (read clientApiVersion)
                                  opts

-- | The GHC server engine proper.
--
-- This function runs in end endless loop inside the @Ghc@ monad, making
-- incremental compilation possible.
ghcServerEngine :: Bool -> Int -> [String] -> RpcConversation -> IO ()
ghcServerEngine configGenerateModInfo
                clientApiVersion
                staticOpts
                conv@RpcConversation{..} = do
  -- Check API versions
  unless (clientApiVersion == ideBackendApiVersion) $
    Ex.throwIO . userError $ "API version mismatch between ide-backend "
                          ++ "(" ++ show clientApiVersion ++ ") "
                          ++ "and ide-backend-server "
                          ++ "(" ++ show ideBackendApiVersion ++ ")"

  -- Submit static opts and get back leftover dynamic opts.
  dOpts <- submitStaticOpts staticOpts
  -- Set up references for the current session of Ghc monad computations.
  pluginRef  <- newIORef StrictMap.empty
  importsRef <- newIORef StrictMap.empty

  -- Start handling requests. From this point on we don't leave the GHC monad.
  runFromGhc $ do
    -- Register startup options and perhaps our plugin in dynamic flags.
    -- This is the only place where the @packageDbArgs@ options are used
    -- and indeed, as the first invocation of @setSessionDynFlags@,
    -- this is the only place they could take any effect.
    -- This also implies that any options specifying package DBs
    -- passed via @updateGhcOptions@ won't have any effect in GHC API
    -- TODO: ban them or at least filter them out from the options
    -- passed to Cabal in @buildExe@ (where they could have some effect).
    -- TODO: options like "-hide-all-packages" or "-package foo"
    -- are suspect as well, even though they work on subsequent invocations
    -- of @updateGhcOptions@, because they nullify and replace cabal commands.
    initialDynFlags <- getSessionDynFlags
    (flags, _, _) <- parseDynamicFlags initialDynFlags dOpts
    let dynFlags | configGenerateModInfo = flags {
          sourcePlugins = extractIdsPlugin pluginRef : sourcePlugins flags
        }
                 | otherwise = flags
    void $ setSessionDynFlags dynFlags

    -- Start handling RPC calls
    let go args = do
          req <- liftIO get
          args' <- case req of
            ReqCompile opts dir distDir genCode -> do
              ghcHandleCompile
                conv opts pluginRef importsRef dir distDir
                genCode configGenerateModInfo
              return args
            ReqRun runCmd -> do
              ghcWithArgs args $ ghcHandleRun conv runCmd
              return args
            ReqSetEnv env -> do
              ghcHandleSetEnv conv env
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
            ReqCrash delay -> do
              ghcHandleCrash delay
              return args
          go args'

    go []

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
  -> Maybe [String]      -- ^ new, user-submitted dynamic flags
  -> StrictIORef (Strict (Map ModuleName) PluginResult)
                         -- ^ ref where the ExtractIdsT plugin stores its data
                         -- (We clear this at the end of each call)
  -> StrictIORef (Strict (Map ModuleName) ModSummary)
                         -- ^ see doc for 'ModSummary'
  -> FilePath            -- ^ source directory
  -> FilePath            -- ^ cabal's dist directory
  -> Bool                -- ^ should we generate code
  -> Bool                -- ^ should we generate per-module info
  -> Ghc ()
ghcHandleCompile RpcConversation{..} ideNewOpts
                 pluginRef modsRef configSourcesDir ideDistDir
                 ideGenerateCode configGenerateModInfo = do
    errsRef <- liftIO $ newIORef StrictList.nil
    (errs, loadedModules) <-
      suppressGhcStdout $ compileInGhc configSourcesDir
                                       dynOpts
                                       ideGenerateCode
                                       verbosity
                                       errsRef
                                       progressCallback
                                       (\_ -> return ()) -- TODO: log?


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
    -- TODO: Should we clear the link env caches here?
    liftIO $ put
           . GhcCompileDone
           $ response { ghcCompileCache = cache }
  where
    dynOpts :: DynamicOpts
    dynOpts =
      let userOpts = maybe [] optsToDynFlags ideNewOpts
          -- Just in case the user specified -hide-all-packages.
          rtsOpts = ["-package ide-backend-rts"]
          -- Include cabal_macros.h.
          cppOpts = [ "-optP-include"
                    , "-optP" ++ cabalMacrosLocation ideDistDir
                    ]
      in userOpts <> optsToDynFlags (rtsOpts ++ cppOpts)

    -- Let GHC API print "compiling M ... done." for each module.
    verbosity :: Int
    verbosity = 1

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

parseProgressMessage :: Text -> Either String (Int, Int, Text)
parseProgressMessage = Att.parseOnly parser
  where
    parser :: Att.Parser (Int, Int, Text)
    parser = do
      _    <- Att.char '['                ; Att.skipSpace
      step <- Att.decimal                 ; Att.skipSpace
      _    <- Att.string (Text.pack "of") ; Att.skipSpace
      numS <- Att.decimal                 ; Att.skipSpace
      _    <- Att.char ']'                ; Att.skipSpace
      rest <- parseCompiling `mplus` Att.takeText
      return (step, numS, rest)

    parseCompiling :: Att.Parser Text
    parseCompiling = do
      compiling <- Att.string (Text.pack "Compiling ")
      modName   <- Att.takeTill isSpace
      return (Text.append compiling modName)

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
      runInGhc runCmd ghcThread

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
ghcHandleSetEnv :: RpcConversation -> [(String, Maybe String)] -> Ghc ()
ghcHandleSetEnv RpcConversation{put} env = liftIO $ do
  setupEnv env
  put ()

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

-- | Lift `withArgs` to the `Ghc` monad. Relies on `unsafeLiftIO`.
ghcWithArgs :: [String] -> Ghc a -> Ghc a
ghcWithArgs = unsafeLiftIO . withArgs
