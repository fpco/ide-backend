{-# LANGUAGE ScopedTypeVariables, CPP #-}
-- | Implementation of the server that controls the long-running GHC instance.
-- This interacts with the ide-backend library through serialized data only.
module Server (ghcServer) where

import Prelude hiding (mod, span)
import Control.Concurrent (throwTo, forkIO, myThreadId, threadDelay)
import Control.Monad (void, unless, when)
import Control.Monad.State (StateT, runStateT)
import Control.Monad.Trans.Class (lift)
import Data.Accessor (accessor, (.>))
import Data.Accessor.Monad.MTL.State (set)
import Data.Function (on)
import System.Environment (getEnvironment)
import qualified Control.Exception as Ex
import qualified Data.List         as List
import qualified Data.Text         as Text

import IdeSession.GHC.API
import IdeSession.RPC.Server
import IdeSession.Strict.Container
import IdeSession.Strict.IORef
import IdeSession.Types.Private
import IdeSession.Types.Progress
import IdeSession.Util
import qualified IdeSession.Strict.List  as StrictList
import qualified IdeSession.Strict.Map   as StrictMap
import qualified IdeSession.Types.Public as Public

import qualified GHC
import qualified ObjLink
import qualified Linker
import Hooks

import Run
import HsWalk
import GhcShim
import RTS
import Auxiliary
import RequestRunning

--------------------------------------------------------------------------------
-- Server-side operations                                                     --
--------------------------------------------------------------------------------

-- | Start the RPC server. Used from within the server executable.
ghcServer :: [String] -> IO ()
ghcServer args =
  withSystemTempDirectory "ghc-server." $ \tmpDir -> do
    -- Prepare the rts support module
    rtsInfo <- deployRts tmpDir

    -- Launch the server
    rpcServer (ghcServerEngine rtsInfo) args


-- | The GHC server engine proper.
--
-- This function runs in end endless loop inside the @Ghc@ monad, making
-- incremental compilation possible.
ghcServerEngine :: RtsInfo -> FilePath -> RpcConversation -> IO ()
ghcServerEngine rtsInfo errorLog conv@RpcConversation{..} = do
  -- The initial handshake with the client
  (configGenerateModInfo, initOpts, sourceDir, sessionDir, distDir) <- handleInit conv
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
            ReqRun runCmd -> runCommand args sessionDir runCmd
            ReqSetEnv env -> do
              ghcHandleSetEnv conv initEnv env
              return args
            ReqSetArgs args' -> do
              liftIO $ put ()
              return args'
            ReqBreakpoint mod span value -> do
              ghcHandleBreak conv mod span value
              return args
            ReqPrint vars bind' forceEval -> do
              ghcHandlePrint conv vars bind' forceEval
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
          put $ GhcCompileProgress Progress {
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
            -> RtsInfo
            -> StrictIORef (Strict [] SourceError)
            -> StrictIORef ExtractIdsSuspendedState
            -> StrictIORef (Strict (Map ModuleName) PluginResult)
            -> (String -> IO ())
            -> Ghc ()
initSession distDir modInfo dOpts rtsInfo errsRef stRef pluginRef callback = do
    flags          <- getSessionDynFlags
    (flags', _, _) <- parseDynamicFlags flags $ dOpts ++ dynOpts

    let flags'' = (if modInfo then installHooks else id)
                . installErrorLoggers
                $ flags'

    void $ setSessionDynFlags flags''
  where
    dynOpts :: DynamicOpts
    dynOpts = optsToDynFlags [
        "-package-db " ++ rtsPackageDb rtsInfo
      , "-package "    ++ rtsPackage   rtsInfo

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
handleInit :: RpcConversation -> IO (Bool, [String], FilePath, FilePath, FilePath)
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
         , ghcInitSourceDir
         , ghcInitSessionDir
         , ghcInitDistDir
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
          , ghcCompileLoaded   = force loadedModules
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

          graph <- lift getModuleGraph
          let name s      = Text.pack (moduleNameString (ms_mod_name s))
              namedGraph  = map (\s -> (name s, s)) graph
              sortedGraph = List.sortBy (compare `on` fst) namedGraph
          oldSummaries <- lift . liftIO $ readIORef modsRef
          go (StrictMap.toList oldSummaries) sortedGraph

        liftIO $ writeIORef modsRef (StrictMap.fromList newSummaries)
        return finalResponse

    cache <- liftIO constructExplicitSharingCache
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
ghcHandlePrint RpcConversation{..} var bind' forceEval = do
  vals <- printVars (Text.unpack var) bind' forceEval
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
    (suppressed, success) <- captureOutput ObjLink.resolveObjs
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
