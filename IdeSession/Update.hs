-- | IDE session updates
--
-- We should only be using internal types here (explicit strictness/sharing)
module IdeSession.Update (
    -- * Starting and stopping
    initSession
  , shutdownSession
  , restartSession
    -- * Session updates
  , IdeSessionUpdate -- Abstract
  , updateSession
  , updateModule
  , updateModuleFromFile
  , updateModuleDelete
  , updateGhcOptions
  , updateCodeGeneration
  , updateDataFile
  , updateDataFileFromFile
  , updateDataFileDelete
  , updateEnv
  , updateStdoutBufferMode
  , updateStderrBufferMode
    -- * Running code
  , runStmt
  )
  where

import Control.Monad (when, void)
import Control.Monad.State (StateT, execStateT)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Exception as Ex
import Data.List (delete)
import Data.Monoid (Monoid(..))
import Data.Accessor ((.>), (^.), (^=))
import Data.Accessor.Monad.MTL.State (get, modify, set)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSS
import qualified System.Directory as Dir
import System.FilePath (takeDirectory, makeRelative, (</>))
import System.Posix.Files (setFileTimes)
import System.IO.Temp (createTempDirectory)
import qualified Data.Text as Text

import IdeSession.State
import IdeSession.Config
import IdeSession.GHC.Server
import IdeSession.Types.Private
import IdeSession.Types.Progress
import IdeSession.Util
import IdeSession.Strict.Container
import qualified IdeSession.Strict.IntMap as IntMap
import qualified IdeSession.Strict.Map    as Map
import qualified IdeSession.Strict.Maybe  as Maybe
import IdeSession.Strict.MVar (newMVar, modifyMVar, modifyMVar_)

{------------------------------------------------------------------------------
  Starting and stopping
------------------------------------------------------------------------------}

-- | Create a fresh session, using some initial configuration.
--
initSession :: SessionConfig -> IO IdeSession
initSession SessionConfig{..} = do
  configDirCanon <- Dir.canonicalizePath configDir
  let ideConfig = SessionConfig {..}
  ideSourcesDir <- createTempDirectory configDirCanon "src."
  ideDataDir    <- createTempDirectory configDirCanon "data."
  _ideGhcServer <- forkGhcServer configGenerateModInfo configStaticOpts (Just ideDataDir) configInProcess
  -- The value of _ideLogicalTimestamp field is a workaround for
  -- the problems with 'invalidateModSummaryCache', which itself is
  -- a workaround for http://hackage.haskell.org/trac/ghc/ticket/7478.
  -- We have to make sure that file times never reach 0, because this will
  -- trigger an exception (http://hackage.haskell.org/trac/ghc/ticket/7567).
  -- We rather arbitrary start at Jan 2, 1970.
  ideState <- newMVar $ IdeSessionIdle IdeIdleState {
                          _ideLogicalTimestamp = 86400
                        , _ideComputed         = Maybe.nothing
                        , _ideNewOpts          = Nothing
                        , _ideGenerateCode     = False
                        , _ideManagedFiles     = ManagedFilesInternal [] []
                        , _ideEnv              = []
                        , _ideUpdatedEnv       = False
                          -- Make sure 'ideComputed' is set on first call
                          -- to updateSession
                        , _ideUpdatedCode      = True
                        , _ideStdoutBufferMode = RunNoBuffering
                        , _ideStderrBufferMode = RunNoBuffering
                        , _ideGhcServer
                        }
  let ideStaticInfo = IdeStaticInfo{..}
  let session = IdeSession{..}

  return session

-- | Close a session down, releasing the resources.
--
-- This operation is the only one that can be run after a shutdown was already
-- performed. This lets the API user execute an early shutdown, e.g., before
-- the @shutdownSession@ placed inside 'bracket' is triggered by a normal
-- program control flow.
--
-- If code is still running, it will be interrupted.
shutdownSession :: IdeSession -> IO ()
shutdownSession IdeSession{ideState, ideStaticInfo} = do
  snapshot <- modifyMVar ideState $ \state -> return (IdeSessionShutdown, state)
  case snapshot of
    IdeSessionRunning runActions idleState -> do
      -- We need to terminate the running program before we can shut down
      -- the session, because the RPC layer will sequentialize all concurrent
      -- calls (and if code is still running we still have an active
      -- RPC conversation)
      interrupt runActions
      void $ runWaitAll runActions
      shutdownGhcServer $ _ideGhcServer idleState
      cleanupDirs
    IdeSessionIdle idleState -> do
      shutdownGhcServer $ _ideGhcServer idleState
      cleanupDirs
    IdeSessionShutdown -> return ()
 where
  cleanupDirs = do
    let dataDir    = ideDataDir ideStaticInfo
        sourcesDir = ideSourcesDir ideStaticInfo
    -- TODO: this has a race condition (not sure we care; if not, say why not)
    dataExists <- Dir.doesDirectoryExist dataDir
    when dataExists $ Dir.removeDirectoryRecursive dataDir
    sourceExists <- Dir.doesDirectoryExist sourcesDir
    when sourceExists $ Dir.removeDirectoryRecursive sourcesDir

-- | Restarts a session. Technically, a new session is created under the old
-- @IdeSession@ handle, with a state cloned from the old session,
-- which is then shut down. The only behavioural difference between
-- the restarted session and the old one is that any running code is stopped
-- (even if it was stuck and didn't respond to interrupt requests)
-- and that no modules are loaded, though all old modules and data files
-- are still contained in the new session and ready to be compiled with
-- the same flags and environment variables as before.
--
-- (We don't automatically recompile the code using the new session, because
-- what would we do with the progress messages?)
restartSession :: IdeSession -> IO ()
restartSession IdeSession{ideStaticInfo, ideState} = do
    modifyMVar_ ideState $ \state ->
      case state of
        IdeSessionIdle idleState ->
          restart idleState
        IdeSessionRunning runActions idleState -> do
          forceCancel runActions
          restart idleState
        IdeSessionShutdown ->
          fail "Shutdown session cannot be restarted."
  where
    restart :: IdeIdleState -> IO IdeSessionState
    restart idleState = do
      forceShutdownGhcServer $ _ideGhcServer idleState
      server <-
        forkGhcServer
          configGenerateModInfo configStaticOpts workingDir configInProcess
      return . IdeSessionIdle
             . (ideComputed    ^= Maybe.nothing)
             . (ideUpdatedEnv  ^= True)
             . (ideUpdatedCode ^= True)
             . (ideGhcServer   ^= server)
             $ idleState

    workingDir = Just (ideDataDir ideStaticInfo)
    SessionConfig{..} = ideConfig ideStaticInfo

{------------------------------------------------------------------------------
  Session updates
------------------------------------------------------------------------------}

-- | We use the 'IdeSessionUpdate' type to represent the accumulation of a
-- bunch of updates.
--
-- In particular it is an instance of 'Monoid', so multiple primitive updates
-- can be easily combined. Updates can override each other left to right.
newtype IdeSessionUpdate = IdeSessionUpdate {
    runSessionUpdate :: IdeStaticInfo -> StateT IdeIdleState IO ()
  }

-- We assume, if updates are combined within the monoid, they can all
-- be applied in the context of the same session.
-- Otherwise, call 'updateSession' sequentially with the updates.
instance Monoid IdeSessionUpdate where
  mempty = IdeSessionUpdate $ \_ideConfig -> return ()
  (IdeSessionUpdate f) `mappend` (IdeSessionUpdate g) =
    IdeSessionUpdate $ \ideConfig -> f ideConfig >> g ideConfig

-- | Given the current IDE session state, go ahead and
-- update the session, eventually resulting in a new session state,
-- with fully updated computed information (typing, etc.).
--
-- The update can be a long running operation, so we support a callback
-- which can be used to monitor progress of the operation.
updateSession :: IdeSession -> IdeSessionUpdate -> (Progress -> IO ()) -> IO ()
updateSession IdeSession{ideStaticInfo, ideState} update callback = do
  modifyMVar_ ideState $ \state ->
    case state of
      IdeSessionIdle idleState -> do
        idleState' <- execStateT (runSessionUpdate update ideStaticInfo) idleState

        -- Update environment
        when (idleState' ^. ideUpdatedEnv) $
          rpcSetEnv (idleState ^. ideGhcServer) (idleState' ^. ideEnv)

        -- Determine imports and completion map from the diffs sent
        -- from the RPC compilationi process.
        let usePrevious :: IdeIdleState
                        -> Strict (Map ModuleName) (Diff ( Strict [] Import
                                                         , Strict Trie (Strict [] IdInfo)))
                        -> ( Strict (Map ModuleName) (Strict [] Import)
                           , Strict (Map ModuleName) (Strict Trie (Strict [] IdInfo))
                           )
            usePrevious idleSt importsAuto =
              ( applyMapDiff (Map.map (fmap fst) importsAuto)
                $ Maybe.maybe Map.empty computedImports $ idleSt ^. ideComputed
              , applyMapDiff (Map.map (fmap snd) importsAuto)
                $ Maybe.maybe Map.empty computedAutoMap $ idleSt ^. ideComputed
              )
        -- Update code
        computed <- if (idleState' ^. ideUpdatedCode) then do
                      ( computedErrors
                       , computedLoadedModules
                       , importsAuto
                       , computedCache'
                       ) <- rpcCompile (idleState ^. ideGhcServer)
                                       (idleState' ^. ideNewOpts)
                                       (ideSourcesDir ideStaticInfo)
                                       (idleState' ^. ideGenerateCode)
                                       callback
                      let (computedImports, computedAutoMap) =
                            usePrevious idleState' importsAuto
                          computedCache = mkRelative computedCache'
                      return $ Maybe.just Computed{..}
                    else return $ idleState' ^. ideComputed

        -- Update state
        return . IdeSessionIdle
               . (ideComputed    ^= computed)
               . (ideUpdatedEnv  ^= False)
               . (ideUpdatedCode ^= False)
               $ idleState'
      IdeSessionRunning _ _ ->
        Ex.throwIO (userError "Cannot update session in running mode")
      IdeSessionShutdown ->
        Ex.throwIO (userError "Session already shut down.")
  where
    mkRelative :: ExplicitSharingCache -> ExplicitSharingCache
    mkRelative ExplicitSharingCache{..} =
      let aux :: BSS.ByteString -> BSS.ByteString
          aux = BSS.pack . makeRelative (ideSourcesDir ideStaticInfo) . BSS.unpack
      in ExplicitSharingCache {
        filePathCache = IntMap.map aux filePathCache
      , idPropCache   = idPropCache
      }


-- | A session update that changes a source module by giving a new value for
-- the module source. This can be used to add a new module or update an
-- existing one. The ModuleName argument determines the directory
-- and file where the module is located within the project. The actual
-- internal compiler module name, such as the one given by the
-- @getLoadedModules@ query, comes from within @module ... end@.
-- Usually the two names are equal, but they needn't be.
--
updateModule :: FilePath -> BSL.ByteString -> IdeSessionUpdate
updateModule m bs = IdeSessionUpdate $ \IdeStaticInfo{ideSourcesDir} -> do
  let internal = internalFile ideSourcesDir m
  old <- get (ideManagedFiles .> managedSource .> lookup' m)
  -- We always overwrite the file, and then later set the timestamp back
  -- to what it was if it turns out the hash was the same. If we compute
  -- the hash first, we would force the entire lazy bytestring into memory
  newHash <- liftIO $ writeFileAtomic internal bs
  case old of
    Just (oldHash, oldTS) | oldHash == newHash ->
      liftIO $ setFileTimes internal oldTS oldTS
    _ -> do
      newTS <- get ideLogicalTimestamp
      liftIO $ setFileTimes internal newTS newTS
      modify ideLogicalTimestamp (+ 1)
      set (ideManagedFiles .> managedSource .> lookup' m) (Just (newHash, newTS))
      set ideUpdatedCode True

-- | Like 'updateModule' except that instead of passing the module source by
-- value, it's given by reference to an existing file, which will be copied.
--
updateModuleFromFile :: FilePath -> IdeSessionUpdate
updateModuleFromFile p = IdeSessionUpdate $ \staticInfo -> do
  -- We just call 'updateModule' because we need to read the file anyway
  -- to compute the hash.
  bs <- liftIO $ BSL.readFile p
  runSessionUpdate (updateModule p bs) staticInfo

-- | A session update that deletes an existing module.
--
updateModuleDelete :: FilePath -> IdeSessionUpdate
updateModuleDelete m = IdeSessionUpdate $ \IdeStaticInfo{ideSourcesDir} -> do
  liftIO $ Dir.removeFile (internalFile ideSourcesDir m)
  set (ideManagedFiles .> managedSource .> lookup' m) Nothing
  set ideUpdatedCode True

-- | Update dynamic compiler flags, including pragmas and packages to use.
-- Warning: only dynamic flags can be set here.
-- Static flags need to be set at server startup.
updateGhcOptions :: (Maybe [String]) -> IdeSessionUpdate
updateGhcOptions opts = IdeSessionUpdate $ \_ -> do
  set ideNewOpts opts
  set ideUpdatedCode True

-- | Enable or disable code generation in addition
-- to type-checking. Required by 'runStmt'.
updateCodeGeneration :: Bool -> IdeSessionUpdate
updateCodeGeneration b = IdeSessionUpdate $ \_ -> do
  set ideGenerateCode b
  set ideUpdatedCode True

-- | A session update that changes a data file by giving a new value for the
-- file. This can be used to add a new file or update an existing one.
--
updateDataFile :: FilePath -> BSL.ByteString -> IdeSessionUpdate
updateDataFile n bs = IdeSessionUpdate $ \IdeStaticInfo{ideDataDir} -> do
  liftIO $ writeFileAtomic (ideDataDir </> n) bs
  modify (ideManagedFiles .> managedData) (n :)

-- | Like 'updateDataFile' except that instead of passing the file content by
-- value, it's given by reference to an existing file (the second argument),
-- which will be copied.
--
updateDataFileFromFile :: FilePath -> FilePath -> IdeSessionUpdate
updateDataFileFromFile n p = IdeSessionUpdate $ \IdeStaticInfo{ideDataDir} -> do
  let targetPath = ideDataDir </> n
      targetDir  = takeDirectory targetPath
  liftIO $ Dir.createDirectoryIfMissing True targetDir
  liftIO $ Dir.copyFile p targetPath
  modify (ideManagedFiles .> managedData) (n :)

-- | A session update that deletes an existing data file.
--
updateDataFileDelete :: FilePath -> IdeSessionUpdate
updateDataFileDelete n = IdeSessionUpdate $ \IdeStaticInfo{ideDataDir} -> do
  liftIO $ Dir.removeFile (ideDataDir </> n)
  modify (ideManagedFiles .> managedData) $ delete n

-- | Set an environment variable
--
-- Use @updateEnv var Nothing@ to unset @var@.
updateEnv :: String -> Maybe String -> IdeSessionUpdate
updateEnv var val = IdeSessionUpdate $ \_ -> do
  set (ideEnv .> lookup' var) (Just val)
  set ideUpdatedEnv True

-- | Set buffering mode for snippets' stdout
updateStdoutBufferMode :: RunBufferMode -> IdeSessionUpdate
updateStdoutBufferMode bufferMode = IdeSessionUpdate $ \_ ->
  set ideStdoutBufferMode bufferMode

-- | Set buffering mode for snippets' stderr
updateStderrBufferMode :: RunBufferMode -> IdeSessionUpdate
updateStderrBufferMode bufferMode = IdeSessionUpdate $ \_ ->
  set ideStderrBufferMode bufferMode

-- | Run a given function in a given module (the name of the module
-- is the one between @module ... end@, which may differ from the file name).
-- The function resembles a query, but it's not instantaneous
-- and the running code can be interrupted or interacted with.
runStmt :: IdeSession -> String -> String -> IO RunActions
runStmt IdeSession{ideState} m fun = do
  modifyMVar ideState $ \state -> case state of
    IdeSessionIdle idleState ->
     case (toLazyMaybe (idleState ^. ideComputed), idleState ^. ideGenerateCode) of
       (Just comp, True) ->
          -- ideManagedFiles is irrelevant, because only the module name
          -- inside 'module .. where' counts.
          if Text.pack m `Map.member` computedLoadedModules comp
          then do
            runActions <- rpcRun (idleState ^. ideGhcServer)
                                 m fun
                                 (idleState ^. ideStdoutBufferMode)
                                 (idleState ^. ideStderrBufferMode)
            registerTerminationCallback runActions restoreToIdle
            return (IdeSessionRunning runActions idleState, runActions)
          else fail $ "Module " ++ show m
                      ++ " not successfully loaded, when trying to run code."
       _ ->
        -- This 'fail' invocation is, in part, a workaround for
        -- http://hackage.haskell.org/trac/ghc/ticket/7539
        -- which would otherwise lead to a hard GHC crash,
        -- instead of providing a sensible error message
        -- that we could show to the user.
        fail "Cannot run before the code is generated."
    IdeSessionRunning _ _ ->
      fail "Cannot run code concurrently"
    IdeSessionShutdown ->
      fail "Session already shut down."
  where
    restoreToIdle :: RunResult -> IO ()
    restoreToIdle _ = modifyMVar_ ideState $ \state -> case state of
      IdeSessionIdle _ ->
        Ex.throwIO (userError "The impossible happened!")
      IdeSessionRunning _ idleState -> do
        return $ IdeSessionIdle idleState
      IdeSessionShutdown ->
        return state
