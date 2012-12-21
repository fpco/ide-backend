{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module IdeSession (

  -- | This module provides an interface to the IDE backend.
  --
  -- It centres around the idea of a single threaded IDE session, and
  -- operations for updating the session or running queries given the current
  -- state of the session.

  -- * Interaction with the compiler.
  --
  -- | Ironically for a pure functional language, the interface to the compiler
  -- is rather stateful and sequential. In part this is because it's dealing
  -- with the state of files in the file system which are of course mutable
  -- variables.
  --
  -- So the general pattern of interaction is sequential and single-threaded.
  -- The state transitions are fairly simple:
  --
  -- * update phase: we have a batch of updates, e.g. changes in module contents.
  --   This part is declarative, we just describe what changes we want to make.
  --
  -- * compile phase: we apply the updates and invoke the compiler, which
  --   incrementally recompiles some modules. This may be a relatively
  --   long running operation and we may want progress info.
  --
  -- * query phase: after compiling we can collect information like
  --   source errors, the list of successfully loaded modules
  --   or symbol maps.
  --
  -- * run phase: regardless of compilation results, we may want to run some
  --   code from a module (compiled recently or compiled many updates ago),
  --   interact with the running code's input and output, interrupt
  --   its execution.
  --
  -- Then the whole process can repeat.
  --
  -- To clarify these different phases we use different types:
  --
  -- * 'IdeSession' for the query mode. This is in a sense also the default
  --   mode.
  --
  -- * 'IdeSessionUpdate' for accumulating updates.
  --
  -- * 'Progress' for the progress information in the compile mode.
  --
  -- * 'RunActions' for handles on the running code, through which
  --   one can interact with the code.

  -- * Sessions
  IdeSession,

  -- ** Initialisation and shutdown
  -- | Sessions are stateful and must be initialised and shut down so that
  -- resources can be released.
  initSession,
  shutdownSession,
  SessionConfig(..),
  getSessionConfig,
  getSourcesDir,
  getDataDir,
  restartSession,

  -- * Updates
  -- | Updates are done in batches: we collect together all of the updates we
  -- want to do and then do a single transition, applying all the updates,
  -- and end up in a new state.

  -- ** Declarative updates
  -- | So that we can batch the updates, all the updates are declarative.
  -- The 'IdeSessionUpdate' monoid is used to represent the updates, and the
  -- sub-sections below describe the various updates that are available.
  IdeSessionUpdate,

  -- ** Modules
  updateModule,
  updateModuleFromFile,
  updateModuleDelete,

  -- ** Flags and other settings
  updateGhcOptions,
  updateCodeGeneration,

  -- ** Data files
  updateDataFile,
  updateDataFileFromFile,
  updateDataFileDelete,

  -- ** Environment variables
  updateEnv,

  -- ** Buffer mode
  updateStdoutBufferMode,
  updateStderrBufferMode,

  -- ** Performing the update
  -- | Once we have accumulated a batch of updates we can perform them all
  -- giving us a new session state. Since performing a bunch of updates can
  -- involve compiling modules and can take some time, the update uses the
  -- 'Progress' type to represent intermediate progress information.
  updateSession,
  Progress,
  progressStep,

  -- * Queries
  Query,

  -- ** Source errors
  getSourceErrors,
  SourceError(..),
  SourceErrorKind(..),

  -- ** Files
  -- | Simply getting the current state of the persistent files fits the
  -- queries pattern.
  getSourceModule,
  getDataFile,

  -- ** The list of managed files, loaded modules and all data files
  getManagedFiles,
  ManagedFiles(..),
  getLoadedModules,
  getAllDataFiles,

  -- ** Environment variables
  getEnv,

  -- ** Symbol definition maps
  getSymbolDefinitionMap,
  SymbolDefinitionMap,

  -- ** Run code
  runStmt,
  RunResult(..),
  RunBufferMode(..),
  RunActions, -- We don't export the constructor nor all accessors
  interrupt,
  runWait,
  supplyStdin,
  runWaitAll,
  afterRunActions,

  -- ** Start and diagnose the server (probably only for debugging)
  ghcServer,
  getGhcServer,
  getGhcExitCode,

  -- * Additional notes
  -- ** Responsibility for managing and mutating files in the sources dir.
  -- | In general, updating and changing source files in the sources dir has to
  -- be coordinated with the IdeSession, since we're in a concurrent mutable
  -- setting.
  --
  -- The model here is that the IdeSession alone manages the files in the
  -- sources directory. All file changes and file reading must be managed
  -- via the session, and sequenced relative to other session state changes.
  --
  -- The session will manage the files carefully, including in the case of
  -- exceptions and things going awry. Thus the caller does not need to
  -- duplicate the file state: it can rely on putting files in, applying
  -- updates to the files via the session, and extracting the files again
  -- at any time.

  -- ** Morally pure queries
  -- | Morally, a compiler is a pure function from the current value of the
  -- various source files (and other bits of the environment) to object code
  -- and\/or other information about the modules (errors, types etc).
  --
  -- The intention is to reflect this purity property in this interface. The
  -- value of an 'IdeSession' represents the state of the files\/modules and
  -- the result of the pure compilation function. It should always be the case
  -- that we can throw away the session and recover it just from the persistent
  -- state in the files.
  --
  -- One example where this notion makes a difference is with warnings.
  -- Traditionally, compilers just return the warnings for the modules they
  -- compiled, skipping warnings for the modules they didn't need to recompile.
  -- But this doesn't match the pure function idea, because the compilation
  -- result now depends on which steps we took to get there, rather than just
  -- on the current value of the files. So one of the things this wrapper can
  -- do is to restore the purity in these corner cases, (which otherwise the
  -- client of this API would probably have to do). [Not done yet.]

  -- ** Persistent and transitory state
  -- | The persistent state is obviously the files: source files and data
  -- files. Internally there is a great deal of transitory and cached state,
  -- either in memory or on disk (such as .hi files on disk or the equivalent
  -- in memory).
  --
  -- It should always be possible to drop all the transitory state and recover,
  -- just at the cost of some extra work.
  --
  -- This property is a useful correctness property for internal testing: the
  -- results of all the queries should be the same before and after blowing
  -- away all the transitory state and recovering.
) where

import Control.Concurrent (MVar, newMVar, modifyMVar, modifyMVar_, withMVar)
import qualified Control.Exception as Ex
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (delete)
import Data.Monoid (Monoid (..))
import System.Directory
import System.FilePath (splitFileName, takeDirectory, (<.>), (</>))
import qualified System.FilePath.Find as Find
import System.IO (hClose, openBinaryTempFile)
import System.IO.Temp (createTempDirectory)
import System.Posix.Files (setFileTimes)

import Common
import GhcServer
import GhcRun (RunResult(..), RunBufferMode(..))
import ModuleName (LoadedModules, ModuleName)
import qualified ModuleName as MN

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (StateT, execStateT)
import Data.Accessor ((.>), (^.), (^=))
import Data.Accessor.Monad.MTL.State (get, modify, set)
import Data.Accessor.Template (nameDeriveAccessors)

-- | Configuration parameters for a session. These remain the same throughout
-- the whole session's lifetime.
--
data SessionConfig = SessionConfig {
    -- | The directory to use for all session files.
    configDir        :: FilePath
    -- | GHC static options. Can also contain default dynamic options,
    -- that are overriden via session update.
  , configStaticOpts :: [String]
  }

data Computed = Computed {
    -- | Last compilation and run errors
    computedErrors        :: [SourceError]
    -- | Modules that got loaded okay
  , computedLoadedModules :: LoadedModules
  }

-- | This type is a handle to a session state. Values of this type
-- point to the non-persistent parts of the session state in memory
-- and to directories containing source and data file that form
-- the persistent part of the session state. Whenever we perform updates
-- or run queries, it's always in the context of a particular handle,
-- representing the session we want to work within. Many sessions
-- can be active at once, but in normal applications this shouldn't be needed.
--
data IdeSession = IdeSession {
    ideStaticInfo :: IdeStaticInfo
  , ideState      :: MVar IdeSessionState
  }

data IdeStaticInfo = IdeStaticInfo {
    -- | Configuration
    ideConfig     :: SessionConfig
    -- | The directory to use for managing source files.
  , ideSourcesDir :: FilePath
    -- | The directory to use for data files that may be accessed by the
    -- running program. The running program will have this as its CWD.
  , ideDataDir    :: FilePath
  }

data IdeSessionState =
    IdeSessionIdle IdeIdleState
  | IdeSessionRunning RunActions IdeIdleState
  | IdeSessionShutdown

data IdeIdleState = IdeIdleState {
    -- Logical timestamps (used to force ghc to recompile files)
    _ideLogicalTimestamp :: Int
    -- The result computed by the last 'updateSession' invocation.
  , _ideComputed         :: Maybe Computed
    -- Compiler dynamic options. If they are not set, the options from
    -- SessionConfig are used.
  , _ideNewOpts          :: Maybe [String]
    -- Whether to generate code in addition to type-checking.
  , _ideGenerateCode     :: Bool
    -- Files submitted by the user and not deleted yet.
  , _ideManagedFiles     :: ManagedFiles
    -- Environment overrides
  , _ideEnv              :: [(String, Maybe String)]
    -- The GHC server (this is replaced in 'restartSession')
  , _ideGhcServer        :: GhcServer
    -- Buffer mode for standard output for 'runStmt'
  , _ideStdoutBufferMode    :: RunBufferMode
    -- Buffer mode for standard error for 'runStmt'
  , _ideStderrBufferMode    :: RunBufferMode
    -- Has the environment (as recorded in this state) diverged from the
    -- environment on the server?
  , _ideUpdatedEnv       :: Bool
    -- TODO: add _ideUpdatedCode
  }

-- | The collection of source and data files submitted by the user.
data ManagedFiles = ManagedFiles
  { _sourceFiles :: [ModuleName]
  , _dataFiles   :: [FilePath]
  }

$(nameDeriveAccessors ''IdeIdleState accessorName)
$(nameDeriveAccessors ''ManagedFiles accessorName)

-- | Recover the fixed config the session was initialized with.
getSessionConfig :: IdeSession -> SessionConfig
getSessionConfig = ideConfig . ideStaticInfo

-- | Obtain the source files directory for this session.
getSourcesDir :: IdeSession -> FilePath
getSourcesDir = ideSourcesDir . ideStaticInfo

-- | Obtain the data files directory for this session.
getDataDir :: IdeSession -> FilePath
getDataDir = ideDataDir . ideStaticInfo

-- | Create a fresh session, using some initial configuration.
--
initSession :: SessionConfig -> IO IdeSession
initSession ideConfig@SessionConfig{..} = do
  ideSourcesDir <- createTempDirectory configDir "src."
  ideDataDir    <- createTempDirectory configDir "data."
  _ideGhcServer <- forkGhcServer configStaticOpts (Just ideDataDir)
  ideState <- newMVar $ IdeSessionIdle IdeIdleState {
                          _ideLogicalTimestamp = 0
                        , _ideComputed         = Nothing
                        , _ideNewOpts          = Nothing
                        , _ideGenerateCode     = False
                        , _ideManagedFiles     = ManagedFiles [] []
                        , _ideEnv              = []
                        , _ideUpdatedEnv       = False
                        , _ideStdoutBufferMode = RunNoBuffering
                        , _ideStderrBufferMode = RunNoBuffering
                        , _ideGhcServer
                        }
  let ideStaticInfo = IdeStaticInfo{..}
  return IdeSession{..}

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
    dataExists <- doesDirectoryExist dataDir
    when dataExists $ removeDirectoryRecursive dataDir
    sourceExists <- doesDirectoryExist sourcesDir
    when sourceExists $ removeDirectoryRecursive sourcesDir

-- | Restarts a session. Techically, a new session is created under the old
-- @IdeSession@ handle, with a state cloned from the old session,
-- which is then shut down. The only behavioural difference between
-- the restarted session and the old one is that any running code is stopped
-- (even if it was stuck and didn't repond to interrupt requests)
-- and that no modules are loaded, though all old modules and data files
-- are still contained in the new session and ready to be compiled with
-- the same flags and environment variables as before.
--
-- (We don't automatically recompile the code using the new session, because
-- what would we do with the progress messages?)
restartSession :: IdeSession -> IO ()
restartSession IdeSession{ideStaticInfo, ideState} =
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
      server <- forkGhcServer opts workingDir
      return . IdeSessionIdle
             . (ideComputed   ^= Nothing)
             . (ideUpdatedEnv ^= True)
             . (ideGhcServer  ^= server)
             $ idleState

    workingDir = Just (ideDataDir ideStaticInfo)
    opts       = configStaticOpts (ideConfig ideStaticInfo)

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

        -- Update environment (if necessary)
        when (idleState' ^. ideUpdatedEnv) $
          rpcSetEnv (idleState ^. ideGhcServer) (idleState' ^. ideEnv)

        -- Update code (TODO: skip this RPC call if not necessary)
        (computedErrors, computedLoadedModules) <-
          rpcCompile (idleState ^. ideGhcServer)
                     (idleState' ^. ideNewOpts)
                     (ideSourcesDir ideStaticInfo)
                     (idleState' ^. ideGenerateCode)
                     callback

        -- Update state
        return . IdeSessionIdle
               . (ideComputed   ^= Just Computed{..})
               . (ideUpdatedEnv ^= False)
               $ idleState'
      IdeSessionRunning _ _ ->
        Ex.throwIO (userError "Cannot update session in running mode")
      IdeSessionShutdown ->
        Ex.throwIO (userError "Session already shut down.")

-- | Writes a file atomically.
--
-- The file is either written sucessfully or an IO exception is raised and
-- the original file is left unchanged.
--
-- On windows it is not possible to delete a file that is open by a process.
-- This case will give an IO exception but the atomic property is not affected.
--
writeFileAtomic :: FilePath -> BS.ByteString -> IO ()
writeFileAtomic targetPath content = do
  let (targetDir, targetFile) = splitFileName targetPath
  createDirectoryIfMissing True targetDir
  Ex.bracketOnError
    (openBinaryTempFile targetDir $ targetFile <.> "tmp")
    (\(tmpPath, handle) -> hClose handle >> removeFile tmpPath)
    (\(tmpPath, handle) -> do
        BS.hPut handle content
        hClose handle
        renameFile tmpPath targetPath)

-- | A session update that changes a source module by giving a new value for
-- the module source. This can be used to add a new module or update an
-- existing one. The ModuleName argument determines the directory
-- and file where the module is located within the project. The actual
-- internal compiler module name, such as the one given by the
-- @getLoadedModules@ query, comes from within @module ... end@.
-- Usually the two names are equal, but they neededn't be.
--
updateModule :: ModuleName -> ByteString -> IdeSessionUpdate
updateModule m bs = IdeSessionUpdate $ \IdeStaticInfo{ideSourcesDir} -> do
  timestamp <- get ideLogicalTimestamp
  let internal = internalFile ideSourcesDir m
  liftIO $ do
    writeFileAtomic internal bs
    setFileTimes internal (fromIntegral timestamp) (fromIntegral timestamp)
  modify ideLogicalTimestamp (+ 1)
  modify (ideManagedFiles .> sourceFiles) (m :)

-- | Like 'updateModule' except that instead of passing the module source by
-- value, it's given by reference to an existing file, which will be copied.
--
updateModuleFromFile :: ModuleName -> FilePath -> IdeSessionUpdate
updateModuleFromFile m p = IdeSessionUpdate $ \IdeStaticInfo{ideSourcesDir} -> do
  timestamp <- get ideLogicalTimestamp
  let internal = internalFile ideSourcesDir m
      targetDir = takeDirectory internal
  liftIO $ do
    createDirectoryIfMissing True targetDir
    copyFile p internal
    setFileTimes internal (fromIntegral timestamp) (fromIntegral timestamp)
  modify ideLogicalTimestamp (+ 1)
  modify (ideManagedFiles .> sourceFiles) (m :)

-- | A session update that deletes an existing module.
--
updateModuleDelete :: ModuleName -> IdeSessionUpdate
updateModuleDelete m = IdeSessionUpdate $ \IdeStaticInfo{ideSourcesDir} -> do
  liftIO $ removeFile (internalFile ideSourcesDir m)
  modify (ideManagedFiles .> sourceFiles) (delete m)

-- | Update dynamic compiler flags, including pragmas and packages to use.
-- Warning: only dynamic flags can be set here.
-- Static flags need to be set at server startup.
updateGhcOptions :: (Maybe [String]) -> IdeSessionUpdate
updateGhcOptions opts = IdeSessionUpdate $ \_ -> set ideNewOpts opts

-- | Enable or disable code generation in addition
-- to type-checking. Required by 'runStmt'.
updateCodeGeneration :: Bool -> IdeSessionUpdate
updateCodeGeneration b = IdeSessionUpdate $ \_ -> set ideGenerateCode b

internalFile :: FilePath -> ModuleName -> FilePath
internalFile ideSourcesDir m =
  ideSourcesDir </> MN.toFilePath m <.> ".hs"

-- | A session update that changes a data file by giving a new value for the
-- file. This can be used to add a new file or update an existing one.
--
updateDataFile :: FilePath -> ByteString -> IdeSessionUpdate
updateDataFile n bs = IdeSessionUpdate $ \IdeStaticInfo{ideDataDir} -> do
  liftIO $ writeFileAtomic (ideDataDir </> n) bs
  modify (ideManagedFiles .> dataFiles) (n :)

-- | Like 'updateDataFile' except that instead of passing the file content by
-- value, it's given by reference to an existing file (the second argument),
-- which will be copied.
--
updateDataFileFromFile :: FilePath -> FilePath -> IdeSessionUpdate
updateDataFileFromFile n p = IdeSessionUpdate $ \IdeStaticInfo{ideDataDir} -> do
  let targetPath = ideDataDir </> n
      targetDir  = takeDirectory targetPath
  liftIO $ createDirectoryIfMissing True targetDir
  liftIO $ copyFile p targetPath
  modify (ideManagedFiles .> dataFiles) (n :)

-- | A session update that deletes an existing data file.
--
updateDataFileDelete :: FilePath -> IdeSessionUpdate
updateDataFileDelete n = IdeSessionUpdate $ \IdeStaticInfo{ideDataDir} -> do
  liftIO $ removeFile (ideDataDir </> n)
  modify (ideManagedFiles .> dataFiles) $ delete n

-- | Set an environment variable
--
-- Use @updateEnv var Nothing@ to unset @var@.
updateEnv :: String -> Maybe String -> IdeSessionUpdate
updateEnv var val = IdeSessionUpdate $ \_ -> do
  modify ideEnv (override var val)
  set ideUpdatedEnv True

-- | Set buffering mode for snippets' stdout
updateStdoutBufferMode :: RunBufferMode -> IdeSessionUpdate
updateStdoutBufferMode bufferMode = IdeSessionUpdate $ \_ ->
  set ideStdoutBufferMode bufferMode

-- | Set buffering mode for snippets' stderr
updateStderrBufferMode :: RunBufferMode -> IdeSessionUpdate
updateStderrBufferMode bufferMode = IdeSessionUpdate $ \_ ->
  set ideStderrBufferMode bufferMode

-- | The type of queries in a given session state.
--
-- Queries are in IO because they depend on the current state of the session
-- but they promise not to alter the session state (at least not in any visible
-- way; they might update caches, etc.).
--
type Query a = IdeSession -> IO a

-- | Read the current value of one of the source modules.
--
getSourceModule :: ModuleName -> Query ByteString
getSourceModule m IdeSession{ideStaticInfo} =
  BS.readFile $ internalFile (ideSourcesDir ideStaticInfo) m

-- | Read the current value of one of the data files.
--
getDataFile :: FilePath -> Query ByteString
getDataFile n IdeSession{ideStaticInfo} =
  BS.readFile $ ideDataDir ideStaticInfo </> n

-- | Get any compilation errors or warnings in the current state of the
-- session, meaning errors that GHC reports for the current state of all the
-- source modules.
--
-- Note that in the initial implementation this will only return warnings from
-- the modules that changed in the last update, the intended semantics is that
-- morally it be a pure function of the current state of the files, and so it
-- would return all warnings (as if you did clean and rebuild each time).
--
getSourceErrors :: Query [SourceError]
getSourceErrors IdeSession{ideState} =
  withMVar ideState $ \st ->
    case st of
      IdeSessionIdle      idleState -> aux idleState
      IdeSessionRunning _ idleState -> aux idleState
      IdeSessionShutdown            -> fail "Session already shut down."
  where
    aux :: IdeIdleState -> IO [SourceError]
    aux idleState = case idleState ^. ideComputed of
      Just Computed{..} -> return computedErrors
-- Optionally, this could give last reported errors, instead forcing
-- IDE to wait for the next sessionUpdate to finish.
      Nothing -> fail "This session state does not admit queries."


-- | Get the collection of files submitted by the user and not deleted yet.
-- The module names are those supplied by the user as the first
-- arguments of the @updateModule@ and @updateModuleFromFile@ calls,
-- as opposed to the compiler internal @module ... end@ module names.
-- Usually the two names are equal, but they neededn't be.
getManagedFiles :: Query ManagedFiles
getManagedFiles IdeSession{ideState} =
  withMVar ideState $ \st ->
  case st of
    IdeSessionIdle idleState ->
      return $ idleState ^. ideManagedFiles
    IdeSessionRunning _ idleState ->
      return $ idleState ^. ideManagedFiles
    IdeSessionShutdown ->
      fail "Session already shut down."

-- | Get the list of correctly compiled modules, as reported by the compiler.
getLoadedModules :: Query LoadedModules
getLoadedModules IdeSession{ideState} =
  withMVar ideState $ \st ->
    case st of
      IdeSessionIdle      idleState -> aux idleState
      IdeSessionRunning _ idleState -> aux idleState
      IdeSessionShutdown            -> fail "Session already shut down."
  where
    aux :: IdeIdleState -> IO LoadedModules
    aux idleState = case idleState ^. ideComputed of
      Just Computed{..} -> return computedLoadedModules
      Nothing -> fail "This session state does not admit queries."

-- | Get the list of all data files currently available to the session:
-- both the files copied via an update and files created by user code.
getAllDataFiles :: Query [FilePath]
getAllDataFiles IdeSession{ideStaticInfo} =
  Find.find Find.always
            (Find.fileType Find.==? Find.RegularFile)
            (ideDataDir ideStaticInfo)

-- | Get a mapping from where symbols are used to where they are defined.
-- That is, given a symbol used at a particular location in a source module
-- the mapping tells us where that symbol is defined, either locally in a
-- source module or a top-level symbol imported from another package.
--
getSymbolDefinitionMap :: Query SymbolDefinitionMap
getSymbolDefinitionMap = undefined

-- | Get all current environment overrides
getEnv :: Query [(String, Maybe String)]
getEnv IdeSession{ideState} =
  withMVar ideState $ \st ->
    case st of
      IdeSessionIdle idleState ->
        return $ idleState ^. ideEnv
      IdeSessionRunning _ idleState ->
        return $ idleState ^. ideEnv
      IdeSessionShutdown ->
        fail "Session already shut down."

-- | Run a given function in a given module (the name of the module
-- is the one between @module ... end@, which may differ from the file name).
-- The function resembles a query, but it's not instantaneous
-- and the running code can be interrupted or interacted with.
runStmt :: IdeSession -> ModuleName -> String -> IO RunActions
runStmt IdeSession{ideState} m fun = do
  modifyMVar ideState $ \state -> case state of
    IdeSessionIdle idleState ->
     case (idleState ^. ideComputed, idleState ^. ideGenerateCode) of
       (Just comp, True) ->
          -- ideManagedFiles is irrelevant, because only the module name
          -- inside 'module .. where' counts.
          if m `elem` computedLoadedModules comp
          then do
            runActions <- rpcRun (idleState ^. ideGhcServer)
                                 m fun
                                 (idleState ^. ideStdoutBufferMode)
                                 (idleState ^. ideStderrBufferMode)
            let runActions' = afterRunActions runActions restoreToIdle
            return (IdeSessionRunning runActions' idleState, runActions')
          else fail $ "Module " ++ show (MN.toString m)
                      ++ " not successfully loaded, when trying to run code."
       _ ->
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

-- | Get the RPC server used by the session.
getGhcServer :: IdeSession -> IO GhcServer
getGhcServer IdeSession{ideState} =
  withMVar ideState $ \st ->
  case st of
    IdeSessionIdle idleState ->
      return $! idleState ^. ideGhcServer
    IdeSessionRunning _ idleState ->
      return $! idleState ^. ideGhcServer
    IdeSessionShutdown ->
      fail "Session already shut down."
