{-# LANGUAGE ScopedTypeVariables #-}
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
  -- * compile phase: we apply the updates and run the compiler. This may be a
  --   relatively long running operation and we may want progress info.
  --
  -- * query phase: after compiling we can collect information like source errors
  --   or symbol maps.
  --
  -- Then the whole process can repeat.
  --
  -- To clarify these different phases we use different types:
  --
  -- * 'IdeSession' for the query mode. This is in a sense also the default
  --   mode.
  --
  -- * 'IdeSessionUpdate' is the type we use to accumulate updates.
  --
  -- * 'Progress' 'IdeSession' is the type for the compile mode.

  -- * Sessions
  IdeSession,

  -- ** Initialisation and shutdown
  -- | Sessions are stateful and must be initialised and shut down so that
  -- resources can be released.
  initSession,
  shutdownSession,
  SessionConfig(..),
  getSessionConfig,

  -- * Updates
  -- | Updates are done in batches: we collect together all of the updates we
  -- want to do and then do a single transition, applying all the updates,
  -- and end up in a new state.

  -- ** Declarative updates
  -- | So that we can batch the updates, all the updates are declarative.
  -- The 'IdeSessionUpdate' monoid is used to represent the updates, and the
  -- sub-sections below describe the various updates that are available.
  IdeSessionUpdate,

  -- ** Performing the update
  -- | Once we have accumulated a batch of updates we can perform them all
  -- giving us a new session state. Since performing a bunch of updates can
  -- involve compiling modules and can take some time, the update uses the
  -- 'Progress' type to represent the action in progress.
  updateSession,
  PCounter,

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

  -- * Queries
  Query,

  -- ** Files
  -- | Simply getting the current state of the persistent files fits the
  -- queries pattern.
  getSourceModule,
  getDataFile,

  -- ** Source errors
  getSourceErrors,
  SourceError(..),

  -- ** Managed files and loaded modules
  getManagedFiles,
  ManagedFiles(..),
  getLoadedModules,
  LoadedModules,

  -- ** Symbol definition maps
  getSymbolDefinitionMap,
  SymbolDefinitionMap,

  -- ** Run code
  runStmt,
  RunResult(..)

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
  -- But this doesn't match the pure function idea, because now the compilation
  -- result now depends on which steps we took to get there, rather than just
  -- on the current value of the files. So one of the things this wrapper can
  -- do is to restore the purity in these corner cases, (which otherwise the
  -- client of this API would probably have to do).

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

import Control.Monad
import Control.Concurrent
import System.IO (openBinaryTempFile, hClose)
import System.Directory
import System.FilePath ((</>), (<.>), splitFileName)
import qualified Control.Exception as Ex
import Data.List (delete)
import Data.Monoid (Monoid(..))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import System.Posix.Files (setFileTimes)

import Common
import ModuleName (LoadedModules, ModuleName)
import qualified ModuleName as MN
import GhcServer

-- | This is a state token for the current state of an IDE session. We can run
-- queries in the current state, and from this state we can perform a batch
-- of updates, ultimately leading to a new 'IdeSession'.
--
-- Note that these state tokens are not persistent, once we perform an update
-- and get a new 'IdeSession', the old one is no longer valid. (And if you do
-- accidentally use an old invalid one, then it's a dynamically checked error.)
--
data IdeSession = IdeSession
  { ideConfig    :: SessionConfig
  , ideGhcServer :: GhcServer
  , ideState     :: MVar IdeSessionState
  }

data IdeSessionState =
    IdeSessionIdle IdeIdleState
  | IdeSessionRunning IdeIdleState
  | IdeSessionShutdown

data IdeIdleState = IdeIdleState {
    -- Logical timestamps (used to force ghc to recompile files)
    ideLogicalTimestamp :: Int
    -- The result computed by the last 'updateSession' invocation.
  , ideComputed  :: Maybe Computed
    -- Compiler dynamic options. If they are not set, the options from
    -- SessionConfig are used.
  , ideNewOpts   :: Maybe [String]
    -- Whether to generate code in addition to type-checking.
  , ideGenerateCode :: Bool
    -- Files submitted by the user and not deleted yet.
  , ideManagedFiles :: ManagedFiles
  }

data ManagedFiles = ManagedFiles
  { sourceFiles :: [ModuleName]
  , dataFiles   :: [FilePath]
  }

-- | Recover the fixed config the session was initialized with.
getSessionConfig :: IdeSession -> SessionConfig
getSessionConfig = ideConfig

-- | Configuration parameters for a session. These remain the same throughout
-- the whole session's lifetime.
--
data SessionConfig = SessionConfig {

       -- | The directory to use for managing source files.
       configSourcesDir :: FilePath,

       -- | The directory to use for session state, such as @.hi@ files.
       configWorkingDir :: FilePath,

       -- | The directory to use for data files that may be accessed by the
       -- running program. The running program will have this as its CWD.
       configDataDir :: FilePath,

       -- | The directory to use for purely temporary files.
       configTempDir :: FilePath,

       -- | GHC static options. Can also contain default dynamic options,
       -- that are overriden via session update.
       configStaticOpts :: [String]
     }

-- In this implementation, it's fully applicative, and so invalid sessions
-- can be queried at will. Note that there may be some working files
-- produced by GHC while obtaining these values. They are not captured here,
-- so queries are not allowed to read them.
data Computed = Computed {
    -- | last compilation and run errors
    computedErrors        :: [SourceError]
    -- | Modules that got loaded okay
  , computedLoadedModules :: LoadedModules
  }

ensureDirEmpty :: FilePath -> IO ()
ensureDirEmpty dir = do
  cnts <- getDirectoryContents dir
  when (any (`notElem` [".", ".."]) cnts)
    $ fail $ "Directory " ++ dir ++ " is not empty."

-- | Create a fresh session, using some initial configuration.
--
initSession :: SessionConfig -> IO IdeSession
initSession ideConfig@SessionConfig{..} = do
  ensureDirEmpty configSourcesDir
  ensureDirEmpty configWorkingDir
  ensureDirEmpty configDataDir

  ideState <- newMVar $ IdeSessionIdle IdeIdleState {
                          ideLogicalTimestamp = 0
                        , ideComputed         = Nothing
                        , ideNewOpts          = Nothing
                        , ideGenerateCode     = False
                        , ideManagedFiles     = ManagedFiles [] []
                        }
  ideGhcServer <- forkGhcServer configStaticOpts
  return IdeSession{..}

-- | Close a session down, releasing the resources.
--
-- This operation is the only one that can be run after a shutdown was already
-- performed. This lets the API user execute an early shutdown, e.g., before
-- the @shutdownSession@ placed inside 'bracket' is triggered by a normal
-- program control flow.
--
shutdownSession :: IdeSession -> IO ()
shutdownSession IdeSession{ideGhcServer, ideState} = do
  -- TODO: we should check that the state is not IdeSessionRunning
  modifyMVar_ ideState $ const $ return IdeSessionShutdown
  shutdownGhcServer ideGhcServer

-- | We use the 'IdeSessionUpdate' type to represent the accumulation of a
-- bunch of updates.
--
-- In particular it is an instance of 'Monoid', so multiple primitive updates
-- can be easily combined. Updates can override each other left to right.
--
newtype IdeSessionUpdate = IdeSessionUpdate (SessionConfig -> IdeIdleState -> IO IdeIdleState)

-- We assume, if updates are combined within the monoid, they can all
-- be applied in the context of the same session.
-- Otherwise, call 'updateSession' sequentially with the updates.
instance Monoid IdeSessionUpdate where
  mempty = IdeSessionUpdate $ \_ sess -> return sess
  mappend (IdeSessionUpdate f) (IdeSessionUpdate g) =
    IdeSessionUpdate $ \cfg -> f cfg >=> g cfg

-- | Given the current IDE session state, go ahead and
-- update the session, eventually resulting in a new session state,
-- with fully updated computed information (typing, etc.).
--
-- The update can be a long running operation, so we support a callback
-- which can be used to monitor progress of the operation.
updateSession ::  IdeSession -> IdeSessionUpdate -> (PCounter -> IO ()) -> IO ()
updateSession IdeSession{ideConfig = ideConfig@SessionConfig{configSourcesDir}, ideState, ideGhcServer} (IdeSessionUpdate update) callback = do
  modifyMVar_ ideState $ \state ->
    case state of
      IdeSessionIdle idleState -> do
        idleState'@IdeIdleState{ideNewOpts, ideGenerateCode} <- update ideConfig idleState
        (computedErrors, computedLoadedModules) <-
          rpcCompile ideGhcServer ideNewOpts configSourcesDir ideGenerateCode callback
        return $ IdeSessionIdle idleState' {ideComputed = Just Computed{..}}
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
updateModule m bs =
    IdeSessionUpdate $ \ideConfig state@IdeIdleState{ ideLogicalTimestamp
                                                    , ideManagedFiles } -> do
      let internal = internalFile ideConfig m
      writeFileAtomic internal bs
      setFileTimes internal (fromIntegral ideLogicalTimestamp) (fromIntegral ideLogicalTimestamp)
      let sF = m : sourceFiles ideManagedFiles
      return state { ideLogicalTimestamp = ideLogicalTimestamp + 1
                   , ideManagedFiles = ideManagedFiles {sourceFiles = sF} }

-- | Like 'updateModule' except that instead of passing the module source by
-- value, it's given by reference to an existing file, which will be copied.
--
updateModuleFromFile :: ModuleName -> FilePath -> IdeSessionUpdate
updateModuleFromFile m p =
    IdeSessionUpdate $ \ideConfig state@IdeIdleState{ ideLogicalTimestamp
                                                    , ideManagedFiles } -> do
      let internal = internalFile ideConfig m
      copyFile p internal
      setFileTimes internal (fromIntegral ideLogicalTimestamp) (fromIntegral ideLogicalTimestamp)
      let sF = m : sourceFiles ideManagedFiles
      return state { ideLogicalTimestamp = ideLogicalTimestamp + 1
                   , ideManagedFiles = ideManagedFiles {sourceFiles = sF} }

-- | A session update that deletes an existing module.
--
updateModuleDelete :: ModuleName -> IdeSessionUpdate
updateModuleDelete m =
    IdeSessionUpdate $ \ideConfig state@IdeIdleState{ideManagedFiles} -> do
      removeFile (internalFile ideConfig m)
      let sF = delete m (sourceFiles ideManagedFiles)
      return state {ideManagedFiles = ideManagedFiles {sourceFiles = sF}}

-- | Warning: only dynamic flags can be set here.
-- Static flags need to be set at server startup.
updateGhcOptions :: (Maybe [String]) -> IdeSessionUpdate
updateGhcOptions opts =
    IdeSessionUpdate $ \_ideConfig state ->
      return $ state {ideNewOpts = opts}

-- | Enable or disable code generation in addition
-- to type-checking. Required by 'runStmt.
updateCodeGeneration :: Bool -> IdeSessionUpdate
updateCodeGeneration b =
    IdeSessionUpdate $ \_ideConfig state ->
      return $ state {ideGenerateCode = b}

internalFile :: SessionConfig -> ModuleName -> FilePath
internalFile SessionConfig{configSourcesDir} m =
  configSourcesDir </> MN.toFilePath m <.> ".hs"

-- | A session update that changes a data file by giving a new value for the
-- file. This can be used to add a new file or update an existing one.
--
updateDataFile :: FilePath -> ByteString -> IdeSessionUpdate
updateDataFile n bs =
    IdeSessionUpdate $ \SessionConfig{configDataDir}
                        state@IdeIdleState{ideManagedFiles} -> do
      writeFileAtomic (configDataDir </> n) bs
      let dF = n : dataFiles ideManagedFiles
      return state {ideManagedFiles = ideManagedFiles {dataFiles = dF}}

-- | Like 'updateDataFile' except that instead of passing the file content by
-- value, it's given by reference to an existing file, which will be copied.
--
updateDataFileFromFile :: FilePath -> FilePath -> IdeSessionUpdate
updateDataFileFromFile n p =
    IdeSessionUpdate $ \SessionConfig{configDataDir}
                        state@IdeIdleState{ideManagedFiles} -> do
      copyFile (configDataDir </> n) (configDataDir </> p)
      let dF = n : dataFiles ideManagedFiles
      return state {ideManagedFiles = ideManagedFiles {dataFiles = dF}}

-- | A session update that deletes an existing data file.
--
updateDataFileDelete :: FilePath -> IdeSessionUpdate
updateDataFileDelete n =
    IdeSessionUpdate $ \SessionConfig{configDataDir}
                        state@IdeIdleState{ideManagedFiles} -> do
      removeFile (configDataDir </> n)
      let dF = delete n (dataFiles ideManagedFiles)
      return state {ideManagedFiles = ideManagedFiles {dataFiles = dF}}

-- | The type of queries in a given session state.
--
-- Queries are in IO because they depend on the current state of the session
-- but they promise not to alter the session state (at least not in any visible
-- way, they might update caches etc).
--
type Query a = IdeSession -> IO a

-- | Read the current value of one of the source modules.
--
getSourceModule :: ModuleName -> Query ByteString
getSourceModule m IdeSession{ideConfig} =
  BS.readFile $ internalFile ideConfig m

-- | Read the current value of one of the data files.
--
getDataFile :: FilePath -> Query ByteString
getDataFile n IdeSession{ideConfig=SessionConfig{configDataDir}} =
  BS.readFile $  configDataDir </> n

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
      IdeSessionIdle IdeIdleState{ideComputed} ->
        case ideComputed of
          Just Computed{..} -> return computedErrors
-- Optionally, this could give last reported errors, instead forcing
-- IDE to wait for the next sessionUpdate to finish.
          Nothing -> fail "This session state does not admit queries."
      IdeSessionShutdown -> fail "Session already shut down."

-- | Get the list of files submitted by the user and not deleted yet.
-- The module names are those supplied by the user as the first
-- arguments of the @updateModule@ and @updateModuleFromFile@ calls,
-- as opposed to the compiler internal @module ... end@ module names.
-- Usually the two names are equal, but they neededn't be.
getManagedFiles :: Query ManagedFiles
getManagedFiles IdeSession{ideState} =
  withMVar ideState $ \st ->
  case st of
    IdeSessionIdle IdeIdleState{ideManagedFiles} ->
      return ideManagedFiles
    IdeSessionRunning IdeIdleState{ideManagedFiles} ->
      return ideManagedFiles
    IdeSessionShutdown -> fail "Session already shut down."

-- | Get the list of correctly compiled modules, as reported by the compiler.
getLoadedModules :: Query LoadedModules
getLoadedModules IdeSession{ideState} =
  withMVar ideState $ \st ->
    case st of
      IdeSessionIdle IdeIdleState{ideComputed} ->
        case ideComputed of
          Just Computed{..} -> return computedLoadedModules
          Nothing -> fail "This session state does not admit queries."
      IdeSessionShutdown -> fail "Session already shut down."

-- | Get a mapping from where symbols are used to where they are defined.
-- That is, given a symbol used at a particular location in a source module
-- the mapping tells us where that symbol is defined, either locally in a
-- source module or a top-level symbol imported from another package.
--
getSymbolDefinitionMap :: Query SymbolDefinitionMap
getSymbolDefinitionMap = undefined

-- | Run a given function in a given module (the name of the module
-- is the one between @module ... end@, which may differ from the file name).
-- The function resembles a query, but it's not instantaneous
-- and the running code can be interrupted or interacted with.
runStmt :: IdeSession -> ModuleName -> String -> IO RunActions
runStmt IdeSession{ideGhcServer,ideState} m fun = do
  modifyMVar ideState $ \state -> case state of
    IdeSessionIdle idleState@IdeIdleState{ ideComputed=Just comp
                                         , ideGenerateCode=True} ->
      -- ideManagedFiles is irrelevant, because only the module name
      -- inside 'module .. where' counts.
      if m `elem` computedLoadedModules comp
      then do
        runActions <- rpcRun ideGhcServer m fun
        let runActions' = afterRunActions runActions restoreToIdle
        return (IdeSessionRunning idleState, runActions')
      else fail $ "Module " ++ show (MN.toString m)
                  ++ " not successfully loaded, when trying to run code."
    IdeSessionIdle _ ->
      fail "Cannot run before the code is generated."
    IdeSessionRunning _ ->
      fail "Cannot run code concurrently"
    IdeSessionShutdown ->
      fail "Session already shut down."
  where
    restoreToIdle :: RunResult -> IO ()
    restoreToIdle _ = modifyMVar_ ideState $ \state -> case state of
      IdeSessionIdle _ ->
        Ex.throwIO (userError "The impossible happened!")
      IdeSessionRunning idleState -> do
        return $ IdeSessionIdle idleState
      IdeSessionShutdown ->
        return state
