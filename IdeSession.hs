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
  Progress,
  progressWaitCompletion,

  -- ** Modules
  updateModule,
  ModuleChange(..),
  ModuleName(..),

  -- ** Data files
  updateDataFile,
  DataFileChange(..),

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

  -- ** Symbol definition maps
  getSymbolDefinitionMap,
  SymbolDefinitionMap,

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

import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Concurrent
import System.IO (openBinaryTempFile, hClose)
import System.Directory
import System.FilePath ((</>), (<.>), splitFileName)
import qualified Control.Exception as Exception
import Data.Monoid (Monoid(..))
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

import Common
import GhcServer

-- This could be equally well implemented as a new mvar created per
-- each new session state, but this implementation has some minor advantages.
-- It gives more useful error messages and provides a counter of session
-- updates, which can be used, e.g.,  to roughly estimate how badly outdated
-- the info therein is, in order to decide whether to show it to the user
-- while waiting for a newer version.
data StateToken = StateToken (MVar Int) Int

initToken :: IO StateToken
initToken = do
  mv <- newMVar 0
  return $ StateToken mv 0

-- Invalidates previous sessions, returns a new token for the new session.
incrementToken :: StateToken -> IO StateToken
incrementToken token@(StateToken mv _) = do
  checkToken token
  let incrCheckToken current = do
        let newT = current + 1
        return (newT, StateToken mv newT)
  modifyMVar mv incrCheckToken

checkToken :: StateToken -> IO ()
checkToken (StateToken mv k) = do
  current <- readMVar mv
  when (k /= current) $
    error $ "Invalid session token " ++ show k ++ " /= " ++ show current

-- In this implementation, it's fully applicative, and so invalid sessions
-- can be queried at will. Note that there may be some working files
-- produced by GHC while obtaining these values. They are not captured here,
-- so queries are not allowed to read them.
type Computed = [SourceError]

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
  , ideToken     :: StateToken
  , ideComputed  :: (Maybe Computed)
  }

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
       configTempDir :: FilePath
     }

ensureDirEmpty :: FilePath -> IO ()
ensureDirEmpty dir = do
  cnts <- getDirectoryContents dir
  when (any (`notElem` [".", ".."]) cnts) $
    error $ "Directory " ++ dir ++ " is not empty"

-- | Create a fresh session, using some initial configuration.
--
initSession :: SessionConfig -> IO IdeSession
initSession ideConfig@SessionConfig{..} = do
  ensureDirEmpty configSourcesDir
  ensureDirEmpty configWorkingDir
  ensureDirEmpty configDataDir
  ensureDirEmpty configTempDir
  let ideComputed = Nothing  -- can't query before the first Progress
  ideToken <- initToken
  ideGhcServer <- forkGhcServer
  return IdeSession{..}

-- | Close a session down, releasing the resources.
--
shutdownSession :: IdeSession -> IO ()
shutdownSession IdeSession{ideToken, ideGhcServer} = do
  -- Invalidate the current session.
  void $ incrementToken ideToken
  -- Shutdown GHC server.
  shutdownGhcServer ideGhcServer

-- | We use the 'IdeSessionUpdate' type to represent the accumulation of a
-- bunch of updates.
--
-- In particular it is an instance of 'Monoid', so multiple primitive updates
-- can be easily combined. Updates can override each other left to right.
--
data IdeSessionUpdate = IdeSessionUpdate (IdeSession -> IO ())

-- We assume, if updates are combined within the monoid, they can all
-- be applied in the context of the same session.
-- Otherwise, call 'updateSession' sequentially with the updates.
instance Monoid IdeSessionUpdate where
  mempty = IdeSessionUpdate $ \ _ -> return ()
  mappend (IdeSessionUpdate f) (IdeSessionUpdate g) =
    IdeSessionUpdate $ \ sess -> f sess >> g sess

-- | Given the current IDE session state, go ahead and
-- update the session, eventually resulting in a new session state,
-- with fully updated computed information (typing, etc.).
--
-- The update can be a long running operation, so it returns a 'Progress'
-- which can be used to monitor and wait on the operation.
-- While the progress is in operation, session state tokens
-- remain valid as usual. If the progress fails or is canceled,
-- all it's observable internal state changes are rolled back
-- and another progress can be initiated. The semantics of @updateFiles@
-- and @updateSession@ is unspecified while any progress runs.
--
updateSession :: IdeSession -> IdeSessionUpdate
              -> (Progress PCounter IdeSession -> IO a) -> IO a
updateSession sess@IdeSession{ ideConfig=SessionConfig{configSourcesDir}
                             , ideGhcServer
                             , ideToken }
              (IdeSessionUpdate update) handler = do
  -- First, invalidating the current session ASAP, because the previous
  -- computed info will shortly no longer be in sync with the files.
  newToken <- incrementToken ideToken

  -- Then, updating files ASAP (using the already invalidated session).
  update sess

  -- Last, communicating with the GHC server.
  let f (RespWorking c) = c
      f (RespDone _)    = error "updateSession: unexpected RespDone"
      g (RespWorking _) = error "updateSession: unexpected RespWorking"
      g (RespDone errs) = sess {ideToken = newToken, ideComputed = Just errs}
  rpcGhcServer ideGhcServer configSourcesDir (handler . fmap2Progress f g)

-- TODO: move elsewhere? Close to Progress? Or only to test utilities?
-- | Block until the operation completes.
--
progressWaitCompletion :: Progress a b -> IO b
progressWaitCompletion p = do
  w <- progressWait p
  case w of
    Left a -> return a
    Right (_, p2) -> progressWaitCompletion p2

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
  Exception.bracketOnError
    (openBinaryTempFile targetDir $ targetFile <.> "tmp")
    (\(tmpPath, handle) -> hClose handle >> removeFile tmpPath)
    (\(tmpPath, handle) -> do
        BS.hPut handle content
        hClose handle
        renameFile tmpPath targetPath)

-- | A session update that changes a source module. Modules can be added,
-- updated or deleted.
--
updateModule :: ModuleChange -> IdeSessionUpdate
updateModule mc = IdeSessionUpdate $ \ IdeSession{ideConfig} ->
  case mc of
    ModulePut m bs -> writeFileAtomic (internalFile ideConfig m) bs
    ModuleSource m p -> copyFile p (internalFile ideConfig m)
    ModuleDelete m -> removeFile (internalFile ideConfig m)

data ModuleChange = ModulePut    ModuleName ByteString
                  | ModuleSource ModuleName FilePath
                  | ModuleDelete ModuleName

newtype ModuleName = ModuleName String

internalFile :: SessionConfig -> ModuleName -> FilePath
internalFile SessionConfig{configSourcesDir} (ModuleName n) =
  configSourcesDir </> n <.> ".hs"

-- | A session update that changes a data file. Data files can be added,
-- updated or deleted.
--
updateDataFile :: DataFileChange -> IdeSessionUpdate
updateDataFile mc =
  IdeSessionUpdate $ \ IdeSession{ideConfig=SessionConfig{configDataDir}} ->
  case mc of
     DataFilePut n bs -> writeFileAtomic (configDataDir </> n) bs
     DataFileSource n p -> copyFile (configDataDir </> n)
                                    (configDataDir </> p)
     DataFileDelete n -> removeFile (configDataDir </> n)

data DataFileChange = DataFilePut    FilePath ByteString
                    | DataFileSource FilePath FilePath
                    | DataFileDelete FilePath

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
getSourceErrors IdeSession{ideComputed} =
  let err = error $ "This session state does not admit queries."
  in return $ fromMaybe err ideComputed

-- | Get a mapping from where symbols are used to where they are defined.
-- That is, given a symbol used at a particular location in a source module
-- the mapping tells us where that symbol is defined, either locally in a
-- source module or a top-level symbol imported from another package.
--
getSymbolDefinitionMap :: Query SymbolDefinitionMap
getSymbolDefinitionMap = undefined
