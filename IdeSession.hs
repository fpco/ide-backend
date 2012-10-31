module IdeSession (

  -- | This module provides an interface to the IDE backend.
  --
  -- It centers around the idea of a single threaded IDE session, and
  -- operations for updating the session or running queries given the current
  -- state of the session.

  -- * Interaction with the compiler.
  --
  -- | Ironically for a pure functional language, the interface to the compiler
  -- is rather stateful and sequential. In part this is because it's dealing
  -- with the state of files in the file system which are of course mutable
  -- variables.
  --
  -- So the general pattern of interation is sequential and single-threaded.
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
  -- | Sessions are staterful and must be initialised and shut down so that
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
  Progress,

  -- ** Modules
  updateModule,
  ModuleChange(..),

  -- ** Data files
  updateDataFile,
  DataFileChange(..),
  
  -- * Queries
  -- ** Source errors
  getSourceErrors,
  SourceError,
  
  -- ** Symbol definition maps
  getSymbolDefinitionMap,
  SymbolDefinitionMap,

  -- * Additional notes
  -- ** Responsibilty for managing and mutating files in the sources dir.
  -- | In general, updating and changing source files in the sources dir has to
  -- be coordinated with the IdeSession, since we're in a concurrent mutable
  -- setting.
  --
  -- The compromise is that the caller can manage the files prior to initialising
  -- a session, but while the session is active, all file changes must be managed
  -- via the session, and sequenced relative to other session state changes.
  --

) where

import Data.Monoid (Monoid(..))
import qualified Data.ByteString.Lazy as BS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BS

-- | This is a state token for the current state of an IDE session. We can run
-- queries in the current state, and from this state we can perform a batch
-- of updates, ultimately leading to a new 'IdeSession'.
--
-- Note that these state tokens are not persistent, once we perform an update
-- and get a new 'IdeSession', the old one is no longer valid.
--
data IdeSession

-- | Configuration paramaters for a session. These remain the same throughout
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

       -- | The directroy to use for purely temporary files.
       configTempDir :: FilePath
     }

-- | Create a fresh session, using some initial configuration.
--
initSession :: SessionConfig -> IO IdeSession
initSession = undefined

-- | Close a session down, releasing the resources.
--
shutdownSession :: IdeSession -> IO ()
shutdownSession = undefined


-- | We use the 'IdeSessionUpdate' type to represent the accumulation of a
-- bunch of updates.
-- 
-- In particular it is an instance of 'Monoid', so multiple primitive updates
-- can be easily combined. Updates can override each other left to right.
--
data IdeSessionUpdate
instance Monoid IdeSessionUpdate


-- | Given the current IDE session state, and a bunch of updates, go ahead and
-- update the session, eventually resulting in a new session state.
--
-- The update can be a long running operation, so it returns a 'Progress'
-- which can be used to monitor and wait on the operation.
--
updateSession :: IdeSession -> IdeSessionUpdate -> Progress IdeSession
updateSession = undefined

-- | A future, a handle on an action that will produce some result.
--
data Progress a

-- | A session update that changes a source module. Modules can be added,
-- updated or deleted.
--
updateModule :: ModuleChange -> IdeSessionUpdate
updateModule = undefined
data ModuleChange = ModulePut    ModuleName ByteString
                  | ModuleDelete ModuleName

data ModuleName

-- | A session update that changes a data file. Data files can be added,
-- updated or deleted.
--
updateDataFile :: DataFileChange -> IdeSessionUpdate
updateDataFile = undefined
data DataFileChange = DataFilePut    FilePath ByteString
                    | DataFileDelete FilePath

-- | The type of queries in a given session state.
--
-- Queries are in IO because they depend on the current state of the session
-- but they promise not to alter the session state (at least not in any visible
-- way, they might update caches etc).
--
type Query a = IdeSession -> IO a

-- | Get any compilation errors or warnings in the current state of the
-- session, meaning errors that GHC reports for the current state of all the
-- source modules.
--
-- Note that it is all errors and warnings in the current state, not just extra
-- errors from the last session change. In particular this applies to warnings.
--
getSourceErrors :: Query [SourceError]
getSourceErrors = undefined

-- | An error or warning in a source module.
--
-- Most errors are associated with a span of text, but some have only a
-- location point.
--
data SourceError


-- | A mapping from symbol uses to symbol definitions
--
data SymbolDefinitionMap

-- | Get a mapping from where symbols are used to where they are defined.
-- That is, given a symbol used at a particular location in a source module
-- the mapping tells us where that symbol is defined, either locally in a
-- source module or a top-level symbol imported from another package.
--
getSymbolDefinitionMap :: Query SymbolDefinitionMap
getSymbolDefinitionMap = undefined

