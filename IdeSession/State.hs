-- | Internal state of the session
--
-- This uses the internal types only.
module IdeSession.State
  ( -- * Types
    Computed(..)
  , IdeSession(..)
  , IdeStaticInfo(..)
  , IdeSessionState(..)
  , LogicalTimestamp
  , IdeIdleState(..)
  , ManagedFilesInternal(..)
    -- * Util
  , internalFile
    -- * Accessors
  , ideLogicalTimestamp
  , ideComputed
  , ideNewOpts
  , ideGenerateCode
  , ideManagedFiles
  , ideEnv
  , ideGhcServer
  , ideStdoutBufferMode
  , ideStderrBufferMode
  , ideUpdatedEnv
  , ideUpdatedCode
  , managedSource
  , managedData
  ) where

-- TODOs:
-- 1. StrictTrie instead of Trie
-- 2. StrictMVar instread of MVar

import Data.Trie (Trie)
import Data.Digest.Pure.MD5 (MD5Digest)
import Data.Accessor (Accessor, accessor)
import Control.Concurrent.MVar (MVar)
import System.FilePath ((</>))
import System.Posix.Types (EpochTime)

import IdeSession.Types.Private
import IdeSession.Config
import IdeSession.GHC.Server (RunActions, GhcServer)
import IdeSession.GHC.Run (RunBufferMode)
import IdeSession.Strict.Map (StrictMap)

data Computed = Computed {
    -- | Last compilation and run errors
    computedErrors :: [SourceError]
    -- | Modules that got loaded okay together with their mappings
    -- from source locations to information about identifiers there
  , computedLoadedModules :: LoadedModules
    -- | Import information. This is (usually) available even for modules
    -- with parsing or type errors
  , computedImports :: !(StrictMap ModuleName [Import])
    -- | Autocompletion map
    --
    -- Mapping, per module, from prefixes to fully qualified names
    -- I.e., @fo@ might map to @Control.Monad.forM@, @Control.Monad.forM_@
    -- etc. (or possibly to @M.forM@, @M.forM_@, etc when Control.Monad
    -- was imported qualified as @M@).
  , computedAutoMap :: !(StrictMap ModuleName (Trie [IdInfo]))
    -- | We access IdProps indirectly through this cache
  , computedCache :: !ExplicitSharingCache
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

type LogicalTimestamp = EpochTime

data IdeIdleState = IdeIdleState {
    -- A workaround for http://hackage.haskell.org/trac/ghc/ticket/7473.
    -- Logical timestamps (used to force ghc to recompile files)
    _ideLogicalTimestamp :: LogicalTimestamp
    -- The result computed by the last 'updateSession' invocation.
  , _ideComputed         :: Maybe Computed
    -- Compiler dynamic options. If they are not set, the options from
    -- SessionConfig are used.
  , _ideNewOpts          :: Maybe [String]
    -- Whether to generate code in addition to type-checking.
  , _ideGenerateCode     :: Bool
    -- Files submitted by the user and not deleted yet.
  , _ideManagedFiles     :: ManagedFilesInternal
    -- Environment overrides
  , _ideEnv              :: [(String, Maybe String)]
    -- The GHC server (this is replaced in 'restartSession')
  , _ideGhcServer        :: GhcServer
    -- Buffer mode for standard output for 'runStmt'
  , _ideStdoutBufferMode :: RunBufferMode
    -- Buffer mode for standard error for 'runStmt'
  , _ideStderrBufferMode :: RunBufferMode
    -- Has the environment (as recorded in this state) diverged from the
    -- environment on the server?
  , _ideUpdatedEnv       :: Bool
    -- Has the code diverged from what has been loaded into GHC on the last
    -- call to 'updateSession'?
  , _ideUpdatedCode      :: Bool
  }

-- | The collection of source and data files submitted by the user.
data ManagedFilesInternal = ManagedFilesInternal
  { _managedSource :: [(FilePath, (MD5Digest, LogicalTimestamp))]
  , _managedData   :: [FilePath]
  }

{------------------------------------------------------------------------------
  Util
------------------------------------------------------------------------------}

internalFile :: FilePath -> FilePath -> FilePath
internalFile ideSourcesDir m = ideSourcesDir </> m

{------------------------------------------------------------------------------
  Accessors
------------------------------------------------------------------------------}

ideLogicalTimestamp :: Accessor IdeIdleState LogicalTimestamp
ideComputed         :: Accessor IdeIdleState (Maybe Computed)
ideNewOpts          :: Accessor IdeIdleState (Maybe [String])
ideGenerateCode     :: Accessor IdeIdleState Bool
ideManagedFiles     :: Accessor IdeIdleState ManagedFilesInternal
ideEnv              :: Accessor IdeIdleState [(String, Maybe String)]
ideGhcServer        :: Accessor IdeIdleState GhcServer
ideStdoutBufferMode :: Accessor IdeIdleState RunBufferMode
ideStderrBufferMode :: Accessor IdeIdleState RunBufferMode
ideUpdatedEnv       :: Accessor IdeIdleState Bool
ideUpdatedCode      :: Accessor IdeIdleState Bool

ideLogicalTimestamp = accessor _ideLogicalTimestamp $ \x s -> s { _ideLogicalTimestamp = x }
ideComputed         = accessor _ideComputed         $ \x s -> s { _ideComputed         = x }
ideNewOpts          = accessor _ideNewOpts          $ \x s -> s { _ideNewOpts          = x }
ideGenerateCode     = accessor _ideGenerateCode     $ \x s -> s { _ideGenerateCode     = x }
ideManagedFiles     = accessor _ideManagedFiles     $ \x s -> s { _ideManagedFiles     = x }
ideEnv              = accessor _ideEnv              $ \x s -> s { _ideEnv              = x }
ideGhcServer        = accessor _ideGhcServer        $ \x s -> s { _ideGhcServer        = x }
ideStdoutBufferMode = accessor _ideStdoutBufferMode $ \x s -> s { _ideStdoutBufferMode = x }
ideStderrBufferMode = accessor _ideStderrBufferMode $ \x s -> s { _ideStderrBufferMode = x }
ideUpdatedEnv       = accessor _ideUpdatedEnv       $ \x s -> s { _ideUpdatedEnv       = x }
ideUpdatedCode      = accessor _ideUpdatedCode      $ \x s -> s { _ideUpdatedCode      = x }

managedSource :: Accessor ManagedFilesInternal [(FilePath, (MD5Digest, LogicalTimestamp))]
managedData   :: Accessor ManagedFilesInternal [FilePath]

managedSource = accessor _managedSource $ \x s -> s { _managedSource = x }
managedData   = accessor _managedData   $ \x s -> s { _managedData   = x }
