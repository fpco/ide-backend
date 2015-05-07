-- | Internal state of the session
--
-- This uses the internal types only.
module IdeSession.State
  ( -- * Types
    IdeSession(..)
  , IdeStaticInfo(..)
  , IdeSessionState(..)
  , LogicalTimestamp
  , IdeIdleState(..)
  , ManagedFilesInternal(..)
  , ManagedFile
  , GhcServer(..)
  , RunActions(..)
    -- * Accessors
  , ideLogicalTimestamp
  , ideComputed
  , ideGhcOpts
  , ideRelativeIncludes
  , ideGenerateCode
  , ideManagedFiles
  , ideObjectFiles
  , ideBuildExeStatus
  , ideBuildDocStatus
  , ideBuildLicensesStatus
  , ideEnv
  , ideArgs
  , ideGhcServer
  , ideGhcVersion
  , ideStdoutBufferMode
  , ideStderrBufferMode
  , ideBreakInfo
  , ideTargets
  , ideRtsOpts
  , managedSource
  , managedData
  ) where

import Control.Concurrent (ThreadId)
import Data.Accessor (Accessor, accessor)
import Data.Digest.Pure.MD5 (MD5Digest)
import System.Exit (ExitCode)
import System.Posix.Types (EpochTime)
import qualified Data.ByteString as BSS

import IdeSession.Config
import IdeSession.GHC.API (GhcVersion)
import IdeSession.RPC.Client (RpcServer, RpcConversation, ExternalException)
import IdeSession.Strict.Container
import IdeSession.Strict.MVar (StrictMVar)
import IdeSession.Types.Private hiding (RunResult)
import qualified IdeSession.Types.Public as Public

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
  , ideState      :: StrictMVar IdeSessionState
  }

data IdeStaticInfo = IdeStaticInfo {
    -- | Configuration
    ideConfig     :: SessionConfig
    -- | (Temporary) directory for session files
    --
    -- See also:
    -- * 'ideSessionSourceDir'
    -- * 'ideSessionDataDir',
    -- * 'ideSessionDistDir'
  , ideSessionDir :: FilePath
  }

data IdeSessionState =
    IdeSessionIdle IdeIdleState
  | IdeSessionShutdown
  | IdeSessionServerDied ExternalException IdeIdleState

type LogicalTimestamp = EpochTime

-- TODO: We should split this into local state and (cached) "remote" state.
-- Then we can change IdeSessionUpdate to have access to the local state only
-- (IdeSessionUpdates _schedules_ remote updates, but doesn't run them; this
-- happens only in updateSession).
data IdeIdleState = IdeIdleState {
    -- | A workaround for http://hackage.haskell.org/trac/ghc/ticket/7473.
    -- Logical timestamps (used to force ghc to recompile files)
    _ideLogicalTimestamp :: !LogicalTimestamp
    -- | The result computed by the GHC API typing/compilation invocation
    -- in the last call to 'updateSession' invocation.
  , _ideComputed         :: !(Strict Maybe Computed)
    -- | Current GHC options
  , _ideGhcOpts          :: ![String]
    -- | Include paths (equivalent of GHC's @-i@ parameter) relative to the
    -- temporary directory where we store the session's source files.
    -- The initial value, used also for server startup, is taken from
    -- 'configRelativeIncludes'.
    --
    -- By default this is the singleton list @[""]@ -- i.e., we include the
    -- sources dir (located there in simple setups, e.g., ide-backend tests)
    -- but nothing else.
  , _ideRelativeIncludes :: ![FilePath]
    -- | Whether to generate code in addition to type-checking.
  , _ideGenerateCode     :: !Bool
    -- | Files submitted by the user and not deleted yet.
  , _ideManagedFiles     :: !ManagedFilesInternal
    -- | Object files created from .c files
  , _ideObjectFiles      :: !ObjectFiles
    -- | Exit status of the last invocation of 'buildExe', if any.
  , _ideBuildExeStatus   :: !(Maybe ExitCode)
    -- | Exit status of the last invocation of 'buildDoc', if any.
  , _ideBuildDocStatus   :: !(Maybe ExitCode)
    -- | Exit status of the last invocation of 'buildDoc', if any.
  , _ideBuildLicensesStatus :: !(Maybe ExitCode)
    -- | Environment overrides
  , _ideEnv              :: ![(String, Maybe String)]
    -- | Command line arguments for snippets (expected value of `getArgs`)
  , _ideArgs             :: ![String]
    -- | The GHC server (this is replaced in 'restartSession')
  , _ideGhcServer        :: GhcServer -- Intentionally lazy
    -- | GHC version
  , _ideGhcVersion       :: GhcVersion -- Intentionally lazy
    -- | Buffer mode for standard output for 'runStmt'
  , _ideStdoutBufferMode :: !Public.RunBufferMode
    -- | Buffer mode for standard error for 'runStmt'
  , _ideStderrBufferMode :: !Public.RunBufferMode
    -- | Are we currently in a breakpoint?
  , _ideBreakInfo        :: !(Strict Maybe Public.BreakInfo)
    -- | Targets for compilation
  , _ideTargets          :: !Public.Targets
    -- | RTS options (for the ghc session, not for executables)
  , _ideRtsOpts          :: [String]
  }

-- | The collection of source and data files submitted by the user.
data ManagedFilesInternal = ManagedFilesInternal
  { _managedSource :: [ManagedFile]
  , _managedData   :: [ManagedFile]
  }

type ManagedFile = (FilePath, (MD5Digest, LogicalTimestamp))

-- | Mapping from C files to the corresponding .o files and their timestamps
type ObjectFiles = [(FilePath, (FilePath, LogicalTimestamp))]

data GhcServer = OutProcess RpcServer
               | InProcess RpcConversation ThreadId

-- | Handles to the running code snippet, through which one can interact
-- with the snippet.
--
-- Requirement: concurrent uses of @supplyStdin@ should be possible,
-- e.g., two threads that share a @RunActions@ should be able to provide
-- input concurrently without problems. (Currently this is ensured
-- by @supplyStdin@ writing to a channel.)
data RunActions a = RunActions {
    -- | Wait for the code to output something or terminate
    runWait :: IO (Either BSS.ByteString a)
    -- | Send a UserInterrupt exception to the code
    --
    -- A call to 'interrupt' after the snippet has terminated has no effect.
  , interrupt :: IO ()
    -- | Make data available on the code's stdin
    --
    -- A call to 'supplyStdin' after the snippet has terminated has no effect.
  , supplyStdin :: BSS.ByteString -> IO ()
    -- | Force terminate the runaction
    -- (The server will be useless after this -- for internal use only).
    --
    -- Guranteed not to block.
  , forceCancel :: IO ()
  }

{------------------------------------------------------------------------------
  Accessors
------------------------------------------------------------------------------}

ideLogicalTimestamp    :: Accessor IdeIdleState LogicalTimestamp
ideComputed            :: Accessor IdeIdleState (Strict Maybe Computed)
ideGhcOpts             :: Accessor IdeIdleState [String]
ideRelativeIncludes    :: Accessor IdeIdleState [FilePath]
ideGenerateCode        :: Accessor IdeIdleState Bool
ideManagedFiles        :: Accessor IdeIdleState ManagedFilesInternal
ideObjectFiles         :: Accessor IdeIdleState ObjectFiles
ideBuildExeStatus      :: Accessor IdeIdleState (Maybe ExitCode)
ideBuildDocStatus      :: Accessor IdeIdleState (Maybe ExitCode)
ideBuildLicensesStatus :: Accessor IdeIdleState (Maybe ExitCode)
ideEnv                 :: Accessor IdeIdleState [(String, Maybe String)]
ideArgs                :: Accessor IdeIdleState [String]
ideGhcServer           :: Accessor IdeIdleState GhcServer
ideGhcVersion          :: Accessor IdeIdleState GhcVersion
ideStdoutBufferMode    :: Accessor IdeIdleState Public.RunBufferMode
ideStderrBufferMode    :: Accessor IdeIdleState Public.RunBufferMode
ideBreakInfo           :: Accessor IdeIdleState (Strict Maybe Public.BreakInfo)
ideTargets             :: Accessor IdeIdleState Public.Targets
ideRtsOpts             :: Accessor IdeIdleState [String]

ideLogicalTimestamp = accessor _ideLogicalTimestamp $ \x s -> s { _ideLogicalTimestamp = x }
ideComputed         = accessor _ideComputed         $ \x s -> s { _ideComputed         = x }
ideGhcOpts          = accessor _ideGhcOpts          $ \x s -> s { _ideGhcOpts          = x }
ideRelativeIncludes = accessor _ideRelativeIncludes $ \x s -> s { _ideRelativeIncludes = x }
ideGenerateCode     = accessor _ideGenerateCode     $ \x s -> s { _ideGenerateCode     = x }
ideManagedFiles     = accessor _ideManagedFiles     $ \x s -> s { _ideManagedFiles     = x }
ideObjectFiles      = accessor _ideObjectFiles      $ \x s -> s { _ideObjectFiles      = x }
ideBuildExeStatus   = accessor _ideBuildExeStatus   $ \x s -> s { _ideBuildExeStatus   = x }
ideBuildDocStatus   = accessor _ideBuildDocStatus   $ \x s -> s { _ideBuildDocStatus   = x }
ideBuildLicensesStatus =
  accessor _ideBuildLicensesStatus $ \x s -> s { _ideBuildLicensesStatus = x }
ideEnv              = accessor _ideEnv              $ \x s -> s { _ideEnv              = x }
ideArgs             = accessor _ideArgs             $ \x s -> s { _ideArgs             = x }
ideGhcServer        = accessor _ideGhcServer        $ \x s -> s { _ideGhcServer        = x }
ideGhcVersion       = accessor _ideGhcVersion       $ \x s -> s { _ideGhcVersion       = x }
ideStdoutBufferMode = accessor _ideStdoutBufferMode $ \x s -> s { _ideStdoutBufferMode = x }
ideStderrBufferMode = accessor _ideStderrBufferMode $ \x s -> s { _ideStderrBufferMode = x }
ideBreakInfo        = accessor _ideBreakInfo        $ \x s -> s { _ideBreakInfo        = x }
ideTargets          = accessor _ideTargets          $ \x s -> s { _ideTargets          = x }
ideRtsOpts          = accessor _ideRtsOpts          $ \x s -> s { _ideRtsOpts          = x }

managedSource :: Accessor ManagedFilesInternal [ManagedFile]
managedData   :: Accessor ManagedFilesInternal [ManagedFile]

managedSource = accessor _managedSource $ \x s -> s { _managedSource = x }
managedData   = accessor _managedData   $ \x s -> s { _managedData   = x }
