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
  , GhcServer(..)
  , RunActions(..)
    -- * Util
  , internalFile
    -- * Accessors
  , ideLogicalTimestamp
  , ideComputed
  , ideGhcOpts
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
  , ideUpdatedEnv
  , ideUpdatedCode
  , ideUpdatedArgs
  , ideUpdatedGhcOpts
  , ideBreakInfo
  , ideTargets
  , managedSource
  , managedData
  ) where

import Control.Concurrent (ThreadId)
import Data.Digest.Pure.MD5 (MD5Digest)
import Data.Accessor (Accessor, accessor)
import qualified Data.ByteString as BSS
import System.Exit (ExitCode)
import System.FilePath ((</>))
import System.Posix.Types (EpochTime)
import IdeSession.Types.Private hiding (RunResult)
import qualified IdeSession.Types.Public as Public
import IdeSession.Config
import IdeSession.Strict.Container
import IdeSession.Strict.MVar (StrictMVar)
import IdeSession.RPC.Client (RpcServer, RpcConversation, ExternalException)
import IdeSession.GHC.API (GhcVersion)

data Computed = Computed {
    -- | Last compilation and run errors
    computedErrors :: !(Strict [] SourceError)
    -- | Modules that got loaded okay
  , computedLoadedModules :: !(Strict [] ModuleName)
    -- | Import information. This is (usually) available even for modules
    -- with parsing or type errors
  , computedImports :: !(Strict (Map ModuleName) (Strict [] Import))
    -- | Autocompletion map
    --
    -- Mapping, per module, from prefixes to fully qualified names
    -- I.e., @fo@ might map to @Control.Monad.forM@, @Control.Monad.forM_@
    -- etc. (or possibly to @M.forM@, @M.forM_@, etc when Control.Monad
    -- was imported qualified as @M@).
  , computedAutoMap :: !(Strict (Map ModuleName) (Strict Trie (Strict [] IdInfo)))
    -- | Information about identifiers/quasi-quotes
  , computedSpanInfo :: !(Strict (Map ModuleName) IdMap)
    -- | Type information about subexpressions
  , computedExpTypes :: !(Strict (Map ModuleName) ExpMap)
    -- | Use sites
  , computedUseSites :: !(Strict (Map ModuleName) UseSites)
    -- | (Transitive) package dependencies
  , computedPkgDeps :: !(Strict (Map ModuleName) (Strict [] PackageId))
    -- | We access IdProps indirectly through this cache
  , computedCache :: !ExplicitSharingCache
  }
  deriving Show

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
    -- | The directory to use for managing source files.
  , ideSourcesDir :: FilePath
    -- | The directory to use for data files that may be accessed by the
    -- running program. The running program will have this as its CWD.
  , ideDataDir    :: FilePath
    -- Cabal "dist" prefix.
  , ideDistDir    :: FilePath
  }

data IdeSessionState =
    IdeSessionIdle IdeIdleState
  | IdeSessionRunning (RunActions Public.RunResult) IdeIdleState
  | IdeSessionShutdown
  | IdeSessionServerDied ExternalException IdeIdleState

type LogicalTimestamp = EpochTime

data IdeIdleState = IdeIdleState {
    -- | A workaround for http://hackage.haskell.org/trac/ghc/ticket/7473.
    -- Logical timestamps (used to force ghc to recompile files)
    _ideLogicalTimestamp :: !LogicalTimestamp
    -- | The result computed by the GHC API typing/compilation invocation
    -- in the last call to 'updateSession' invocation.
  , _ideComputed         :: !(Strict Maybe Computed)
    -- | Additional ghc options
  , _ideGhcOpts          :: ![String]
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
    -- | Has the environment (as recorded in this state) diverged from the
    -- environment on the server?
  , _ideUpdatedEnv       :: !Bool
    -- | Has the code diverged from what has been loaded into GHC on the last
    -- call to 'updateSession'?
  , _ideUpdatedCode      :: !Bool
    -- | Has the value of ideArgs diverged from what's recorded on the server?
  , _ideUpdatedArgs      :: !Bool
    -- | Has the value of ideGhcOpts diverged from what's recorded on the server?
  , _ideUpdatedGhcOpts   :: !Bool
    -- | Are we currently in a breakpoint?
  , _ideBreakInfo        :: !(Strict Maybe Public.BreakInfo)
    -- | Targets for compilation
  , _ideTargets          :: !(Maybe [FilePath])
  }

-- | The collection of source and data files submitted by the user.
data ManagedFilesInternal = ManagedFilesInternal
  { _managedSource :: [(FilePath, (MD5Digest, LogicalTimestamp))]
  , _managedData   :: [FilePath]
  }

-- | Mapping from C files to the corresponding .o files and their timestamps
type ObjectFiles = [(FilePath, (FilePath, LogicalTimestamp))]

data GhcServer = OutProcess RpcServer
               | InProcess RpcConversation ThreadId

-- | Handles to the running code, through which one can interact with the code.
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
    -- | Register a callback to be invoked when the program terminates
    -- The callback will only be invoked once.
    --
    -- A call to 'registerTerminationCallback' after the snippet has terminated
    -- has no effect. The termination handler is NOT called when the the
    -- 'RunActions' is 'forceCancel'ed.
  , registerTerminationCallback :: (a -> IO ()) -> IO ()
    -- | Force terminate the runaction
    -- (The server will be useless after this -- for internal use only).
    --
    -- Guranteed not to block.
  , forceCancel :: IO ()
  }

{------------------------------------------------------------------------------
  Util
------------------------------------------------------------------------------}

internalFile :: FilePath -> FilePath -> FilePath
internalFile ideSourcesDir m = ideSourcesDir </> m

{------------------------------------------------------------------------------
  Accessors
------------------------------------------------------------------------------}

ideLogicalTimestamp    :: Accessor IdeIdleState LogicalTimestamp
ideComputed            :: Accessor IdeIdleState (Strict Maybe Computed)
ideGhcOpts             :: Accessor IdeIdleState [String]
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
ideUpdatedEnv          :: Accessor IdeIdleState Bool
ideUpdatedCode         :: Accessor IdeIdleState Bool
ideUpdatedArgs         :: Accessor IdeIdleState Bool
ideUpdatedGhcOpts      :: Accessor IdeIdleState Bool
ideBreakInfo           :: Accessor IdeIdleState (Strict Maybe Public.BreakInfo)
ideTargets             :: Accessor IdeIdleState (Maybe [FilePath])

ideLogicalTimestamp = accessor _ideLogicalTimestamp $ \x s -> s { _ideLogicalTimestamp = x }
ideComputed         = accessor _ideComputed         $ \x s -> s { _ideComputed         = x }
ideGhcOpts          = accessor _ideGhcOpts          $ \x s -> s { _ideGhcOpts          = x }
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
ideUpdatedEnv       = accessor _ideUpdatedEnv       $ \x s -> s { _ideUpdatedEnv       = x }
ideUpdatedCode      = accessor _ideUpdatedCode      $ \x s -> s { _ideUpdatedCode      = x }
ideUpdatedArgs      = accessor _ideUpdatedArgs      $ \x s -> s { _ideUpdatedArgs      = x }
ideUpdatedGhcOpts   = accessor _ideUpdatedGhcOpts   $ \x s -> s { _ideUpdatedGhcOpts   = x }
ideBreakInfo        = accessor _ideBreakInfo        $ \x s -> s { _ideBreakInfo        = x }
ideTargets          = accessor _ideTargets          $ \x s -> s { _ideTargets          = x }

managedSource :: Accessor ManagedFilesInternal [(FilePath, (MD5Digest, LogicalTimestamp))]
managedData   :: Accessor ManagedFilesInternal [FilePath]

managedSource = accessor _managedSource $ \x s -> s { _managedSource = x }
managedData   = accessor _managedData   $ \x s -> s { _managedData   = x }
