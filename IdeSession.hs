{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, DeriveDataTypeable #-}
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

    -- * Common data types
    IdNameSpace(..)
  , IdInfo
  , IdProp(..)
  , IdScope(..)
  , SourceSpan(..)
  , EitherSpan(..)
  , SourceError(..)
  , SourceErrorKind(..)
  , ModuleName
  , ModuleId(..)
  , PackageId(..)
  , IdMap(..)
  , LoadedModules

    -- * Explicit sharing
  , XShared
  , ExplicitSharingCache(..)
  , ExplicitSharing(..)
  , showNormalized

    -- * Progress
  , Progress(..)
  , initialProgress
  , updateProgress
  , progressStep

  -- * Sessions
  , IdeSession

  -- ** Initialisation and shutdown
  -- | Sessions are stateful and must be initialised and shut down so that
  -- resources can be released.
  , InProcess
  , initSession
  , shutdownSession
  , SessionConfig(..)
  , getSessionConfig
  , getSourcesDir
  , getDataDir
  , restartSession

  -- * Updates
  -- | Updates are done in batches: we collect together all of the updates we
  -- want to do and then do a single transition, applying all the updates,
  -- and end up in a new state.

  -- ** Declarative updates
  -- | So that we can batch the updates, all the updates are declarative.
  -- The 'IdeSessionUpdate' monoid is used to represent the updates, and the
  -- sub-sections below describe the various updates that are available.
  , IdeSessionUpdate

  -- ** Modules
  , updateModule
  , updateModuleFromFile
  , updateModuleDelete

  -- ** Flags and other settings
  , updateGhcOptions
  , updateCodeGeneration

  -- ** Data files
  , updateDataFile
  , updateDataFileFromFile
  , updateDataFileDelete

  -- ** Environment variables
  , updateEnv

  -- ** Buffer mode
  , updateStdoutBufferMode
  , updateStderrBufferMode

  -- ** Performing the update
  -- | Once we have accumulated a batch of updates we can perform them all
  -- giving us a new session state. Since performing a bunch of updates can
  -- involve compiling modules and can take some time, the update uses the
  -- 'Progress' type to represent intermediate progress information.
  , updateSession

  -- * Queries
  , Query

  -- ** Source errors
  , getSourceErrors

  -- ** Files
  -- | Simply getting the current state of the persistent files fits the
  -- queries pattern.
  , getSourceModule
  , getDataFile

  -- ** The list of managed files and all data files
  , ManagedFiles(..)
  , getManagedFiles
  , getAllDataFiles

  -- ** Environment variables
  , getEnv

  -- ** Symbol definition maps
  , getLoadedModules
  , getExplicitSharingCache
  , haddockLink
  -- , idInfoAtLocation

  -- ** Import information and autocompletion
  , Import(..)
  , getImports
  , getAutocompletion
  , idInfoQN

  -- ** Run code
  , runStmt
  , RunResult(..)
  , RunBufferMode(..)
  , RunActions -- We don't export the constructor nor all accessors
  , interrupt
  , runWait
  , supplyStdin
  , runWaitAll
  , registerTerminationCallback

  -- ** Start and diagnose the server (probably only for debugging)
  , ghcServer
  , getGhcServer
  , getGhcExitCode

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
  -- at any time (before the session is closed).

  -- ** Morally pure queries
  -- | Morally, a compiler is a pure function from the current value of the
  -- various source files (and other bits of the environment) to object code
  -- and\/or other information about the modules (errors, types etc).
  --
  -- The intention is to reflect this purity property in this interface. The
  -- value of an 'IdeSession' represents the state of the files\/modules and
  -- contains the other parameters supplied by the user (compiler options,
  -- environment variables). It also contains or represents the result
  -- of the pure compilation function. It should always be the case
  -- that we can throw away all the compilation results and recover
  -- them just from the file state and user parameters.
  --
  -- One example where this notion makes a difference is with warnings.
  -- Traditionally, compilers just return the warnings for the modules they
  -- compiled, skipping warnings for the modules they didn't need to recompile.
  -- But this doesn't match the pure function idea, because the compilation
  -- result now depends on which steps we took to get there, rather than just
  -- on the current value of the files. So one of the things this wrapper can
  -- do is to restore the purity in these corner cases (which otherwise the
  -- client of this API would probably have to do).

  -- ** Persistent and transitory state
  -- | The persistent state is obviously the files: source files and data
  -- files, as well as user-supplied parameters of the compilation.
  -- Internally there is a great deal of transitory and cached state,
  -- either in memory or on disk (such as .hi files on disk or the equivalent
  -- in memory). Note that none of the state persists in case of a fatal
  -- internal error (the files are wiped out before shutdown) and only
  -- the files persist in case of a power failure (but have to be
  -- recovered manually).
  --
  -- It should be possible to drop all the transitory state and recover,
  -- just at the cost of some extra work, as long as the original @Session@
  -- value is available. The 'restartSession' function does almost
  -- exactly that.
  --
  -- This property is a useful correctness property for internal testing: the
  -- results of all the queries should be the same before and after blowing
  -- away all the transitory state and recovering.
) where

import IdeSession.Types.Public
import IdeSession.Types.Private (ExplicitSharingCache(..))
import IdeSession.Types.Translation
import IdeSession.Types.Progress
import IdeSession.GHC.Server
import IdeSession.GHC.Run (RunResult(..), RunBufferMode(..))
import IdeSession.GHC.HsWalk
import IdeSession.State
import IdeSession.Config
import IdeSession.Query
import IdeSession.Update
