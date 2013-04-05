{-# LANGUAGE TemplateHaskell #-}
-- | Session queries
--
-- We have to be very careful in the types in this module. We should not be
-- using internal types (with explicit sharing or types such as StrictMap),
-- except as part of abstract XShared types.
module IdeSession.Query (
    -- * Types
    ManagedFiles(..)
  , Query
    -- * Queries
  , getSessionConfig
  , getSourcesDir
  , getDataDir
  , getSourceModule
  , getDataFile
  , getSourceErrors
  , getManagedFiles
  , getLoadedModules
  , getExplicitSharingCache
  , getImports
  , getAutocompletion
  , getCodeGeneration
  , getAllDataFiles
  , getEnv
  , getGhcServer
  ) where

import Data.List (isInfixOf)
import Data.Accessor ((^.))
import Data.Trie (Trie)
import qualified Data.Trie as Trie
import Data.Map (Map)
import qualified Data.Map as Map
import qualified System.FilePath.Find as Find
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSSC
import System.FilePath ((</>))

import IdeSession.Config
import IdeSession.State
import IdeSession.Types.Translation
import IdeSession.Types.Public
import IdeSession.Types.Private (ExplicitSharingCache)
import IdeSession.BlockingOps (withMVar)
import IdeSession.GHC.Server (GhcServer)

-- | The collection of source and data files submitted by the user.
data ManagedFiles = ManagedFiles
  { sourceFiles :: [FilePath]
  , dataFiles   :: [FilePath]
  }

-- | Recover the fixed config the session was initialized with.
getSessionConfig :: IdeSession -> SessionConfig
getSessionConfig = ideConfig . ideStaticInfo

-- | Obtain the source files directory for this session.
getSourcesDir :: IdeSession -> FilePath
getSourcesDir = ideSourcesDir . ideStaticInfo

-- | Obtain the data files directory for this session.
getDataDir :: IdeSession -> FilePath
getDataDir = ideDataDir . ideStaticInfo


-- | The type of queries in a given session state.
--
-- Queries are in IO because they depend on the current state of the session
-- but they promise not to alter the session state (at least not in any visible
-- way; they might update caches, etc.).
--
type Query a = IdeSession -> IO a

-- | Read the current value of one of the source modules.
--
getSourceModule :: FilePath -> Query BSL.ByteString
getSourceModule m IdeSession{ideStaticInfo} =
  BSL.readFile $ internalFile (ideSourcesDir ideStaticInfo) m

-- | Read the current value of one of the data files.
--
getDataFile :: FilePath -> Query BSL.ByteString
getDataFile n IdeSession{ideStaticInfo} =
  BSL.readFile $ ideDataDir ideStaticInfo </> n

-- | Get any compilation errors or warnings in the current state of the
-- session, meaning errors that GHC reports for the current state of all the
-- source modules.
--
-- Note that in the initial implementation this will only return warnings from
-- the modules that changed in the last update, the intended semantics is that
-- morally it be a pure function of the current state of the files, and so it
-- would return all warnings (as if you did clean and rebuild each time).
--
-- getSourceErrors does internal normalization. This simplifies the life of the
-- client and anyway there shouldn't be that many soruce errors that it really
-- makes a big difference.
getSourceErrors :: Query [SourceError]
getSourceErrors IdeSession{ideState, ideStaticInfo} =
  $withMVar ideState $ \st ->
    case st of
      IdeSessionIdle      idleState -> aux idleState
      IdeSessionRunning _ idleState -> aux idleState
      IdeSessionShutdown            -> fail "Session already shut down."
  where
    aux :: IdeIdleState -> IO [SourceError]
    aux idleState = case idleState ^. ideComputed of
      Just Computed{..} ->
        return $ map (removeExplicitSharing computedCache) computedErrors
      Nothing -> fail "This session state does not admit queries."

-- | Get the collection of files submitted by the user and not deleted yet.
-- The module names are those supplied by the user as the first
-- arguments of the @updateModule@ and @updateModuleFromFile@ calls,
-- as opposed to the compiler internal @module ... end@ module names.
-- Usually the two names are equal, but they needn't be.
getManagedFiles :: Query ManagedFiles
getManagedFiles IdeSession{ideState} =
  $withMVar ideState $ \st ->
    case st of
      IdeSessionIdle idleState ->
        return $ plugLeaks $ idleState ^. ideManagedFiles
      IdeSessionRunning _ idleState ->
        return $ plugLeaks $ idleState ^. ideManagedFiles
      IdeSessionShutdown ->
        fail "Session already shut down."
 where
  plugLeaks :: ManagedFilesInternal -> ManagedFiles
  plugLeaks files = ManagedFiles { sourceFiles = map fst $ _managedSource files
                                 , dataFiles = _managedData files }

-- | Get the list of correctly compiled modules, as reported by the compiler,
-- together with a mapping from symbol uses to symbol info.
-- That is, given a symbol used at a particular location in a source module
-- the mapping tells us where that symbol is defined, either locally in a
-- source module or a top-level symbol imported from another package,
-- what is the type of this symbol and some more information.
-- This information lets us, e.g, construct Haddock URLs for symbols,
-- like @parallel-3.2.0.3/Control-Parallel.html#v:pseq@.
getLoadedModules :: Query (XShared LoadedModules)
getLoadedModules IdeSession{ideState, ideStaticInfo} =
  $withMVar ideState $ \st ->
    case st of
      IdeSessionIdle      idleState -> aux idleState
      IdeSessionRunning _ idleState -> aux idleState
      IdeSessionShutdown            -> fail "Session already shut down."
  where
    aux :: IdeIdleState -> IO (XShared LoadedModules)
    aux idleState = case idleState ^. ideComputed of
      Just Computed{..} -> return computedLoadedModules
      Nothing -> fail "This session state does not admit queries."

getExplicitSharingCache :: Query ExplicitSharingCache
getExplicitSharingCache IdeSession{ideState, ideStaticInfo} =
  $withMVar ideState $ \st ->
    case st of
      IdeSessionIdle      idleState -> aux idleState
      IdeSessionRunning _ idleState -> aux idleState
      IdeSessionShutdown            -> fail "Session already shut down."
  where
    aux :: IdeIdleState -> IO ExplicitSharingCache
    aux idleState = case idleState ^. ideComputed of
      Just Computed{..} -> return computedCache
      Nothing -> fail "This session state does not admit queries."

-- | Get import information
--
-- This information is available even for modules with parse/type errors
getImports :: Query (Map ModuleName [Import])
getImports IdeSession{ideState, ideStaticInfo} =
  $withMVar ideState $ \st ->
    case st of
      IdeSessionIdle      idleState -> aux idleState
      IdeSessionRunning _ idleState -> aux idleState
      IdeSessionShutdown            -> fail "Session already shut down."
  where
    aux :: IdeIdleState -> IO (Map ModuleName [Import])
    aux idleState = case idleState ^. ideComputed of
      Just Computed{..} -> return computedImports
      Nothing -> fail "This session state does not admit queries."

-- | Autocompletion
--
-- TODO: At the moment, this returns a function with internally does
-- normalization.  Hence, this is not useful for explicit sharing. If the
-- autocompletion info needs to be shipped, we need to change this to a list
-- and avoid normalization here.
getAutocompletion :: Query (ModuleName -> String -> [IdInfo])
getAutocompletion IdeSession{ideState, ideStaticInfo} =
  $withMVar ideState $ \st ->
    case st of
      IdeSessionIdle      idleState -> aux idleState
      IdeSessionRunning _ idleState -> aux idleState
      IdeSessionShutdown            -> fail "Session already shut down."
  where
    aux :: IdeIdleState -> IO (ModuleName -> String -> [IdInfo])
    aux idleState = case idleState ^. ideComputed of
      Just Computed{..} -> return (autocomplete computedCache computedAutoMap)
      Nothing           -> fail "This session state does not admit queries."

    autocomplete :: ExplicitSharingCache
                 -> Map ModuleName (Trie [XShared IdInfo])
                 -> ModuleName -> String
                 -> [IdInfo]
    autocomplete cache mapOfTries modName name =
        let name' = BSSC.pack name
            n     = last (BSSC.split '.' name')
        in filter (\idInfo -> name `isInfixOf` idInfoQN idInfo)
             $ map (removeExplicitSharing cache)
             . concat
             . Trie.elems
             . Trie.submap n
             $ mapOfTries Map.! modName

-- | Is code generation currently enabled?
getCodeGeneration :: Query Bool
getCodeGeneration IdeSession{ideState} =
  $withMVar ideState $ \st ->
    case st of
      IdeSessionIdle idleState ->
        return $ idleState ^. ideGenerateCode
      IdeSessionRunning _ idleState ->
        return $ idleState ^. ideGenerateCode
      IdeSessionShutdown ->
        fail "Session already shut down."

-- | Get the list of all data files currently available to the session:
-- both the files copied via an update and files created by user code.
getAllDataFiles :: Query [FilePath]
getAllDataFiles IdeSession{ideStaticInfo} =
  Find.find Find.always
            (Find.fileType Find.==? Find.RegularFile)
            (ideDataDir ideStaticInfo)

-- | Get all current environment overrides
getEnv :: Query [(String, Maybe String)]
getEnv IdeSession{ideState} =
  $withMVar ideState $ \st ->
    case st of
      IdeSessionIdle idleState ->
        return $ idleState ^. ideEnv
      IdeSessionRunning _ idleState ->
        return $ idleState ^. ideEnv
      IdeSessionShutdown ->
        fail "Session already shut down."

-- | Get the RPC server used by the session.
getGhcServer :: IdeSession -> IO GhcServer
getGhcServer IdeSession{ideState} =
  $withMVar ideState $ \st ->
    case st of
      IdeSessionIdle idleState ->
        return $! idleState ^. ideGhcServer
      IdeSessionRunning _ idleState ->
        return $! idleState ^. ideGhcServer
      IdeSessionShutdown ->
        fail "Session already shut down."
