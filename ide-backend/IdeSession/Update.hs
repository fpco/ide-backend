{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, ScopedTypeVariables, TemplateHaskell #-}
-- | IDE session updates
--
-- We should only be using internal types here (explicit strictness/sharing)
module IdeSession.Update (
    -- * Starting and stopping
    initSession
  , SessionInitParams(..)
  , defaultSessionInitParams
  , shutdownSession
  , forceShutdownSession
  , restartSession
    -- * Session updates
  , IdeSessionUpdate -- Abstract
  , updateSession
  , updateSourceFile
  , updateSourceFileFromFile
  , updateSourceFileDelete
  , updateGhcOpts
  , updateRtsOpts
  , updateRelativeIncludes
  , updateCodeGeneration
  , updateDataFile
  , updateDataFileFromFile
  , updateDataFileDelete
  , updateDeleteManagedFiles
  , updateEnv
  , updateArgs
  , updateStdoutBufferMode
  , updateStderrBufferMode
  , updateTargets
  , buildExe
  , buildDoc
  , buildLicenses
    -- * Running code
  , runStmt
  , runStmtPty
  , runExe
  , resume
  , setBreakpoint
  , printVar
    -- * Debugging
  , crashGhcServer
  , buildLicsFromPkgs
  , LicenseArgs(..)
  )
  where

import Prelude hiding (mod, span)
import Control.Concurrent (threadDelay)
import Control.Monad (when, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Accessor (Accessor, (^.))
import Data.List (elemIndices, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid(..), (<>))
import Distribution.Simple (PackageDBStack, PackageDB(..))
import System.Environment (getEnvironment, unsetEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.IO.Temp (createTempDirectory)
import System.Posix.IO.ByteString
import System.Process (proc, CreateProcess(..), StdStream(..), createProcess, waitForProcess, interruptProcessGroupOf, terminateProcess)
import qualified Control.Exception         as Ex
import qualified Data.ByteString           as BSS
import qualified Data.ByteString.Lazy      as BSL
import qualified Data.ByteString.Lazy.UTF8 as BSL.UTF8
import qualified Data.Set                  as Set
import qualified Data.Text                 as Text
import qualified System.Directory          as Dir
import qualified System.IO                 as IO

import IdeSession.Cabal
import IdeSession.Config
import IdeSession.GHC.API
import IdeSession.GHC.Client
import IdeSession.RPC.API (ExternalException(..))
import IdeSession.State
import IdeSession.Strict.Container
import IdeSession.Strict.MVar (newMVar, newEmptyMVar, StrictMVar)
import IdeSession.Types.Private hiding (RunResult(..))
import IdeSession.Types.Progress
import IdeSession.Types.Public (RunBufferMode(..))
import IdeSession.Update.ExecuteSessionUpdate
import IdeSession.Update.IdeSessionUpdate
import IdeSession.Util
import IdeSession.Util.BlockingOps
import qualified IdeSession.Query         as Query
import qualified IdeSession.Strict.List   as List
import qualified IdeSession.Strict.Map    as Map
import qualified IdeSession.Strict.Maybe  as Maybe
import qualified IdeSession.Types.Private as Private
import qualified IdeSession.Types.Public  as Public

{-------------------------------------------------------------------------------
  Session initialization
-------------------------------------------------------------------------------}

-- | How should the session be initialized?
--
-- Client code should use 'defaultSessionInitParams' to protect itself against
-- future extensions of this record.
data SessionInitParams = SessionInitParams {
    -- | Previously computed cabal macros,
    -- or 'Nothing' to compute them on startup
    sessionInitCabalMacros :: Maybe BSL.ByteString

    -- | Initial ghc options
  , sessionInitGhcOptions :: [String]

    -- | Include paths (equivalent of GHC's @-i@ parameter) relative to the
    -- temporary directory where we store the session's source files.
    --
    -- By default this is the singleton list @[""]@ -- i.e., we include the
    -- sources dir but nothing else.
  , sessionInitRelativeIncludes :: [FilePath]

    -- | Targets for compilation
    --
    -- Defaults to @TargetsExclude []@ -- i.e., compile all modules in the
    -- project.
  , sessionInitTargets :: Public.Targets

    -- | RTS options
    --
    -- Defaults to @-K8M@
  , sessionInitRtsOpts :: [String]

    -- | dist/ directory.
  , sessionInitDistDir :: !(Maybe FilePath)
  }
  deriving Show

defaultSessionInitParams :: SessionInitParams
defaultSessionInitParams = SessionInitParams {
    sessionInitCabalMacros      = Nothing
  , sessionInitGhcOptions       = []
  , sessionInitRelativeIncludes = [""]
  , sessionInitTargets          = Public.TargetsExclude []
  , sessionInitRtsOpts          = ["-K8M"]
  , sessionInitDistDir          = Nothing
  }

-- | Session initialization parameters for an existing session.
--
-- For internal use only (used in 'updateSession' when restarting the session).
--
-- We set 'sessionInitCabalMacros' to 'Nothing' because the cabal macros file
-- has already been written to disk, and we don't remove the project directory
-- on a session restart.
sessionRestartParams :: IdeIdleState -> IdeSessionUpdate -> SessionInitParams
sessionRestartParams st IdeSessionUpdate{..} = SessionInitParams {
    sessionInitCabalMacros      = Nothing
  , sessionInitGhcOptions       = fromMaybe (st ^. ideGhcOpts)          ideUpdateGhcOpts
  , sessionInitRelativeIncludes = fromMaybe (st ^. ideRelativeIncludes) ideUpdateRelIncls
  , sessionInitTargets          = fromMaybe (st ^. ideTargets)          ideUpdateTargets
  , sessionInitRtsOpts          = fromMaybe (st ^. ideRtsOpts)          ideUpdateRtsOpts
  , sessionInitDistDir          = Nothing
  }

-- | Set up the initial state of the session according to the given parameters
execInitParams :: IdeStaticInfo -> SessionInitParams -> IO ()
execInitParams staticInfo SessionInitParams{..} = do
  writeMacros staticInfo sessionInitCabalMacros

-- | Write per-package CPP macros.
writeMacros :: IdeStaticInfo -> Maybe BSL.ByteString -> IO ()
writeMacros IdeStaticInfo{ideConfig = SessionConfig {..}, ..}
            configCabalMacros = do
  macros <- case configCabalMacros of
              Nothing     -> generateMacros configPackageDBStack configExtraPathDirs
              Just macros -> return (BSL.UTF8.toString macros)
  writeFile (cabalMacrosLocation ideDistDir) macros

{-------------------------------------------------------------------------------
  Session startup
-------------------------------------------------------------------------------}

-- | Create a fresh session, using some initial configuration.
--
-- Throws an exception if the configuration is invalid, or if GHC_PACKAGE_PATH
-- is set.
initSession :: SessionInitParams -> SessionConfig -> IO IdeSession
initSession initParams@SessionInitParams{..} ideConfig@SessionConfig{..} = do
  -- verifyConfig used to bail if GHC_PACKAGE_PATH was set.  Instead,
  -- we just unset it so that cabal invocations are happy.  It's up to
  -- the user of ide-backend to set 'configPackageDBStack' based on
  -- this environment variable.
  unsetEnv "GHC_PACKAGE_PATH"
  verifyConfig ideConfig

  configDirCanon <- Dir.canonicalizePath configDir
  ideSessionDir  <- createTempDirectory configDirCanon "session."
  let ideDistDir = fromMaybe (ideSessionDir </> "dist/") sessionInitDistDir

  let ideStaticInfo = IdeStaticInfo{..}

  -- Create the common subdirectories of session.nnnn so that we don't have to
  -- worry about creating these elsewhere
  case configLocalWorkingDir of
    Just _  -> return ()
    Nothing -> do
      Dir.createDirectoryIfMissing True (ideSourceDir  ideStaticInfo)
      Dir.createDirectoryIfMissing True (ideDataDir    ideStaticInfo)
  Dir.createDirectoryIfMissing True ideDistDir
  Dir.createDirectoryIfMissing True (ideSessionObjDir  ideSessionDir)
  -- Local initialization
  execInitParams ideStaticInfo initParams

  -- Start the GHC server (as a separate process)
  mServer <- forkGhcServer sessionInitGhcOptions
                           sessionInitRelativeIncludes
                           sessionInitRtsOpts
                           ideStaticInfo
  let (state, server, version) = case mServer of
         Right (s, v) -> (IdeSessionIdle,         s,          v)
         Left e       -> (IdeSessionServerDied e, Ex.throw e, Ex.throw e)

  -- The value of _ideLogicalTimestamp field is a workaround for
  -- the problems with 'invalidateModSummaryCache', which itself is
  -- a workaround for http://hackage.haskell.org/trac/ghc/ticket/7478.
  -- We have to make sure that file times never reach 0, because this will
  -- trigger an exception (http://hackage.haskell.org/trac/ghc/ticket/7567).
  -- We rather arbitrary start at Jan 2, 1970.
  let idleState = IdeIdleState {
          _ideLogicalTimestamp    = 86400
        , _ideComputed            = Maybe.nothing
        , _ideGenerateCode        = False
        , _ideManagedFiles        = ManagedFilesInternal [] []
        , _ideObjectFiles         = []
        , _ideBuildExeStatus      = Nothing
        , _ideBuildDocStatus      = Nothing
        , _ideBuildLicensesStatus = Nothing
        , _ideEnv                 = []
        , _ideArgs                = []
        , _ideStdoutBufferMode    = RunNoBuffering
        , _ideStderrBufferMode    = RunNoBuffering
        , _ideBreakInfo           = Maybe.nothing
        , _ideGhcServer           = server
        , _ideGhcVersion          = version
        , _ideGhcOpts             = sessionInitGhcOptions
        , _ideRelativeIncludes    = sessionInitRelativeIncludes
        , _ideTargets             = sessionInitTargets
        , _ideRtsOpts             = sessionInitRtsOpts
        }

  ideState <- newMVar (state idleState)
  return IdeSession{..}

-- | Verify configuration, and throw an exception if configuration is invalid
verifyConfig :: SessionConfig -> IO ()
verifyConfig SessionConfig{..} = do
    unless (isValidPackageDB configPackageDBStack) $
      Ex.throw . userError $ "Invalid package DB stack: "
                             ++ show configPackageDBStack
  where
    isValidPackageDB :: PackageDBStack -> Bool
    isValidPackageDB stack =
          elemIndices GlobalPackageDB stack == [0]
       && elemIndices UserPackageDB stack `elem` [[], [1]]

{-------------------------------------------------------------------------------
  Session shutdown
-------------------------------------------------------------------------------}

-- | Close a session down, releasing the resources.
--
-- This operation is the only one that can be run after a shutdown was already
-- performed. This lets the API user execute an early shutdown, e.g., before
-- the @shutdownSession@ placed inside 'bracket' is triggered by a normal
-- program control flow.
--
-- If code is still running, it will be interrupted.
shutdownSession :: IdeSession -> IO ()
shutdownSession = shutdownSession' False

-- | Like shutdownSession, but don't be nice about it (SIGKILL)
forceShutdownSession :: IdeSession -> IO ()
forceShutdownSession = shutdownSession' True

-- | Internal generalization of 'shutdownSession' and 'forceShutdownSession'
shutdownSession' :: Bool -> IdeSession -> IO ()
shutdownSession' forceTerminate IdeSession{ideState, ideStaticInfo} = do
  $modifyStrictMVar_ ideState $ \state ->
    case state of
      IdeSessionIdle idleState -> do
        if forceTerminate
          then forceShutdownGhcServer $ _ideGhcServer idleState
          else shutdownGhcServer      $ _ideGhcServer idleState
        cleanupDirs
        return IdeSessionShutdown
      IdeSessionShutdown ->
        return IdeSessionShutdown
      IdeSessionServerDied _ _ -> do
        cleanupDirs
        return IdeSessionShutdown
  where
    cleanupDirs :: IO ()
    cleanupDirs =
      when (configDeleteTempFiles . ideConfig $ ideStaticInfo) $
        ignoreDoesNotExist $
          Dir.removeDirectoryRecursive (ideSessionDir ideStaticInfo)

{-------------------------------------------------------------------------------
  Session restart
-------------------------------------------------------------------------------}

-- | Restart a session
--
-- This puts the session in a "dead" state; it won't _actually_ be restarted
-- until the next call to 'updateSession'.
restartSession :: IdeSession -> IO ()
restartSession IdeSession{ideState} =
  $modifyStrictMVar_ ideState $ \state ->
    case state of
      IdeSessionIdle idleState ->
        return $ IdeSessionServerDied forcedRestart idleState
      IdeSessionServerDied _ _ ->
        return state -- Leave Died state as is
      IdeSessionShutdown ->
        fail "Shutdown session cannot be restarted."

data RestartResult =
    ServerRestarted IdeIdleState IdeSessionUpdate
  | ServerRestartFailed IdeIdleState

executeRestart :: SessionInitParams
               -> IdeStaticInfo
               -> IdeIdleState
               -> IO RestartResult
executeRestart initParams@SessionInitParams{..} staticInfo idleState = do
  forceShutdownGhcServer $ _ideGhcServer idleState
  mServer <- forkGhcServer sessionInitGhcOptions
                           sessionInitRelativeIncludes
                           sessionInitRtsOpts
                           staticInfo
  case mServer of
    Right (server, version) -> do
      execInitParams staticInfo initParams

      -- Reset back to initial values ..
      let idleState' = idleState {
              _ideComputed         = Maybe.nothing
            , _ideGhcOpts          = sessionInitGhcOptions
            , _ideRelativeIncludes = sessionInitRelativeIncludes
            , _ideRtsOpts          = sessionInitRtsOpts
            , _ideGenerateCode     = False
            , _ideObjectFiles      = []
            , _ideEnv              = []
            , _ideArgs             = []
            , _ideGhcServer        = server
            , _ideGhcVersion       = version
            , _ideTargets          = sessionInitTargets
            }
      -- .. and let an update make sure we bring the state back to where it was
      let upd = mconcat [
              updateEnv            (idleState ^. ideEnv)
            , updateArgs           (idleState ^. ideArgs)
            , updateCodeGeneration (idleState ^. ideGenerateCode)
            ]
      return (ServerRestarted idleState' upd)
    Left e -> do
      let idleState' = idleState {
              _ideGhcServer  = Ex.throw e
            , _ideGhcVersion = Ex.throw e
            }
      return (ServerRestartFailed idleState')

{-------------------------------------------------------------------------------
  Session update

  Here we deal only with the top-level logic: restart the session and then run
  the session update. The specifics of how to execute the individual parts
  of the session update are defined in IdeSession.Update.ExecuteSessionUpdate.
-------------------------------------------------------------------------------}

-- | Given the current IDE session state, go ahead and update the session,
-- eventually resulting in a new session state, with fully updated computed
-- information (typing, etc.).
--
-- The update can be a long running operation, so we support a callback which
-- can be used to monitor progress of the operation.
updateSession :: IdeSession -> IdeSessionUpdate -> (Progress -> IO ()) -> IO ()
updateSession = flip . updateSession'

updateSession' :: IdeSession -> (Progress -> IO ()) -> IdeSessionUpdate -> IO ()
updateSession' IdeSession{ideStaticInfo, ideState} callback = \update ->
    $modifyStrictMVar_ ideState $ go False update
  where
    go :: Bool -> IdeSessionUpdate -> IdeSessionState -> IO IdeSessionState
    go justRestarted update (IdeSessionIdle idleState) =
      if not (requiresSessionRestart idleState update)
        then do
          (idleState', mex) <- runSessionUpdate justRestarted update ideStaticInfo callback idleState
          case mex of
            Nothing -> return $ IdeSessionIdle          idleState'
            Just ex -> return $ IdeSessionServerDied ex idleState'
        else do
          let restartParams = sessionRestartParams idleState update
          restart justRestarted update restartParams idleState
    go justRestarted update (IdeSessionServerDied _ex idleState) = do
      let restartParams = sessionRestartParams idleState update
      restart justRestarted update restartParams idleState
    go _ _ IdeSessionShutdown =
      Ex.throwIO (userError "Session already shut down.")

    restart :: Bool -> IdeSessionUpdate -> SessionInitParams -> IdeIdleState -> IO IdeSessionState
    restart True _ _ idleState =
      return $ IdeSessionServerDied serverRestartLoop idleState
    restart False update restartParams idleState = do
      -- To avoid "<stdout> hPutChar: resource vanished (Broken pipe)":
      -- TODO: I wish I knew why this is necessary :(
      threadDelay 100000

      restartResult <- executeRestart restartParams ideStaticInfo idleState
      case restartResult of
        ServerRestarted idleState' resetSession ->
          go True (resetSession <> update) (IdeSessionIdle idleState')
        ServerRestartFailed idleState' ->
          return $ IdeSessionServerDied failedToRestart idleState'

-- | @requiresSessionRestart st upd@ returns true if update @upd@ requires a
-- session restart given current state @st@.
--
-- See 'sessionRestartParams' to compute the session initialization parameters
-- for the new session.
requiresSessionRestart :: IdeIdleState -> IdeSessionUpdate -> Bool
requiresSessionRestart st IdeSessionUpdate{..} =
         (ideUpdateRelIncls `changes` ideRelativeIncludes)
      || (ideUpdateTargets  `changes` ideTargets)
      || (ideUpdateRtsOpts  `changes` ideRtsOpts)
      || (any optRequiresRestart (listChanges' ideUpdateGhcOpts ideGhcOpts))
  where
    optRequiresRestart :: String -> Bool
    optRequiresRestart str =
         -- Library flags cannot be changed dynamically (#214)
         "-l" `isPrefixOf` str

    changes :: Eq a => Maybe a -> Accessor IdeIdleState a -> Bool
    changes Nothing  _ = False
    changes (Just x) y = x /= st ^. y

    listChanges' :: Ord a => Maybe [a] -> Accessor IdeIdleState [a] -> [a]
    listChanges' Nothing   _  = []
    listChanges' (Just xs) ys = listChanges xs (st ^. ys)

-- | @listChanges xs ys@ is the list of elements that appear in @xs@ but not
-- in @ys@ and the set of elements that appear in @ys@ but not in @xs@.
--
-- Considering the lists as sets, it is the complement of the intersection
-- between the two sets.
listChanges :: Ord a => [a] -> [a] -> [a]
listChanges xs ys =
    Set.toList $ (a `Set.union` b) `Set.difference` (a `Set.intersection` b)
  where
    a = Set.fromList xs
    b = Set.fromList ys

{-------------------------------------------------------------------------------
  Running code
-------------------------------------------------------------------------------}

-- | Run a given function in a given module (the name of the module
-- is the one between @module ... end@, which may differ from the file name).
-- The function resembles a query, but it's not instantaneous
-- and the running code can be interrupted or interacted with.
--
-- 'runStmt' will throw an exception if the code has not been compiled yet,
-- or when the server is in a dead state (i.e., when ghc has crashed). In the
-- latter case 'getSourceErrors' will report the ghc exception; it is the
-- responsibility of the client code to check for this.
runStmt :: IdeSession -> String -> String -> IO (RunActions Public.RunResult)
runStmt ideSession m fun = runCmd ideSession $ \idleState -> RunStmt {
    runCmdModule   = m
  , runCmdFunction = fun
  , runCmdStdout   = idleState ^. ideStdoutBufferMode
  , runCmdStderr   = idleState ^. ideStderrBufferMode
  , runCmdPty      = False
  }

-- | Like 'runStmt', but runs the statement in a pseudoterminal.
runStmtPty :: IdeSession -> String -> String -> IO (RunActions Public.RunResult)
runStmtPty ideSession m fun = runCmd ideSession $ \idleState -> RunStmt {
    runCmdModule   = m
  , runCmdFunction = fun
  , runCmdStdout   = idleState ^. ideStdoutBufferMode
  , runCmdStderr   = idleState ^. ideStderrBufferMode
  , runCmdPty      = True
  }

-- | Run the main function from the last compiled executable.
--
-- 'runExe' will throw an exception if there were no executables
-- compiled since session init, or if the last compilation was not
-- successful (checked as in @getBuildExeStatus@)
-- or if none of the executables last compiled have the supplied name
-- or when the server is in a dead state (i.e., when ghc has crashed). In the
-- last case 'getSourceErrors' will report the ghc exception; it is the
-- responsibility of the client code to check for this.
runExe :: IdeSession -> String -> IO (RunActions ExitCode)
runExe session m = do
 let handleQueriesExc (_ :: Query.InvalidSessionStateQueries) =
       fail $ "Wrong session state when trying to run an executable."
 Ex.handle handleQueriesExc $ do
  mstatus <- Query.getBuildExeStatus session
  case mstatus of
    Nothing ->
      fail $ "No executable compilation initiated since session init."
    (Just status@ExitFailure{}) ->
      fail $ "Last executable compilation failed with status "
             ++ show status ++ "."
    Just ExitSuccess -> do
      distDir <- Query.getDistDir session
      dataDir <- Query.getDataDir session
      args <- Query.getArgs session
      envInherited <- getEnvironment
      envOverride <- Query.getEnv session
      let overrideVar :: (String, Maybe String) -> Strict (Map String) String
                      -> Strict (Map String) String
          overrideVar (var, Just val) env = Map.insert var val env
          overrideVar (var, Nothing) env = Map.delete var env
          envMap = foldr overrideVar (Map.fromList envInherited) envOverride
      let exePath = distDir </> "build" </> m </> m
      exeExists <- Dir.doesFileExist exePath
      unless exeExists $
        fail $ "No compiled executable file "
               ++ m ++ " exists at path "
               ++ exePath ++ "."
      (stdRd, stdWr) <- liftIO createPipe
      std_rd_hdl <- fdToHandle stdRd
      std_wr_hdl <- fdToHandle stdWr
      let cproc = (proc exePath args) { cwd = Just dataDir
                                      , env = Just $ Map.toList envMap
                                      , create_group = True
                                          -- for interruptProcessGroupOf
                                      , std_in = CreatePipe
                                      , std_out = UseHandle std_wr_hdl
                                      , std_err = UseHandle std_wr_hdl
                                      }
      (Just stdin_hdl, Nothing, Nothing, ph) <- createProcess cproc

      -- The runActionState holds 'Just' the result of the snippet, or 'Nothing' if
      -- it has not yet terminated.
      runActionsState <- newMVar Nothing

      return $ RunActions
        { runWait = $modifyStrictMVar runActionsState $ \st -> case st of
            Just outcome ->
              return (Just outcome, Right outcome)
            Nothing -> do
              bs <- BSS.hGetSome std_rd_hdl blockSize
              if BSS.null bs
                then do
                  res <- waitForProcess ph
                  return (Just res, Right res)
                else
                  return (Nothing, Left bs)
        , interrupt = interruptProcessGroupOf ph
        , supplyStdin = \bs -> BSS.hPut stdin_hdl bs >> IO.hFlush stdin_hdl
        , forceCancel = terminateProcess ph
        }
      -- We don't need to close any handles. At the latest GC closes them.
 where
  -- TODO: What is a good value here?
  blockSize :: Int
  blockSize = 4096

-- | Resume a previously interrupted statement
resume :: IdeSession -> IO (RunActions Public.RunResult)
resume ideSession = runCmd ideSession (const Resume)

-- | Internal geneneralization used in 'runStmt' and 'resume'
runCmd :: IdeSession -> (IdeIdleState -> RunCmd) -> IO (RunActions Public.RunResult)
runCmd session mkCmd = modifyIdleState session $ \idleState ->
  case (toLazyMaybe (idleState ^. ideComputed), idleState ^. ideGenerateCode) of
    (Just comp, True) -> do
      let cmd = mkCmd idleState

      checkStateOk comp cmd
      isBreak    <- newEmptyMVar
      runActions <- rpcRun (idleState ^. ideGhcServer)
                           cmd
                           (translateRunResult isBreak)

      -- TODO: We should register the runActions somewhere so we can do a
      -- clean session shutdown?
      return (IdeSessionIdle idleState, runActions)
    _ ->
      -- This 'fail' invocation is, in part, a workaround for
      -- http://hackage.haskell.org/trac/ghc/ticket/7539
      -- which would otherwise lead to a hard GHC crash,
      -- instead of providing a sensible error message
      -- that we could show to the user.
      fail "Cannot run before the code is generated."
  where
    checkStateOk :: Computed -> RunCmd -> IO ()
    checkStateOk comp RunStmt{..} =
      -- ideManagedFiles is irrelevant, because only the module name inside
      -- 'module .. where' counts.
      unless (Text.pack runCmdModule `List.elem` computedLoadedModules comp) $
        fail $ "Module " ++ show runCmdModule
                         ++ " not successfully loaded, when trying to run code."
    checkStateOk _comp Resume =
      -- TODO: should we check that there is anything to resume here?
      return ()

    translateRunResult :: StrictMVar (Strict Maybe BreakInfo)
                       -> Maybe Private.RunResult
                       -> IO Public.RunResult
    translateRunResult isBreak (Just Private.RunOk) = do
      $putStrictMVar isBreak Maybe.nothing
      return $ Public.RunOk
    translateRunResult isBreak (Just (Private.RunProgException str)) = do
      $putStrictMVar isBreak Maybe.nothing
      return $ Public.RunProgException str
    translateRunResult isBreak (Just (Private.RunGhcException str)) = do
      $putStrictMVar isBreak Maybe.nothing
      return $ Public.RunGhcException str
    translateRunResult isBreak (Just (Private.RunBreak breakInfo)) = do
      $putStrictMVar isBreak (Maybe.just breakInfo)
      return $ Public.RunBreak
    translateRunResult isBreak Nothing = do
      -- On a force cancellation we definitely didn't hit a breakpoint
      $putStrictMVar isBreak Maybe.nothing
      return $ Public.RunForceCancelled

-- | Breakpoint
--
-- Set a breakpoint at the specified location. Returns @Just@ the old value of the
-- breakpoint if successful, or @Nothing@ otherwise.
setBreakpoint :: IdeSession
              -> ModuleName        -- ^ Module where the breakshould should be set
              -> Public.SourceSpan -- ^ Location of the breakpoint
              -> Bool              -- ^ New value for the breakpoint
              -> IO (Maybe Bool)   -- ^ Old value of the breakpoint (if valid)
setBreakpoint session mod span value = withIdleState session $ \idleState ->
  rpcBreakpoint (idleState ^. ideGhcServer) mod span value

-- | Print and/or force values during debugging
--
-- Only valid in breakpoint state.
printVar :: IdeSession
         -> Public.Name -- ^ Variable to print
         -> Bool        -- ^ Should printing bind new vars? (@:print@ vs. @:sprint@)
         -> Bool        -- ^ Should the value be forced? (@:print@ vs. @:force@)
         -> IO Public.VariableEnv
printVar session var bind forceEval = withBreakInfo session $ \idleState _ ->
  rpcPrint (idleState ^. ideGhcServer) var bind forceEval

-- TODO: We should do this translation only when we talk to ghc, and keep
-- the original Targets in the session state
-- **  IdeStaticInfo{..} <- asks ideSessionUpdateStaticInfo
-- **  let sourceDir = ideSessionSourceDir ideSessionDir
-- **      newTargets = case targets of
-- **        Public.TargetsInclude l ->
-- **          Public.TargetsInclude $ map (sourceDir </>) l
-- **        Public.TargetsExclude l ->
-- **          Public.TargetsExclude $ map (sourceDir </>) l
-- **
-- **  oldTargets <- get ideTargets
-- **  when (oldTargets /= newTargets) $ do
-- **    set ideTargets newTargets
-- **    restartSession'

{------------------------------------------------------------------------------
  Debugging of ide-backend itself
------------------------------------------------------------------------------}

-- | Crash the GHC server. For debugging only. If the specified delay is
-- @Nothing@, crash immediately; otherwise, set up a thread that throws
-- an exception to the main thread after the delay.
crashGhcServer :: IdeSession -> Maybe Int -> IO ()
crashGhcServer IdeSession{..} delay = $withStrictMVar ideState $ \state ->
  case state of
    IdeSessionIdle idleState ->
      rpcCrash (idleState ^. ideGhcServer) delay
    _ ->
      Ex.throwIO $ userError "State not idle"

{-------------------------------------------------------------------------------
  Auxiliary (ide-backend specific)
-------------------------------------------------------------------------------}

withBreakInfo :: IdeSession -> (IdeIdleState -> Public.BreakInfo -> IO a) -> IO a
withBreakInfo session act = withIdleState session $ \idleState ->
  case toLazyMaybe (idleState ^. ideBreakInfo) of
    Just breakInfo -> act idleState breakInfo
    Nothing        -> Ex.throwIO (userError "Not in breakpoint state")

withIdleState :: IdeSession -> (IdeIdleState -> IO a) -> IO a
withIdleState session act = modifyIdleState session $ \idleState -> do
  result <- act idleState
  return (IdeSessionIdle idleState, result)

modifyIdleState :: IdeSession -> (IdeIdleState -> IO (IdeSessionState, a)) -> IO a
modifyIdleState IdeSession{..} act = $modifyStrictMVar ideState $ \state -> case state of
  IdeSessionIdle idleState -> act idleState
  _ -> Ex.throwIO $ userError "State not idle"

failedToRestart :: ExternalException
failedToRestart = ExternalException {
    externalStdErr    = "Failed to restart server"
  , externalException = Nothing
  }

forcedRestart :: ExternalException
forcedRestart = ExternalException {
    externalStdErr    = "Session manually restarted"
  , externalException = Nothing
  }

serverRestartLoop :: ExternalException
serverRestartLoop = ExternalException {
    externalStdErr    = "Server restart loop"
  , externalException = Nothing
  }
