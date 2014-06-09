{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving, RankNTypes, ScopedTypeVariables, MultiParamTypeClasses, ExistentialQuantification, TemplateHaskell #-}
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
  , canExecuteWhileRunning
  , updateSession
  , updateSourceFile
  , updateSourceFileFromFile
  , updateSourceFileDelete
  , updateDynamicOpts
  , updateRelativeIncludes
  , updateCodeGeneration
  , updateDataFile
  , updateDataFileFromFile
  , updateDataFileDelete
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
  , runExe
  , resume
  , setBreakpoint
  , printVar
    -- * Debugging
  , forceRecompile
  , crashGhcServer
  , buildLicsFromPkgs
  )
  where

import Prelude hiding (mod, span)
import Control.Concurrent (threadDelay)
import Control.Applicative ((<|>))
import Control.Monad (when, void, forM, unless)
import Control.Monad.State (MonadState, StateT, execStateT, runStateT)
import Control.Monad.Reader (MonadReader(..), asks)
import Control.Monad.Writer (MonadWriter, WriterT, execWriterT, tell)
import Control.Monad.Trans.Class (lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Applicative (Applicative(..), (<$>), (<*>))
import qualified Control.Exception as Ex
import qualified Control.Monad.State as St
import Data.Digest.Pure.MD5 (MD5Digest)
import Data.List (elemIndices)
import Data.Monoid (Monoid(..))
import Data.Accessor (Accessor, (.>), (^.), (^=))
import Data.Accessor.Monad.MTL.State (get, modify, set)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BSS
import qualified Data.ByteString as BSS (hGetSome)
import Data.Foldable (forM_)
import qualified System.Directory as Dir
import System.FilePath (
    makeRelative
  , (</>)
  , takeExtension
  , replaceExtension
  , dropFileName
  )
import System.FilePath.Find (find, always, extension, (&&?), (||?), fileType, (==?), FileType (RegularFile))
import System.Posix.Files (setFileTimes, getFileStatus, modificationTime)
import qualified System.IO as IO
import System.IO.Temp (createTempDirectory)
import System.IO.Error (isDoesNotExistError)
import qualified Data.Text as Text
import System.Environment (getEnv, getEnvironment)
import System.Exit (ExitCode(..))
import System.Posix.IO.ByteString
import System.Process (proc, CreateProcess(..), StdStream(..), createProcess, waitForProcess, interruptProcessGroupOf, terminateProcess)
import qualified Control.Monad.State as State

import Distribution.Simple (PackageDBStack, PackageDB(..))

import IdeSession.Cabal
import IdeSession.Config
import IdeSession.ExeCabalClient (invokeExeCabal)
import IdeSession.GHC.API
import IdeSession.GHC.Client
import IdeSession.RPC.Client (ExternalException)
import IdeSession.State
import IdeSession.Strict.Container
import IdeSession.Strict.MVar (newMVar, newEmptyMVar, StrictMVar)
import IdeSession.Types.Private hiding (RunResult(..))
import IdeSession.Types.Progress
import IdeSession.Types.Public (RunBufferMode(..))
import IdeSession.Types.Translation (removeExplicitSharing)
import IdeSession.Util
import IdeSession.Util.BlockingOps
import qualified IdeSession.Query as Query
import qualified IdeSession.Strict.IntMap as IntMap
import qualified IdeSession.Strict.List   as List
import qualified IdeSession.Strict.Map    as Map
import qualified IdeSession.Strict.Maybe  as Maybe
import qualified IdeSession.Strict.Trie   as Trie
import qualified IdeSession.Types.Private as Private
import qualified IdeSession.Types.Public  as Public

{------------------------------------------------------------------------------
  Starting and stopping
------------------------------------------------------------------------------}

-- | How should the session be initialized?
--
-- Client code should use 'defaultSessionInitParams' to protect itself against
-- future extensions of this record.
data SessionInitParams = SessionInitParams {
    -- | Previously computed cabal macros,
    -- or 'Nothing' to compute them on startup
    sessionInitCabalMacros :: Maybe BSL.ByteString
  }

defaultSessionInitParams :: SessionInitParams
defaultSessionInitParams = SessionInitParams {
    sessionInitCabalMacros = Nothing
  }

-- | Create a fresh session, using some initial configuration.
--
-- Throws an exception if the configuration is invalid, or if GHC_PACKAGE_PATH
-- is set.
initSession :: SessionInitParams -> SessionConfig -> IO IdeSession
initSession initParams@SessionInitParams{..} ideConfig@SessionConfig{..} = do
  verifyConfig ideConfig

  configDirCanon <- Dir.canonicalizePath configDir
  ideSessionDir  <- createTempDirectory configDirCanon "session."
  let ideStaticInfo = IdeStaticInfo{..}

  -- Create the common subdirectories of session.nnnn so that we don't have to
  -- worry about creating these elsewhere
  Dir.createDirectoryIfMissing True (ideSessionSourceDir ideSessionDir)
  Dir.createDirectoryIfMissing True (ideSessionDataDir   ideSessionDir)
  Dir.createDirectoryIfMissing True (ideSessionDistDir   ideSessionDir)
  Dir.createDirectoryIfMissing True (ideSessionObjDir    ideSessionDir)

  -- Local initialization
  execInitParams ideStaticInfo initParams

  -- Start the GHC server (as a separate process)
  mServer <- forkGhcServer ideStaticInfo
  let (state, server, version) = case mServer of
         Right (s, v) -> (        IdeSessionPendingChanges,  s,          v)
         Left e       -> (const $ IdeSessionServerDied e,    Ex.throw e, Ex.throw e)

  -- The value of _ideLogicalTimestamp field is a workaround for
  -- the problems with 'invalidateModSummaryCache', which itself is
  -- a workaround for http://hackage.haskell.org/trac/ghc/ticket/7478.
  -- We have to make sure that file times never reach 0, because this will
  -- trigger an exception (http://hackage.haskell.org/trac/ghc/ticket/7567).
  -- We rather arbitrary start at Jan 2, 1970.
  let idleState = IdeIdleState {
          _ideLogicalTimestamp = 86400
        , _ideComputed         = Maybe.nothing
        , _ideDynamicOpts      = []
        , _ideRelativeIncludes = configRelativeIncludes
        , _ideGenerateCode     = False
        , _ideManagedFiles     = ManagedFilesInternal [] []
        , _ideObjectFiles      = []
        , _ideBuildExeStatus   = Nothing
        , _ideBuildDocStatus   = Nothing
        , _ideBuildLicensesStatus = Nothing
        , _ideEnv              = []
        , _ideArgs             = []
        , _ideStdoutBufferMode = RunNoBuffering
        , _ideStderrBufferMode = RunNoBuffering
        , _ideBreakInfo        = Maybe.nothing
        , _ideGhcServer        = server
        , _ideGhcVersion       = version
        , _ideTargets          = Public.TargetsExclude []
        }

  let pendingRemoteChanges = PendingRemoteChanges {
          -- Make sure 'ideComputed' is set on first call to updateSession
          -- TODO: Would be nicer if we did this a different way
          pendingUpdatedCode = True
        , pendingUpdatedEnv  = []
        , pendingUpdatedArgs = Nothing -- Server default is []
        , pendingUpdatedOpts = Nothing
        , pendingUpdatedIncl = Nothing
        }

  ideState <- newMVar (state pendingRemoteChanges idleState)
  return IdeSession{..}

-- | Set up the initial state of the session according to the given parameters
execInitParams :: IdeStaticInfo -> SessionInitParams -> IO ()
execInitParams staticInfo SessionInitParams{..} = do
  writeMacros staticInfo sessionInitCabalMacros

-- | Verify configuration, and throw an exception if configuration is invalid
verifyConfig :: SessionConfig -> IO ()
verifyConfig SessionConfig{..} = do
    unless (isValidPackageDB configPackageDBStack) $
      Ex.throw . userError $ "Invalid package DB stack: "
                             ++ show configPackageDBStack

    checkPackageDbEnvVar
  where
    isValidPackageDB :: PackageDBStack -> Bool
    isValidPackageDB stack =
          elemIndices GlobalPackageDB stack == [0]
       && elemIndices UserPackageDB stack `elem` [[], [1]]

-- Copied directly from Cabal
checkPackageDbEnvVar :: IO ()
checkPackageDbEnvVar = do
    hasGPP <- (getEnv "GHC_PACKAGE_PATH" >> return True)
              `catchIO` (\_ -> return False)
    when hasGPP $
      die $ "Use of GHC's environment variable GHC_PACKAGE_PATH is "
         ++ "incompatible with Cabal. Use the flag --package-db to specify a "
         ++ "package database (it can be used multiple times)."
  where
    -- Definitions so that the copied code from Cabal works

    die = Ex.throwIO . userError

    catchIO :: IO a -> (IOError -> IO a) -> IO a
    catchIO = Ex.catch

-- | Write per-package CPP macros.
writeMacros :: IdeStaticInfo -> Maybe BSL.ByteString -> IO ()
writeMacros IdeStaticInfo{ideConfig = SessionConfig {..}, ..}
            configCabalMacros = do
  macros <- case configCabalMacros of
              Nothing     -> generateMacros configPackageDBStack configExtraPathDirs
              Just macros -> return (BSL.unpack macros)
  writeFile (cabalMacrosLocation (ideSessionDistDir ideSessionDir)) macros

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
shutdownSession' forceTerminate session@IdeSession{ideState, ideStaticInfo} = do
  -- Try to terminate the server, unless we currently have a snippet running
  mStillRunning <- $modifyStrictMVar ideState $ \state ->
    case state of
      IdeSessionRunning runActions _idleState -> do
         return (state, Just runActions)
      IdeSessionIdle idleState -> do
        if forceTerminate
          then forceShutdownGhcServer $ _ideGhcServer idleState
          else shutdownGhcServer      $ _ideGhcServer idleState
        cleanupDirs
        return (IdeSessionShutdown, Nothing)
      IdeSessionPendingChanges _pendingChanges idleState -> do
        if forceTerminate
          then forceShutdownGhcServer $ _ideGhcServer idleState
          else shutdownGhcServer      $ _ideGhcServer idleState
        cleanupDirs
        return (IdeSessionShutdown, Nothing)
      IdeSessionShutdown ->
        return (IdeSessionShutdown, Nothing)
      IdeSessionServerDied _ _ -> do
        cleanupDirs
        return (IdeSessionShutdown, Nothing)

  case mStillRunning of
    Just runActions -> do
      -- If there is a snippet running, interrupt it and wait for it to finish.
      --
      -- We cannot do this while we hold the session lock, because the
      -- runactions will change the state of the session to Idle when the
      -- snippet terminates
      if forceTerminate then forceCancel runActions
                        else interrupt runActions
      void $ runWaitAll runActions
      shutdownSession' forceTerminate session
    Nothing ->
      -- We're done
      return ()
  where
    cleanupDirs :: IO ()
    cleanupDirs =
      when (configDeleteTempFiles . ideConfig $ ideStaticInfo) $
        ignoreDoesNotExist $
          Dir.removeDirectoryRecursive (ideSessionDir ideStaticInfo)

-- | Restarts a session. Technically, a new session is created under the old
-- @IdeSession@ handle, with a state cloned from the old session,
-- which is then shut down. The only behavioural difference between
-- the restarted session and the old one is that any running snippet code
-- (but not the executable binaries invoked with @runExe@) is stopped
-- (even if it was stuck and didn't respond to interrupt requests)
-- and that no modules are loaded, though all old modules and data files
-- are still contained in the new session and ready to be compiled with
-- the same flags and environment variables as before.
--
-- (We don't automatically recompile the code using the new session, because
-- what would we do with the progress messages?)
--
-- If the environment changed, you should pass in new 'SessionInitParams';
-- otherwise, pass 'Nothing'.
restartSession :: IdeSession -> Maybe SessionInitParams -> IO ()
restartSession IdeSession{ideStaticInfo, ideState} mInitParams = do
    -- Reflect changes in the environment, if any
    forM_ mInitParams (execInitParams ideStaticInfo)

    -- Restart the session
    $modifyStrictMVar_ ideState $ \state ->
      case state of
        IdeSessionIdle idleState ->
          restart noPendingRemoteChanges idleState
        IdeSessionRunning runActions idleState -> do
          forceCancel runActions
          restart noPendingRemoteChanges idleState
        IdeSessionPendingChanges pendingChanges idleState ->
          restart pendingChanges idleState
        IdeSessionShutdown ->
          fail "Shutdown session cannot be restarted."
        IdeSessionServerDied _externalException idleState ->
          restart noPendingRemoteChanges idleState
  where
    restart :: PendingRemoteChanges -> IdeIdleState -> IO IdeSessionState
    restart pendingChanges idleState = do
      forceShutdownGhcServer $ _ideGhcServer idleState
      mServer <- forkGhcServer ideStaticInfo
      case mServer of
        Right (server, version) -> do
          let idleState' =
                  (ideComputed       ^= Maybe.nothing)
                . (ideGhcServer      ^= server)
                . (ideGhcVersion     ^= version)
                . (ideObjectFiles    ^= [])
                $ idleState
              -- TODO: We could optimize this, and avoid a few RPC calls, by
              -- setting these fields to Nothing if the current value in
              -- idleState happens to be the server default.
              pendingRemoteChanges = pendingChanges {
                  pendingUpdatedCode = True
                , pendingUpdatedEnv  = pendingUpdatedEnv  pendingChanges ++        idleState ^. ideEnv
                , pendingUpdatedArgs = pendingUpdatedArgs pendingChanges <|> Just (idleState ^. ideArgs)
                , pendingUpdatedOpts = pendingUpdatedOpts pendingChanges <|> Just (idleState ^. ideDynamicOpts)
                , pendingUpdatedIncl = pendingUpdatedIncl pendingChanges <|> Just (idleState ^. ideRelativeIncludes)
                }
          return $ IdeSessionPendingChanges pendingRemoteChanges idleState'
        Left e ->
          return . IdeSessionServerDied e
                 . (ideGhcServer   ^= Ex.throw e)
                 . (ideGhcVersion  ^= Ex.throw e)
                 $ idleState

{-------------------------------------------------------------------------------
  Datatype describing session updates
-------------------------------------------------------------------------------}

data IdeSessionUpdateEnv = IdeSessionUpdateEnv {
    ideSessionUpdateStaticInfo :: IdeStaticInfo
  , ideSessionUpdateCallback   :: forall m. MonadIO m => Progress -> m ()
  }

data IdeSingleUpdate r =
    -- | Run an arbitrary IO action
    --
    -- As soon we as have to execute a single Run action we will mark the action
    -- as not executable while the session is in Running state
    forall a. Run (IO a) (a -> r)

    -- | Restart the session
    --
    -- Will mark the session as not updateable in Running mode.
  | Restart r

    -- | Get the static info
  | GetEnv (IdeSessionUpdateEnv -> r)

    -- | Get the state
  | GetState (IdeIdleState -> r)

    -- | Set the state
  | PutState IdeIdleState r

    -- | Write a file
  | FileCmd FileCmd (Bool -> r)

    -- | Schedule a remote change
  | Schedule (PendingRemoteChanges -> PendingRemoteChanges) r

instance Functor IdeSingleUpdate where
  fmap f (Run      act k) = Run      act (f . k)
  fmap f (Restart      k) = Restart      (f k)
  fmap f (GetEnv       k) = GetEnv       (f . k)
  fmap f (GetState     k) = GetState     (f . k)
  fmap f (PutState st  k) = PutState st  (f k)
  fmap f (FileCmd  cmd k) = FileCmd  cmd (f . k)
  fmap f (Schedule g   k) = Schedule g   (f k)

data IdeSessionUpdate a = Pure a | Free (IdeSingleUpdate (IdeSessionUpdate a))

instance Functor IdeSessionUpdate where
  fmap f (Pure x) = Pure (f x)
  fmap f (Free x) = Free (fmap (fmap f) x)

instance Monad IdeSessionUpdate where
  return = Pure
  Pure x >>= f = f x
  Free x >>= f = Free (fmap (>>= f) x)

instance Applicative IdeSessionUpdate where
  pure      = return
  mf <*> mx = do f <- mf ; x <- mx ; return (f x)

instance MonadReader IdeSessionUpdateEnv IdeSessionUpdate where
  ask = Free (GetEnv Pure)
  local _ (Pure x)          = Pure x
  local f (Free (GetEnv k)) = Free (fmap (local f) (GetEnv (k . f)))
  local f (Free x)          = Free (fmap (local f) x)

instance MonadState IdeIdleState IdeSessionUpdate where
  get    = Free (GetState Pure)
  put st = Free (PutState st (Pure ()))

instance MonadIO IdeSessionUpdate where
  liftIO act = Free (Run act Pure)

canExecuteWhileRunning :: IdeSession -> IdeSessionUpdate a -> Bool
canExecuteWhileRunning IdeSession{ideStaticInfo = staticInfo} = go
  where
    go :: IdeSessionUpdate a -> Bool
    go (Pure _)              = True
    go (Free (Run      _ _)) = False
    go (Free (Restart    _)) = False
    go (Free (GetEnv     k)) = go (k $ IdeSessionUpdateEnv staticInfo dummyCallback)
    go (Free (GetState   _)) = False
    go (Free (PutState _ _)) = False
    go (Free (FileCmd  _ k)) = go (k True) && go (k False)
    go (Free (Schedule _ _)) = False

    dummyCallback _ = return ()

restartSession' :: IdeSessionUpdate ()
restartSession' = Free (Restart (Pure ()))

schedule :: (PendingRemoteChanges -> PendingRemoteChanges) -> IdeSessionUpdate ()
schedule g = Free (Schedule g (Pure ()))

-- We assume, if updates are combined within the monoid, they can all
-- be applied in the context of the same session.
-- Otherwise, call 'updateSession' sequentially with the updates.
instance Monoid a => Monoid (IdeSessionUpdate a) where
  mempty        = return mempty
  f `mappend` g = mappend <$> f <*> g

data IdeSessionUpdateResult a =
    UpdateComplete a
  | RestartThenRun (IdeSessionUpdate a)

runSessionUpdate :: IdeStaticInfo
                 -> (Progress -> IO ())
                 -> IdeIdleState
                 -> PendingRemoteChanges
                 -> IdeSessionUpdate a
                 -> IO (IdeSessionUpdateResult a, IdeIdleState, PendingRemoteChanges)
runSessionUpdate staticInfo callback = go
  where
    go :: IdeIdleState -> PendingRemoteChanges
       -> IdeSessionUpdate a
       -> IO (IdeSessionUpdateResult a, IdeIdleState, PendingRemoteChanges)
    go st r (Pure x)                = return (UpdateComplete x, st, r)
    go st r (Free (Run      g   k)) = do a <- g ; go st r (k a)
    go st r (Free (Restart      k)) = return (RestartThenRun k, st, r)
    go st r (Free (GetEnv       k)) = go st  r (k env)
    go st r (Free (GetState     k)) = go st  r (k st)
    go _  r (Free (PutState st' k)) = go st' r k
    go st r (Free (FileCmd  cmd k)) = do (filesChanged, st') <- runStateT (executeFileCmd staticInfo cmd) st
                                         go st' r (k filesChanged)
    go st r (Free (Schedule g   k)) = go st (g r) k

    env :: IdeSessionUpdateEnv
    env = IdeSessionUpdateEnv {
              ideSessionUpdateStaticInfo = staticInfo
            , ideSessionUpdateCallback   = liftIO . callback
            }

{------------------------------------------------------------------------------
  Session updates
------------------------------------------------------------------------------}

-- | Given the current IDE session state, go ahead and
-- update the session, eventually resulting in a new session state,
-- with fully updated computed information (typing, etc.).
--
-- The update can be a long running operation, so we support a callback
-- which can be used to monitor progress of the operation.
updateSession :: IdeSession -> IdeSessionUpdate () -> (Progress -> IO ()) -> IO ()
updateSession = flip . updateSession'

updateSession' :: IdeSession -> (Progress -> IO ()) -> IdeSessionUpdate () -> IO ()
updateSession' session@IdeSession{ideStaticInfo, ideState} callback = \update ->
    go False (update >> recompileObjectFiles)
  where
    go :: Bool -> IdeSessionUpdate (Int, [SourceError]) -> IO ()
    go justRestarted update = do
      shouldRestart <- $modifyStrictMVar ideState $ \state ->
        case state of
          IdeSessionIdle idleState ->
            goAtomic noPendingRemoteChanges idleState update
          IdeSessionPendingChanges pendingChanges idleState ->
            goAtomic pendingChanges idleState update
          IdeSessionServerDied _ _ ->
            if not justRestarted
              then return (state, Just update)
              else return (state, Nothing)
          IdeSessionRunning _ _ ->
            Ex.throwIO (userError "Cannot update session in running mode")
          IdeSessionShutdown ->
            Ex.throwIO (userError "Session already shut down.")

      case shouldRestart of
        Just update' -> do
          restartSession session Nothing
          go True update'
        Nothing ->
          return ()

    -- The real work happens here. We will have the lock on the session while
    -- this executes. Returns Just an update if we need to restart the session
    -- before we can continue (we don't want to restart the session while we
    -- hold the lock).
    goAtomic :: PendingRemoteChanges
             -> IdeIdleState
             -> IdeSessionUpdate (Int, [SourceError])
             -> IO (IdeSessionState, Maybe (IdeSessionUpdate (Int, [SourceError])))
    goAtomic pendingChanges idleState update = Ex.handle (handleExternal idleState) $ do
      (result, idleState', pendingChanges') <- runSessionUpdate ideStaticInfo
                                                                callback
                                                                idleState
                                                                pendingChanges
                                                                update

      case result of
        RestartThenRun update' -> do
          -- To avoid "<stdout> hPutChar: resource vanished (Broken pipe)":
          -- TODO: Why is this necessary?
          threadDelay 100000

          -- We ignore justRestarted when we get an explicit RestartThenRun
          return (IdeSessionPendingChanges pendingChanges' idleState', Just update')

        UpdateComplete (numActions, cErrors) -> do
           let callback' p = callback p {
                   progressStep     = progressStep     p + numActions
                 , progressNumSteps = progressNumSteps p + numActions
                 }

           -- Update environment
           ideEnv' <- case pendingUpdatedEnv pendingChanges' of
             [] -> return $ idleState' ^. ideEnv
             cs -> do let newEnv :: [(String, Maybe String)] -> [(String, Maybe String)]
                          newEnv [] =
                            idleState' ^. ideEnv
                          newEnv ((var, val) : cs') =
                            lookup' var ^= Just val $ newEnv cs'

                          ideEnv' = newEnv cs

                      rpcSetEnv (idleState ^. ideGhcServer) ideEnv'
                      return ideEnv'

           -- Update command line arguments
           ideArgs' <- case pendingUpdatedArgs pendingChanges' of
             Nothing   -> return $ idleState' ^. ideArgs
             Just args -> do rpcSetArgs (idleState ^. ideGhcServer) args
                             return args

           -- Update ghc options
           let (ideDynamicOpts', ideRelativeIncludes', needSetOpts) =
                 case (pendingUpdatedOpts pendingChanges', pendingUpdatedIncl pendingChanges') of
                   (Nothing,   Nothing)   -> ( idleState' ^. ideDynamicOpts
                                             , idleState' ^. ideRelativeIncludes
                                             , False
                                             )
                   (Just opts, Nothing)   -> ( opts
                                             , idleState' ^. ideRelativeIncludes
                                             , True
                                             )
                   (Nothing,   Just incl) -> ( idleState' ^. ideDynamicOpts
                                             , incl
                                             , True
                                             )
                   (Just opts, Just incl) -> ( opts
                                             , incl
                                             , True
                                             )
           optionWarnings <- if needSetOpts
             then do
               -- relative include path is part of the state rather than the
               -- config as of c0bf0042
               let relOpts = relInclToOpts (ideSessionSourceDir (ideSessionDir ideStaticInfo))
                                           ideRelativeIncludes'
               (leftover, warnings) <- rpcSetGhcOpts (idleState ^. ideGhcServer)
                                                     (ideDynamicOpts' ++ relOpts)
               return $ force $
                 [ SourceError {
                       errorKind = KindWarning
                     , errorSpan = TextSpan (Text.pack "No location information")
                     , errorMsg  = Text.pack w
                     }
                 | w <- warnings ++ map unrecognized leftover
                 ]
             else
               return $ force []

           -- Recompile
           computed <- if pendingUpdatedCode pendingChanges'
             then do
               GhcCompileResult{..} <- rpcCompile (idleState' ^. ideGhcServer)
                                                  (idleState' ^. ideGenerateCode)
                                                  (idleState' ^. ideTargets)
                                                  callback'

               let applyDiff :: Strict (Map ModuleName) (Diff v)
                             -> (Computed -> Strict (Map ModuleName) v)
                             -> Strict (Map ModuleName) v
                   applyDiff diff f = applyMapDiff diff
                                    $ Maybe.maybe Map.empty f
                                    $ idleState' ^. ideComputed

               let diffSpan  = Map.map (fmap mkIdMap)  ghcCompileSpanInfo
                   diffTypes = Map.map (fmap mkExpMap) ghcCompileExpTypes
                   diffAuto  = Map.map (fmap (constructAuto ghcCompileCache)) ghcCompileAuto

               return $ Maybe.just Computed {
                   computedErrors        = force cErrors List.++ ghcCompileErrors List.++ optionWarnings
                 , computedLoadedModules = ghcCompileLoaded
                 , computedImports       = ghcCompileImports  `applyDiff` computedImports
                 , computedAutoMap       = diffAuto           `applyDiff` computedAutoMap
                 , computedSpanInfo      = diffSpan           `applyDiff` computedSpanInfo
                 , computedExpTypes      = diffTypes          `applyDiff` computedExpTypes
                 , computedUseSites      = ghcCompileUseSites `applyDiff` computedUseSites
                 , computedPkgDeps       = ghcCompilePkgDeps  `applyDiff` computedPkgDeps
                 , computedCache         = mkRelative ghcCompileCache
                 }
             else return $ idleState' ^. ideComputed

           -- Update state
           let idleState'' = (ideComputed         ^= computed)
                           . (ideDynamicOpts      ^= ideDynamicOpts')
                           . (ideRelativeIncludes ^= ideRelativeIncludes')
                           . (ideEnv              ^= ideEnv')
                           . (ideArgs             ^= ideArgs')
                           $ idleState'
           return (IdeSessionIdle idleState'', Nothing)

    unrecognized :: String -> String
    unrecognized str = "Unrecognized option " ++ show str

    mkRelative :: ExplicitSharingCache -> ExplicitSharingCache
    mkRelative ExplicitSharingCache{..} =
      let aux :: BSS.ByteString -> BSS.ByteString
          aux = BSS.pack . makeRelative (ideSessionSourceDir (ideSessionDir ideStaticInfo)) . BSS.unpack
      in ExplicitSharingCache {
        filePathCache = IntMap.map aux filePathCache
      , idPropCache   = idPropCache
      }

    handleExternal :: IdeIdleState -> ExternalException -> IO (IdeSessionState, Maybe (IdeSessionUpdate a))
    handleExternal idleState e = return (IdeSessionServerDied e idleState, Nothing)

    constructAuto :: ExplicitSharingCache -> Strict [] IdInfo
                  -> Strict Trie (Strict [] IdInfo)
    constructAuto cache lk =
        Trie.fromListWith (List.++) $ map aux (toLazyList lk)
      where
        aux :: IdInfo -> (BSS.ByteString, Strict [] IdInfo)
        aux idInfo@IdInfo{idProp = k} =
          let idProp = IntMap.findWithDefault
                         (error "constructAuto: could not resolve idPropPtr")
                         (idPropPtr k)
                         (idPropCache cache)
          in ( BSS.pack . Text.unpack . idName $ idProp
             , List.singleton idInfo
             )

-- | In 'recompileObjectFiles' we first collect a number of 'RecompileAction's,
-- before executing them. This makes it possible to generate better progress
-- messages. These recompile actions write out a list of C files that got
-- recompiled (for dependency tracking) as well as a list of compiliation errors.
type RecompileAction = (forall m. MonadIO m => String -> m ())
                    -> StateT ([FilePath], [SourceError]) IdeSessionUpdate ()

-- | Recompile any C files that need recompiling; if any, also mark all Haskell
-- modules are requiring recompilation.
--
-- Returns the number of actions that were executed, so we can adjust
-- Progress messages returned by ghc
recompileObjectFiles :: IdeSessionUpdate (Int, [SourceError])
recompileObjectFiles = do
    actions  <- execWriterT $ recompile
    callback <- asks ideSessionUpdateCallback
    (recompiled, errs) <- flip execStateT ([], []) $
       forM_ (zip actions [1..]) $ \(act, i) -> do
         let callback' :: forall m. MonadIO m => String -> m ()
             callback' msg = liftIO $ callback $
               Progress {
                  progressStep      = i
                , progressNumSteps  = length actions
                , progressParsedMsg = Just (Text.pack msg)
                , progressOrigMsg   = Just (Text.pack msg)
                }
         act callback'
    markAsUpdated (update recompiled)
    return (length actions, errs)
  where
    recompile :: WriterT [RecompileAction] IdeSessionUpdate ()
    recompile = do
      IdeStaticInfo{..} <- asks ideSessionUpdateStaticInfo
      managedFiles      <- get (ideManagedFiles .> managedSource)

      let cFiles :: [(FilePath, LogicalTimestamp)]
          cFiles = filter ((`elem` cExtensions) . takeExtension . fst)
                 $ map (\(fp, (_, ts)) -> (fp, ts))
                 $ managedFiles

          srcDir, objDir :: FilePath
          srcDir = ideSessionSourceDir ideSessionDir
          objDir = ideSessionObjDir    ideSessionDir

          compiling, loading, unloading, skipped :: FilePath -> String
          compiling src = "Compiling "       ++ makeRelative srcDir src
          loading   obj = "Loading "         ++ makeRelative objDir obj
          unloading obj = "Unloading "       ++ makeRelative objDir obj
          skipped   obj = "Skipped loading " ++ makeRelative objDir obj

      forM_ cFiles $ \(fp, ts) -> do
        let absC     = srcDir </> fp
            absObj   = objDir </> replaceExtension fp ".o"

        mObjFile <- get (ideObjectFiles .> lookup' fp)

        -- Unload the old object (if necessary)
        case mObjFile of
          Just (objFile, ts') | ts' < ts -> do
            delay $ \callback -> do
              callback (unloading objFile)
              lift $ unloadObject objFile
          _ ->
            return ()

        -- Recompile (if necessary)
        case mObjFile of
          Just (_objFile, ts') | ts' > ts ->
            -- The object is newer than the C file. Recompilation unnecessary
            return ()
          _ -> do
            delay $ \callback -> do
              callback (compiling fp)
              liftIO $ Dir.createDirectoryIfMissing True (dropFileName absObj)
              errs <- lift $ do
                errs <- runGcc absC absObj objDir
                if (null errs)
                  then do
                    ts' <- updateFileTimes absObj
                    set (ideObjectFiles .> lookup' fp) (Just (absObj, ts'))
                  else
                    set (ideObjectFiles .> lookup' fp) Nothing
                return errs
              if null errs then tellSt ([fp], [])
                           else tellSt ([], errs)
            delay $ \callback -> do
              (compiled, _errs) <- St.get
              if (fp `elem` compiled)
                then do callback (loading absObj)
                        lift $ loadObject absObj
                else callback (skipped absObj)

    delay :: MonadWriter [RecompileAction] m => RecompileAction -> m ()
    delay act = tell [act]

    -- NOTE: When using HscInterpreted/LinkInMemory, then C symbols get
    -- resolved during compilation, not during a separate linking step. To be
    -- precise, they get resolved from deep inside the compiler. Example
    -- callchain:
    --
    -- >            lookupStaticPtr   <-- does the resolution
    -- > called by  generateCCall
    -- > called by  schemeT
    -- > called by  schemeE
    -- > called by  doCase
    -- > called by  schemeE
    -- > called by  schemeER_wrk
    -- > called by  schemeR_wrk
    -- > called by  schemeR
    -- > called by  schemeTopBind
    -- > called by  byteCodeGen
    -- > called by  hscInteractive
    --
    -- Hence, we really need to recompile, rather than just relink.
    --
    -- TODO: If we knew which Haskell modules depended on which C files,
    -- we should do better here. For now we recompile all Haskell modules
    -- whenever any C file gets recompiled.
    update :: [FilePath] -> FilePath -> Bool
    update recompiled src = not (null recompiled)
                         && takeExtension src == ".hs"

-- | A session update that changes a source file by providing some contents.
-- This can be used to add a new module or update an existing one.
-- The @FilePath@ argument determines the directory
-- and file where the module is located within the project.
-- In case of Haskell source files, the actual internal
-- compiler module name, such as the one given by the
-- @getLoadedModules@ query, comes from within @module ... end@.
-- Usually the two names are equal, but they needn't be.
--
updateSourceFile :: FilePath -> BSL.ByteString -> IdeSessionUpdate ()
updateSourceFile fp bs =
  let fileInfo = FileInfo {
          fileInfoRemoteFile = fp
        , fileInfoRemoteDir  = ideSessionSourceDir
        , fileInfoAccessor   = managedSource
        }
  in do filesChanged <- Free (FileCmd (FileWrite fileInfo bs) Pure)
        when filesChanged $ schedule (\r -> r { pendingUpdatedCode = True })

-- | Like 'updateSourceFile' except that instead of passing the source by
-- value, it's given by reference to an existing file, which will be copied.
--
updateSourceFileFromFile :: FilePath -> IdeSessionUpdate ()
updateSourceFileFromFile fp =
  let fileInfo = FileInfo {
          fileInfoRemoteFile = fp
        , fileInfoRemoteDir  = ideSessionSourceDir
        , fileInfoAccessor   = managedSource
        }
  in do filesChanged <- Free (FileCmd (FileCopy fileInfo fp) Pure)
        when filesChanged $ schedule (\r -> r { pendingUpdatedCode = True })

-- | A session update that deletes an existing source file.
--
updateSourceFileDelete :: FilePath -> IdeSessionUpdate ()
updateSourceFileDelete fp =
  let fileInfo = FileInfo {
          fileInfoRemoteFile = fp
        , fileInfoRemoteDir  = ideSessionSourceDir
        , fileInfoAccessor   = managedSource
        }
  in do filesChanged <- Free (FileCmd (FileDelete fileInfo) Pure)
        when filesChanged $ schedule (\r -> r { pendingUpdatedCode = True })

-- | Set ghc dynamic options
--
-- This function is stateless: semantically, the full set of "active" options
-- are those in 'configStaticOpts' plus whatever options were set in the last
-- call to updateDynamicOptions.
updateDynamicOpts :: [String] -> IdeSessionUpdate ()
updateDynamicOpts opts = do
  schedule $ \r -> r {
      pendingUpdatedCode = True -- In case we need to recompile due to new opts
    , pendingUpdatedOpts = Just opts
    }

-- | Set include paths (equivalent of GHC's @-i@ parameter).
-- In general, this requires session restart,
-- because GHC doesn't revise module dependencies when targets
-- or include paths change, but only when files change.
--
-- This function is stateless: semantically, the set of currently active
-- include paths are those set in the last call to updateRelativeIncludes.
-- Any paths set earlier (including those from 'configRelativeIncludes')
-- are wiped out and overwritten in each call to updateRelativeIncludes.
updateRelativeIncludes :: [FilePath] -> IdeSessionUpdate ()
updateRelativeIncludes relIncl = do
  schedule $ \r -> r {
      pendingUpdatedCode = True -- In case we need to recompile due to new opts:
    , pendingUpdatedIncl = Just relIncl
    }
  restartSession'

-- | Enable or disable code generation in addition
-- to type-checking. Required by 'runStmt'.
updateCodeGeneration :: Bool -> IdeSessionUpdate ()
updateCodeGeneration b = do
  set ideGenerateCode b
  -- TODO: Shouldn't we do this only if b == True?
  schedule (\r -> r { pendingUpdatedCode = True })

-- | A session update that changes a data file by giving a new value for the
-- file. This can be used to add a new file or update an existing one.
--
updateDataFile :: FilePath -> BSL.ByteString -> IdeSessionUpdate ()
updateDataFile fp bs =
  let fileInfo = FileInfo {
          fileInfoRemoteFile = fp
        , fileInfoRemoteDir  = ideSessionDataDir
        , fileInfoAccessor   = managedData
        }
  in do filesChanged <- Free (FileCmd (FileWrite fileInfo bs) Pure)
        when filesChanged $ schedule (\r -> r { pendingUpdatedCode = True })

-- | Like 'updateDataFile' except that instead of passing the file content by
-- value, it's given by reference to an existing file (the second argument),
-- which will be copied.
--
updateDataFileFromFile :: FilePath -> FilePath -> IdeSessionUpdate ()
updateDataFileFromFile remoteFile localFile =
  let fileInfo = FileInfo {
          fileInfoRemoteFile = remoteFile
        , fileInfoRemoteDir  = ideSessionDataDir
        , fileInfoAccessor   = managedData
        }
  in do filesChanged <- Free (FileCmd (FileCopy fileInfo localFile) Pure)
        when filesChanged $ schedule (\r -> r { pendingUpdatedCode = True })

-- | Deletes an existing data file.
--
updateDataFileDelete :: FilePath -> IdeSessionUpdate ()
updateDataFileDelete fp =
  let fileInfo = FileInfo {
          fileInfoRemoteFile = fp
        , fileInfoRemoteDir  = ideSessionDataDir
        , fileInfoAccessor   = managedData
        }
  in do filesChanged <- Free (FileCmd (FileDelete fileInfo) Pure)
        when filesChanged $ schedule (\r -> r { pendingUpdatedCode = True })

-- | Set an environment variable
--
-- Use @updateEnv var Nothing@ to unset @var@.
updateEnv :: String -> Maybe String -> IdeSessionUpdate ()
updateEnv var val =
  schedule $ \r -> r { pendingUpdatedEnv = (var, val) : pendingUpdatedEnv r }

-- | Set command line arguments for snippets
-- (i.e., the expected value of `getArgs`)
updateArgs :: [String] -> IdeSessionUpdate ()
updateArgs args =
  schedule $ \r -> r { pendingUpdatedArgs = Just args }

-- | Set buffering mode for snippets' stdout
updateStdoutBufferMode :: RunBufferMode -> IdeSessionUpdate ()
updateStdoutBufferMode = set ideStdoutBufferMode

-- | Set buffering mode for snippets' stderr
updateStderrBufferMode :: RunBufferMode -> IdeSessionUpdate ()
updateStderrBufferMode = set ideStderrBufferMode

-- | Set compilation targets. In general, this requires session restart,
-- because GHC doesn't revise module dependencies when targets
-- or include paths change, but only when files change.
updateTargets :: Public.Targets -> IdeSessionUpdate ()
updateTargets targets = do
  IdeStaticInfo{..} <- asks ideSessionUpdateStaticInfo
  let sourceDir = ideSessionSourceDir ideSessionDir
  let dirTargets = case targets of
        Public.TargetsInclude l ->
          Public.TargetsInclude $ map (sourceDir </>) l
        Public.TargetsExclude l ->
          Public.TargetsExclude $ map (sourceDir </>) l
  set ideTargets dirTargets
  restartSession'

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
                                          -- ^ for interruptProcessGroupOf
                                      , std_in = CreatePipe
                                      , std_out = UseHandle std_wr_hdl
                                      , std_err = UseHandle std_wr_hdl
                                      }
      (Just stdin_hdl, Nothing, Nothing, ph) <- createProcess cproc

      -- The runActionState initially is the termination callback to be called
      -- when the snippet terminates. After termination
      -- it becomes (Right outcome).
      -- This means that we will only execute the termination callback once,
      -- avoid the race condition between mutliople @waitForProcess@
      -- and the user can safely call runWait after termination and get the same
      -- result.
      runActionsState <- newMVar (Left $ \_ -> return ())

      forceTermination <- newMVar False

      return $ RunActions
        { runWait = $modifyStrictMVar runActionsState $ \st -> case st of
            Right outcome ->
              return (Right outcome, Right outcome)
            Left terminationCallback -> do
              bs <- BSS.hGetSome std_rd_hdl blockSize
              if BSS.null bs
                then do
                  res <- waitForProcess ph
                  forceTerm <- $takeStrictMVar forceTermination
                  unless forceTerm $ terminationCallback res
                  return (Right res, Right res)
                else
                  return (Left terminationCallback, Left bs)
        , interrupt = interruptProcessGroupOf ph
        , supplyStdin = \bs -> BSS.hPut stdin_hdl bs >> IO.hFlush stdin_hdl
        , registerTerminationCallback = \callback' ->
            $modifyStrictMVar_ runActionsState $ \st -> case st of
              Right outcome ->
                return (Right outcome)
              Left callback ->
                return (Left (\res -> callback res >> callback' res))
        , forceCancel = do
            $swapStrictMVar forceTermination True
            terminateProcess ph
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
      let cmd   = mkCmd idleState
          cache = computedCache comp

      checkStateOk comp cmd
      isBreak    <- newEmptyMVar
      runActions <- rpcRun (idleState ^. ideGhcServer)
                           cmd
                           (translateRunResult isBreak)
      registerTerminationCallback runActions (restoreToIdle cache isBreak)
      return (IdeSessionRunning runActions idleState, runActions)
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

    restoreToIdle :: ExplicitSharingCache -> StrictMVar (Strict Maybe BreakInfo) -> Public.RunResult -> IO ()
    restoreToIdle cache isBreak _ = do
      mBreakInfo <- $readStrictMVar isBreak
      $modifyStrictMVar_ (ideState session) $ \state -> case state of
        IdeSessionIdle _ ->
          Ex.throwIO (userError "The impossible happened!")
        IdeSessionPendingChanges _ _ ->
          Ex.throwIO (userError "The impossible happened!")
        IdeSessionRunning _ idleState -> do
          let upd = ideBreakInfo ^= fmap (removeExplicitSharing cache) mBreakInfo
          return $ IdeSessionIdle (upd idleState)
        IdeSessionShutdown ->
          return state
        IdeSessionServerDied _ _ ->
          return state

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

-- | Build an exe from sources added previously via the ide-backend
-- updateSourceFile* mechanism. The modules that contains the @main@ code are
-- indicated in second argument to @buildExe@. The function can be called
-- multiple times with different arguments. Additional GHC options,
-- applied only when building executables, are supplied in the first argument.
--
-- We assume any indicated module is already successfully processed by GHC API
-- in a compilation mode that makes @computedImports@ available (but no code
-- needs to be generated). The environment (package dependencies, ghc options,
-- preprocessor program options, etc.) for building the exe is the same as when
-- previously compiling the code via GHC API. The module does not have to be
-- called @Main@, but we assume the main function is always @main@ (we don't
-- check this and related conditions, but GHC does when eventually called to
-- build the exe).
--
-- The executable files are placed in the filesystem inside the @build@
-- subdirectory of 'Query.getDistDir', in subdirectories corresponding
-- to the given module names. The build directory does not overlap
-- with any of the other used directories and with its path.
--
-- Logs from the building process are saved in files
-- @build\/ide-backend-exe.stdout@ and @build\/ide-backend-exe.stderr@
-- in the 'Query.getDistDir' directory.
--
-- Note: currently it requires @configGenerateModInfo@ to be set (see #86).
-- Also, after session restart, one has to call @updateSession@ at least once
-- (even with empty updates list) before calling it for @buildExe@.
-- This ensures the code is compiled again and the results made accessible.
buildExe :: [String] -> [(ModuleName, FilePath)] -> IdeSessionUpdate ()
buildExe extraOpts ms = do
    ideStaticInfo@IdeStaticInfo{..} <- asks ideSessionUpdateStaticInfo
    let SessionConfig{..} = ideConfig
    let ideDistDir   = ideSessionDistDir   ideSessionDir
        ideDataDir   = ideSessionDataDir   ideSessionDir
        ideSourceDir = ideSessionSourceDir ideSessionDir

    callback          <- asks ideSessionUpdateCallback
    mcomputed         <- get ideComputed
    dynamicOpts       <- get ideDynamicOpts
    relativeIncludes  <- get ideRelativeIncludes
        -- Note that these do not contain the @packageDbArgs@ options.
    when (not configGenerateModInfo) $
      -- TODO: replace the check with an inspection of state component (#87)
      fail "Features using cabal API require configGenerateModInfo, currently (#86)."
    liftIO $ Dir.createDirectoryIfMissing False $ ideDistDir </> "build"
    let beStdoutLog = ideDistDir </> "build/ide-backend-exe.stdout"
        beStderrLog = ideDistDir </> "build/ide-backend-exe.stderr"
        errors = case toLazyMaybe mcomputed of
          Nothing ->
            error "This session state does not admit artifact generation."
          Just Computed{computedErrors} -> toLazyList computedErrors
    exitCode <-
      if any (== KindError) $ map errorKind errors then do
        liftIO $ do
          writeFile beStderrLog
            "Source errors encountered. Not attempting to build executables."
          return $ ExitFailure 1
      else do
        let ghcOpts = "-rtsopts=some"
                    : configStaticOpts ++ dynamicOpts ++ extraOpts
        liftIO $ Ex.bracket
          Dir.getCurrentDirectory
          Dir.setCurrentDirectory
          (const $
             do Dir.setCurrentDirectory ideDataDir
                (loadedMs, pkgs) <- buildDeps mcomputed
                libDeps <- externalDeps pkgs
                let beArgs =
                      BuildExeArgs{ bePackageDBStack   = configPackageDBStack
                                  , beExtraPathDirs    = configExtraPathDirs
                                  , beSourcesDir       = ideSourceDir
                                  , beDistDir          = ideDistDir
                                  , beRelativeIncludes = relativeIncludes
                                  , beGhcOpts          = ghcOpts
                                  , beLibDeps          = libDeps
                                  , beLoadedMs         = loadedMs
                                  , beStdoutLog
                                  , beStderrLog
                                  }
                invokeExeCabal ideStaticInfo (ReqExeBuild beArgs ms) callback)
    -- Solution 2. to #119: update timestamps of .o (and all other) files
    -- according to the session's artificial timestamp.
    newTS <- nextLogicalTimestamp
    liftIO $ do
      objectPaths <- find always
                          (fileType ==? RegularFile
                           &&? (extension ==? ".o"
                                ||? extension ==? ".hi"
                                ||? extension ==? ".a"))
                          (ideDistDir </> "build")
      forM_ objectPaths $ \path -> do
        fileStatus <- getFileStatus path
        -- We only reset the timestamp, if ghc modified the file.
        when (modificationTime fileStatus > newTS) $
          setFileTimes path newTS newTS
    set ideBuildExeStatus (Just exitCode)

-- | Build haddock documentation from sources added previously via
-- the ide-backend updateSourceFile* mechanism. Similarly to 'buildExe',
-- it needs the project modules to be already loaded within the session
-- and the generated docs can be found in the @doc@ subdirectory
-- of 'Query.getDistDir'.
--
-- Logs from the documentation building process are saved in files
-- @doc\/ide-backend-doc.stdout@ and @doc\/ide-backend-doc.stderr@
-- in the 'Query.getDistDir' directory.
--
-- Note: currently it requires @configGenerateModInfo@ to be set (see #86).
buildDoc :: IdeSessionUpdate ()
buildDoc = do
    ideStaticInfo@IdeStaticInfo{..} <- asks ideSessionUpdateStaticInfo
    let SessionConfig{..} = ideConfig
    let ideDistDir   = ideSessionDistDir   ideSessionDir
        ideDataDir   = ideSessionDataDir   ideSessionDir
        ideSourceDir = ideSessionSourceDir ideSessionDir

    callback          <- asks ideSessionUpdateCallback
    mcomputed         <- get ideComputed
    dynamicOpts       <- get ideDynamicOpts
    relativeIncludes  <- get ideRelativeIncludes
    when (not configGenerateModInfo) $
      -- TODO: replace the check with an inspection of state component (#87)
      fail "Features using cabal API require configGenerateModInfo, currently (#86)."
    liftIO $ Dir.createDirectoryIfMissing False $ ideDistDir </> "doc"
    let ghcOpts = configStaticOpts ++ dynamicOpts
        beStdoutLog = ideDistDir </> "doc/ide-backend-doc.stdout"
        beStderrLog = ideDistDir </> "doc/ide-backend-doc.stderr"
    exitCode <- liftIO $ Ex.bracket
      Dir.getCurrentDirectory
      Dir.setCurrentDirectory
      (const $ do Dir.setCurrentDirectory ideDataDir
                  (loadedMs, pkgs) <- buildDeps mcomputed
                  libDeps <- externalDeps pkgs
                  let beArgs =
                        BuildExeArgs{ bePackageDBStack   = configPackageDBStack
                                    , beExtraPathDirs    = configExtraPathDirs
                                    , beSourcesDir       = ideSourceDir
                                    , beDistDir          = ideDistDir
                                    , beRelativeIncludes = relativeIncludes
                                    , beGhcOpts          = ghcOpts
                                    , beLibDeps          = libDeps
                                    , beLoadedMs         = loadedMs
                                    , beStdoutLog
                                    , beStderrLog
                                    }
                  invokeExeCabal ideStaticInfo (ReqExeDoc beArgs) callback)
    set ideBuildDocStatus (Just exitCode)

-- | Build a file containing licenses of all used packages.
-- Similarly to 'buildExe', the function needs the project modules to be
-- already loaded within the session. The concatenated licenses can be found
-- in file @licenses.txt@ inside the 'Query.getDistDir' directory.
--
-- The function expects .cabal files of all used packages,
-- except those mentioned in 'configLicenseExc',
-- to be gathered in the directory given as the first argument.
-- The code then expects to find those packages installed and their
-- license files in the usual place that Cabal puts them
-- (or the in-place packages should be correctly embedded in the GHC tree).
--
-- We guess the installed locations of the license files on the basis
-- of the haddock interfaces path. If the default setting does not work
-- properly, the haddock interfaces path should be set manually. E.g.,
-- @cabal configure --docdir=the_same_path --htmldir=the_same_path@
-- affects the haddock interfaces path (because it is by default based
-- on htmldir) and is reported to work for some values of @the_same_path@.
--
-- Logs from the license search and catenation process are saved in files
-- @licenses.stdout@ and @licenses.stderr@
-- in the 'Query.getDistDir' directory.
--
-- Note: currently 'configGenerateModInfo' needs to be set
-- for this function to work (see #86).
--
-- Note: if the executable uses TH and its module is named @Main@
-- (and so it's not compiled as a part of a temporary library)
-- 'Config.configDynLink' needs to be set. See #162.
buildLicenses :: FilePath -> IdeSessionUpdate ()
buildLicenses cabalsDir = do
    IdeStaticInfo{..} <- asks ideSessionUpdateStaticInfo
    let SessionConfig{configGenerateModInfo} = ideConfig
    let ideDistDir = ideSessionDistDir ideSessionDir

    callback  <- asks ideSessionUpdateCallback
    mcomputed <- get ideComputed
    when (not configGenerateModInfo) $
      -- TODO: replace the check with an inspection of state component (#87)
      fail "Features using cabal API require configGenerateModInfo, currently (#86)."
    exitCode <- liftIO $
      buildLicenseCatenation ideConfig mcomputed cabalsDir ideDistDir callback
    set ideBuildLicensesStatus (Just exitCode)

{------------------------------------------------------------------------------
  Debugging
------------------------------------------------------------------------------}

-- | Force recompilation of all modules. For debugging only.
forceRecompile :: IdeSessionUpdate ()
forceRecompile = markAsUpdated (const True)

-- | Crash the GHC server. For debugging only. If the specified delay is
-- @Nothing@, crash immediately; otherwise, set up a thread that throws
-- an exception to the main thread after the delay.
crashGhcServer :: IdeSession -> Maybe Int -> IO ()
crashGhcServer IdeSession{..} delay = $withStrictMVar ideState $ \state ->
  case state of
    IdeSessionIdle idleState ->
      rpcCrash (idleState ^. ideGhcServer) delay
    IdeSessionPendingChanges _ idleState ->
      rpcCrash (idleState ^. ideGhcServer) delay
    _ ->
      Ex.throwIO $ userError "State not idle"

{------------------------------------------------------------------------------
  Internal session updates
------------------------------------------------------------------------------}

-- | Load an object file
loadObject :: FilePath -> IdeSessionUpdate ()
loadObject path = do
  ghcServer <- get ideGhcServer
  liftIO $ rpcLoad ghcServer path False

-- | Unload an object file
unloadObject :: FilePath -> IdeSessionUpdate ()
unloadObject path = do
  ghcServer <- get ideGhcServer
  liftIO $ rpcLoad ghcServer path True

-- | Force recompilation of the given modules
--
-- TODO: Should we update data files here too?
markAsUpdated :: (FilePath -> Bool) -> IdeSessionUpdate ()
markAsUpdated shouldMark = do
  IdeStaticInfo{..} <- asks ideSessionUpdateStaticInfo
  sources  <- get (ideManagedFiles .> managedSource)
  sources' <- forM sources $ \(path, (digest, oldTS)) ->
    if shouldMark path
      then do schedule (\r -> r { pendingUpdatedCode = True })
              newTS <- updateFileTimes (ideSessionSourceDir ideSessionDir </> path)
              return (path, (digest, newTS))
      else return (path, (digest, oldTS))
  set (ideManagedFiles .> managedSource) sources'

-- | Update the file times of the given file with the next logical timestamp
updateFileTimes :: (MonadState IdeIdleState m, MonadIO m) => FilePath -> m LogicalTimestamp
updateFileTimes path = do
  ts <- nextLogicalTimestamp
  liftIO $ setFileTimes path ts ts
  return ts

-- | Get the next available logical timestamp
nextLogicalTimestamp :: MonadState IdeIdleState m => m LogicalTimestamp
nextLogicalTimestamp = do
  newTS <- get ideLogicalTimestamp
  modify ideLogicalTimestamp (+ 1)
  return newTS

-- | Call gcc via ghc, with the same parameters cabal uses.
runGcc :: FilePath -> FilePath -> FilePath -> IdeSessionUpdate [SourceError]
runGcc absC absObj pref = do
    ideStaticInfo@IdeStaticInfo{..} <- asks ideSessionUpdateStaticInfo
    let ideDistDir = ideSessionDistDir ideSessionDir

    callback  <- asks ideSessionUpdateCallback
    liftIO $ do
     -- Direct call to gcc, for testing only:
     let SessionConfig{configPackageDBStack, configExtraPathDirs} = ideConfig
         _gcc :: FilePath
         _gcc = "/usr/bin/gcc"
         _args :: [String]
         _args = [ "-c"
                 , "-o", absObj
                 , absC
                 ]
         _stdin :: String
         _stdin = ""
         stdoutLog = ideDistDir </> "ide-backend-cc.stdout"
         stderrLog = ideDistDir </> "ide-backend-cc.stderr"
         runCcArgs = RunCcArgs{ rcPackageDBStack = configPackageDBStack
                              , rcExtraPathDirs = configExtraPathDirs
                              , rcDistDir = ideDistDir
                              , rcStdoutLog = stdoutLog
                              , rcStderrLog = stderrLog
                              , rcAbsC = absC
                              , rcAbsObj = absObj
                              , rcPref = pref }
     -- (_exitCode, _stdout, _stderr)
     --   <- readProcessWithExitCode _gcc _args _stdin
     -- The real deal; we call gcc via ghc via cabal functions:
     exitCode <- invokeExeCabal ideStaticInfo (ReqExeCc runCcArgs) callback
     stdout <- readFile stdoutLog
     stderr <- readFile stderrLog
     case exitCode of
       ExitSuccess   -> return []
       ExitFailure _ -> return (parseErrorMsgs stdout stderr)
  where
    -- TODO: Parse the error messages returned by gcc. For now, we just
    -- return all output as a single, unlocated, error.
    parseErrorMsgs :: String -> String -> [SourceError]
    parseErrorMsgs stdout stderr = [SourceError
      { errorKind = KindError
      , errorSpan = TextSpan (Text.pack "<gcc error>")
      , errorMsg  = Text.pack (stdout ++ stderr)
      }]

{------------------------------------------------------------------------------
  Aux
------------------------------------------------------------------------------}

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

-- | Variaton on 'tell' for the State monad rather than writer monad
tellSt :: (Monoid w, MonadState w m) => w -> m ()
tellSt w = St.modify (`mappend` w)

{-------------------------------------------------------------------------------
  File commands
-------------------------------------------------------------------------------}

data FileInfo = FileInfo {
    fileInfoRemoteDir  :: FilePath -> FilePath
  , fileInfoRemoteFile :: FilePath
  , fileInfoAccessor   :: Accessor ManagedFilesInternal [ManagedFile]
  }

data FileCmd =
    -- | Write a file from a bytestring
    FileWrite FileInfo BSL.ByteString

    -- | Copy a local file (the FilePath is interpreted as an absolute path)
  | FileCopy FileInfo FilePath

    -- | Delete a file
  | FileDelete FileInfo

-- | Execute a file command
--
-- Returns 'True' if any files were changed.
executeFileCmd :: (MonadState IdeIdleState m, MonadIO m) => IdeStaticInfo -> FileCmd -> m Bool
executeFileCmd staticInfo@IdeStaticInfo{..} cmd = case cmd of
    FileWrite _ bs -> do
      old <- get cachedInfo
      -- We always overwrite the file, and then later set the timestamp back
      -- to what it was if it turns out the hash was the same. If we compute
      -- the hash first, we would force the entire lazy bytestring into memory
      newHash <- liftIO $ writeFileAtomic remotePath bs
      case old of
        Just (oldHash, oldTS) | oldHash == newHash -> do
          liftIO $ setFileTimes remotePath oldTS oldTS
          return False
        _ -> do
          newTS <- updateFileTimes remotePath
          set cachedInfo (Just (newHash, newTS))
          return True
    FileCopy _ localFile -> do
      -- We just call 'FileWrite' because we need to read the file anyway to
      -- compute the hash. Note that `localPath` is interpreted relative to the
      -- current directory
      bs <- liftIO $ BSL.readFile localFile
      executeFileCmd staticInfo (FileWrite info bs)
    FileDelete _ -> do
      liftIO $ ignoreDoesNotExist $ Dir.removeFile remotePath
      set cachedInfo Nothing
      -- TODO: We should really return True only if the file existed
      return True
  where
    remotePath :: FilePath
    remotePath = fileInfoRemoteDir info ideSessionDir </> fileInfoRemoteFile info

    info :: FileInfo
    info = case cmd of FileWrite  i _ -> i
                       FileCopy   i _ -> i
                       FileDelete i   -> i

    cachedInfo :: Accessor IdeIdleState (Maybe (MD5Digest, LogicalTimestamp))
    cachedInfo = ideManagedFiles .> fileInfoAccessor info .> lookup' (fileInfoRemoteFile info)

ignoreDoesNotExist :: IO () -> IO ()
ignoreDoesNotExist = Ex.handle $ \e ->
  if isDoesNotExistError e then return ()
                           else Ex.throwIO e
