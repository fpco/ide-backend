{-# LANGUAGE FlexibleContexts #-}
-- | IDE session updates
--
-- We should only be using internal types here (explicit strictness/sharing)
module IdeSession.Update (
    -- * Starting and stopping
    initSession
  , SessionInitParams(..)
  , defaultSessionInitParams
  , shutdownSession
  , restartSession
    -- * Session updates
  , IdeSessionUpdate -- Abstract
  , updateSession
  , updateModule
  , updateModuleFromFile
  , updateModuleDelete
  , updateGhcOptions
  , updateCodeGeneration
  , updateDataFile
  , updateDataFileFromFile
  , updateDataFileDelete
  , updateEnv
  , updateArgs
  , updateStdoutBufferMode
  , updateStderrBufferMode
  , buildExe
  , buildDoc
  , buildLicenses
    -- * Running code
  , runStmt
    -- * Debugging
  , forceRecompile
  , crashGhcServer
  )
  where

import Control.Monad (when, void, forM, unless)
import Control.Monad.State (MonadState, StateT, execStateT, lift)
import Control.Monad.IO.Class (liftIO)
import qualified Control.Exception as Ex
import Data.List (delete, elemIndices, intercalate)
import Data.Monoid (Monoid(..))
import Data.Accessor ((.>), (^.), (^=))
import Data.Accessor.Monad.MTL.State (get, modify, set)
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BSS
import Data.Maybe (fromMaybe)
import Data.Foldable (forM_)
import qualified System.Directory as Dir
import System.FilePath (takeDirectory, makeRelative, (</>),
                        splitSearchPath, searchPathSeparator)
import System.Posix.Files (setFileTimes)
import System.IO.Temp (createTempDirectory)
import qualified Data.Text as Text
import System.Environment (getEnv, getEnvironment)
import Data.Version (Version(..))

import Distribution.Simple (PackageDBStack, PackageDB(..))

import IdeSession.State
import IdeSession.Cabal
import IdeSession.Config
import IdeSession.GHC.API
import IdeSession.GHC.Client
import IdeSession.Types.Private
import IdeSession.Types.Progress
import IdeSession.Util
import IdeSession.Strict.Container
import IdeSession.RPC.Server (ExternalException)
import qualified IdeSession.Strict.IntMap as IntMap
import qualified IdeSession.Strict.Map    as Map
import qualified IdeSession.Strict.Maybe  as Maybe
import qualified IdeSession.Strict.List   as List
import IdeSession.Strict.MVar (newMVar, modifyMVar, modifyMVar_, withMVar)

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
initSession initParams ideConfig@SessionConfig{..} = do
  verifyConfig ideConfig

  -- TODO: Don't hardcode ghc version
  let ghcOpts = configStaticOpts
             ++ packageDbArgs (Version [7,4,2] []) configPackageDBStack

  configDirCanon <- Dir.canonicalizePath configDir
  ideSourcesDir  <- createTempDirectory configDirCanon "src."
  ideDataDir     <- createTempDirectory configDirCanon "data."
  ideDistDir     <- createTempDirectory configDirCanon "dist."
  env            <- envWithPathOverride configExtraPathDirs
  _ideGhcServer  <- forkGhcServer configGenerateModInfo ghcOpts
                                  (Just ideDataDir) env configInProcess
  -- The value of _ideLogicalTimestamp field is a workaround for
  -- the problems with 'invalidateModSummaryCache', which itself is
  -- a workaround for http://hackage.haskell.org/trac/ghc/ticket/7478.
  -- We have to make sure that file times never reach 0, because this will
  -- trigger an exception (http://hackage.haskell.org/trac/ghc/ticket/7567).
  -- We rather arbitrary start at Jan 2, 1970.
  ideState <- newMVar $ IdeSessionIdle IdeIdleState {
                          _ideLogicalTimestamp = 86400
                        , _ideComputed         = Maybe.nothing
                        , _ideNewOpts          = Nothing
                        , _ideGenerateCode     = False
                        , _ideManagedFiles     = ManagedFilesInternal [] []
                        , _ideBuildExeStatus   = Nothing
                        , _ideBuildDocStatus   = Nothing
                        , _ideBuildLicensesStatus = Nothing
                        , _ideEnv              = []
                        , _ideArgs             = []
                        , _ideUpdatedEnv       = False
                        , _ideUpdatedArgs      = False -- Server default is []
                          -- Make sure 'ideComputed' is set on first call
                          -- to updateSession
                        , _ideUpdatedCode      = True
                        , _ideStdoutBufferMode = RunNoBuffering
                        , _ideStderrBufferMode = RunNoBuffering
                        , _ideGhcServer
                        }
  let ideStaticInfo = IdeStaticInfo{..}
  let session = IdeSession{..}

  execInitParams ideStaticInfo initParams

  return session

-- | Set up the initial state of the session according to the given parameters
execInitParams :: IdeStaticInfo -> SessionInitParams -> IO ()
execInitParams staticInfo SessionInitParams{..} = do
  -- TODO: for now, this location is safe, but when the user
  -- is allowed to overwrite .h files, we need to create an extra dir.
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

envWithPathOverride :: [FilePath] -> IO (Maybe [(String, String)])
envWithPathOverride []            = return Nothing
envWithPathOverride extraPathDirs = do
    env <- getEnvironment
    let path  = fromMaybe "" (lookup "PATH" env)
        path' = intercalate [searchPathSeparator]
                  (splitSearchPath path ++ extraPathDirs)
        env'  = ("PATH", path') : filter (\(var, _) -> var /= "PATH") env
    return (Just env')

-- | Write per-package CPP macros.
writeMacros :: IdeStaticInfo -> Maybe BSL.ByteString -> IO ()
writeMacros IdeStaticInfo{ ideConfig = SessionConfig {..}
                         , ideSourcesDir
                         }
            configCabalMacros = do
  macros <- case configCabalMacros of
              Nothing     -> generateMacros configPackageDBStack configExtraPathDirs
              Just macros -> return (BSL.unpack macros)
  writeFile (cabalMacrosLocation ideSourcesDir) macros

-- | Close a session down, releasing the resources.
--
-- This operation is the only one that can be run after a shutdown was already
-- performed. This lets the API user execute an early shutdown, e.g., before
-- the @shutdownSession@ placed inside 'bracket' is triggered by a normal
-- program control flow.
--
-- If code is still running, it will be interrupted.
shutdownSession :: IdeSession -> IO ()
shutdownSession IdeSession{ideState, ideStaticInfo} = do
  snapshot <- modifyMVar ideState $ \state -> return (IdeSessionShutdown, state)
  case snapshot of
    IdeSessionRunning runActions idleState -> do
      -- We need to terminate the running program before we can shut down
      -- the session, because the RPC layer will sequentialize all concurrent
      -- calls (and if code is still running we still have an active
      -- RPC conversation)
      interrupt runActions
      void $ runWaitAll runActions
      shutdownGhcServer $ _ideGhcServer idleState
      cleanupDirs
    IdeSessionIdle idleState -> do
      shutdownGhcServer $ _ideGhcServer idleState
      cleanupDirs
    IdeSessionShutdown ->
      return ()
    IdeSessionServerDied _ _ ->
      cleanupDirs
 where
  cleanupDirs = do
    let dataDir    = ideDataDir ideStaticInfo
        sourcesDir = ideSourcesDir ideStaticInfo
    -- TODO: this has a race condition (not sure we care; if not, say why not)
    dataExists <- Dir.doesDirectoryExist dataDir
    when dataExists $ Dir.removeDirectoryRecursive dataDir
    sourceExists <- Dir.doesDirectoryExist sourcesDir
    when sourceExists $ Dir.removeDirectoryRecursive sourcesDir

-- | Restarts a session. Technically, a new session is created under the old
-- @IdeSession@ handle, with a state cloned from the old session,
-- which is then shut down. The only behavioural difference between
-- the restarted session and the old one is that any running code is stopped
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
    modifyMVar_ ideState $ \state ->
      case state of
        IdeSessionIdle idleState ->
          restart idleState
        IdeSessionRunning runActions idleState -> do
          forceCancel runActions
          restart idleState
        IdeSessionShutdown ->
          fail "Shutdown session cannot be restarted."
        IdeSessionServerDied _externalException idleState ->
          restart idleState
  where
    restart :: IdeIdleState -> IO IdeSessionState
    restart idleState = do
      forceShutdownGhcServer $ _ideGhcServer idleState
      env    <- envWithPathOverride configExtraPathDirs
      let ghcOpts = configStaticOpts
                 ++ packageDbArgs (Version [7,4,2] []) configPackageDBStack
      server <-
        forkGhcServer
          configGenerateModInfo ghcOpts workingDir env configInProcess
      return . IdeSessionIdle
             . (ideComputed    ^= Maybe.nothing)
             . (ideUpdatedEnv  ^= True)
             . (ideUpdatedArgs ^= True)
             . (ideUpdatedCode ^= True)
             . (ideGhcServer   ^= server)
             $ idleState

    workingDir = Just (ideDataDir ideStaticInfo)
    SessionConfig{..} = ideConfig ideStaticInfo

{------------------------------------------------------------------------------
  Session updates
------------------------------------------------------------------------------}

-- | We use the 'IdeSessionUpdate' type to represent the accumulation of a
-- bunch of updates.
--
-- In particular it is an instance of 'Monoid', so multiple primitive updates
-- can be easily combined. Updates can override each other left to right.
newtype IdeSessionUpdate = IdeSessionUpdate {
    runSessionUpdate :: (Progress -> IO ()) -> IdeStaticInfo
                     -> StateT IdeIdleState IO ()
  }

-- We assume, if updates are combined within the monoid, they can all
-- be applied in the context of the same session.
-- Otherwise, call 'updateSession' sequentially with the updates.
instance Monoid IdeSessionUpdate where
  mempty = IdeSessionUpdate $ \_callback _staticInfo -> return ()
  (IdeSessionUpdate f) `mappend` (IdeSessionUpdate g) =
    IdeSessionUpdate $ \callback staticInfo ->
      f callback staticInfo >> g callback staticInfo

-- | Given the current IDE session state, go ahead and
-- update the session, eventually resulting in a new session state,
-- with fully updated computed information (typing, etc.).
--
-- The update can be a long running operation, so we support a callback
-- which can be used to monitor progress of the operation.
updateSession :: IdeSession -> IdeSessionUpdate -> (Progress -> IO ()) -> IO ()
updateSession session@IdeSession{ideStaticInfo, ideState} update callback = do
    -- We don't want to call 'restartSession' while we hold the lock
    shouldRestart <- modifyMVar ideState $ \state -> case state of
      IdeSessionIdle idleState -> Ex.handle (handleExternal idleState) $ do
        idleState' <-
          execStateT (runSessionUpdate update callback ideStaticInfo)
                     idleState

        -- Update environment
        when (idleState' ^. ideUpdatedEnv) $
          rpcSetEnv (idleState ^. ideGhcServer) (idleState' ^. ideEnv)

        when (idleState' ^. ideUpdatedArgs) $
          rpcSetArgs (idleState ^. ideGhcServer) (idleState' ^. ideArgs)

        -- Recompile
        computed <- if (idleState' ^. ideUpdatedCode)
          then do
            (  errs
             , loaded
             , diffImports
             , diffAuto
             , diffIdList
             , diffPkgDeps
             , cache ) <- rpcCompile (idleState' ^. ideGhcServer)
                                     (idleState' ^. ideNewOpts)
                                     (ideSourcesDir ideStaticInfo)
                                     (idleState' ^. ideGenerateCode)
                                     callback

            let applyDiff :: Strict (Map ModuleName) (Diff v)
                          -> (Computed -> Strict (Map ModuleName) v)
                          -> Strict (Map ModuleName) v
                applyDiff diff f = applyMapDiff diff
                                 $ Maybe.maybe Map.empty f
                                 $ idleState' ^. ideComputed

            let diffSpan = Map.map (fmap idListToMap) diffIdList

            return $ Maybe.just Computed {
                computedErrors        = errs
              , computedLoadedModules = loaded
              , computedImports       = diffImports `applyDiff` computedImports
              , computedAutoMap       = diffAuto    `applyDiff` computedAutoMap
              , computedSpanInfo      = diffSpan    `applyDiff` computedSpanInfo
              , computedPkgDeps       = diffPkgDeps `applyDiff` computedPkgDeps
              , computedCache         = mkRelative cache
              }
          else return $ idleState' ^. ideComputed

        -- Update state
        return ( IdeSessionIdle
               . (ideComputed    ^= computed)
               . (ideUpdatedEnv  ^= False)
               . (ideUpdatedCode ^= False)
               . (ideUpdatedArgs ^= False)
               $ idleState'
               , False
               )

      IdeSessionServerDied _ _ ->
        return (state, True)

      IdeSessionRunning _ _ ->
        Ex.throwIO (userError "Cannot update session in running mode")
      IdeSessionShutdown ->
        Ex.throwIO (userError "Session already shut down.")

    when shouldRestart $ do
      restartSession session Nothing
      updateSession session update callback
  where
    mkRelative :: ExplicitSharingCache -> ExplicitSharingCache
    mkRelative ExplicitSharingCache{..} =
      let aux :: BSS.ByteString -> BSS.ByteString
          aux = BSS.pack . makeRelative (ideSourcesDir ideStaticInfo) . BSS.unpack
      in ExplicitSharingCache {
        filePathCache = IntMap.map aux filePathCache
      , idPropCache   = idPropCache
      }

    handleExternal :: IdeIdleState -> ExternalException -> IO (IdeSessionState, Bool)
    handleExternal idleState e = return (IdeSessionServerDied e idleState, False)

-- | A session update that changes a source module by giving a new value for
-- the module source. This can be used to add a new module or update an
-- existing one. The ModuleName argument determines the directory
-- and file where the module is located within the project. The actual
-- internal compiler module name, such as the one given by the
-- @getLoadedModules@ query, comes from within @module ... end@.
-- Usually the two names are equal, but they needn't be.
--
updateModule :: FilePath -> BSL.ByteString -> IdeSessionUpdate
updateModule m bs = IdeSessionUpdate $ \_ IdeStaticInfo{ideSourcesDir} -> do
  let internal = internalFile ideSourcesDir m
  old <- get (ideManagedFiles .> managedSource .> lookup' m)
  -- We always overwrite the file, and then later set the timestamp back
  -- to what it was if it turns out the hash was the same. If we compute
  -- the hash first, we would force the entire lazy bytestring into memory
  newHash <- liftIO $ writeFileAtomic internal bs
  case old of
    Just (oldHash, oldTS) | oldHash == newHash ->
      liftIO $ setFileTimes internal oldTS oldTS
    _ -> do
      newTS <- nextLogicalTimestamp
      liftIO $ setFileTimes internal newTS newTS
      set (ideManagedFiles .> managedSource .> lookup' m) (Just (newHash, newTS))
      set ideUpdatedCode True

-- | Get the next available logical timestamp
nextLogicalTimestamp :: MonadState IdeIdleState m => m LogicalTimestamp
nextLogicalTimestamp = do
  newTS <- get ideLogicalTimestamp
  modify ideLogicalTimestamp (+ 1)
  return newTS

-- | Like 'updateModule' except that instead of passing the module source by
-- value, it's given by reference to an existing file, which will be copied.
--
updateModuleFromFile :: FilePath -> IdeSessionUpdate
updateModuleFromFile p = IdeSessionUpdate $ \callback staticInfo -> do
  -- We just call 'updateModule' because we need to read the file anyway
  -- to compute the hash.
  bs <- liftIO $ BSL.readFile p
  runSessionUpdate (updateModule p bs) callback staticInfo

-- | A session update that deletes an existing module.
--
updateModuleDelete :: FilePath -> IdeSessionUpdate
updateModuleDelete m = IdeSessionUpdate $ \_ IdeStaticInfo{ideSourcesDir} -> do
  liftIO $ Dir.removeFile (internalFile ideSourcesDir m)
  set (ideManagedFiles .> managedSource .> lookup' m) Nothing
  set ideUpdatedCode True

-- | Update dynamic compiler flags, including pragmas and packages to use.
-- Warning: only dynamic flags can be set here.
-- Static flags need to be set at server startup.
updateGhcOptions :: (Maybe [String]) -> IdeSessionUpdate
updateGhcOptions opts = IdeSessionUpdate $ \_ _ -> do
  set ideNewOpts opts
  set ideUpdatedCode True

-- | Enable or disable code generation in addition
-- to type-checking. Required by 'runStmt'.
updateCodeGeneration :: Bool -> IdeSessionUpdate
updateCodeGeneration b = IdeSessionUpdate $ \_ _ -> do
  set ideGenerateCode b
  set ideUpdatedCode True

-- | A session update that changes a data file by giving a new value for the
-- file. This can be used to add a new file or update an existing one.
-- Since source files can include data files (e.g., via TH), we set
-- @ideUpdatedCode@ to force recompilation (see #94).
--
updateDataFile :: FilePath -> BSL.ByteString -> IdeSessionUpdate
updateDataFile n bs = IdeSessionUpdate $ \_ IdeStaticInfo{ideDataDir} -> do
  liftIO $ writeFileAtomic (ideDataDir </> n) bs
  modify (ideManagedFiles .> managedData) (n :)
  set ideUpdatedCode True

-- | Like 'updateDataFile' except that instead of passing the file content by
-- value, it's given by reference to an existing file (the second argument),
-- which will be copied.
--
updateDataFileFromFile :: FilePath -> FilePath -> IdeSessionUpdate
updateDataFileFromFile n p = IdeSessionUpdate
                             $ \_ IdeStaticInfo{ideDataDir} -> do
  let targetPath = ideDataDir </> n
      targetDir  = takeDirectory targetPath
  liftIO $ Dir.createDirectoryIfMissing True targetDir
  liftIO $ Dir.copyFile p targetPath
  modify (ideManagedFiles .> managedData) (n :)
  set ideUpdatedCode True

-- | A session update that deletes an existing data file.
--
updateDataFileDelete :: FilePath -> IdeSessionUpdate
updateDataFileDelete n = IdeSessionUpdate $ \_ IdeStaticInfo{ideDataDir} -> do
  liftIO $ Dir.removeFile (ideDataDir </> n)
  modify (ideManagedFiles .> managedData) $ delete n
  set ideUpdatedCode True

-- | Set an environment variable
--
-- Use @updateEnv var Nothing@ to unset @var@.
updateEnv :: String -> Maybe String -> IdeSessionUpdate
updateEnv var val = IdeSessionUpdate $ \_ _ -> do
  set (ideEnv .> lookup' var) (Just val)
  set ideUpdatedEnv True

-- | Set command line arguments for snippets
-- (i.e., the expected value of `getArgs`)
updateArgs :: [String] -> IdeSessionUpdate
updateArgs args = IdeSessionUpdate $ \_ _ -> do
  set ideArgs args
  set ideUpdatedArgs True

-- | Set buffering mode for snippets' stdout
updateStdoutBufferMode :: RunBufferMode -> IdeSessionUpdate
updateStdoutBufferMode bufferMode = IdeSessionUpdate $ \_ _ ->
  set ideStdoutBufferMode bufferMode

-- | Set buffering mode for snippets' stderr
updateStderrBufferMode :: RunBufferMode -> IdeSessionUpdate
updateStderrBufferMode bufferMode = IdeSessionUpdate $ \_ _ ->
  set ideStderrBufferMode bufferMode

-- | Run a given function in a given module (the name of the module
-- is the one between @module ... end@, which may differ from the file name).
-- The function resembles a query, but it's not instantaneous
-- and the running code can be interrupted or interacted with.
--
-- 'runStmt' will throw an exception if the code has not been compiled yet,
-- or when the server is in a dead state (i.e., when ghc has crashed). In the
-- latter case 'getSourceErrors' will report the ghc exception; it is the
-- responsibility of the client code to check for this.
runStmt :: IdeSession -> String -> String -> IO RunActions
runStmt IdeSession{ideState} m fun = do
  modifyMVar ideState $ \state -> case state of
    IdeSessionIdle idleState ->
     case (toLazyMaybe (idleState ^. ideComputed), idleState ^. ideGenerateCode) of
       (Just comp, True) ->
          -- ideManagedFiles is irrelevant, because only the module name
          -- inside 'module .. where' counts.
          if Text.pack m `List.elem` computedLoadedModules comp
          then do
            runActions <- rpcRun (idleState ^. ideGhcServer)
                                 m fun
                                 (idleState ^. ideStdoutBufferMode)
                                 (idleState ^. ideStderrBufferMode)
            registerTerminationCallback runActions restoreToIdle
            return (IdeSessionRunning runActions idleState, runActions)
          else fail $ "Module " ++ show m
                      ++ " not successfully loaded, when trying to run code."
       _ ->
        -- This 'fail' invocation is, in part, a workaround for
        -- http://hackage.haskell.org/trac/ghc/ticket/7539
        -- which would otherwise lead to a hard GHC crash,
        -- instead of providing a sensible error message
        -- that we could show to the user.
        fail "Cannot run before the code is generated."
    IdeSessionRunning _ _ ->
      fail "Cannot run code concurrently"
    IdeSessionShutdown ->
      fail "Session already shut down."
    IdeSessionServerDied _ _ ->
      fail "Server died (reset or call updateSession)"
  where
    restoreToIdle :: RunResult -> IO ()
    restoreToIdle _ = modifyMVar_ ideState $ \state -> case state of
      IdeSessionIdle _ ->
        Ex.throwIO (userError "The impossible happened!")
      IdeSessionRunning _ idleState -> do
        return $ IdeSessionIdle idleState
      IdeSessionShutdown ->
        return state
      IdeSessionServerDied _ _ ->
        return state

-- | Build an exe from sources added previously via the ide-backend
-- updateModule* mechanism. The modules that contains the @main@ code are
-- indicated by the arguments to @buildExe@. The function can be called
-- multiple times with different arguments.
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
-- subdirectory of @getDistDir@, in subdirectories corresponding to the given
-- module names. The build directory does not overlap with any of the other
-- used directories and its path.
--
-- Note: currently it requires @configGenerateModInfo@ to be set (see #86).
buildExe :: [(ModuleName, FilePath)] -> IdeSessionUpdate
buildExe ms = IdeSessionUpdate $ \callback IdeStaticInfo{..} -> do
    mcomputed <- get ideComputed
    ghcNewOpts <- get ideNewOpts
    let SessionConfig{ configDynLink
                     , configPackageDBStack
                     , configGenerateModInfo
                     , configStaticOpts
                     , configExtraPathDirs } = ideConfig
        -- Note that these do not contain the @packageDbArgs@ options.
        ghcOpts = fromMaybe configStaticOpts ghcNewOpts
    when (not configGenerateModInfo) $
      -- TODO: replace the check with an inspection of state component (#87)
      fail "Features using cabal API require configGenerateModInfo, currently (#86)."
    exitCode <- lift $ Ex.bracket
      Dir.getCurrentDirectory
      Dir.setCurrentDirectory
      (const $ do Dir.setCurrentDirectory ideDataDir
                  buildExecutable ideSourcesDir ideDistDir configExtraPathDirs
                                  ghcOpts configDynLink configPackageDBStack
                                  mcomputed callback ms)
    set ideBuildExeStatus (Just exitCode)

-- | Build haddock documentation from sources added previously via
-- the ide-backend updateModule* mechanism. Similarly to 'buildExe',
-- it needs the project modules to be already loaded within the session
-- and the generated docs can be found in the @doc@ subdirectory
-- of @getDistDir@.
--
-- Note: currently it requires @configGenerateModInfo@ to be set (see #86).
buildDoc :: IdeSessionUpdate
buildDoc = IdeSessionUpdate $ \callback IdeStaticInfo{..} -> do
    mcomputed <- get ideComputed
    ghcNewOpts <- get ideNewOpts
    let SessionConfig{ configDynLink
                     , configPackageDBStack
                     , configGenerateModInfo
                     , configStaticOpts
                     , configExtraPathDirs } = ideConfig
        ghcOpts = fromMaybe configStaticOpts ghcNewOpts
    when (not configGenerateModInfo) $
      -- TODO: replace the check with an inspection of state component (#87)
      fail "Features using cabal API require configGenerateModInfo, currently (#86)."
    exitCode <- lift $ Ex.bracket
      Dir.getCurrentDirectory
      Dir.setCurrentDirectory
      (const $ do Dir.setCurrentDirectory ideDataDir
                  buildHaddock ideSourcesDir ideDistDir configExtraPathDirs
                               ghcOpts configDynLink configPackageDBStack
                               mcomputed callback)
    set ideBuildDocStatus (Just exitCode)

-- | Build a file containing licenses of all used packages.
-- Similarly to 'buildExe', it needs the project modules to be already
-- loaded within the session and the concatenated licences can be found
-- in the @licenses.txt@ file of @getDistDir@.
--
-- Note: currently it requires @configGenerateModInfo@ to be set (see #86).
buildLicenses :: FilePath -> IdeSessionUpdate
buildLicenses cabalsDir = IdeSessionUpdate $ \callback IdeStaticInfo{..} -> do
    mcomputed <- get ideComputed
    let SessionConfig{ configExtraPathDirs
                     , configPackageDBStack
                     , configGenerateModInfo
                     , configLicenseExc } = ideConfig
    when (not configGenerateModInfo) $
      -- TODO: replace the check with an inspection of state component (#87)
      fail "Features using cabal API require configGenerateModInfo, currently (#86)."
    exitCode <-
      lift $ buildLicenseCatenation cabalsDir ideDistDir configExtraPathDirs
                                    configPackageDBStack
                                    configLicenseExc mcomputed callback
    set ideBuildLicensesStatus (Just exitCode)

{------------------------------------------------------------------------------
  Debugging
------------------------------------------------------------------------------}

-- | Force recompilation of all modules. For debugging only.
forceRecompile :: IdeSessionUpdate
forceRecompile = IdeSessionUpdate $ \_ IdeStaticInfo{ideSourcesDir} -> do
  sources  <- get (ideManagedFiles .> managedSource)
  sources' <- forM sources $ \(path, (digest, _oldTS)) -> do
    newTS <- nextLogicalTimestamp
    liftIO $ setFileTimes (internalFile ideSourcesDir path) newTS newTS
    return (path, (digest, newTS))
  set (ideManagedFiles .> managedSource) sources'
  set ideUpdatedCode True

-- | Crash the GHC server. For debugging only. If the specified delay is
-- @Nothing@, crash immediately; otherwise, set up a thread that throws
-- an exception to the main thread after the delay.
crashGhcServer :: IdeSession -> Maybe Int -> IO ()
crashGhcServer IdeSession{ideState} delay = do
  withMVar ideState $ \state ->
    case state of
      IdeSessionIdle idleState ->
        rpcCrash (idleState ^. ideGhcServer) delay
      _ ->
        Ex.throwIO (userError "Call to crashGhcServer while state not idle")
