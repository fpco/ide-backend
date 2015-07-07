-- | Execution of session updates
--
-- Note that we do _NOT_ deal with session restarts here.
--
-- See comments for IdeSessionUpdate for a motivation of the split between the
-- IdeSessionUpdate type and the ExecuteSessionUpdate type.
{-# LANGUAGE GeneralizedNewtypeDeriving, ScopedTypeVariables, FlexibleInstances, TemplateHaskell, OverloadedStrings #-}
module IdeSession.Update.ExecuteSessionUpdate (runSessionUpdate) where

import Prelude hiding (mod, span)
import Control.Applicative (Applicative, (<$>))
import Control.Monad (when, void, forM, liftM, filterM)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..), ReaderT, runReaderT, asks)
import Control.Monad.State (MonadState(..))
import Data.Accessor (Accessor, (.>))
import Data.Digest.Pure.MD5 (MD5Digest)
import Data.Foldable (forM_)
import Data.Maybe (isJust, catMaybes, fromMaybe)
import Data.Monoid (Monoid(..))
import System.Exit (ExitCode(..))
import System.FilePath (makeRelative, (</>), takeExtension, replaceExtension, dropFileName)
import System.FilePath.Find (find, always, extension, (&&?), (||?), fileType, (==?), FileType (RegularFile))
import System.PosixCompat.Files (setFileTimes, getFileStatus, modificationTime)
import qualified Control.Exception             as Ex
import qualified Data.Accessor.Monad.MTL.State as Acc
import qualified Data.ByteString.Char8         as BSS
import qualified Data.ByteString.Lazy.Char8    as BSL
import qualified Data.Text                     as Text
import qualified System.Directory              as Dir

import IdeSession.Cabal
import IdeSession.Config
import IdeSession.ExeCabalClient (invokeExeCabal)
import IdeSession.GHC.API
import IdeSession.RPC.API (ExternalException)
import IdeSession.State
import IdeSession.Strict.Container
import IdeSession.Strict.IORef (StrictIORef)
import IdeSession.Types.Private hiding (RunResult(..))
import IdeSession.Types.Progress
import IdeSession.Types.Public (RunBufferMode(..), Targets(..), UpdateStatus(..))
import IdeSession.Update.IdeSessionUpdate
import IdeSession.Util
import IdeSession.Util.Logger
import qualified IdeSession.GHC.Client    as GHC
import qualified IdeSession.Strict.IORef  as IORef
import qualified IdeSession.Strict.IntMap as IntMap
import qualified IdeSession.Strict.List   as List
import qualified IdeSession.Strict.Map    as Map
import qualified IdeSession.Strict.Maybe  as Maybe
import qualified IdeSession.Strict.Trie   as Trie

{-------------------------------------------------------------------------------
  We execute session updates in a monad in which we have Reader access to
  the IdeStaticInfo and the progress back, and State access to the IdeIdleState.

  We do _not_ deal with different IDE states here; that is the responsibility
  of updateSession itself.
-------------------------------------------------------------------------------}

data IdeSessionUpdateEnv = IdeSessionUpdateEnv {
    ideUpdateStaticInfo :: IdeStaticInfo
  , ideUpdateStatus :: UpdateStatus -> IO ()
    -- For the StateT instance
  , ideUpdateStateRef :: StrictIORef IdeIdleState
    -- For liftIO
  , ideUpdateExceptionRef :: StrictIORef (Maybe ExternalException)
  , ideUpdateCallbacks :: IdeCallbacks
  }

newtype ExecuteSessionUpdate a = ExecuteSessionUpdate {
    unwrapUpdate :: ReaderT IdeSessionUpdateEnv IO a
  }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadReader IdeSessionUpdateEnv
           , MonadIO
           )

-- We define MonadState using an IORef rather than StateT so that if an exception
-- happens during the execution of a session update, writes to the state are not
-- lost
instance MonadState IdeIdleState ExecuteSessionUpdate where
  get   = ExecuteSessionUpdate $ do
            stRef <- ideUpdateStateRef <$> ask
            liftIO $ IORef.readIORef  stRef

  put s = ExecuteSessionUpdate $ do
            stRef <- ideUpdateStateRef <$> ask
            liftIO $ IORef.writeIORef stRef s

-- | Kind of like 'liftIO', but with a special treatment for external
-- exceptions. When the IO action throws an ExternalException, _or_ when a
-- previous IO exception threw an ExternalException, we return a dummy value
-- and record the exception separately. Thus, for an action of type
--
-- > foo :: IO ()
--
-- we will return () on an exception, as if the action returned successfully;
-- similarly, for an action of type
--
-- > bar :: IO [SourceError]
--
-- we will return the empty list on an exception. The rationale is that if
-- ExternalException occurs, we will eventually record the session as dead (in
-- the top-level updateSession function), but everything else in the execution
-- of the session update should still happen (#251).
--
-- NOTE: We deal _only_ with external exceptions here. Any other kind of
-- exception should be explicitly caught and dealt with by the IO action.
tryIO :: Dummy a => IO a -> ExecuteSessionUpdate a
tryIO act = ExecuteSessionUpdate $ do
  exRef <- ideUpdateExceptionRef <$> ask
  mPreviousException <- liftIO $ IORef.readIORef exRef
  case mPreviousException of
    Just _  ->
      return dummy
    Nothing -> do
      mNewException <- liftIO $ Ex.try $ act
      case mNewException of
        Left ex -> do liftIO $ IORef.writeIORef exRef (Just ex)
                      return dummy
        Right a -> return a

-- | For code that we _know_ is exception free we can just execute the action.
-- Of course, this needs manual verification that these cases are actually okay.
-- If the action _does_ throw an exception this may end the session update
-- prematurely.
exceptionFree :: IO a -> ExecuteSessionUpdate a
exceptionFree = ExecuteSessionUpdate . liftIO

runSessionUpdate :: Bool
                 -> IdeSessionUpdate
                 -> IdeStaticInfo
                 -> (UpdateStatus -> IO ())
                 -> IdeCallbacks
                 -> IdeIdleState
                 -> IO (IdeIdleState, Maybe ExternalException)
runSessionUpdate justRestarted update staticInfo callback ideCallbacks ideIdleState = do
    stRef <- IORef.newIORef ideIdleState
    exRef <- IORef.newIORef Nothing

    runReaderT (unwrapUpdate $ executeSessionUpdate justRestarted update')
               IdeSessionUpdateEnv {
                   ideUpdateStaticInfo   = staticInfo
                 , ideUpdateStatus       = liftIO . callback
                 , ideUpdateStateRef     = stRef
                 , ideUpdateExceptionRef = exRef
                 , ideUpdateCallbacks    = ideCallbacks
                 }

    ideIdleState' <- IORef.readIORef stRef
    mException    <- IORef.readIORef exRef
    return (ideIdleState', mException)
  where
    update' = reflectSessionState ideIdleState update

-- | Execute a session update
--
-- Notes:
--
-- * We do NOT deal with updates there that require a session restart (updates
--   to relative includes, targets, certain ghc options).
-- * The assume the session update has already been passed through
--   'reflectSessionState' to reflect the state of the session.
-- * Due to the MonadState instance, writes to the state will be preserved
--   even if an exception occurs during the execution of the session update.
--   Such exceptions should only happen when we contact the server (do RPC
--   calls); it is therefore important we make sure to update our local state
--   before updating the rmote state, so that _if_ a remote exception occurs,
--   we can reset the state on the next call to updateSession.
executeSessionUpdate :: Bool -> IdeSessionUpdate -> ExecuteSessionUpdate ()
executeSessionUpdate justRestarted IdeSessionUpdate{..} = do
    executeUpdateBufferModes ideUpdateStdoutMode ideUpdateStderrMode
    executeUpdateEnv         ideUpdateEnv
    executeUpdateArgs        ideUpdateArgs

    filesChanged   <- executeUpdateFiles   ideUpdateFileCmds
    enabledCodeGen <- executeUpdateCodeGen ideUpdateCodeGen
    optionWarnings <- executeUpdateGhcOpts ideUpdateGhcOpts

    -- Unrecord object files whose C files have been deleted (#241)
    removeObsoleteObjectFiles

    let ghcOptionsChanged :: Bool
        ghcOptionsChanged = isJust optionWarnings

    -- Recompile C files. Notes:
    --
    -- - Unless ghc options have changed, we recompile C files only when they
    --   have been modified (by comparing the timestamp on the C file to the
    --   timestamp on the correponding object file).
    -- - We do this _after_ setting ghc because because some ghc options are
    --   passed to the C compiler (#218)
    -- - Similarly, when the ghc options have changed, we conversatively
    --   recompile (and, importantly, relink -- #218) _all_ C files.
    (numActions, cErrors) <- updateObjectFiles ghcOptionsChanged
    let cFilesChanged :: Bool
        cFilesChanged = numActions > 0

    IdeStaticInfo{ideConfig} <- asks ideUpdateStaticInfo
    let hasLocalWorkingDir = isJust (configLocalWorkingDir ideConfig)

    let needsRecompile =
             -- We recompile both when source code and when data files change
             -- as we don't know which data files are included at compile time
             filesChanged
          || -- We need to (re)compile if codegen was just enabled
             enabledCodeGen
          || -- Changing ghc options might require a recompile
             ghcOptionsChanged
             -- If any C files changed we may have to recompile Haskell files
          || cFilesChanged
             -- If we just restarted we have to recompile even if the files
             -- didn't change
          || justRestarted
             -- Recompile when we move to having a local working directory.
          || hasLocalWorkingDir

    logFunc <- asks (ideCallbacksLogFunc . ideUpdateCallbacks)
    $logDebug $ if needsRecompile
      then "Recompile required, starting..."
      else "Recompile not required, so skipping"

    when needsRecompile $ local (incrementNumSteps numActions) $ do
      GhcCompileResult{..} <- rpcCompile
      oldComputed <- Acc.get ideComputed
      srcDir      <- asks $ ideSourceDir . ideUpdateStaticInfo

      let applyDiff :: Strict (Map ModuleName) (Diff v)
                    -> (Computed -> Strict (Map ModuleName) v)
                    -> Strict (Map ModuleName) v
          applyDiff diff f =  applyMapDiff diff $ Maybe.maybe Map.empty f oldComputed

      let diffSpan  = Map.map (fmap mkIdMap)  ghcCompileSpanInfo
          diffTypes = Map.map (fmap mkExpMap) ghcCompileExpTypes
          diffAuto  = Map.map (fmap (constructAuto ghcCompileCache)) ghcCompileAuto

      Acc.set ideComputed $ Maybe.just Computed {
          computedErrors        = force cErrors
                          List.++ ghcCompileErrors
                          List.++ force (fromMaybe [] optionWarnings)
        , computedLoadedModules = ghcCompileLoaded
        , computedFileMap       = mkFileMapRelative srcDir ghcCompileFileMap
        , computedImports       = ghcCompileImports  `applyDiff` computedImports
        , computedAutoMap       = diffAuto           `applyDiff` computedAutoMap
        , computedSpanInfo      = diffSpan           `applyDiff` computedSpanInfo
        , computedExpTypes      = diffTypes          `applyDiff` computedExpTypes
        , computedUseSites      = ghcCompileUseSites `applyDiff` computedUseSites
        , computedPkgDeps       = ghcCompilePkgDeps  `applyDiff` computedPkgDeps
        , computedCache         = mkCacheRelative srcDir ghcCompileCache
        }

    when ideUpdateDocs      $ executeBuildDoc
    forM_ ideUpdateExes     $ uncurry executeBuildExe
    forM_ ideUpdateLicenses $ executeBuildLicenses
  where
    incrementNumSteps :: Int -> IdeSessionUpdateEnv -> IdeSessionUpdateEnv
    incrementNumSteps count IdeSessionUpdateEnv{..} = IdeSessionUpdateEnv{
        ideUpdateStatus = \s -> ideUpdateStatus $ case s of
          UpdateStatusProgress p -> UpdateStatusProgress p {
              progressStep     = progressStep     p + count
            , progressNumSteps = progressNumSteps p + count
            }
          _ -> s
      , ..
      }

    mkCacheRelative :: FilePath -> ExplicitSharingCache -> ExplicitSharingCache
    mkCacheRelative srcDir ExplicitSharingCache{..} =
        ExplicitSharingCache {
            filePathCache = IntMap.map aux filePathCache
          , idPropCache   = idPropCache
          }
      where
        aux :: BSS.ByteString -> BSS.ByteString
        aux = BSS.pack . makeRelative srcDir . BSS.unpack

    mkFileMapRelative :: FilePath -> Strict (Map FilePath) ModuleId -> Strict (Map FilePath) ModuleId
    mkFileMapRelative srcDir = Map.mapKeys (makeRelative srcDir)

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

-- | Remove context sensitivty from session updates
--
-- Some updates (such as ideUpdateDeleteFiles) are context sensitive: their
-- meaning depends on the state of the session when the update is performed.
-- For these updates we "reflect" the current state of the session once, just
-- before we execute the update.
reflectSessionState :: IdeIdleState -> IdeSessionUpdate -> IdeSessionUpdate
reflectSessionState IdeIdleState{..} update = mconcat [
      when' (ideUpdateDeleteFiles update) $ mconcat $
           [updateSourceFileDelete (fst m) | m <- _managedSource]
        ++ [updateDataFileDelete   (fst m) | m <- _managedData]
    , update {
          ideUpdateDeleteFiles = False
        }
    ]
  where
    when' :: forall m. Monoid m => Bool -> m -> m
    when' True  a = a
    when' False _ = mempty

    ManagedFilesInternal{..} = _ideManagedFiles

{-------------------------------------------------------------------------------
  Simple updates
-------------------------------------------------------------------------------}

-- | Execute file update commands
--
-- Returns whether any files actually changed.
--
-- We share the directory where the files are stored with the server, so we
-- don't need to explicitly send over any changed files
executeUpdateFiles :: [FileCmd] -> ExecuteSessionUpdate Bool
executeUpdateFiles fileCmds = or <$> forM fileCmds executeFileCmd

-- | Enable/disable code gen
--
-- Returns if code gen was enabled (and wasn't previously)
--
-- Enabling/disabling code generation has no effect on the server, as we pass
-- this flag on each call to rpcCompile
executeUpdateCodeGen :: Maybe Bool -> ExecuteSessionUpdate Bool
executeUpdateCodeGen = maybeSet ideGenerateCode

-- | Update buffer modes
--
-- Updating buffer modes is local only, because we pass the required buffer
-- mode on each and every call to runStmt, rather than setting a server side
-- flag
executeUpdateBufferModes :: Maybe RunBufferMode -> Maybe RunBufferMode -> ExecuteSessionUpdate ()
executeUpdateBufferModes stdoutMode stderrMode = do
  void $ maybeSet ideStdoutBufferMode stdoutMode
  void $ maybeSet ideStderrBufferMode stderrMode

-- | Update server environment
executeUpdateEnv :: Maybe [(String, Maybe String)] -> ExecuteSessionUpdate ()
executeUpdateEnv env = do
  changed <- maybeSet ideEnv env
  when changed rpcSetEnv

-- | Update snippet arguments
executeUpdateArgs :: Maybe [String] -> ExecuteSessionUpdate ()
executeUpdateArgs args = do
  changed <- maybeSet ideArgs args
  when changed rpcSetArgs

-- | Update ghc options
--
-- Returns Just a (possibly empty) set of a warnings if options were changed,
-- or Nothing otherwise.
--
-- We don't deal with setting relative includes here: this always requires a
-- session restart; similarly, we assume that if any of the changed options
-- require a session restart that this has already happened.
executeUpdateGhcOpts :: Maybe [String] -> ExecuteSessionUpdate (Maybe [SourceError])
executeUpdateGhcOpts opts = do
  changed <- maybeSet ideGhcOpts opts
  if changed then Just <$> rpcSetGhcOpts
             else return Nothing

{-------------------------------------------------------------------------------
  Recompile object files
-------------------------------------------------------------------------------}

-- | Recompile any C files that need recompiling and mark all Haskell modules
-- that require recompilation.
--
-- Returns the number of actions that were executed, so we can adjust Progress
-- messages returned by ghc.
updateObjectFiles :: Bool -> ExecuteSessionUpdate (Int, [SourceError])
updateObjectFiles ghcOptionsChanged = do
    -- We first figure out which files are updated so that we can number
    -- progress messages
    outdated <- outdatedObjectFiles ghcOptionsChanged

    if not (null outdated)
      then do
        -- We first unload all object files in case any symbols need to be
        -- re-resolved.
        rpcUnloadObjectFiles =<< Acc.get ideObjectFiles

        -- Recompile the C files and load the corresponding object files
        cErrors   <- recompileCFiles outdated
        objErrors <- rpcLoadObjectFiles

        -- Finally, mark Haskell files as updated
        --
        -- When C files change, the addresses of the symbols exported in the
        -- corresponding object files may change. To make sure that these
        -- changes are properly propagated, we unload and reload all object
        -- files (so that we reapply symbol resolution, necessary in case the
        -- object files refer to each other), and we mark all Haskell modules
        -- as updated so that we will recompile them.
        --
        -- NOTE: When using HscInterpreted/LinkInMemory C symbols get resolved
        -- during compilation, not during a separate linking step. To be
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
        markAsUpdated $ dependenciesOf outdated
        return (length outdated, cErrors ++ objErrors)
      else
        return (0, [])
  where
    -- We don't know what the dependencies of the C files are, so we just
    -- reload _all_ Haskell modules
    dependenciesOf :: [FilePath] -> FilePath -> Bool
    dependenciesOf _recompiled src = takeExtension src == ".hs"

removeObsoleteObjectFiles :: ExecuteSessionUpdate ()
removeObsoleteObjectFiles = do
    objectFiles <- Acc.get ideObjectFiles
    obsolete    <- filterM isObsolete objectFiles
    forM_ obsolete $ \(cFile, (objFile, _timestamp)) -> do
      exceptionFree $ Dir.removeFile objFile
      Acc.set (ideObjectFiles .> lookup' cFile) Nothing
    rpcUnloadObjectFiles obsolete
  where
    isObsolete :: (FilePath, (FilePath, LogicalTimestamp)) -> ExecuteSessionUpdate Bool
    isObsolete (cFile, _) = do
      cInfo <- Acc.get (ideManagedFiles .> managedSource .> lookup' cFile)
      return $ not (isJust cInfo)

recompileCFiles :: [FilePath] -> ExecuteSessionUpdate [SourceError]
recompileCFiles cFiles = do
  updateStatus <- asks ideUpdateStatus
  sessionDir <- asks $ ideSessionDir . ideUpdateStaticInfo
  ideStaticInfo <- asks ideUpdateStaticInfo

  let srcDir, objDir :: FilePath
      srcDir = ideSourceDir ideStaticInfo
      objDir = ideSessionObjDir sessionDir

  errorss <- forM (zip cFiles [1..]) $ \(relC, i) -> do
    let relObj = replaceExtension relC ".o"
        absC   = srcDir </> relC
        absObj = objDir </> relObj

    let msg = "Compiling " ++ relC
    exceptionFree $ updateStatus $ UpdateStatusProgress $ Progress {
        progressStep      = i
      , progressNumSteps  = length cFiles
      , progressParsedMsg = Just (Text.pack msg)
      , progressOrigMsg   = Just (Text.pack msg)
      }

    exceptionFree $ Dir.createDirectoryIfMissing True (dropFileName absObj)

    errors <- runGcc absC absObj objDir
    if null errors
      then do
        ts' <- updateFileTimes absObj
        Acc.set (ideObjectFiles .> lookup' relC) (Just (absObj, ts'))
      else do
        Acc.set (ideObjectFiles .> lookup' relC) Nothing

    return errors

  return $ concat errorss

-- | Figure out which C files need to be recompiled
outdatedObjectFiles :: Bool -> ExecuteSessionUpdate [FilePath]
outdatedObjectFiles ghcOptionsChanged = do
  IdeStaticInfo{..} <- asks ideUpdateStaticInfo
  managedFiles      <- Acc.get (ideManagedFiles .> managedSource)

  let cFiles :: [(FilePath, LogicalTimestamp)]
      cFiles = filter ((`elem` cExtensions) . takeExtension . fst)
             $ map (\(fp, (_, ts)) -> (fp, ts))
             $ managedFiles

  -- If ghc options have changed we consider _all_ C files to be outdated; see
  -- comments in executeSessionUpdate.
  if ghcOptionsChanged
    then return $ map fst cFiles
    else liftM catMaybes $ do
      forM cFiles $ \(c_fp, c_ts) -> do
        -- ideObjectFiles is indexed by the names of the corresponding C files
        mObjFile <- Acc.get (ideObjectFiles .> lookup' c_fp)
        return $ case mObjFile of
          -- No existing object file yet
          Nothing -> Just c_fp
          -- We _do_ have an existing object file, and it is older than
          -- the C file. We need to recompile
          Just (_, obj_ts) | obj_ts < c_ts -> Just c_fp
          -- Otherwise we don't have to do anything
          _ -> Nothing

-- | Call gcc via ghc, with the same parameters cabal uses.
runGcc :: FilePath -> FilePath -> FilePath -> ExecuteSessionUpdate [SourceError]
runGcc absC absObj pref = do
    ideStaticInfo@IdeStaticInfo{..} <- asks ideUpdateStaticInfo
    updateStatus                    <- asks ideUpdateStatus
    ideCallbacks                    <- asks ideUpdateCallbacks
    relIncl                         <- Acc.get ideRelativeIncludes
    -- Pass GHC options so that ghc can pass the relevant options to gcc
    ghcOpts <- Acc.get ideGhcOpts
    exceptionFree $ do
     let SessionConfig{..} = ideConfig
         stdoutLog   = ideDistDir </> "ide-backend-cc.stdout"
         stderrLog   = ideDistDir </> "ide-backend-cc.stderr"
         includeDirs = map (ideSourceDir ideStaticInfo </>) relIncl
         runCcArgs   = RunCcArgs{ rcPackageDBStack = configPackageDBStack
                                , rcExtraPathDirs  = configExtraPathDirs
                                , rcDistDir        = ideDistDir
                                , rcStdoutLog      = stdoutLog
                                , rcStderrLog      = stderrLog
                                , rcAbsC           = absC
                                , rcAbsObj         = absObj
                                , rcPref           = pref
                                , rcIncludeDirs    = includeDirs
                                , rcOptions        = ghcOpts
                                }
     -- (_exitCode, _stdout, _stderr)
     --   <- readProcessWithExitCode _gcc _args _stdin
     -- The real deal; we call gcc via ghc via cabal functions:
     exitCode <- invokeExeCabal ideStaticInfo ideCallbacks (ReqExeCc runCcArgs) updateStatus
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

-- | Force recompilation of the given modules
--
-- NOTE: Calling markAsUpdated _by itself_ is not sufficient to call a recompile
-- of these files, as executeSessionUpdate needs some additional information to
-- even ask ghc for a recompile at all (see 'needsRecompile'). Currently we use
-- 'markAsUpdated' after we (re)compile C files, which executeSessionUpdate is
-- told about separately.
--
-- TODO: Should we update data files here too?
markAsUpdated :: (FilePath -> Bool) -> ExecuteSessionUpdate ()
markAsUpdated shouldMark = do
  ideStaticInfo@IdeStaticInfo{..} <- asks ideUpdateStaticInfo
  sources  <- Acc.get (ideManagedFiles .> managedSource)
  sources' <- forM sources $ \(path, (digest, oldTS)) ->
    if shouldMark path
      then do newTS <- updateFileTimes (ideSourceDir ideStaticInfo)
              return (path, (digest, newTS))
      else return (path, (digest, oldTS))
  Acc.set (ideManagedFiles .> managedSource) sources'

{-------------------------------------------------------------------------------
  File commands
-------------------------------------------------------------------------------}

-- | Execute a file command
--
-- Returns 'True' if any files were changed.
--
-- TODO: We should verify each use of exceptionFree here.
executeFileCmd :: FileCmd -> ExecuteSessionUpdate Bool
executeFileCmd cmd = do
  ideStaticInfo@IdeStaticInfo{..} <- asks ideUpdateStaticInfo

  let remotePath :: FilePath
      remotePath = fileInfoRemoteDir info ideStaticInfo </> fileInfoRemoteFile info

  case configLocalWorkingDir ideConfig of
    Just _ -> fail "We can't use update functions with configLocalWorkingDir."
    Nothing -> case cmd of
                 FileWrite _ bs -> do
                   old <- Acc.get cachedInfo
                   -- We always overwrite the file, and then later set the timestamp back
                   -- to what it was if it turns out the hash was the same. If we compute
                   -- the hash first, we would force the entire lazy bytestring into memory
                   newHash <- exceptionFree $ writeFileAtomic remotePath bs
                   case old of
                     Just (oldHash, oldTS) | oldHash == newHash -> do
                       exceptionFree $ setFileTimes remotePath oldTS oldTS
                       return False
                     _ -> do
                       newTS <- updateFileTimes remotePath
                       Acc.set cachedInfo (Just (newHash, newTS))
                       return True
                 FileCopy _ localFile -> do
                   -- We just call 'FileWrite' because we need to read the file anyway to
                   -- compute the hash. Note that `localPath` is interpreted relative to the
                   -- current directory
                   bs <- exceptionFree $ BSL.readFile localFile
                   executeFileCmd (FileWrite info bs)
                 FileDelete _ -> do
                   exceptionFree $ ignoreDoesNotExist $ Dir.removeFile remotePath
                   Acc.set cachedInfo Nothing
                   -- TODO: We should really return True only if the file existed
                   return True

  where
    info :: FileInfo
    info = case cmd of FileWrite  i _ -> i
                       FileCopy   i _ -> i
                       FileDelete i   -> i

    cachedInfo :: Accessor IdeIdleState (Maybe (MD5Digest, LogicalTimestamp))
    cachedInfo = ideManagedFiles .> fileInfoAccessor info .> lookup' (fileInfoRemoteFile info)

{-------------------------------------------------------------------------------
  Executables, documentation, licenses
-------------------------------------------------------------------------------}

executeBuildExe :: [String] -> [(ModuleName, FilePath)] -> ExecuteSessionUpdate ()
executeBuildExe extraOpts ms = do
    ideStaticInfo@IdeStaticInfo{..} <- asks ideUpdateStaticInfo
    let SessionConfig{..} = ideConfig
    updateStatus      <- asks ideUpdateStatus
    ideCallbacks      <- asks ideUpdateCallbacks
    mcomputed         <- Acc.get ideComputed
    ghcOpts           <- Acc.get ideGhcOpts
    relativeIncludes  <- Acc.get ideRelativeIncludes
        -- Note that these do not contain the @packageDbArgs@ options.
    when (not configGenerateModInfo) $
      -- TODO: replace the check with an inspection of state component (#87)
      fail "Features using cabal API require configGenerateModInfo, currently (#86)."
    exceptionFree $ Dir.createDirectoryIfMissing False $ ideDistDir </> "build"
    let beStdoutLog = ideDistDir </> "build/ide-backend-exe.stdout"
        beStderrLog = ideDistDir </> "build/ide-backend-exe.stderr"
        errors = case toLazyMaybe mcomputed of
          Nothing ->
            error "This session state does not admit artifact generation."
          Just Computed{computedErrors} -> toLazyList computedErrors
    exitCode <-
      if any (== KindError) $ map errorKind errors then do
        exceptionFree $ do
          writeFile beStderrLog
            "Source or other errors encountered. Not attempting to build executables."
          return $ ExitFailure 1
      else do
        let ghcOpts' = "-rtsopts=some" : ghcOpts ++ extraOpts
        exceptionFree $ do
                (loadedMs, pkgs) <- buildDeps mcomputed
                libDeps <- externalDeps pkgs
                let beArgs =
                      BuildExeArgs{ bePackageDBStack   = configPackageDBStack
                                  , beExtraPathDirs    = configExtraPathDirs
                                  , beSourcesDir       = ideSourceDir ideStaticInfo
                                  , beDistDir          = ideDistDir
                                  , beRelativeIncludes = relativeIncludes
                                  , beGhcOpts          = ghcOpts'
                                  , beLibDeps          = libDeps
                                  , beLoadedMs         = loadedMs
                                  , beStdoutLog
                                  , beStderrLog
                                  }
                invokeExeCabal ideStaticInfo ideCallbacks (ReqExeBuild beArgs ms) updateStatus
    -- Solution 2. to #119: update timestamps of .o (and all other) files
    -- according to the session's artificial timestamp.
    newTS <- nextLogicalTimestamp
    exceptionFree $ do
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
    Acc.set ideBuildExeStatus (Just exitCode)

executeBuildDoc :: ExecuteSessionUpdate ()
executeBuildDoc = do
    ideStaticInfo@IdeStaticInfo{..} <- asks ideUpdateStaticInfo
    let SessionConfig{..} = ideConfig
    let srcDir = ideSourceDir ideStaticInfo

    updateStatus      <- asks ideUpdateStatus
    ideCallbacks      <- asks ideUpdateCallbacks
    mcomputed         <- Acc.get ideComputed
    ghcOpts           <- Acc.get ideGhcOpts
    relativeIncludes  <- Acc.get ideRelativeIncludes
    when (not configGenerateModInfo) $
      -- TODO: replace the check with an inspection of state component (#87)
      fail "Features using cabal API require configGenerateModInfo, currently (#86)."
    exceptionFree $ Dir.createDirectoryIfMissing False $ ideDistDir </> "doc"
    let beStdoutLog = ideDistDir </> "doc/ide-backend-doc.stdout"
        beStderrLog = ideDistDir </> "doc/ide-backend-doc.stderr"
        errors = case toLazyMaybe mcomputed of
          Nothing ->
            error "This session state does not admit artifact generation."
          Just Computed{computedErrors} -> toLazyList computedErrors
        isDummyError err =
          errorKind err == KindError
          && errorMsg err == Text.pack "GHC server died (dummy error)"
    exitCode <-
      -- If some modules contain source errors we might still want to be able
      -- to generate documentation of the remainder. However, we rely on ghc's
      -- dependency tracking to tell us _which_ modules we need to compile.
      -- If the ghc server died we don't get any dependency information, and
      -- would hence conclude that the module graph is empty and generate
      -- empty documentation, which is confusing.
      -- (Note that a similar situation will arise if the root of the module
      -- graph contains source errors, in which case we will also get an
      -- empty dependency graph. This is harder to take into account, however.)
      if any isDummyError errors then do
        exceptionFree $ do
          writeFile beStderrLog
            "GHC server died. Not attempting to build documentation."
          return $ ExitFailure 1
      else exceptionFree $ do
                  (loadedMs, pkgs) <- buildDeps mcomputed
                  libDeps <- externalDeps pkgs
                  let beArgs =
                        BuildExeArgs{ bePackageDBStack   = configPackageDBStack
                                    , beExtraPathDirs    = configExtraPathDirs
                                    , beSourcesDir       =
                                        makeRelative ideSessionDir srcDir
                                    , beDistDir          =
                                        makeRelative ideSessionDir ideDistDir
                                    , beRelativeIncludes = relativeIncludes
                                    , beGhcOpts          = ghcOpts
                                    , beLibDeps          = libDeps
                                    , beLoadedMs         = loadedMs
                                    , beStdoutLog
                                    , beStderrLog
                                    }
                  invokeExeCabal ideStaticInfo ideCallbacks (ReqExeDoc beArgs) updateStatus
    Acc.set ideBuildDocStatus (Just exitCode)

executeBuildLicenses :: FilePath -> ExecuteSessionUpdate ()
executeBuildLicenses cabalsDir = do
    ideStaticInfo@IdeStaticInfo{..} <- asks ideUpdateStaticInfo
    let SessionConfig{configGenerateModInfo} = ideConfig

    updateStatus  <- asks ideUpdateStatus
    ideCallbacks <- asks ideUpdateCallbacks
    mcomputed <- Acc.get ideComputed
    when (not configGenerateModInfo) $
      -- TODO: replace the check with an inspection of state component (#87)
      fail "Features using cabal API require configGenerateModInfo, currently (#86)."
    let liStdoutLog = ideDistDir </> "licenses.stdout"  -- progress
        liStderrLog = ideDistDir </> "licenses.stderr"  -- warnings and errors
        errors = case toLazyMaybe mcomputed of
          Nothing ->
            error "This session state does not admit artifact generation."
          Just Computed{computedErrors} -> toLazyList computedErrors
    exitCode <-
      if any (== KindError) $ map errorKind errors then do
        exceptionFree $ do
          writeFile liStderrLog
            "Source or other errors encountered. Not attempting to build licenses."
          return $ ExitFailure 1
      else exceptionFree $ do
        (_, pkgs) <- buildDeps mcomputed
        let liArgs =
              LicenseArgs{ liPackageDBStack = configPackageDBStack ideConfig
                         , liExtraPathDirs = configExtraPathDirs ideConfig
                         , liLicenseExc = configLicenseExc ideConfig
                         , liDistDir = ideDistDir
                         , liStdoutLog
                         , liStderrLog
                         , licenseFixed = configLicenseFixed ideConfig
                         , liCabalsDir = cabalsDir
                         , liPkgs = pkgs
                         }
        invokeExeCabal ideStaticInfo ideCallbacks (ReqExeLic liArgs) updateStatus
    Acc.set ideBuildLicensesStatus (Just exitCode)

{-------------------------------------------------------------------------------
  Auxiliary (ide-backend specific)
-------------------------------------------------------------------------------}

-- | Update the file times of the given file with the next logical timestamp
updateFileTimes :: FilePath -> ExecuteSessionUpdate LogicalTimestamp
updateFileTimes path = do
  ts <- nextLogicalTimestamp
  exceptionFree $ setFileTimes path ts ts
  return ts

-- | Get the next available logical timestamp
nextLogicalTimestamp :: ExecuteSessionUpdate LogicalTimestamp
nextLogicalTimestamp = do
  newTS <- Acc.get ideLogicalTimestamp
  Acc.modify ideLogicalTimestamp (+ 1)
  return newTS

{-------------------------------------------------------------------------------
  Convenience wrappers around the RPC calls
-------------------------------------------------------------------------------}

rpcCompile :: ExecuteSessionUpdate GhcCompileResult
rpcCompile = do
    IdeIdleState{..} <- get
    updateStatus     <- asks ideUpdateStatus
    sourceDir        <- asks $ ideSourceDir . ideUpdateStaticInfo

    -- We need to translate the targets to absolute paths
    let targets = case _ideTargets of
          TargetsInclude l -> TargetsInclude $ map (sourceDir </>) l
          TargetsExclude l -> TargetsExclude $ map (sourceDir </>) l

    tryIO $ GHC.rpcCompile _ideGhcServer _ideGenerateCode targets updateStatus

rpcSetEnv :: ExecuteSessionUpdate ()
rpcSetEnv = do
    IdeIdleState{..} <- get
    tryIO $ GHC.rpcSetEnv _ideGhcServer _ideEnv

rpcSetArgs :: ExecuteSessionUpdate ()
rpcSetArgs = do
    IdeIdleState{..} <- get
    tryIO $ GHC.rpcSetArgs _ideGhcServer _ideArgs

rpcSetGhcOpts :: ExecuteSessionUpdate [SourceError]
rpcSetGhcOpts = do
    IdeIdleState{..} <- get
    srcDir <- asks $ ideSourceDir . ideUpdateStaticInfo
    -- relative include path is part of the state rather than the
    -- config as of c0bf0042
    let relOpts = relInclToOpts srcDir _ideRelativeIncludes
    (leftover, warnings) <- tryIO $ GHC.rpcSetGhcOpts _ideGhcServer (_ideGhcOpts ++ relOpts)
    return
      [ SourceError {
            errorKind = KindWarning
          , errorSpan = TextSpan (Text.pack "No location information")
          , errorMsg  = Text.pack w
          }
      | w <- warnings ++ map unrecognized leftover
      ]
  where
    unrecognized :: String -> String
    unrecognized str = "Unrecognized option " ++ show str

-- | Unload all current object files
rpcUnloadObjectFiles :: [(FilePath, (FilePath, LogicalTimestamp))] -> ExecuteSessionUpdate ()
rpcUnloadObjectFiles objects = do
  IdeIdleState{..} <- get
  tryIO $ GHC.rpcUnload _ideGhcServer $ map (fst . snd) objects

-- | Reload all current object files
rpcLoadObjectFiles :: ExecuteSessionUpdate [SourceError]
rpcLoadObjectFiles = do
  IdeIdleState{..} <- get
  didLoad <- tryIO $ GHC.rpcLoad _ideGhcServer $ map (fst . snd) _ideObjectFiles
  case didLoad of
    Just err ->
      return [ SourceError {
          errorKind = KindError
        , errorSpan = TextSpan (Text.pack "No location information")
        , errorMsg  = Text.pack $ "Failure during object loading: " ++ err
        }]
    Nothing ->
      return []

{-------------------------------------------------------------------------------
  Dummy values (see tryIO)
-------------------------------------------------------------------------------}

class Dummy a where
  dummy :: a

instance Dummy () where
  dummy = ()
instance Dummy [a] where
  dummy = []
instance (Dummy a, Dummy b) => Dummy (a, b) where
  dummy = (dummy, dummy)
instance Dummy (Maybe a) where
  dummy = Nothing

instance Dummy (Strict [] a) where
  dummy = List.nil
instance Dummy (Strict (Map k) a) where
  dummy = Map.empty
instance Dummy (Strict IntMap a) where
  dummy = IntMap.empty

instance Dummy ExplicitSharingCache where
  dummy = ExplicitSharingCache {
      filePathCache = dummy
    , idPropCache   = dummy
    }

instance Dummy GhcCompileResult where
  dummy = GhcCompileResult {
      ghcCompileLoaded   = dummy
    , ghcCompileCache    = dummy
    , ghcCompileFileMap  = dummy
    , ghcCompileImports  = dummy
    , ghcCompileAuto     = dummy
    , ghcCompileSpanInfo = dummy
    , ghcCompilePkgDeps  = dummy
    , ghcCompileExpTypes = dummy
    , ghcCompileUseSites = dummy
    , ghcCompileErrors   = force [SourceError {
          errorKind = KindError
        , errorSpan = TextSpan (Text.pack "No location information")
        , errorMsg  = Text.pack "GHC server died (dummy error)"
        }]
    }

{-------------------------------------------------------------------------------
  Auxiliary (generic)
-------------------------------------------------------------------------------}

maybeSet :: (MonadState st m, Eq a) => Accessor st a -> Maybe a -> m Bool
maybeSet _   Nothing    = return False
maybeSet acc (Just new) = do
  old <- Acc.get acc
  if old /= new then Acc.set acc new >> return True
                else return False
