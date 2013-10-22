-- | Session queries
--
-- We have to be very careful in the types in this module. We should not be
-- using internal types (with explicit sharing or types such as StrictMap),
-- except as part of abstract XShared types.
module IdeSession.Query (
    -- * Types
    Query
  , ManagedFiles(..)
    -- * Queries that rely on the static part of the state only
  , getSessionConfig
  , getSourcesDir
  , getDataDir
  , getDistDir
  , getSourceModule
  , getDataFile
  , getAllDataFiles
  , getCabalMacros
    -- * Queries that do not rely on computed state
  , getCodeGeneration
  , getEnv
  , getGhcServer
  , getManagedFiles
  , getBuildExeStatus
  , getBuildDocStatus
  , getBuildLicensesStatus
    -- * Queries that rely on computed state
  , getSourceErrors
  , getLoadedModules
  , getSpanInfo
  , getExpTypes
  , getImports
  , getAutocompletion
  , getPkgDeps
  , getUseSites
    -- * Debugging (internal use only)
  , dumpIdInfo
  ) where

import Prelude hiding (mod, span)
import Data.Maybe (listToMaybe, maybeToList)
import Data.List (isInfixOf)
import Data.Accessor ((^.), (^:), getVal)
import qualified System.FilePath.Find as Find
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSSC
import System.Exit (ExitCode)
import System.FilePath ((</>))
import Control.Monad (forM_)
import Data.Text (Text)
import qualified Data.Text as Text (pack, unpack)

import IdeSession.Config
import IdeSession.State
import IdeSession.GHC.API (cabalMacrosLocation)
import IdeSession.Types.Translation
import IdeSession.Types.Public
import qualified IdeSession.Types.Private as Private
import IdeSession.GHC.Client (GhcServer)
import IdeSession.RPC.Client (ExternalException)
import IdeSession.Strict.Container
import qualified IdeSession.Strict.Map    as StrictMap
import qualified IdeSession.Strict.List   as StrictList
import qualified IdeSession.Strict.Trie   as StrictTrie
import qualified IdeSession.Strict.Maybe  as StrictMaybe
import qualified IdeSession.Strict.IntMap as StrictIntMap
import qualified IdeSession.Strict.IntervalMap as StrictIntervalMap
import IdeSession.Strict.MVar (withMVar)

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

-- | The type of queries in a given session state.
--
-- Queries are in IO because they depend on the current state of the session
-- but they promise not to alter the session state (at least not in any visible
-- way; they might update caches, etc.).
--
type Query a = IdeSession -> IO a

-- | The collection of source and data files submitted by the user.
data ManagedFiles = ManagedFiles
  { sourceFiles :: [FilePath]
  , dataFiles   :: [FilePath]
  }

{------------------------------------------------------------------------------
  Queries that rely on the static part of the state only
------------------------------------------------------------------------------}

-- | Recover the fixed config the session was initialized with.
getSessionConfig :: Query SessionConfig
getSessionConfig = staticQuery $ return . ideConfig

-- | Obtain the source files directory for this session.
getSourcesDir :: Query FilePath
getSourcesDir = staticQuery $ return . ideSourcesDir

-- | Obtain the data files directory for this session.
getDataDir :: Query FilePath
getDataDir = staticQuery $ return . ideDataDir

-- | Obtain the directory prefix for results of Cabal invocations.
-- Executables compiled in this session end up in a subdirectory @build@,
-- haddocks in @doc@, concatenated licenses in file @licenses@, etc.
getDistDir :: Query FilePath
getDistDir = staticQuery $ return . ideDistDir

-- | Read the current value of one of the source modules.
getSourceModule :: FilePath -> Query BSL.ByteString
getSourceModule path = staticQuery $ \IdeStaticInfo{ideSourcesDir} ->
  BSL.readFile $ internalFile ideSourcesDir path

-- | Read the current value of one of the data files.
getDataFile :: FilePath -> Query BSL.ByteString
getDataFile path = staticQuery $ \IdeStaticInfo{ideDataDir} ->
  BSL.readFile $ ideDataDir </> path

-- | Get the list of all data files currently available to the session:
-- both the files copied via an update and files created by user code.
getAllDataFiles :: Query [FilePath]
getAllDataFiles = staticQuery $ \IdeStaticInfo{ideDataDir} ->
  Find.find Find.always
            (Find.fileType Find.==? Find.RegularFile)
            ideDataDir

getCabalMacros :: Query BSL.ByteString
getCabalMacros = staticQuery $ \IdeStaticInfo{ideSourcesDir} ->
  BSL.readFile $ cabalMacrosLocation ideSourcesDir

{------------------------------------------------------------------------------
  Queries that do not rely on computed state
------------------------------------------------------------------------------}

-- | Is code generation currently enabled?
getCodeGeneration :: Query Bool
getCodeGeneration = simpleQuery $ getVal ideGenerateCode

-- | Get all current environment overrides
getEnv :: Query [(String, Maybe String)]
getEnv = simpleQuery $ getVal ideEnv

-- | Get the RPC server used by the session.
getGhcServer :: Query GhcServer
getGhcServer = simpleQuery $ getVal ideGhcServer

-- | Get the collection of files submitted by the user and not deleted yet.
-- The module names are those supplied by the user as the first
-- arguments of the @updateModule@ and @updateModuleFromFile@ calls,
-- as opposed to the compiler internal @module ... end@ module names.
-- Usually the two names are equal, but they needn't be.
getManagedFiles :: Query ManagedFiles
getManagedFiles = simpleQuery $ translate . getVal ideManagedFiles
  where
    translate :: ManagedFilesInternal -> ManagedFiles
    translate files = ManagedFiles {
        sourceFiles = map fst $ _managedSource files
      , dataFiles   = _managedData files
      }

-- Get exit status of the last invocation of 'buildExe', if any.
getBuildExeStatus :: Query (Maybe ExitCode)
getBuildExeStatus = simpleQuery $ getVal ideBuildExeStatus

-- Get exit status of the last invocation of 'buildDoc', if any.
getBuildDocStatus :: Query (Maybe ExitCode)
getBuildDocStatus = simpleQuery $ getVal ideBuildDocStatus

-- Get exit status of the last invocation of 'buildLicenses', if any.
getBuildLicensesStatus :: Query (Maybe ExitCode)
getBuildLicensesStatus = simpleQuery $ getVal ideBuildLicensesStatus

{------------------------------------------------------------------------------
  Queries that rely on computed state
------------------------------------------------------------------------------}

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
-- client and anyway there shouldn't be that many source errors that it really
-- makes a big difference.
getSourceErrors :: Query [SourceError]
getSourceErrors = computedQuery $ \Computed{..} ->
  toLazyList $ StrictList.map (removeExplicitSharing computedCache) computedErrors

-- | Get the list of correctly compiled modules, as reported by the compiler
getLoadedModules :: Query [ModuleName]
getLoadedModules = computedQuery $ \Computed{..} ->
  toLazyList $ computedLoadedModules

-- | Get information about an identifier at a specific location
getSpanInfo :: Query (ModuleName -> SourceSpan -> [(SourceSpan, SpanInfo)])
getSpanInfo = computedQuery $ \computed@Computed{..} mod span ->
  let aux (a, b) = ( removeExplicitSharing computedCache a
                 , removeExplicitSharing computedCache b
                 )
  in map aux . maybeToList $ internalGetSpanInfo computed mod span

internalGetSpanInfo :: Computed -> ModuleName -> SourceSpan
                    -> Maybe (Private.SourceSpan, Private.SpanInfo)
internalGetSpanInfo Computed{..} mod span = case (mSpan, mIdMap) of
    (Just span', Just (Private.IdMap idMap)) ->
      let doms = Private.dominators span' idMap
      in case filter isQQ doms of
           qq : _ -> Just qq
           _      -> listToMaybe doms
    _ -> Nothing
  where
    mSpan  = introduceExplicitSharing computedCache span
    mIdMap = StrictMap.lookup mod computedSpanInfo

    isQQ (_, Private.SpanQQ _) = True
    isQQ _                     = False

-- | Get information the type of a subexpressions and the subexpressions
-- around it
getExpTypes :: Query (ModuleName -> SourceSpan -> [(SourceSpan, Text)])
getExpTypes = computedQuery $ \Computed{..} mod span ->
  let mSpan   = introduceExplicitSharing computedCache span
      mExpMap = StrictMap.lookup mod computedExpTypes
  in case (mSpan, mExpMap) of
    (Just span', Just (Private.ExpMap expMap)) ->
      let aux (a, b) = ( removeExplicitSharing computedCache a
                       , b
                       )
          doms = map aux $ Private.dominators span' expMap
      in doms
    _ ->
      []

-- | Get import information
--
-- This information is available even for modules with parse/type errors
getImports :: Query (ModuleName -> Maybe [Import])
getImports = computedQuery $ \Computed{..} mod ->
  fmap (toLazyList . StrictList.map (removeExplicitSharing computedCache)) $
    StrictMap.lookup mod computedImports

-- | Autocompletion
--
-- Use 'idInfoQN' to translate these 'IdInfo's into qualified names, taking
-- into account the module imports.
getAutocompletion :: Query (ModuleName -> String -> [IdInfo])
getAutocompletion = computedQuery $ \Computed{..} ->
    autocomplete computedCache computedAutoMap
  where
    autocomplete :: Private.ExplicitSharingCache
                 -> Strict (Map ModuleName) (Strict Trie (Strict [] (XShared IdInfo)))
                 -> ModuleName -> String
                 -> [IdInfo]
    autocomplete cache mapOfTries modName name =
        let name' = BSSC.pack name
            n     = last (BSSC.split '.' name')
        in filter (\idInfo -> name `isInfixOf` idInfoQN idInfo)
             $ concatMap (toLazyList . StrictList.map (removeExplicitSharing cache))
             . StrictTrie.elems
             . StrictTrie.submap n
             $ mapOfTries StrictMap.! modName

-- | (Transitive) package dependencies
--
-- These are only available for modules that got compiled successfully.
getPkgDeps :: Query (ModuleName -> Maybe [PackageId])
getPkgDeps = computedQuery $ \Computed{..} mod ->
  fmap (toLazyList . StrictList.map (removeExplicitSharing computedCache)) $
    StrictMap.lookup mod computedPkgDeps

-- | Use sites
--
-- Use sites are only reported in modules that get compiled successfully.
getUseSites :: Query (ModuleName -> SourceSpan -> [SourceSpan])
getUseSites = computedQuery $ \computed@Computed{..} mod span ->
  maybeListToList $ do
    (_, spanId)        <- internalGetSpanInfo computed mod span
    Private.IdInfo{..} <- case spanId of
                            Private.SpanId idInfo -> return idInfo
                            Private.SpanQQ _      -> Nothing
    return $ map (removeExplicitSharing computedCache)
           . concatMap (maybeListToList . StrictMap.lookup idProp)
           $ StrictMap.elems computedUseSites
  where
    maybeListToList :: Maybe [a] -> [a]
    maybeListToList (Just xs) = xs
    maybeListToList Nothing   = []

{------------------------------------------------------------------------------
  Debugging
------------------------------------------------------------------------------}

-- | Print the id info maps to the stdout (for debugging purposes only)
dumpIdInfo :: IdeSession -> IO ()
dumpIdInfo session = withComputedState session $ \_ Computed{..} ->
  forM_ (StrictMap.toList computedSpanInfo) $ \(mod, idMap) -> do
    putStrLn $ "*** " ++ Text.unpack mod ++ " ***"
    forM_ (StrictIntervalMap.toList (Private.idMapToMap idMap)) $ \(i, idInfo) -> do
      let idInfo' = removeExplicitSharing computedCache idInfo
          (StrictIntervalMap.Interval (_, fromLine, fromCol) (_, toLine, toCol)) = i
      putStrLn $ show (fromLine, fromCol, toLine, toCol)  ++ ": " ++ show idInfo'

{------------------------------------------------------------------------------
  Auxiliary
------------------------------------------------------------------------------}

-- | For the purposes of queries, we pretend that the 'IdeSessionServerDied'
-- state is a regular state, but we report the exception as a 'SourceError'
withIdleState :: IdeSession -> (IdeIdleState -> IO a) -> IO a
withIdleState IdeSession{ideState} f =
  withMVar ideState $ \st ->
    case st of
      IdeSessionIdle         idleState -> f idleState
      IdeSessionRunning _    idleState -> f idleState
      IdeSessionServerDied e idleState -> f (reportExAsErr e idleState)
      IdeSessionShutdown               -> fail "Session already shut down."
  where
    reportExAsErr :: ExternalException -> IdeIdleState -> IdeIdleState
    reportExAsErr e = ideComputed ^:
      StrictMaybe.just . updateComputed e . StrictMaybe.fromMaybe emptyComputed

    updateComputed :: ExternalException -> Computed -> Computed
    updateComputed e c =
      let err = Private.SourceError {
              Private.errorKind = Private.KindError
            , Private.errorSpan = Private.TextSpan (Text.pack "<<server died>>")
            , Private.errorMsg  = Text.pack (show e)
            }
      in c { computedErrors = StrictList.singleton err }

    -- TODO: Do we really want an empty computed here? This means that if the
    -- user does not check getSourceErrors they might get nil/empty rather
    -- than an error.
    emptyComputed :: Computed
    emptyComputed = Computed {
        computedErrors        = StrictList.nil
      , computedLoadedModules = StrictList.nil
      , computedSpanInfo      = StrictMap.empty
      , computedExpTypes      = StrictMap.empty
      , computedUseSites      = StrictMap.empty
      , computedImports       = StrictMap.empty
      , computedAutoMap       = StrictMap.empty
      , computedPkgDeps       = StrictMap.empty
      , computedCache         = Private.ExplicitSharingCache {
            Private.filePathCache = StrictIntMap.empty
          , Private.idPropCache   = StrictIntMap.empty
          }
      }

withComputedState :: IdeSession -> (IdeIdleState -> Computed -> IO a) -> IO a
withComputedState session f = withIdleState session $ \idleState ->
  case toLazyMaybe (idleState ^. ideComputed) of
    Just computed -> f idleState computed
    Nothing       -> fail "This session state does not admit queries."

staticQuery :: (IdeStaticInfo -> IO a) -> Query a
staticQuery f = f . ideStaticInfo

simpleQuery :: (IdeIdleState -> a) -> Query a
simpleQuery f session = withIdleState session $ return . f

computedQuery :: (Computed -> a) -> Query a
computedQuery f session = withComputedState session $ const (return . f)
