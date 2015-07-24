{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
-- | Session queries
--
-- We have to be very careful in the types in this module. We should not be
-- using internal types (with explicit sharing or types such as StrictMap),
-- except as part of abstract XShared types.
module IdeSession.Query (
    -- * Types
    Query
  , ManagedFiles(..)
  , InvalidSessionStateQueries(..)
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
  , getArgs
  , getGhcServer
  , getGhcVersion
  , getManagedFiles
  , getBuildExeStatus
  , getBuildDocStatus
  , getBuildLicensesStatus
  , getBreakInfo
    -- * Queries that rely on computed state
  , getSourceErrors
  , getLoadedModules
  , getFileMap
  , getSpanInfo
  , getExpTypes
  , getImports
  , getAutocompletion
  , getPkgDeps
  , getUseSites
  , getDotCabal
    -- * Debugging (internal use only)
  , dumpIdInfo
  , dumpAutocompletion
  , dumpFileMap
  ) where

import Prelude hiding (mod, span)
import Control.Exception (Exception, throwIO)
import Control.Monad (forM_)
import Data.Accessor ((^.), (^:), getVal)
import Data.List (isInfixOf, sortBy)
import Data.Maybe (listToMaybe, maybeToList)
import Data.Proxy
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Version (Version)
import System.Exit (ExitCode)
import System.FilePath ((</>))
import qualified Data.ByteString.Char8 as BSSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text (pack, unpack)
import qualified System.FilePath.Find as Find

import IdeSession.Cabal
import IdeSession.Config
import IdeSession.GHC.API
import IdeSession.RPC.Client (ExternalException(..))
import IdeSession.State
import IdeSession.Strict.Container
import IdeSession.Types.Public
import IdeSession.Types.Translation
import IdeSession.Util.BlockingOps
import qualified IdeSession.Strict.IntMap as StrictIntMap
import qualified IdeSession.Strict.IntervalMap as StrictIntervalMap
import qualified IdeSession.Strict.List   as StrictList
import qualified IdeSession.Strict.Map    as StrictMap
import qualified IdeSession.Strict.Maybe  as StrictMaybe
import qualified IdeSession.Strict.Trie   as StrictTrie
import qualified IdeSession.Types.Private as Private

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
  deriving Show

{------------------------------------------------------------------------------
  Queries that rely on the static part of the state only
------------------------------------------------------------------------------}

-- | Recover the fixed config the session was initialized with.
getSessionConfig :: Query SessionConfig
getSessionConfig = staticQuery $ return . ideConfig

-- | Obtain the source files directory for this session.
getSourcesDir :: Query FilePath
getSourcesDir = staticQuery $ return . ideSourceDir

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
getSourceModule path = staticQuery $ BSL.readFile . (</> path) . ideSourceDir

-- | Read the current value of one of the data files.
getDataFile :: FilePath -> Query BSL.ByteString
getDataFile path = staticQuery $ BSL.readFile . (</> path) . ideDataDir

-- | Get the list of all data files currently available to the session:
-- both the files copied via an update and files created by user code.
getAllDataFiles :: Query [FilePath]
getAllDataFiles = staticQuery $ \ideStaticInfo ->
  Find.find Find.always
            (Find.fileType Find.==? Find.RegularFile)
            (ideDataDir ideStaticInfo)

getCabalMacros :: Query BSL.ByteString
getCabalMacros = staticQuery $ \IdeStaticInfo{ideDistDir} ->
  BSL.readFile $ cabalMacrosLocation ideDistDir

{------------------------------------------------------------------------------
  Queries that do not rely on computed state
------------------------------------------------------------------------------}

-- | Is code generation currently enabled?
getCodeGeneration :: Query Bool
getCodeGeneration = simpleQuery $ getVal ideGenerateCode

-- | Get all current environment overrides
getEnv :: Query [(String, Maybe String)]
getEnv = simpleQuery $ getVal ideEnv

-- | Get all current snippet args
getArgs :: Query [String]
getArgs = simpleQuery $ getVal ideArgs

-- | Get the RPC server used by the session.
getGhcServer :: Query GhcServer
getGhcServer = simpleQuery $ getVal ideGhcServer

-- | Which GHC version is `ide-backend-server` using?
getGhcVersion :: Query GhcVersion
getGhcVersion = simpleQuery $ getVal ideGhcVersion

-- | Get the collection of files submitted by the user and not deleted yet.
-- The module names are those supplied by the user as the first
-- arguments of the @updateSourceFile@ and @updateSourceFileFromFile@ calls,
-- as opposed to the compiler internal @module ... end@ module names.
-- Usually the two names are equal, but they needn't be.
getManagedFiles :: Query ManagedFiles
getManagedFiles = simpleQuery $ translate . getVal ideManagedFiles
  where
    translate :: ManagedFilesInternal -> ManagedFiles
    translate files = ManagedFiles {
        sourceFiles = map fst $ _managedSource files
      , dataFiles   = map fst $ _managedData   files
      }

-- | Get exit status of the last invocation of 'buildExe', if any.
getBuildExeStatus :: Query (Maybe ExitCode)
getBuildExeStatus = simpleQuery $ getVal ideBuildExeStatus

-- | Get exit status of the last invocation of 'buildDoc', if any.
getBuildDocStatus :: Query (Maybe ExitCode)
getBuildDocStatus = simpleQuery $ getVal ideBuildDocStatus

-- | Get exit status of the last invocation of 'buildLicenses', if any.
getBuildLicensesStatus :: Query (Maybe ExitCode)
getBuildLicensesStatus = simpleQuery $ getVal ideBuildLicensesStatus

-- | Get information about the last breakpoint that we hit
--
-- Returns Nothing if we are not currently stopped on a breakpoint.
getBreakInfo :: Query (Maybe BreakInfo)
getBreakInfo = simpleQuery $ toLazyMaybe . getVal ideBreakInfo

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
  toLazyList $ StrictList.map (removeExplicitSharing Proxy computedCache) computedErrors

-- | Get the list of correctly compiled modules, as reported by the compiler
getLoadedModules :: Query [ModuleName]
getLoadedModules = computedQuery $ \Computed{..} ->
  toLazyList $ computedLoadedModules

-- | Get the mapping from filenames to modules (as computed by GHC)
getFileMap :: Query (FilePath -> Maybe ModuleId)
getFileMap = computedQuery $ \Computed{..} path ->
  fmap (removeExplicitSharing Proxy computedCache) $
    StrictMap.lookup path computedFileMap

-- | Get information about an identifier at a specific location
getSpanInfo :: Query (ModuleName -> SourceSpan -> [(SourceSpan, SpanInfo)])
getSpanInfo = computedQuery $ \computed@Computed{..} mod span ->
  let aux (a, b) = ( removeExplicitSharing Proxy computedCache a
                   , removeExplicitSharing Proxy computedCache b
                   )
  in map aux . maybeToList $ internalGetSpanInfo computed mod span

internalGetSpanInfo :: Computed -> ModuleName -> SourceSpan
                    -> Maybe (Private.SourceSpan, Private.SpanInfo)
internalGetSpanInfo Computed{..} mod span = case (mSpan, mIdMap) of
    (Just span', Just (Private.IdMap idMap)) ->
      let doms = Private.dominators span' idMap
      in listToMaybe (prioritize doms)
    _ -> Nothing
  where
    mSpan  = introduceExplicitSharing computedCache span
    mIdMap = StrictMap.lookup mod computedSpanInfo

    prioritize :: [(Private.SourceSpan, Private.SpanInfo)]
               -> [(Private.SourceSpan, Private.SpanInfo)]
    prioritize = sortBy $ \(_, a) (_, b) ->
      case (a, b) of
        (Private.SpanQQ _,       Private.SpanId _)       -> LT
        (Private.SpanInSplice _, Private.SpanId _)       -> LT
        (Private.SpanId _,       Private.SpanQQ _)       -> GT
        (Private.SpanId _,       Private.SpanInSplice _) -> GT
        (_, _) -> EQ

-- | Get information the type of a subexpressions and the subexpressions
-- around it
getExpTypes :: Query (ModuleName -> SourceSpan -> [(SourceSpan, Text)])
getExpTypes = computedQuery $ \Computed{..} mod span ->
  let mSpan   = introduceExplicitSharing computedCache span
      mExpMap = StrictMap.lookup mod computedExpTypes
  in case (mSpan, mExpMap) of
    (Just span', Just (Private.ExpMap expMap)) ->
      let aux (a, b) = ( removeExplicitSharing Proxy computedCache a
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
  fmap (toLazyList . StrictList.map (removeExplicitSharing Proxy computedCache)) $
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
            ns    = BSSC.split '.' name'
            n     = if null ns then BSSC.empty else last ns
        in filter (\idInfo -> name `isInfixOf` idInfoQN idInfo)
             $ concatMap (toLazyList . StrictList.map (removeExplicitSharing Proxy cache))
             . StrictTrie.elems
             . StrictTrie.submap n
             $ StrictMap.findWithDefault StrictTrie.empty modName mapOfTries

-- | (Transitive) package dependencies
--
-- These are only available for modules that got compiled successfully.
getPkgDeps :: Query (ModuleName -> Maybe [PackageId])
getPkgDeps = computedQuery $ \Computed{..} mod ->
  fmap (toLazyList . StrictList.map (removeExplicitSharing Proxy computedCache)) $
    StrictMap.lookup mod computedPkgDeps

-- | Use sites
--
-- Use sites are only reported in modules that get compiled successfully.
getUseSites :: Query (ModuleName -> SourceSpan -> [SourceSpan])
getUseSites = computedQuery $ \computed@Computed{..} mod span ->
  maybeListToList $ do
    (_, spanId)        <- internalGetSpanInfo computed mod span
    Private.IdInfo{..} <- case spanId of
                            Private.SpanId       idInfo -> return idInfo
                            Private.SpanQQ       _      -> Nothing
                            Private.SpanInSplice idInfo -> return idInfo
    return $ map (removeExplicitSharing Proxy computedCache)
           . concatMap (maybeListToList . StrictMap.lookup idProp)
           $ StrictMap.elems computedUseSites
  where
    maybeListToList :: Maybe [a] -> [a]
    maybeListToList (Just xs) = xs
    maybeListToList Nothing   = []

-- | Minimal .cabal file for the loaded modules seen as a library.
-- The argument specifies the name of the library.
--
-- License is set to @AllRightsReserved@.
-- All transitive package dependencies are included,
-- with package versions set to the currently used versions.
-- Only modules that get compiled successfully are included.
-- Source directory is the currently used session source directory.
-- Warning: all modules named @Main@ (even in subdirectories
-- or files with different names) are ignored so that they
-- don't get in the way when we build an executable using the library
-- and so that the behaviour is consistent with that of @buildExe@.
getDotCabal :: Query (String -> Version -> BSL.ByteString)
getDotCabal session = withComputedState session
                      $ \idleState computed@Computed{..} -> do
  let sourcesDir       = ideSourceDir $ ideStaticInfo session
      options          = idleState ^. ideGhcOpts
      relativeIncludes = idleState ^. ideRelativeIncludes
  buildDotCabal sourcesDir relativeIncludes options computed

{------------------------------------------------------------------------------
  Debugging
------------------------------------------------------------------------------}

-- | Print the id info maps to stdout (for debugging purposes only)
dumpIdInfo :: IdeSession -> IO ()
dumpIdInfo session = withComputedState session $ \_ Computed{..} ->
  forM_ (StrictMap.toList computedSpanInfo) $ \(mod, idMap) -> do
    putStrLn $ "*** " ++ Text.unpack mod ++ " ***"
    forM_ (StrictIntervalMap.toList (Private.idMapToMap idMap)) $ \(i, idInfo) -> do
      let idInfo' = removeExplicitSharing (Proxy :: Proxy SpanInfo) computedCache idInfo
          (StrictIntervalMap.Interval (fn, fromLine, fromCol) (_, toLine, toCol)) = i
          fn' = dereferenceFilePathPtr computedCache fn
      putStrLn $ show (fn', fromLine, fromCol, toLine, toCol)  ++ ": " ++ show idInfo'

-- | Print autocompletion to stdout (for debugging purposes only)
dumpAutocompletion :: IdeSession -> IO ()
dumpAutocompletion session = withComputedState session $ \_ Computed{..} ->
  forM_ (StrictMap.toList computedAutoMap) $ \(mod, autoMap) -> do
    putStrLn $ "*** " ++ Text.unpack mod ++ " ***"
    forM_ (StrictTrie.toList autoMap) $ \(key, idInfos) ->
      forM_ (toLazyList idInfos) $ \idInfo -> do
        let idInfo' :: IdInfo
            idInfo' = removeExplicitSharing Proxy computedCache idInfo
        putStrLn $ show key  ++ ": " ++ show idInfo'

-- | Print file mapping to stdout (for debugging purposes only)
dumpFileMap :: IdeSession -> IO ()
dumpFileMap session = withComputedState session $ \_ Computed{..} ->
  forM_ (StrictMap.toList computedFileMap) $ \(path, mod) -> do
    let mod' = removeExplicitSharing (Proxy :: Proxy ModuleId) computedCache mod
    putStrLn $ path ++ ": " ++ show mod'

{------------------------------------------------------------------------------
  Auxiliary
------------------------------------------------------------------------------}

-- | For the purposes of queries, we pretend that the 'IdeSessionServerDied'
-- state is a regular state, but we report the exception as a 'SourceError'
withIdleState :: IdeSession -> (IdeIdleState -> IO a) -> IO a
withIdleState IdeSession{ideState} f =
  $withStrictMVar ideState $ \st ->
    case st of
      IdeSessionIdle         idleState -> f idleState
      IdeSessionServerDied _ idleState -> f idleState
      IdeSessionShutdown               -> fail "Session is shut down."
  where
    -- TODO: Do we really want an empty computed here? This means that if the
    -- user does not check getSourceErrors they might get nil/empty rather
    -- than an error.
    emptyComputed :: Computed
    emptyComputed = Computed {
        computedErrors        = StrictList.nil
      , computedLoadedModules = StrictList.nil
      , computedFileMap       = StrictMap.empty
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
    Nothing       -> throwIO InvalidSessionStateQueries

data InvalidSessionStateQueries = InvalidSessionStateQueries
    deriving Typeable
instance Show InvalidSessionStateQueries where
    show InvalidSessionStateQueries = "This session state does not admit queries."
instance Exception InvalidSessionStateQueries

staticQuery :: (IdeStaticInfo -> IO a) -> Query a
staticQuery f = f . ideStaticInfo

simpleQuery :: (IdeIdleState -> a) -> Query a
simpleQuery f session = withIdleState session $ return . f

computedQuery :: (Computed -> a) -> Query a
computedQuery f session = withComputedState session $ const (return . f)
