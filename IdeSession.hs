{-# LANGUAGE CPP #-}
-- Copyright   : (c) JP Moresmau 2011,
--                   Well-Typed 2012
-- (JP Moresmau's buildwrapper package used as template for GHC API use)
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
  -- * compile phase: we apply the updates and run the compiler. This may be a
  --   relatively long running operation and we may want progress info.
  --
  -- * query phase: after compiling we can collect information like source errors
  --   or symbol maps.
  --
  -- Then the whole process can repeat.
  --
  -- To clarify these different phases we use different types:
  --
  -- * 'IdeSession' for the query mode. This is in a sense also the default
  --   mode.
  --
  -- * 'IdeSessionUpdate' is the type we use to accumulate updates.
  --
  -- * 'Progress' 'IdeSession' is the type for the compile mode.

  -- * Sessions
  IdeSession,

  -- ** Initialisation and shutdown
  -- | Sessions are stateful and must be initialised and shut down so that
  -- resources can be released.
  initSession,
  shutdownSession,
  SessionConfig(..),

  -- * Updates
  -- | Updates are done in batches: we collect together all of the updates we
  -- want to do and then do a single transition, applying all the updates,
  -- and end up in a new state.

  -- ** Declarative updates
  -- | So that we can batch the updates, all the updates are declarative.
  -- The 'IdeSessionUpdate' monoid is used to represent the updates, and the
  -- sub-sections below describe the various updates that are available.
  IdeSessionUpdate,

  -- ** Performing the update
  -- | Once we have accumulated a batch of updates we can perform them all
  -- giving us a new session state. Since performing a bunch of updates can
  -- involve compiling modules and can take some time, the update uses the
  -- 'Progress' type to represent the action in progress.
  updateSession,
  Progress,
  progressWaitCompletion,

  -- ** Modules
  updateModule,
  ModuleChange(..),

  -- ** Data files
  updateDataFile,
  DataFileChange(..),

  -- * Queries
  Query,

  -- ** Files
  -- | Simply getting the current state of the persistent files fits the
  -- queries pattern.
  getSourceModule,
  getDataFile,

  -- ** Source errors
  getSourceErrors,
  SourceError(..),

  -- ** Symbol definition maps
  getSymbolDefinitionMap,
  SymbolDefinitionMap,

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
  -- at any time.

  -- ** Morally pure queries
  -- | Morally, a compiler is a pure function from the current value of the
  -- various source files (and other bits of the environment) to object code
  -- and\/or other information about the modules (errors, types etc).
  --
  -- The intention is to reflect this purity property in this interface. The
  -- value of an 'IdeSession' represents the state of the files\/modules and
  -- the result of the pure compilation function. It should always be the case
  -- that we can throw away the session and recover it just from the persistent
  -- state in the files.
  --
  -- One example where this notion makes a difference is with warnings.
  -- Traditionally, compilers just return the warnings for the modules they
  -- compiled, skipping warnings for the modules they didn't need to recompile.
  -- But this doesn't match the pure function idea, because now the compilation
  -- result now depends on which steps we took to get there, rather than just
  -- on the current value of the files. So one of the things this wrapper can
  -- do is to restore the purity in these corner cases, (which otherwise the
  -- client of this API would probably have to do).

  -- ** Persistent and transitory state
  -- | The persistent state is obviously the files: source files and data
  -- files. Internally there is a great deal of transitory and cached state,
  -- either in memory or on disk (such as .hi files on disk or the equivalent
  -- in memory).
  --
  -- It should always be possible to drop all the transitory state and recover,
  -- just at the cost of some extra work.
  --
  -- This property is a useful correctness property for internal testing: the
  -- results of all the queries should be the same before and after blowing
  -- away all the transitory state and recovering.

) where

import GHC hiding (flags, ModuleName)
import qualified Config as GHC
#if __GLASGOW_HASKELL__ >= 706
import ErrUtils   ( MsgDoc )
#else
import ErrUtils   ( Message )
#endif
import Outputable ( PprStyle, showSDocForUser, qualName, qualModule )
import FastString ( unpackFS )
import StringBuffer ( stringToStringBuffer )

import System.Process
#if __GLASGOW_HASKELL__ >= 706
import Data.Time
#else
import System.Time
#endif
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IORef
import Data.Maybe (fromMaybe)
import Control.Monad
import Control.Applicative
import Control.Exception
import Control.Concurrent
import System.Directory
import System.FilePath (combine, takeExtension)

import Data.Monoid (Monoid(..))
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

type GhcState = [Located String]

-- This could be equally well implemented as a new mvar created per
-- each new session state, but this implementation has some minor advantages.
-- It gives more useful error messages and provides a counter of session
-- updates, which can be used, e.g.,  to rougly estimate how badly outdated
-- the info therein is, in order to decide whether to show it to the user,
-- or hand up and wait for a newer version.
data StateToken = StateToken (MVar Int) Int

initToken :: IO StateToken
initToken = do
  mv <- newMVar 0
  return $ StateToken mv 0

-- Invalidates previous sessions, returns a new token for the new session.
incrementToken :: StateToken -> IO StateToken
incrementToken token@(StateToken mv _) = do
  checkToken token
  let incrCheckToken current = do
        let newT = current + 1
        return (newT, StateToken mv newT)
  modifyMVar mv incrCheckToken

checkToken :: StateToken -> IO ()
checkToken (StateToken mv k) = do
  current <- readMVar mv
  when (k /= current) $
    error $ "Invalid session token " ++ show k ++ " /= " ++ show current

-- In this implementation, it's fully applicative, and so invalid sessions
-- can be queried at will. Note that there may be some working files
-- produced by GHC while obtaining these values. They are not captured here,
-- so queries are not allowed to read them.
type Computed = [SourceError]

-- | This is a state token for the current state of an IDE session. We can run
-- queries in the current state, and from this state we can perform a batch
-- of updates, ultimately leading to a new 'IdeSession'.
--
-- Note that these state tokens are not persistent, once we perform an update
-- and get a new 'IdeSession', the old one is no longer valid. (And if you do
-- accidentally use an old invalid one, then it's a dynamically checked error.)
--
data IdeSession = IdeSession
  { ideConfig   :: SessionConfig
  , ideGhcState :: GhcState
  , ideToken    :: StateToken
  , ideComputed :: (Maybe Computed)
  }

-- Mock-up filesystem to avoid trashing our computers when testing.
-- Files not present in the map should be read from the real filesystem.
type Filesystem = Map FilePath ByteString

-- | Configuration parameters for a session. These remain the same throughout
-- the whole session's lifetime.
--
data SessionConfig = SessionConfig {

       -- | The directory to use for managing source files.
       configSourcesDir :: FilePath,

       -- | The directory to use for session state, such as @.hi@ files.
       configWorkingDir :: FilePath,

       -- | The directory to use for data files that may be accessed by the
       -- running program. The running program will have this as its CWD.
       configDataDir :: FilePath,

       -- | The directory to use for purely temporary files.
       configTempDir :: FilePath,

       -- | TMP hack for testing.
       configFilesystem :: IORef Filesystem
     }

-- | Create a fresh session, using some initial configuration.
--
initSession :: SessionConfig -> IO IdeSession
initSession ideConfig = do
  let opts = []  -- GHC static flags; set them in sessionConfig?
  (ideGhcState, _) <- parseStaticFlags (map noLoc opts)
  let ideComputed = Nothing  -- can't query before the first Progress
  ideToken <- initToken
  return IdeSession{..}

-- | Close a session down, releasing the resources.
--
shutdownSession :: IdeSession -> IO ()
shutdownSession IdeSession{ideToken} = do
  -- Invalidate the current session.
  void $ incrementToken ideToken
  -- no other resources to free

-- | We use the 'IdeSessionUpdate' type to represent the accumulation of a
-- bunch of updates.
--
-- In particular it is an instance of 'Monoid', so multiple primitive updates
-- can be easily combined. Updates can override each other left to right.
--
data IdeSessionUpdate = IdeSessionUpdate (IdeSession -> IO ())

-- We assume, if updates are combined within the monoid, they can all
-- be applied in the context of the same session.
-- Otherwise, call 'updateSession' sequentially with the updates.
instance Monoid IdeSessionUpdate where
  mempty = IdeSessionUpdate $ \ _ -> return ()
  mappend (IdeSessionUpdate f) (IdeSessionUpdate g) =
    IdeSessionUpdate $ \ sess -> f sess >> g sess

-- | Given the current IDE session state, go ahead and
-- update the session, eventually resulting in a new session state,
-- with fully updated computed information (typing, etc.).
--
-- The update can be a long running operation, so it returns a 'Progress'
-- which can be used to monitor and wait on the operation.
-- While the progress is in operation, session state tokens
-- remain valid as usual. If the progress fails or is canceled,
-- all it's observable internal state changes are rolled back
-- and another progress can be initiated. The semantics of @updateFiles@
-- and @updateSession@ is unspecified while any progress runs.
--

updateSession :: IdeSession -> IdeSessionUpdate -> IO (Progress IdeSession)
updateSession sess@IdeSession{ ideConfig
                             , ideGhcState
                             , ideToken }
              (IdeSessionUpdate update) = do
  -- First, invalidating the current session ASAP, because the previous
  -- computed info will shortly no longer be in sync with the files.
  newToken <- incrementToken ideToken

  -- Then, updating files ASAP (using the already invalidated session).
  update sess

  -- Last, spawning a future.
  progressSpawn $ do
    fs <- readIORef (configFilesystem ideConfig)
    let sourcesDir = configSourcesDir ideConfig
        checkSingle file = do
          let mcontent = fmap BS.unpack $ Map.lookup file fs
              target = combine sourcesDir file
          checkModule target mcontent ideGhcState
    cnts <- getDirectoryContents sourcesDir
    let files = filter ((`elem` [".hs"]) . takeExtension) cnts
    allErrs <- mapM checkSingle files
    let ideComputed = Just $ concat allErrs  -- can query now
    return $ sess {ideToken = newToken, ideComputed}

-- | A future, a handle on an action that will produce some result.
--
-- Currently this is just a simple future, but there is the option to extend
-- it with actual progress info, and\/or with cancellation.
--
data Progress a = Progress (MVar a)

progressSpawn :: IO a -> IO (Progress a)
progressSpawn action = do
  mv <- newEmptyMVar
  let actionMv = do
        a <- action
        putMVar mv a
  void $ forkIO actionMv
  return $ Progress mv

-- | Block until the operation completes.
--
progressWaitCompletion :: Progress a -> IO a
progressWaitCompletion (Progress mv) = takeMVar mv

-- TODO:
-- 12:31 < dcoutts> mikolaj: steal the writeFileAtomic code from Cabal
-- 12:31 < dcoutts> from D.S.Utils
-- 12:32 < dcoutts> though check it's the version that uses ByteString
-- 12:32 < dcoutts> rather than String
--
-- | A session update that changes a source module. Modules can be added,
-- updated or deleted.
--
updateModule :: ModuleChange -> IdeSessionUpdate
updateModule mc = IdeSessionUpdate $ \ IdeSession{ideConfig} -> do
  fs <- readIORef (configFilesystem ideConfig)
  let newFs = case mc of
        ModulePut n bs -> Map.insert n bs fs
        ModuleDelete n -> Map.delete n fs
  writeIORef (configFilesystem ideConfig) newFs

data ModuleChange = ModulePut    ModuleName ByteString
                  | ModuleDelete ModuleName

type ModuleName = String  -- TODO: use GHC.Module.ModuleName ?

-- | A session update that changes a data file. Data files can be added,
-- updated or deleted.
--
updateDataFile :: DataFileChange -> IdeSessionUpdate
updateDataFile mc = IdeSessionUpdate $ \ IdeSession{ideConfig} -> do
  fs <- readIORef (configFilesystem ideConfig)
  let newFs = case mc of
        DataFilePut n bs -> Map.insert n bs fs
        DataFileDelete n -> Map.delete n fs
  writeIORef (configFilesystem ideConfig) newFs

data DataFileChange = DataFilePut    FilePath ByteString
                    | DataFileDelete FilePath

-- | The type of queries in a given session state.
--
-- Queries are in IO because they depend on the current state of the session
-- but they promise not to alter the session state (at least not in any visible
-- way, they might update caches etc).
--
type Query a = IdeSession -> IO a

-- | Read the current value of one of the source modules.
--
getSourceModule :: ModuleName -> Query ByteString
getSourceModule n IdeSession{ideConfig=SessionConfig{ configSourcesDir
                                                    , configFilesystem }} = do
  fs <- readIORef configFilesystem
  case Map.lookup n fs of
    Just bs -> return bs
    Nothing -> BS.readFile (combine configSourcesDir n)

-- | Read the current value of one of the data files.
--
getDataFile :: FilePath -> Query ByteString
getDataFile = getSourceModule  -- TODO

-- | Get any compilation errors or warnings in the current state of the
-- session, meaning errors that GHC reports for the current state of all the
-- source modules.
--
-- Note that in the initial implementation this will only return warnings from
-- the modules that changed in the last update, the intended semantics is that
-- morally it be a pure function of the current state of the files, and so it
-- would return all warnings (as if you did clean and rebuild each time).
--
getSourceErrors :: Query [SourceError]
getSourceErrors IdeSession{ideComputed} =
  let err = error $ "This session state does not admit queries."
  in return $ fromMaybe err ideComputed

-- | An error or warning in a source module.
--
-- Most errors are associated with a span of text, but some have only a
-- location point.
--
-- * This is currently a stub, but it will be a full concrete type.
--
data SourceError =
    SrcError ErrorKind FilePath (Int, Int) (Int, Int) String
  | OtherError String
  deriving Show

data ErrorKind = Error | Warning
  deriving Show

-- | A mapping from symbol uses to symbol definitions
--
-- * This is currently a stub, but it will be a full concrete type so that
-- it can be serialised etc.
--
data SymbolDefinitionMap

-- | Get a mapping from where symbols are used to where they are defined.
-- That is, given a symbol used at a particular location in a source module
-- the mapping tells us where that symbol is defined, either locally in a
-- source module or a top-level symbol imported from another package.
--
getSymbolDefinitionMap :: Query SymbolDefinitionMap
getSymbolDefinitionMap = undefined

-- TODO: Move all the rest elsewhere.

checkModule :: FilePath          -- ^ target file
            -> Maybe String      -- ^ optional content of the file
            -> [Located String]  -- ^ leftover ghc static options
            -> IO [SourceError]  -- ^ any errors and warnings
checkModule filename mfilecontent leftoverOpts = handleOtherErrors $ do

    libdir <- getGhcLibdir

    errsRef <- newIORef []

    mcontent <- case mfilecontent of
                  Nothing          -> return Nothing
                  Just filecontent -> do
#if __GLASGOW_HASKELL__ >= 704
                    let strbuf = stringToStringBuffer filecontent
#else
                    strbuf <- stringToStringBuffer filecontent
#endif
#if __GLASGOW_HASKELL__ >= 706
                    strtime <- getCurrentTime
#else
                    strtime <- getClockTime
#endif
                    return (Just (strbuf, strtime))

    runGhc (Just libdir) $
#if __GLASGOW_HASKELL__ >= 706
      handleSourceError printException $ do
#else
      handleSourceError printExceptionAndWarnings $ do
#endif

      flags0 <- getSessionDynFlags
      (flags, _, _) <- parseDynamicFlags flags0 leftoverOpts

      defaultCleanupHandler flags $ do
        setSessionDynFlags flags {
                             hscTarget  = HscNothing,
                             ghcLink    = NoLink,
                             ghcMode    = CompManager,
                             log_action = collectSrcError errsRef
                           }
        addTarget Target {
                    targetId           = TargetFile filename Nothing,
                    targetAllowObjCode = True,
                    targetContents     = mcontent
                  }
        load LoadAllTargets
        return ()

    reverse <$> readIORef errsRef
  where
    handleOtherErrors =
      handle $ \e -> return [OtherError (show (e :: SomeException))]

getGhcLibdir :: IO FilePath
getGhcLibdir = do
  let ghcbinary = "ghc-" ++ GHC.cProjectVersion
  out <- readProcess ghcbinary ["--print-libdir"] ""
  case lines out of
    [libdir] -> return libdir
    _        -> fail "cannot parse output of ghc --print-libdir"

#if __GLASGOW_HASKELL__ >= 706
collectSrcError :: IORef [SourceError]
                -> DynFlags
                -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()
collectSrcError errsRef flags severity srcspan style msg
  | Just errKind <- case severity of
                      SevWarning -> Just IdeSession.Warning
                      SevError   -> Just IdeSession.Error
                      SevFatal   -> Just IdeSession.Error
                      _          -> Nothing
  , Just (file, st, end) <- extractErrSpan srcspan
  = let msgstr = showSDocForUser flags (qualName style,qualModule style) msg
     in modifyIORef errsRef (SrcError errKind file st end msgstr:)

collectSrcError errsRef flags SevError _srcspan style msg
  = let msgstr = showSDocForUser flags (qualName style,qualModule style) msg
     in modifyIORef errsRef (OtherError msgstr:)

collectSrcError _ _ _ _ _ _ = return ()
#else
collectSrcError :: IORef [ErrorMessage]
                -> Severity -> SrcSpan -> PprStyle -> Message -> IO ()
collectSrcError errsRef severity srcspan style msg
  | Just errKind <- case severity of
                      SevWarning -> Just IdeSession.Warning
                      SevError   -> Just IdeSession.Error
                      SevFatal   -> Just IdeSession.Error
                      _          -> Nothing
  , Just (file, st, end) <- extractErrSpan srcspan
  = let msgstr = showSDocForUser (qualName style,qualModule style) msg
     in modifyIORef errsRef (SrcError errKind file st end msgstr:)

collectSrcError errsRef SevError _srcspan style msg
  = let msgstr = showSDocForUser (qualName style,qualModule style) msg
     in modifyIORef errsRef (OtherError msgstr:)

collectSrcError _ _ _ _ _ = return ()
#endif

extractErrSpan :: SrcSpan -> Maybe (FilePath, (Int, Int), (Int, Int))
#if __GLASGOW_HASKELL__ >= 704
extractErrSpan (RealSrcSpan srcspan) =
#else
extractErrSpan srcspan | isGoodSrcSpan srcspan =
#endif
  Just (unpackFS (srcSpanFile srcspan)
       ,(srcSpanStartLine srcspan, srcSpanStartCol srcspan)
       ,(srcSpanEndLine   srcspan, srcSpanEndCol   srcspan))
extractErrSpan _ = Nothing
