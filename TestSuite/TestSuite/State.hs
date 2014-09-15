-- | Test suite global state
module TestSuite.State (
    -- * Top-level test groups
    TestSuiteState -- Opaque
  , TestSuiteConfig(..)
  , TestSuiteEnv(..)
  , TestSuiteServerConfig(..)
  , TestSuiteSessionSetup(..)
  , testSuite
  , testSuiteCommandLineOptions
    -- * Operations on the test suite state
  , withAvailableSession
  , withAvailableSession'
  , startNewSession
  , defaultSessionSetup
  , defaultServerConfig
  , withGhcOpts
  , withIncludes
  , withModInfo
  , withDBStack
  , dontReuse
  , skipTest
  , ifTestingExe
    -- * Constructing tests
  , stdTest
  , withOK
  , docTests
  , exeTests
    -- * Test suite global state
  , withCurrentDirectory
  , findExe
  , withInstalledPackage
  , packageCheck
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.DeepSeq (rnf)
import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.Typeable
import System.Directory
import System.Environment
import System.FilePath
import System.IO (hGetContents, hClose)
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import Test.HUnit (Assertion)
import Test.HUnit.Lang (HUnitFailure(..))
import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.Providers
import qualified Data.Map                         as Map
import qualified Distribution.Simple.Program.Find as OurCabal
import qualified Data.ByteString.Lazy             as L

import IdeSession

{-------------------------------------------------------------------------------
  Test suite top-level
-------------------------------------------------------------------------------}

data TestSuiteConfig = TestSuiteConfig {
    -- | Keep session temp files
    testSuiteConfigKeepTempFiles :: Bool

    -- | Start a new session for each test
  , testSuiteConfigNoSessionReuse :: Bool

    -- | No haddock documentation installed on the test system
  , testSuiteConfigNoHaddocks :: Bool

    -- | Skip buildExe tests
  , testSuiteConfigNoExe :: Bool

    -- | Package DB stack for GHC 7.4
  , testSuiteConfigPackageDb74 :: Maybe String

    -- | Package DB stack for GHC 7.8
  , testSuiteConfigPackageDb78 :: Maybe String

    -- | Extra paths for GHC 7.4
  , testSuiteConfigExtraPaths74 :: String

    -- | Extra paths for GHC 7.8
  , testSuiteConfigExtraPaths78 :: String

    -- | Should we test against GHC 7.4?
  , testSuiteConfigTest74 :: Bool

    -- | Should we test against GHC 7.8?
  , testSuiteConfigTest78 :: Bool
  }
  deriving (Eq, Show)

data TestSuiteState = TestSuiteState {
    -- | Previously created sessions
    --
    -- These sessions are free (no concurrent test is currently using them)
    testSuiteStateAvailableSessions :: MVar [(TestSuiteServerConfig, IdeSession)]
  }

data TestSuiteEnv = TestSuiteEnv {
    testSuiteEnvConfig     :: TestSuiteConfig
  , testSuiteEnvState      :: IO TestSuiteState
  , testSuiteEnvGhcVersion :: GhcVersion
  }

testSuite :: (TestSuiteEnv -> TestTree) -> TestTree
testSuite tests =
  parseOptions $ \testSuiteEnvConfig ->
    withResource initTestSuiteState cleanupTestSuiteState $ \testSuiteEnvState ->
      tests TestSuiteEnv {
          testSuiteEnvGhcVersion = error "testSuiteEnvGhcVersion not yet set"
        , ..
        }

{-------------------------------------------------------------------------------
  Stateful operations (on the test suite state)
-------------------------------------------------------------------------------}

-- | Run the given action with an available session (new or previously created)
withAvailableSession :: TestSuiteEnv -> (IdeSession -> IO a) -> IO a
withAvailableSession env = withAvailableSession' env id

data TestSuiteSessionSetup = TestSuiteSessionSetup {
    testSuiteSessionServer  :: TestSuiteServerConfig
  , testSuiteSessionGhcOpts :: [String]
  , testSuiteSessionReuse   :: Bool
  }

defaultSessionSetup :: TestSuiteEnv -> TestSuiteSessionSetup
defaultSessionSetup env = TestSuiteSessionSetup {
    testSuiteSessionServer  = defaultServerConfig env
  , testSuiteSessionGhcOpts = []
  , testSuiteSessionReuse   = True
  }

withGhcOpts :: [String] -> TestSuiteSessionSetup -> TestSuiteSessionSetup
withGhcOpts opts setup = setup {
    testSuiteSessionGhcOpts = opts
  }

withIncludes :: [FilePath] -> TestSuiteSessionSetup -> TestSuiteSessionSetup
withIncludes incls setup = setup {
    testSuiteSessionServer = (testSuiteSessionServer setup) {
        testSuiteServerRelativeIncludes = Just (incls ++ sessionInitRelativeIncludes defaultSessionInitParams)
      }
  }

withModInfo :: Bool -> TestSuiteSessionSetup -> TestSuiteSessionSetup
withModInfo modInfo setup = setup {
    testSuiteSessionServer = (testSuiteSessionServer setup) {
        testSuiteServerGenerateModInfo = Just modInfo
      }
  }

withDBStack :: PackageDBStack -> TestSuiteSessionSetup -> TestSuiteSessionSetup
withDBStack dbStack setup = setup {
    testSuiteSessionServer = (testSuiteSessionServer setup) {
        testSuiteServerPackageDBStack = Just dbStack
      }
  }

dontReuse :: TestSuiteSessionSetup -> TestSuiteSessionSetup
dontReuse setup = setup {
    testSuiteSessionReuse = False
  }

-- | More general version of 'withAvailableSession'
withAvailableSession' :: TestSuiteEnv -> (TestSuiteSessionSetup -> TestSuiteSessionSetup) -> (IdeSession -> IO a) -> IO a
withAvailableSession' env@TestSuiteEnv{..} sessionSetup act = do
    TestSuiteState{..} <- testSuiteEnvState

    -- Find an available session, if one exists
    msession <- extractMVar ((== testSuiteSessionServer) . fst)
                            testSuiteStateAvailableSessions

    -- If there is none, start a new one
    session <- case msession of
                 Just session -> return (snd session)
                 Nothing      -> startNewSession testSuiteSessionServer

    -- Setup session parameters
    let setup = updateGhcOpts testSuiteSessionGhcOpts
             <> updateRelativeIncludes (sessionInitRelativeIncludes (deriveSessionInitParams testSuiteSessionServer))
    updateSession session setup (\_ -> return ())

    -- Run the test
    mresult <- try $ act session

    -- Make the session available for further tests, or shut it down if the
    -- @--no-session-reuse@ command line option was used
    if testSuiteConfigNoSessionReuse testSuiteEnvConfig || not testSuiteSessionReuse
      then
        shutdownSession session
      else do
        resetSession session

        -- resetSession does some sanity checks to make sure that the session
        -- reset worked okay. If these sanity checks fail, it will throw an
        -- exception, in which case we will _not_ make that session available
        -- for further use. This will leak the session, but that's okay: it's a
        -- bug when this happens.
        consMVar (testSuiteSessionServer, session) testSuiteStateAvailableSessions

    -- Return test result
    case mresult of
      Left  ex     -> throwIO (ex :: SomeException)
      Right result -> return result
  where
    TestSuiteSessionSetup{..} = sessionSetup (defaultSessionSetup env)

-- | Reset a session so that it can be reused in subsequent tests
--
-- This does not change any parameters that we have to set anyway
-- (that is, anything set in `setup` in `withAvailableSession'`).
--
-- An alternative would be to set these parameters here to their ide-backend
-- defaults; in that case, we could actually add an 'updateReset' to the
-- ide-backend API.
resetSession :: IdeSession -> IO ()
resetSession session = do
    updateSession session reset (\_ -> return ())

    -- Sanity check: after updateDeleteManagedFiles the managed files should
    -- actually be gone! (#238)
    -- Ignoring object files due to #249.
    checkIsEmpty ignoredExtensions =<< getSourcesDir session
    checkIsEmpty ignoredExtensions =<< getDataDir    session
  where
    reset = updateDeleteManagedFiles
         <> updateCodeGeneration False
         <> updateEnv []
         <> updateTargets (sessionInitTargets defaultSessionInitParams)
         <> updateRtsOpts (sessionInitRtsOpts defaultSessionInitParams)

    ignoredExtensions = [".o", ".dyn_o"]

defaultServerConfig :: TestSuiteEnv -> TestSuiteServerConfig
defaultServerConfig TestSuiteEnv{..} = TestSuiteServerConfig {
      testSuiteServerConfig           = testSuiteEnvConfig
    , testSuiteServerGhcVersion       = testSuiteEnvGhcVersion
    , testSuiteServerRelativeIncludes = Nothing
    , testSuiteServerGenerateModInfo  = Nothing
    , testSuiteServerPackageDBStack   = Nothing
    , testSuiteServerCabalMacros      = Nothing
    }

-- | Skip (the remainder of) this test
skipTest :: String -> IO ()
skipTest = throwIO . SkipTest

-- | Skip this (part of) the test if --no-exe is passed
ifTestingExe :: TestSuiteEnv -> Assertion -> Assertion
ifTestingExe TestSuiteEnv{..} act =
    unless testSuiteConfigNoExe act
  where
    TestSuiteConfig{..} = testSuiteEnvConfig

{-------------------------------------------------------------------------------
  Constructing tests

  This is similar to what tasty-hunit provides, but we provide better support
  for skipping tests
-------------------------------------------------------------------------------}

data TestCase =
    StdTest Assertion
    -- Tests that report more than just "OK"
  | WithOK (IO String)
  deriving Typeable

runTestCase :: TestCase -> IO Result
runTestCase (StdTest t) = registerTest t >> return (testPassed "")
runTestCase (WithOK  t) = registerTest t >>= return . testPassed

instance IsTest TestCase where
  -- TODO: Measure time and use for testPassed in normal case
  run _ test _ = runTestCase test `catches` [
      Handler $ \(HUnitFailure msg) -> return (testFailed msg)
    , Handler $ \(SkipTest msg)     -> return (testPassed ("Skipped (" ++ msg ++ ")"))
    ]

  -- TODO: Should this reflect testCaseEnabled?
  testOptions = return []

newtype SkipTest = SkipTest String
  deriving (Show, Typeable)

instance Exception SkipTest

-- | Construct a standard test case
stdTest :: TestSuiteEnv -> TestName -> (TestSuiteEnv -> Assertion) -> TestTree
stdTest st name = singleTest name . StdTest . ($ st)

-- | Construct a test case that reports OK with a non-standard string
withOK :: TestSuiteEnv -> TestName -> (TestSuiteEnv -> IO String) -> TestTree
withOK st name = singleTest name . WithOK . ($ st)

-- | Lists of tests that should be run only if Haddocks are installed
docTests :: TestSuiteEnv -> [TestTree] -> [TestTree]
docTests TestSuiteEnv{..} ts
    | testSuiteConfigNoHaddocks = []
    | otherwise                 = ts
  where
    TestSuiteConfig{..} = testSuiteEnvConfig

-- | Lists of tests that should be run only of --no-exe is not passed
exeTests :: TestSuiteEnv -> [TestTree] -> [TestTree]
exeTests TestSuiteEnv{..} ts
    | testSuiteConfigNoExe = []
    | otherwise            = ts
  where
    TestSuiteConfig{..} = testSuiteEnvConfig

{-------------------------------------------------------------------------------
  Internal
-------------------------------------------------------------------------------}

-- | When we request a session, we request one given a 'TestSuiteServerConfig'.
-- This should therefore have two properties:
--
-- 1. They should be testable for equality
-- 2. The server configuration should be determined fully by a
--    'TestSuiteServerConfig'.
data TestSuiteServerConfig = TestSuiteServerConfig {
    testSuiteServerConfig           :: TestSuiteConfig
  , testSuiteServerGhcVersion       :: GhcVersion
  , testSuiteServerRelativeIncludes :: Maybe [FilePath]
  , testSuiteServerGenerateModInfo  :: Maybe Bool
  , testSuiteServerPackageDBStack   :: Maybe PackageDBStack
  , testSuiteServerCabalMacros      :: Maybe L.ByteString
  }
  deriving (Eq, Show)

startNewSession :: TestSuiteServerConfig -> IO IdeSession
startNewSession cfg = initSession (deriveSessionInitParams cfg)
                                  (deriveSessionConfig     cfg)

deriveSessionInitParams :: TestSuiteServerConfig -> SessionInitParams
deriveSessionInitParams TestSuiteServerConfig{..} = defaultSessionInitParams {
      sessionInitCabalMacros =
          testSuiteServerCabalMacros
        `mplus`
          sessionInitCabalMacros defaultSessionInitParams
    , sessionInitRelativeIncludes =
        fromMaybe (sessionInitRelativeIncludes defaultSessionInitParams) $
          testSuiteServerRelativeIncludes
    }
  where
    TestSuiteConfig{..} = testSuiteServerConfig

deriveSessionConfig :: TestSuiteServerConfig -> SessionConfig
deriveSessionConfig TestSuiteServerConfig{..} = defaultSessionConfig {
      configDeleteTempFiles =
        not testSuiteConfigKeepTempFiles
    , configPackageDBStack  =
        fromMaybe (configPackageDBStack defaultSessionConfig) $
          (
            testSuiteServerPackageDBStack
          `mplus`
            do packageDb <- case testSuiteServerGhcVersion of
                              GHC742 -> testSuiteConfigPackageDb74
                              GHC78  -> testSuiteConfigPackageDb78
               return [GlobalPackageDB, SpecificPackageDB packageDb]
          )
    , configExtraPathDirs =
        splitSearchPath $ case testSuiteServerGhcVersion of
                            GHC742 -> testSuiteConfigExtraPaths74
                            GHC78  -> testSuiteConfigExtraPaths78
    , configGenerateModInfo =
        fromMaybe (configGenerateModInfo defaultSessionConfig) $
          testSuiteServerGenerateModInfo
    }
  where
    TestSuiteConfig{..} = testSuiteServerConfig

initTestSuiteState :: IO TestSuiteState
initTestSuiteState = do
  testSuiteStateAvailableSessions <- newMVar []
  return TestSuiteState{..}

cleanupTestSuiteState :: TestSuiteState -> IO ()
cleanupTestSuiteState TestSuiteState{..} = do
  sessions <- modifyMVar testSuiteStateAvailableSessions $ \xs -> return ([], xs)
  mapM_ (shutdownSession . snd) sessions

{-------------------------------------------------------------------------------
  Tasty additional command line options

  (used to configure the test suite)
-------------------------------------------------------------------------------}

newtype TestSuiteOptionKeepTempFiles = TestSuiteOptionKeepTempFiles Bool
  deriving (Eq, Ord, Typeable)
newtype TestSuiteOptionNoSessionReuse = TestSuiteOptionNoSessionReuse Bool
  deriving (Eq, Ord, Typeable)
newtype TestSuiteOptionNoHaddocks = TestSuiteOptionNoHaddocks Bool
  deriving (Eq, Ord, Typeable)
newtype TestSuiteOptionNoExe = TestSuiteOptionNoExe Bool
  deriving (Eq, Ord, Typeable)
newtype TestSuiteOptionPackageDb74 = TestSuiteOptionPackageDb74 (Maybe String)
  deriving (Eq, Ord, Typeable)
newtype TestSuiteOptionPackageDb78 = TestSuiteOptionPackageDb78 (Maybe String)
  deriving (Eq, Ord, Typeable)
newtype TestSuiteOptionExtraPaths74 = TestSuiteOptionExtraPaths74 String
  deriving (Eq, Ord, Typeable)
newtype TestSuiteOptionExtraPaths78 = TestSuiteOptionExtraPaths78 String
  deriving (Eq, Ord, Typeable)
newtype TestSuiteOptionTest74 = TestSuiteOptionTest74 Bool
  deriving (Eq, Ord, Typeable)
newtype TestSuiteOptionTest78 = TestSuiteOptionTest78 Bool
  deriving (Eq, Ord, Typeable)

instance IsOption TestSuiteOptionKeepTempFiles where
  defaultValue   = TestSuiteOptionKeepTempFiles False
  parseValue     = fmap TestSuiteOptionKeepTempFiles . safeRead
  optionName     = return "keep-temp-files"
  optionHelp     = return "Keep session temp files"
  optionCLParser = flagCLParser Nothing (TestSuiteOptionKeepTempFiles True)

instance IsOption TestSuiteOptionNoSessionReuse where
  defaultValue   = TestSuiteOptionNoSessionReuse False
  parseValue     = fmap TestSuiteOptionNoSessionReuse . safeRead
  optionName     = return "no-session-reuse"
  optionHelp     = return "Start a new session for each test"
  optionCLParser = flagCLParser Nothing (TestSuiteOptionNoSessionReuse True)

instance IsOption TestSuiteOptionNoHaddocks where
  defaultValue   = TestSuiteOptionNoHaddocks False
  parseValue     = fmap TestSuiteOptionNoHaddocks . safeRead
  optionName     = return "no-haddocks"
  optionHelp     = return "No haddock documentation installed on the test system"
  optionCLParser = flagCLParser Nothing (TestSuiteOptionNoHaddocks True)

instance IsOption TestSuiteOptionNoExe where
  defaultValue   = TestSuiteOptionNoExe False
  parseValue     = fmap TestSuiteOptionNoExe . safeRead
  optionName     = return "no-exe"
  optionHelp     = return "Skip buildExe tests"
  optionCLParser = flagCLParser Nothing (TestSuiteOptionNoExe True)

instance IsOption TestSuiteOptionPackageDb74 where
  defaultValue   = TestSuiteOptionPackageDb74 Nothing
  parseValue     = Just . TestSuiteOptionPackageDb74 . Just . expandHomeDir
  optionName     = return "package-db-74"
  optionHelp     = return "Package DB stack for GHC 7.4"

instance IsOption TestSuiteOptionPackageDb78 where
  defaultValue   = TestSuiteOptionPackageDb78 Nothing
  parseValue     = Just . TestSuiteOptionPackageDb78 . Just . expandHomeDir
  optionName     = return "package-db-78"
  optionHelp     = return "Package DB stack for GHC 7.8"

instance IsOption TestSuiteOptionExtraPaths74 where
  defaultValue   = TestSuiteOptionExtraPaths74 ""
  parseValue     = Just . TestSuiteOptionExtraPaths74 . expandHomeDir
  optionName     = return "extra-paths-74"
  optionHelp     = return "Package DB stack for GHC 7.4"

instance IsOption TestSuiteOptionExtraPaths78 where
  defaultValue   = TestSuiteOptionExtraPaths78 ""
  parseValue     = Just . TestSuiteOptionExtraPaths78 . expandHomeDir
  optionName     = return "extra-paths-78"
  optionHelp     = return "Package DB stack for GHC 7.8"

instance IsOption TestSuiteOptionTest74 where
  defaultValue   = TestSuiteOptionTest74 False
  parseValue     = fmap TestSuiteOptionTest74 . safeRead
  optionName     = return "test-74"
  optionHelp     = return "Run tests against GHC 7.4"
  optionCLParser = flagCLParser Nothing (TestSuiteOptionTest74 True)

instance IsOption TestSuiteOptionTest78 where
  defaultValue   = TestSuiteOptionTest78 False
  parseValue     = fmap TestSuiteOptionTest78 . safeRead
  optionName     = return "test-78"
  optionHelp     = return "Run tests against GHC 7.8"
  optionCLParser = flagCLParser Nothing (TestSuiteOptionTest78 True)

testSuiteCommandLineOptions :: [OptionDescription]
testSuiteCommandLineOptions = [
    Option (Proxy :: Proxy TestSuiteOptionKeepTempFiles)
  , Option (Proxy :: Proxy TestSuiteOptionNoSessionReuse)
  , Option (Proxy :: Proxy TestSuiteOptionNoHaddocks)
  , Option (Proxy :: Proxy TestSuiteOptionNoExe)
  , Option (Proxy :: Proxy TestSuiteOptionPackageDb74)
  , Option (Proxy :: Proxy TestSuiteOptionPackageDb78)
  , Option (Proxy :: Proxy TestSuiteOptionExtraPaths74)
  , Option (Proxy :: Proxy TestSuiteOptionExtraPaths78)
  , Option (Proxy :: Proxy TestSuiteOptionTest74)
  , Option (Proxy :: Proxy TestSuiteOptionTest78)
  ]

parseOptions :: (TestSuiteConfig -> TestTree) -> TestTree
parseOptions f =
  askOption $ \(TestSuiteOptionKeepTempFiles  testSuiteConfigKeepTempFiles)  ->
  askOption $ \(TestSuiteOptionNoSessionReuse testSuiteConfigNoSessionReuse) ->
  askOption $ \(TestSuiteOptionNoHaddocks     testSuiteConfigNoHaddocks)     ->
  askOption $ \(TestSuiteOptionNoExe          testSuiteConfigNoExe)          ->
  askOption $ \(TestSuiteOptionPackageDb74    testSuiteConfigPackageDb74)    ->
  askOption $ \(TestSuiteOptionPackageDb78    testSuiteConfigPackageDb78)    ->
  askOption $ \(TestSuiteOptionExtraPaths74   testSuiteConfigExtraPaths74)   ->
  askOption $ \(TestSuiteOptionExtraPaths78   testSuiteConfigExtraPaths78)   ->
  askOption $ \(TestSuiteOptionTest74         testSuiteConfigTest74)         ->
  askOption $ \(TestSuiteOptionTest78         testSuiteConfigTest78)         ->
  f TestSuiteConfig{..}

{-------------------------------------------------------------------------------
  Test suite global state
-------------------------------------------------------------------------------}

-- | Temporarily switch directory
--
-- (and make sure to switch back even in the presence of exceptions)
withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory fp act =
  requireExclusiveAccess $
    bracket (do cwd <- getCurrentDirectory
                setCurrentDirectory fp
                return cwd)
            (setCurrentDirectory)
            (\_ -> act)

findExe :: TestSuiteEnv -> String -> IO FilePath
findExe TestSuiteEnv{..} name = do
    mLoc <- OurCabal.findProgramOnSearchPath minBound searchPath name
    case mLoc of
      Nothing   -> fail $ "Could not find " ++ name
      Just prog -> return prog
  where
    extraPathDirs =
      case testSuiteEnvGhcVersion of
        GHC742 -> testSuiteConfigExtraPaths74 testSuiteEnvConfig
        GHC78  -> testSuiteConfigExtraPaths78 testSuiteEnvConfig

    searchPath :: OurCabal.ProgramSearchPath
    searchPath = OurCabal.ProgramSearchPathDefault
               : map OurCabal.ProgramSearchPathDir (splitSearchPath extraPathDirs)

withInstalledPackage :: TestSuiteEnv -> FilePath -> IO a -> IO a
withInstalledPackage env pkgDir act =
    requireExclusiveAccess $
      bracket_ (packageInstall env pkgDir)
               (packageDelete  env pkgDir)
               act

-- | Used only in the definition of 'withInstalledPackage'
--
-- This should not be used in isolation because it changes test global state.
packageInstall :: TestSuiteEnv -> FilePath -> IO ()
packageInstall env@TestSuiteEnv{..} pkgDir = do
  cabalExe <- findExe env "cabal"
  oldEnv   <- System.Environment.getEnvironment
  let oldEnvMap          = Map.fromList oldEnv
      adjustPATH oldPATH = extraPathDirs ++ ":" ++ oldPATH
      newEnvMap          = Map.adjust adjustPATH "PATH" oldEnvMap
      newEnv             = Map.toList newEnvMap
  forM_ [ ["clean"]
        , ["configure", "--package-db=" ++ packageDb, "--disable-library-profiling"]
        , ["build"]
        , ["copy"]
        , ["register"]
        ] $ \cmd -> do
    let opts = cmd ++ ["-v0"]
    (_,_,_,r2) <- createProcess (proc cabalExe opts)
                    { cwd = Just pkgDir
                    , env = Just newEnv
                    }
    void $ waitForProcess r2
  where
    extraPathDirs =
      case testSuiteEnvGhcVersion of
        GHC742 -> testSuiteConfigExtraPaths74 testSuiteEnvConfig
        GHC78  -> testSuiteConfigExtraPaths78 testSuiteEnvConfig
    packageDb = fromMaybe "" $
      case testSuiteEnvGhcVersion of
        GHC742 -> testSuiteConfigPackageDb74 testSuiteEnvConfig
        GHC78  -> testSuiteConfigPackageDb78 testSuiteEnvConfig

-- | Used only in the definition of 'withInstalledPackage'
--
-- This should not be used in isolation because it changes test global state.
packageDelete :: TestSuiteEnv -> FilePath -> IO ()
packageDelete env@TestSuiteEnv{..} pkgDir = do
    ghcPkgExe  <- findExe env "ghc-pkg"
    (_,_,_,r2) <- createProcess (proc ghcPkgExe opts)
                    { cwd     = Just pkgDir
                    , std_err = CreatePipe
                    }
    void $ waitForProcess r2
  where
    packageDb = fromMaybe "" $
      case testSuiteEnvGhcVersion of
        GHC742 -> testSuiteConfigPackageDb74 testSuiteEnvConfig
        GHC78  -> testSuiteConfigPackageDb78 testSuiteEnvConfig

    opts = [ "--package-conf=" ++ packageDb, "-v0", "unregister"
           , takeFileName pkgDir
           ]


-- TODO: We need to be careful with concurrency here
--
-- See comments for packageDelete.
packageCheck :: TestSuiteEnv -> FilePath -> IO String
packageCheck env pkgDir = do
    cabalExe <- findExe env "cabal"
    (_, mlocal_std_out, _, r2)
      <- createProcess (proc cabalExe ["check"])
           { cwd     = Just pkgDir
           , std_out = CreatePipe
           }
    let local_std_out = fromJust mlocal_std_out
    checkWarns <- hGetContents local_std_out
    evaluate $ rnf checkWarns
    hClose local_std_out
    void $ waitForProcess r2
    return checkWarns

{-------------------------------------------------------------------------------
  Concurrency control
-------------------------------------------------------------------------------}

-- | We run many tests concurrently, but occassionally a test needs to modify
-- the test global state (for instance, it might need to modify the current
-- working directory temporarily). When this happens, no other tests should
-- currently be executing.
data TestSuiteThreads =
    -- | Normal execution of multiple threads, none of have exclusive access
    -- right now
    --
    -- We record the set of running threads as well as the set of threads
    -- waiting to gain exclusive access, so that we don't start new threads
    -- when there are other threads waiting for exclusive access.
    NormalExecution [ThreadId] [ThreadId]

    -- | A thread currently has exclusive access
    --
    -- We record which other threads were waiting to gain exclusive access
  | ExclusiveExecution [ThreadId]
  deriving Show

testSuiteThreadsTVar :: TVar TestSuiteThreads
{-# NOINLINE testSuiteThreadsTVar #-}
testSuiteThreadsTVar = unsafePerformIO $ newTVarIO $ NormalExecution [] []

-- | Every test execution should be wrapped in registerTest
registerTest :: IO a -> IO a
registerTest act = do
    tid <- myThreadId
    bracket_ (register tid) (unregister tid) act
  where
    register :: ThreadId -> IO ()
    register t = atomically $ do
      testSuiteThreads <- readTVar testSuiteThreadsTVar
      case testSuiteThreads of
        NormalExecution running waiting -> do
          -- Don't start if there are threads waiting for exclusive access
          guard (waiting == [])
          writeTVar testSuiteThreadsTVar $ NormalExecution (t:running) []
        ExclusiveExecution _ ->
          -- Some other thread currently needs exclusive access.. Wait.
          retry

    unregister :: ThreadId -> IO ()
    unregister t = atomically $ do
      testSuiteThreads <- readTVar testSuiteThreadsTVar
      case testSuiteThreads of
        NormalExecution running waiting -> do
          let Just (_, running') = extract (== t) running
          writeTVar testSuiteThreadsTVar $ NormalExecution running' waiting
        ExclusiveExecution _ ->
          -- This should never happen
          error "The impossible happened"

requireExclusiveAccess :: IO a -> IO a
requireExclusiveAccess act = do
    tid <- myThreadId
    bracket_ (lock tid) (unlock tid) act
  where
    lock :: ThreadId -> IO ()
    lock t = do
      -- Record that we are no longer running, but are waiting
      atomically $ do
        testSuiteThreads <- readTVar testSuiteThreadsTVar
        case testSuiteThreads of
          NormalExecution running waiting -> do
            let Just (_, running') = extract (== t) running
                waiting'           = t : waiting
            writeTVar testSuiteThreadsTVar $ NormalExecution running' waiting'
          ExclusiveExecution _ ->
            error "lock: the impossible happened"

      -- Wait until there are no more threads running (i.e., all other threads
      -- have terminated or are themselves waiting to get exclusive access)
      atomically $ do
        testSuiteThreads <- readTVar testSuiteThreadsTVar
        case testSuiteThreads of
          NormalExecution [] waiting -> do
            let Just (_, waiting') = extract (== t) waiting
            writeTVar testSuiteThreadsTVar (ExclusiveExecution waiting')
          _  -> retry

    unlock :: ThreadId -> IO ()
    unlock t = do
      -- Give up exclusive access
      atomically $ do
        testSuiteThreads <- readTVar testSuiteThreadsTVar
        case testSuiteThreads of
          NormalExecution _ _ ->
            error "unlock: the impossible happened"
          ExclusiveExecution waiting ->
            writeTVar testSuiteThreadsTVar (NormalExecution [] waiting)

      -- And try to start running again
      atomically $ do
        testSuiteThreads <- readTVar testSuiteThreadsTVar
        case testSuiteThreads of
          NormalExecution running waiting -> do
            -- Don't start if there are threads waiting for exclusive access
            guard (waiting == [])
            writeTVar testSuiteThreadsTVar $ NormalExecution (t:running) []
          ExclusiveExecution _ ->
            -- Some other thread currently needs exclusive access.. Wait.
            retry

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

extractMVar :: (a -> Bool) -> MVar [a] -> IO (Maybe a)
extractMVar p listMVar = do
  modifyMVar listMVar $ \xs ->
    case extract p xs of
      Just (x, xs') -> return (xs', Just x)
      Nothing       -> return (xs, Nothing)

consMVar :: a -> MVar [a] -> IO ()
consMVar x listMVar = modifyMVar_ listMVar $ \xs -> return (x : xs)

extract :: (a -> Bool) -> [a] -> Maybe (a, [a])
extract _ []                 = Nothing
extract p (x:xs) | p x       = return (x, xs)
                 | otherwise = do (mx, xs') <- extract p xs
                                  return (mx, x : xs')

expandHomeDir :: FilePath -> FilePath
expandHomeDir path = unsafePerformIO $ do
    home <- getHomeDirectory

    let expand :: FilePath -> FilePath
        expand []       = []
        expand ('~':xs) = home ++ expand xs
        expand (x:xs)   = x     : expand xs

    return $ expand path

-- Check that the specified directory contains no files
-- (it may however contain subdirectories)
checkIsEmpty :: [String] -> FilePath -> IO ()
checkIsEmpty ignoredExtensions = go
  where
    go :: FilePath -> IO ()
    go parent = do
      children <- filter (not . ignore) `liftM` getDirectoryContents parent
      forM_ children $ \relChild ->
        unless (takeExtension relChild `elem` ignoredExtensions) $ do
          let absChild = parent </> relChild
          isFile <- doesFileExist      absChild
          isDir  <- doesDirectoryExist absChild
          when isFile $ throwIO (userError ("unexpected file " ++ relChild ++ " in " ++ parent))
          when isDir  $ go absChild

    ignore :: FilePath -> Bool
    ignore "."  = True
    ignore ".." = True
    ignore _    = False
