-- | Test suite global state
module TestSuite.State (
    -- * Top-level test groups
    TestSuiteState -- Opaque
  , TestSuiteConfig(..)
  , TestSuiteEnv(..)
  , testSuite
  , testSuiteCommandLineOptions
    -- * Operations on the test suite state
  , withAvailableSession
  , withAvailableSession'
  , requireHaddocks
  , skipTest
    -- * Constructing tests
  , stdTest
  , docTest
  ) where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.Typeable
import System.Directory (getHomeDirectory)
import System.FilePath (splitSearchPath)
import System.IO.Unsafe (unsafePerformIO)
import Test.HUnit (Assertion)
import Test.HUnit.Lang (HUnitFailure(..))
import Test.Tasty
import Test.Tasty.Options
import Test.Tasty.Providers

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
withAvailableSession env = withAvailableSession' env []

-- | Like 'withAvailableSession', but provide convenient way to set dynamic opts
withAvailableSession' :: TestSuiteEnv -> [String] -> (IdeSession -> IO a) -> IO a
withAvailableSession' TestSuiteEnv{..} dynOpts act = do
    TestSuiteState{..} <- testSuiteEnvState

    -- Find an available session, if one exists
    msession <- extractMVar ((== testSuiteServerConfig) . fst)
                            testSuiteStateAvailableSessions

    -- If there is none, start a new one
    session <- case msession of
                 Just session -> return (snd session)
                 Nothing      -> startNewSession testSuiteServerConfig

    -- Reset session state
    updateSession session
                  (    updateDynamicOpts dynOpts
                    <> updateDeleteManagedFiles
                    <> updateCodeGeneration False
                  )
                  (\_ -> return ())

    -- Run the test
    mresult <- try $ act session

    -- Make the session available for further tests, or shut it down if the
    -- @--no-session-reuse@ command line option was used
    if testSuiteConfigNoSessionReuse testSuiteEnvConfig
      then shutdownSession session
      else consMVar (testSuiteServerConfig, session) testSuiteStateAvailableSessions

    -- Return test result
    case mresult of
      Left  ex     -> throwIO (ex :: SomeException)
      Right result -> return result
  where
    testSuiteServerConfig = TestSuiteServerConfig {
        testSuiteServerConfig     = testSuiteEnvConfig
      , testSuiteServerGhcVersion = testSuiteEnvGhcVersion
      }

-- | Skip this test if the --no-haddocks flag is passed
requireHaddocks :: TestSuiteEnv -> IO ()
requireHaddocks st = do
  let TestSuiteConfig{..} = testSuiteEnvConfig st
  when testSuiteConfigNoHaddocks $ throwIO $ SkipTest "--no-haddocks"

-- | Skip (the remainder of) this test
skipTest :: String -> IO ()
skipTest = throwIO . SkipTest

{-------------------------------------------------------------------------------
  Constructing tests

  This is similar to what tasty-hunit provides, but we provide better support
  for skipping tests
-------------------------------------------------------------------------------}

newtype TestCase = TestCase Assertion
  deriving Typeable

instance IsTest TestCase where
  -- TODO: Measure time and use for testPassed in normal case
  run _ (TestCase assertion) _ = do
    (assertion >> return (testPassed "")) `catches` [
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
stdTest st name = singleTest name . TestCase . ($ st)

-- | Construct a test that relies on Haddocks being installed
docTest :: TestSuiteEnv -> TestName -> (TestSuiteEnv -> Assertion) -> TestTree
docTest st name test = stdTest st name (\env' -> requireHaddocks env' >> test env')

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
    testSuiteServerConfig     :: TestSuiteConfig
  , testSuiteServerGhcVersion :: GhcVersion
  }
  deriving (Eq, Show)

startNewSession :: TestSuiteServerConfig -> IO IdeSession
startNewSession TestSuiteServerConfig{..} =
    initSession
      defaultSessionInitParams
      defaultSessionConfig {
          configDeleteTempFiles =
            not testSuiteConfigKeepTempFiles
        , configPackageDBStack  =
            fromMaybe (configPackageDBStack defaultSessionConfig) $ do
              packageDb <- case testSuiteServerGhcVersion of
                             GHC742 -> testSuiteConfigPackageDb74
                             GHC78  -> testSuiteConfigPackageDb78
              return [GlobalPackageDB, SpecificPackageDB packageDb]
        , configExtraPathDirs =
            splitSearchPath $ case testSuiteServerGhcVersion of
                                GHC742 -> testSuiteConfigExtraPaths74
                                GHC78  -> testSuiteConfigExtraPaths78
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
  , Option (Proxy :: Proxy TestSuiteOptionPackageDb74)
  , Option (Proxy :: Proxy TestSuiteOptionPackageDb78)
  , Option (Proxy :: Proxy TestSuiteOptionExtraPaths74)
  , Option (Proxy :: Proxy TestSuiteOptionExtraPaths78)
  , Option (Proxy :: Proxy TestSuiteOptionTest74)
  , Option (Proxy :: Proxy TestSuiteOptionTest78)
  ]

parseOptions :: (TestSuiteConfig -> TestTree) -> TestTree
parseOptions f =
  askOption $ \(TestSuiteOptionKeepTempFiles  testSuiteConfigKeepTempFiles) ->
  askOption $ \(TestSuiteOptionNoSessionReuse testSuiteConfigNoSessionReuse) ->
  askOption $ \(TestSuiteOptionNoHaddocks     testSuiteConfigNoHaddocks)    ->
  askOption $ \(TestSuiteOptionPackageDb74    testSuiteConfigPackageDb74)   ->
  askOption $ \(TestSuiteOptionPackageDb78    testSuiteConfigPackageDb78)   ->
  askOption $ \(TestSuiteOptionExtraPaths74   testSuiteConfigExtraPaths74)  ->
  askOption $ \(TestSuiteOptionExtraPaths78   testSuiteConfigExtraPaths78)  ->
  askOption $ \(TestSuiteOptionTest74         testSuiteConfigTest74)        ->
  askOption $ \(TestSuiteOptionTest78         testSuiteConfigTest78)        ->
  f TestSuiteConfig{..}

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
