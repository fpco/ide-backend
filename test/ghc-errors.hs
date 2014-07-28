{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TypeSynonymInstances, FlexibleInstances, RecordWildCards, OverlappingInstances, OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.MVar
import qualified Control.Exception as Ex
import Control.Monad
import Control.DeepSeq (rnf)
import qualified Data.ByteString            as BSS
import qualified Data.ByteString.Char8      as BSSC
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Lazy.UTF8  as BSL8
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sort, elemIndex)
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Monoid (mconcat, mempty, (<>))
import Data.Text (Text)
import Data.Char (isLower)
import Data.Either (lefts)
import qualified Data.Text as Text
import Debug.Trace (traceEventIO)
import Data.Version (Version (..))
import Distribution.License (License (..))
import Distribution.Simple.Program.Find ( -- From our patched cabal
    ProgramSearchPath
  , findProgramOnSearchPath
  , ProgramSearchPathEntry(..)
  )
import Prelude hiding (mod, span)
import System.Directory
import qualified System.Environment as System.Environment
import System.Exit (ExitCode (..))
import System.FilePath
import System.FilePath.Find (always, extension, find)
import System.IO as IO
import System.IO.Unsafe (unsafePerformIO)
import System.Process (readProcess, readProcessWithExitCode)
import qualified System.Process as Process
import System.Random (randomRIO)
import System.Timeout (timeout)
import Text.Regex (mkRegex, subRegex)

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertBool, assertEqual, assertFailure, (@?=))
import Test.HUnit.Lang (HUnitFailure(..))

import Debug
import IdeSession hiding (defaultSessionConfig, defaultSessionInitParams)
import qualified IdeSession as IdeSession
import TestTools

-- Tests using various functions of the IdeSession API
-- and a variety of small test Haskell projects.



ifIdeBackendHaddockTestsEnabled :: SessionSetup -> (IdeSession -> IO ()) -> IO ()
ifIdeBackendHaddockTestsEnabled setup io = do
  wtsystem <- (System.Environment.getEnv "IDE_BACKEND_DISABLE_HADDOCK_TESTS")
               `Ex.catch` (\(_ :: Ex.IOException) -> return "1")
  unless (wtsystem == "0" || wtsystem == "False") $
    withSession setup io

type SessionSetup = ((SessionInitParams, SessionConfig) -> (SessionInitParams, SessionConfig))

-- | Run the specified action with a new IDE session, configured to use a
-- temporary directory
withSession :: SessionSetup -> (IdeSession -> IO a) -> IO a
withSession setup io =
    Ex.bracket (initSession initParams config) tryShutdownSession io
  where
    (initParams, config) = setup (defaultSessionInitParams, defaultSessionConfig)

    tryShutdownSession session = do
      mDidShutdown <- timeout 2000000 $ shutdownSession session
      case mDidShutdown of
        Just () -> return ()
        Nothing -> do putStrLn "WARNING: Failed to shutdown session (timeout)"
                      forceShutdownSession session

defaultSession :: SessionSetup
defaultSession = id


withMacros :: BSL.ByteString -> SessionSetup
withMacros macros (initParams, config) = (
    initParams { sessionInitCabalMacros = Just macros }
  , config
  )

-- Set of api calls and checks to perform on each project.
--
-- TODO: we need much more tests to recover the functionality of the old set,
-- and then we need to much more to test all API functions.
-- E.g., check that the values of Progress do not exceeed the number of files.
-- Also, check ModuleDelete and all the DataFileChange constructors,
-- getSourceModule an getDataFile.
multipleTests :: [(String, IdeSession -> IdeSessionUpdate () -> [String] -> Assertion)]
multipleTests =
  [ ( "Overwrite with error"
    , \session originalUpdate lm -> do
        updateSessionD session originalUpdate (length lm)
        -- No errors in the original test code.
        assertNoErrors session
        -- Overwrite one of the copied files.
        (_, ms) <- getModules session
        let update = loadModule (head ms) "a = unknownX"
        updateSessionD session update (length ms)  -- we don't know how many modules get recompiled
        assertSourceErrors' session ["Not in scope: `unknownX'"]
    )
  , ( "Overwrite with the same module name in all files"
    , \session originalUpdate lm -> do
        let upd m =
              updateSourceFile m (BSLC.pack "module Wrong where\na = 1")
            update = originalUpdate <> mconcat (map upd lm)
        updateSessionD session update 2
        if length lm >= 2
          then assertSourceErrors' session ["module `main:Wrong' is defined in multiple files"]
          else assertNoErrors session
    )
  , ( "Overwrite modules many times"
    , \session originalUpdate lm0 -> do
        let doubleUpdate = mempty <> originalUpdate <> originalUpdate <> mempty
        -- Updates are idempotent, so no errors and no recompilation.
        updateSessionD session doubleUpdate (length lm0)
        assertNoErrors session
        -- Overwrite one of the copied files with an error.
        (_, lm) <- getModules session
        let update1 =
              updateCodeGeneration False
              <> loadModule (head lm) "a = unknownX"
        updateSessionD session update1 (length lm)
        assertSourceErrors' session ["Not in scope: `unknownX'"]
        updateSessionD session mempty 1  -- was an error, so trying again
        assertSourceErrors' session ["Not in scope: `unknownX'"]
        -- Overwrite all files, many times, with correct code eventually.
        let upd m = loadModule m "x = unknownX"
                    <> loadModule m "y = 2"
                    <> updateCodeGeneration True
            update2 = mconcat $ map upd lm
        updateSessionD session update2 (length lm)
        assertNoErrors session
        -- Overwrite again with the error.
        updateSessionD session update1 1  -- drop bytecode, don't recompile
        assertSourceErrors' session ["Not in scope: `unknownX'"]
        assertRaises "runStmt session Main main"
          (== userError "Cannot run before the code is generated.")
          (runStmt session "Main" "main")
      )
    , ( "Run the sample code; succeed or raise an exception"
      , \session originalUpdate lm -> do
        updateSessionD session originalUpdate (length lm)
        let update = updateCodeGeneration True
        updateSessionD session update (length lm)  -- all recompiled
        assertNoErrors session
        mex <- Ex.try $ runStmt session "Main" "main"
        case mex of
          Right runActions -> void $ runWaitAll runActions
          Left ex -> assertEqual "runStmt" (userError "Module \"Main\" not successfully loaded, when trying to run code.") ex
      )
    , ( "Overwrite all with exception-less code and run it"
      , \session originalUpdate lm -> do
        let upd m = loadModule m "x = 1"
            update =
              originalUpdate
              <> updateSourceFile
                   "Central/TotallyMain.hs"
                   (BSLC.pack
                      "module Central.TotallyMain where\nmain = print \"test run\"")
              <> mconcat (map upd lm)
        let update2 = update <> updateCodeGeneration True
        -- Compile from scratch, generating code from the start.
        updateSessionD session update2 (length lm + 1)
        assertNoErrors session
        runActions <- runStmt session "Central.TotallyMain" "main"
        (output, result) <- runWaitAll runActions
        assertEqual "" RunOk result
        assertEqual "" output (BSLC.pack "\"test run\"\n")
      )
    , ( "Make sure deleting modules removes them from the directory"
      , \session originalUpdate lm -> do
        updateSessionD session originalUpdate (length lm)
        let updateDel = mconcat $ map updateSourceFileDelete lm
        updateSessionD session updateDel 0
        assertNoErrors session
        -- The updates cancel each other out.
        updateSessionD session (originalUpdate <> updateDel) 0
        let update2 = updateCodeGeneration True
        updateSessionD session update2 0  -- 0: nothing to generate code from
        assertNoErrors session
      )
    , ( "Make sure restartSession does not lose source files"
      , \session originalUpdate lm -> do
        let update = originalUpdate <> updateCodeGeneration True
        updateSessionD session update (length lm)
        serverBefore <- getGhcServer session
        mex <- Ex.try $ runStmt session "Main" "main"
        case mex of
          Right _runActions -> return ()  -- don't runWaitAll
          Left ex -> assertEqual "runStmt" (userError "Module \"Main\" not successfully loaded, when trying to run code.") ex
        restartSession session Nothing
        updateSessionD session mempty (length lm)  -- all compiled anew
        assertNoErrors session
        mex2 <- Ex.try $ runStmt session "Main" "main"
        case mex2 of
          Right runActions -> void $ runWaitAll runActions  -- now runWaitAll
          Left ex -> assertEqual "runStmt" (userError "Module \"Main\" not successfully loaded, when trying to run code.") ex
        restartSession session Nothing
        let update2 = mconcat $ map updateSourceFileDelete lm
        updateSessionD session update2 0  -- if any file missing, would yell
        assertNoErrors session
        let update3 = updateCodeGeneration True
        updateSessionD session update3 0  -- 0: nothing to generate code from
        exitCodeBefore <- getGhcExitCode serverBefore
        assertEqual "exitCodeBefore" (Just ExitSuccess) exitCodeBefore
      )
  ]

_ignoreFailure :: IO () -> IO ()
_ignoreFailure io = Ex.catch io $ \(HUnitFailure err) ->
  putStrLn $ "WARNING: " ++ err






-- Set of projects and options to use for them.
projects :: [(String, FilePath, [String])]
projects =
  [ ( "A depends on B, throws exception"
    , "test/ABnoError"
    , []
    )
  , ( "Cabal code"
    , "test/Cabal"
    , []
    )
  , ("A single file with a code to run in parallel"
    , "test/MainModule"
    , []
    )
  ]

-- Driver
tests :: [Test]
tests =
  let groupProject genModInfo ((featureName, check), k) =
        testGroup featureName
        $ map (caseFeature genModInfo featureName check k) projects
      caseFeature genModInfo featureName check k
                  (projectName, originalSourcesDir, opts) = do
        let caseName = projectName ++ " (" ++ show k ++ ")"
        testCase caseName $ do
          debug dVerbosity $ featureName ++ " / " ++ caseName ++ ":"
          traceEventIO ("TEST " ++ featureName ++ " / " ++ caseName)
          withSession (withModInfo genModInfo . withOpts opts) $ \session -> do
            (originalUpdate, lm) <- getModulesFrom session originalSourcesDir
            check session originalUpdate lm
  in [ testGroup "Full integration tests on multiple projects"
       $ map (groupProject False) $ zip multipleTests [1 :: Int ..]
     , testGroup "Synthetic integration tests"
       $ map (uncurry testCase) (traceTests syntheticTests)
     ]

traceTests :: [(String, Assertion)] -> [(String, Assertion)]
traceTests = map $ \(label, test) -> (label, do traceEventIO ("TEST " ++ label) ; test)

main :: IO ()
main = defaultMain tests


-- Extra test tools.
--











{------------------------------------------------------------------------------
  Default configuration

  This is slightly bold: we use an unsafePerformIO in order to read the
  system environment.
------------------------------------------------------------------------------}

defaultSessionInitParams :: SessionInitParams
{-# NOINLINE defaultSessionInitParams #-}
defaultSessionInitParams = unsafePerformIO $
  return IdeSession.defaultSessionInitParams

defaultSessionConfig :: SessionConfig
{-# NOINLINE defaultSessionConfig #-}
defaultSessionConfig = unsafePerformIO $ do
  packageDb     <- (System.Environment.getEnv "IDE_BACKEND_PACKAGE_DB")
                     `Ex.catch` (\(_ :: Ex.IOException) -> return "")
  extraPathDirs <- (System.Environment.getEnv "IDE_BACKEND_EXTRA_PATH_DIRS")
                     `Ex.catch` (\(_ :: Ex.IOException) -> return "")
  keepTempFiles <- (System.Environment.getEnv "IDE_BACKEND_KEEP_TEMP_FILES")
                     `Ex.catch` (\(_ :: Ex.IOException) -> return "")
  let packageDbStack
        | null packageDb = configPackageDBStack IdeSession.defaultSessionConfig
        | otherwise      = [GlobalPackageDB, SpecificPackageDB packageDb]
  return IdeSession.defaultSessionConfig {
             configPackageDBStack  = packageDbStack
           , configExtraPathDirs   = splitSearchPath extraPathDirs
           , configDeleteTempFiles = null keepTempFiles
           }
