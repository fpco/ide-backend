module Main (main) where

import System.Environment (getArgs)
import System.FilePath ((</>), takeExtension, dropExtension)
import System.Directory
import System.Unix.Directory (withTemporaryDirectory)
import qualified Data.List as List
import Data.Monoid ((<>), mconcat, mempty)
import Data.ByteString.Lazy.Char8 (pack)
import System.IO (hFlush, stdout, stderr)
import Data.Maybe (isJust)
import Control.Exception (bracket)

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertBool, assertEqual)

import IdeSession
import GhcServer
import Progress
import Common
import TestTools

-- Tests using various functions of the IdeSession API
-- and a variety of small test Haskell projects.

-- Test a single sequence of API calls.
testSingle :: [String] -> FilePath -> (IdeSession -> IO ()) -> Assertion
testSingle opts originalSourcesDir check =
  withTemporaryDirectory "ide-backend-test" $ \ configSourcesDir -> do
    debug dVerbosity $ "\nCopying files from: " ++ originalSourcesDir
                       ++ " to: " ++ configSourcesDir
    -- Init session.
    let sessionConfig = SessionConfig{ configSourcesDir
                                     , configWorkingDir = configSourcesDir
                                     , configDataDir    = configSourcesDir
                                     , configTempDir    = "."
                                     , configStaticOpts = opts
                                     }
    bracket (initSession sessionConfig) shutdownSession $ \session -> do
      -- Send the source files from 'originalSourcesDir' to 'configSourcesDir'
      -- using the IdeSession's update mechanism.
      cnts <- getDirectoryContents originalSourcesDir
      let originalFiles = filter ((`elem` cpExtentions) . takeExtension) cnts
          -- HACK: here we fake module names, guessing them from file names.
          originalModules =
            map (\ f -> (ModuleName f, f)) originalFiles
          upd (m, f) = updateModule $ ModuleSource m $ originalSourcesDir </> f
          -- Let's also disable ChangeCodeGeneration, to keep the test stable
          -- in case the default value of CodeGeneration changes.
          originalUpdate = updateModule (ChangeCodeGeneration False)
                           <> (mconcat $ map upd originalModules)
      updateSessionD session originalUpdate
      -- Perform some API calls and check the results.
      check session

-- Set of api calls and checks to perform on each project.
--
-- TODO: we need much more tests to recover the functionality of the old,
-- undreadable set, and then we need to much more to test all API functions.
-- E.g., check that the values of PCounter do not exceeed the number of files.
featureTests :: [(String, IdeSession -> Assertion)]
featureTests =
  [ ("Just typecheck", \_ -> return ())
  , ("Overwrite with error"
    , \session -> do
        -- Overwrite one of the copied files.
        (m, _) <- getModules session
        let update = loadModule m "a = unknownX"
        updateSessionD session update
        msgs <- getSourceErrors session
        assertBool "Type error lost" $ length msgs >= 1
    )
  , ("Overwrite with module name not matching file name"
    , \session -> do
        (_, lm) <- getModules session
        let upd m =
              updateModule (ModulePut m (pack "module Wrong where\na = 1"))
            update = mconcat $ map upd lm
        updateSessionD session update
        msgs <- getSourceErrors session
        assertBool "Wrong module name not caught" $ length msgs >= 1
    )
  , ("Overwrite modules many times"
    , \session -> do
        -- Overwrite one of the copied files with an error.
        (m1, lm) <- getModules session
        let update1 =
              updateModule (ChangeCodeGeneration False)
              <> loadModule m1 "a = unknownX"
        updateSessionD session update1
        updateSessionD session mempty
        -- Overwrite all files, many times, with correct modules.
        let upd m = loadModule m "x = 1"
                    <> updateModule (ChangeCodeGeneration True)
                    <> loadModule m "y = 2"
            update2 = mconcat $ map upd lm
        updateSessionD session update2
        -- Overwrite again with the error.
        updateSessionD session update1
        msgs5 <- getSourceErrors session
        assertBool "Type error lost" $ length msgs5 >= 1
        assertBool ("Too many type errors: "
                    ++ List.intercalate "\n" (map formatSourceError msgs5))
          $ length msgs5 <= 1
        msgs4 <- getSourceErrors session -- old session
        assertBool ("Unexpected type errors: "
                    ++ List.intercalate "\n" (map formatSourceError msgs4))
          $ null msgs4
        assertRaises "runStmt session Main main"
          (== userError "Can't run before the code is generated. Set ChangeCodeGeneration.")
          (runStmt session "Main" "main")
    )
    , ("Run the sample code; don't fail without an explanation"
      , \session -> do
        let update = updateModule (ChangeCodeGeneration True)
        updateSessionD session update
        (msgs, resOrEx) <- runStmt session "Main" "main"
        assertBool "No errors detected, but the run failed" $
          case resOrEx of
            Just (Left _ident) -> True
            Just (Right _ex)   -> length msgs >= 1
            Nothing            -> length msgs >= 1
    )
    , ("Run manually corrected code; don't fail at all"
      , \session -> do
        (_, lm) <- getModules session
        let upd m = loadModule m "x = 1"
            update =
              updateModule (ModulePut
                              (ModuleName "Main")
                              (pack "module Main where\nmain = return ()"))
              <> mconcat (map upd lm)
              <> updateModule (ChangeCodeGeneration True)
        updateSessionD session update
        (msgs, resOrEx) <- runStmt session "Main" "main"
        assertBool ("Manually corrected code not run successfully: "
                    ++ List.intercalate "\n" (map formatSourceError msgs)) $
          case resOrEx of
            Just (Left _ident) -> True
            Just (Right _ex)   -> False
            Nothing            -> False
    )
  ]

{-
  assertRaises "updateSession s2 update1 (progressWaitConsume displayCounter)"
               (== userError "Invalid session token 2 /= 5")
               (updateSession s2 update1 (progressWaitConsume displayCounter))
  shutdownSession s6
  assertRaises "initSession sessionConfig"
               (== userError
                 ("Directory " ++ configSourcesDir ++ " is not empty"))
               (initSession sessionConfig)
  -- Remove file from the source directory to satisfy the precondition
  -- of initSession.
  mapM_ removeFile $ map (configSourcesDir </>) originalFiles
  -- Init another session. It strarts a new process with GHC,
  -- so the old state does not interfere.
  s9 <- initSession sessionConfig
  assertRaises "getSourceErrors s9"
               (== userError "This session state does not admit queries.")
               (getSourceErrors s9)
  shutdownSession s9
  s10 <- initSession sessionConfig
  let punOpts = opts ++ [ "-XNamedFieldPuns", "-XRecordWildCards"]
      optionsUpdate = originalUpdate
                      <> updateModule (ChangeOptions $ Just punOpts)
  s11 <- updateSession s10 optionsUpdate (progressWaitConsume displayCounter)
  msgs11 <- getSourceErrors s11
  putFlush $ "Error 11:\n" ++ List.intercalate "\n\n"
    (map formatSourceError msgs11) ++ "\n"
  assertRaises "shutdownSession s11"
               (== userError "Invalid session token 1 /= 2")
               (shutdownSession s11)
  shutdownSession s12
-}

-- | Test that the list of successfully compiled modules is reported correctly
testListCompiledModules :: Assertion
testListCompiledModules = testSingle defOpts "." $ \session -> do
  let m = ModuleName "A"
  updateSessionD session (loadModule m "")
  assertEqual "" [m] =<< getLoadedModules session

defOpts :: [String]
defOpts = [ "-no-user-package-conf" ]

-- Set of projects and options to use for them.
projects :: [(String, FilePath, [String])]
projects =
  [ ("A depends on B, no errors", "test/ABnoError", defOpts)
  , ("A depends on B, error in A", "test/AerrorB", defOpts)
  , ("A depends on B, error in B", "test/ABerror", defOpts)
  , ("Our own code, package 'ghc' missing", ".", [])
  , ( "A subdirectory of Cabal code"
    , "test/Cabal.Distribution.PackageDescription"
    , defOpts
    )
  , ("A file requiring -XNamedFieldPuns"
    , "test/Puns"
    , [ "-hide-all-packages"
      , "-package mtl"
      , "-package base"
      , "-package array"
      , "-package bytestring"
      , "-package containers"
      , "-package binary"
      ])
  , ("A single file with a code to run in parallel"
    , "test/MainModule"
    , [ "-hide-all-packages"
      , "-package parallel"
      , "-package base"
      , "-package old-time"
      ])
  ]

-- Driver
tests :: [Test]
tests =
  let groupProject (name, path, opts) =
        testGroup name $ map (caseFeature path opts) featureTests
      caseFeature originalSourcesDir opts (featureName, check) =
        testCase featureName
        $ testSingle opts originalSourcesDir check
  in [ testGroup "Full integration tests on multiple project"
       $ map groupProject projects
     , testGroup "Synthetic integration tests"
         [ testCase "Maintain list of compiled modules" testListCompiledModules
         ]
     ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    "--server" : opts -> ghcServer opts  -- @opts@ are GHC static flags
    _ -> defaultMain tests


-- Extra debug facilities. Normally turned off.

displayCounter :: PCounter -> IO ()
displayCounter n = debug dVerbosity $ "PCounter: " ++ (show n) ++ ". "

updateSessionD :: IdeSession -> IdeSessionUpdate -> IO ()
updateSessionD session update = do
  updateSession session update (progressWaitConsume displayCounter)
  msgs <- getSourceErrors session
  debug dVerbosity $ "getSourceErrors after update: "
                     ++ List.intercalate "\n" (map formatSourceError msgs)

-- Extra test tools.

getModules :: IdeSession -> IO (ModuleName, [ModuleName])
getModules sess = do
  let SessionConfig{configSourcesDir} = getSessionConfig sess
  cnts <- getDirectoryContents configSourcesDir
  let originalFiles = filter ((`elem` cpExtentions) . takeExtension) cnts
      originalModules = map (\ f -> (ModuleName f, f)) originalFiles
      m = case originalModules of
        [] -> ModuleName "testDirIsEmpty"
        (x, _) : _ -> x
  return (m, map fst originalModules)

loadModule :: ModuleName -> String -> IdeSessionUpdate
loadModule m contents =
  let ModuleName n = m
      name = dropExtension n
  in updateModule . ModulePut m . pack
     $ "module " ++ name ++ " where\n" ++ contents
