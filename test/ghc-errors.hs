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

syntheticTests :: [(String, Assertion)]
syntheticTests = [
  , ( "Duplicate shutdown"
    , withSession defaultSession $ \session ->
        -- withConfiguredSession will shutdown the session as well
        shutdownSession session
    )
  , ( "Permit a session within a session and duplicated shutdownSession"
    , withSession defaultSession $ \session -> do
        loadModulesFrom session "test/ABnoError"

        withSession defaultSession $ \s2 -> do
         withSession defaultSession $ \s3 -> do
          withSession defaultSession $ \_s4 -> do
           let update2 = loadModule "M.hs" "a = unknownX"
           updateSessionD s2 update2 1
           assertOneError s2
           withSession defaultSession $ \s5 -> do
            let update3 = loadModule "M.hs" "a = 3"
            updateSessionD s3 update3 1
            assertNoErrors session
            shutdownSession s5 -- <-- duplicate "nested" shutdown
    )
  , ( "Caching cabal macros"
    , do macros <- withSession defaultSession getCabalMacros
         withSession (withOpts ["-XCPP"] . withMacros macros) $ \session -> do
           let update = (updateCodeGeneration True)
                     <> (updateSourceFile "Main.hs" $ BSLC.pack $ unlines [
                             "#if !MIN_VERSION_base(999,0,0)"
                           , "main = print 5"
                           , "#else"
                           , "terrible error"
                           , "#endif"
                           ])
           updateSessionD session update 1
           assertNoErrors session
           runActions <- runStmt session "Main" "main"
           (output, _) <- runWaitAll runActions
           assertEqual "result of ifdefed print 5" (BSLC.pack "5\n") output
           let m = "Main"
               updExe = buildExe [] [(Text.pack m, "Main.hs")]
           updateSessionD session updExe 2
           runActionsExe <- runExe session m
           (outExe, statusExe) <- runWaitAll runActionsExe
           assertEqual "Output from runExe"
                       "5\n"
                       outExe
           assertEqual "after runExe" ExitSuccess statusExe
         let customMacros = BSLC.pack "#define HELLO 1"
         withSession (withOpts ["-XCPP"] . withMacros customMacros) $ \session -> do
           let update = (updateCodeGeneration True)
                     <> (updateSourceFile "Main.hs" $ BSLC.pack $ unlines [
                             "#if HELLO"
                           , "main = print 6"
                           , "#else"
                           , "main = print 7"
                           , "#endif"
                           ])
           updateSessionD session update 1
           assertNoErrors session
           runActions <- runStmt session "Main" "main"
           (output, _) <- runWaitAll runActions
           assertEqual "result of ifdefed print 6" (BSLC.pack "6\n") output
           let m = "Main"
               updExe = buildExe [] [(Text.pack m, "Main.hs")]
           updateSessionD session updExe 2
           runActionsExe <- runExe session m
           (outExe, statusExe) <- runWaitAll runActionsExe
           assertEqual "Output from runExe"
                       "7\n"  -- FIXME
                       outExe
           assertEqual "after runExe" ExitSuccess statusExe
    )
{- Now that we always load the RTS, we're never in this situation
  , ("Reject getSourceErrors without updateSession"
    , withSession defaultSession $ \session ->
        assertRaises "getSourceErrors session"
          (== userError "This session state does not admit queries.")
          (getSourceErrors session)
    )
-}
  , ("Reject updateSession after shutdownSession"
    , withSession defaultSession $ \session -> do
        shutdownSession session
        assertRaises "updateSessionD session mempty"
          (== userError "Session already shut down.")
          (updateSessionD session mempty 0)
    )
  , ("Reject getSourceErrors after shutdownSession"
    , withSession defaultSession $ \session -> do
        shutdownSession session
        assertRaises "getSourceErrors session"
          (== userError "Session already shut down.")
          (getSourceErrors session)
    )
  , ("Reject runStmt after shutdownSession"
    , withSession defaultSession $ \session -> do
        shutdownSession session
        assertRaises "runStmt session Main main"
          (== userError "State not idle")
          (runStmt session "Main" "main")
    )
  , ( "Fail on empty package DB"
    , assertRaises ""
        (\e -> e == userError "Invalid package DB stack: []")
        (withSession (withDBStack []) $ \_ -> return ())
    )
  , ( "Reject a module with mangled header"
    , withSession defaultSession $ \session -> do
        let update = updateSourceFile "M.hs"
                                  (BSLC.pack "module very-wrong where")
        updateSessionD session update 1
        assertSourceErrors' session ["parse error on input `very'"]
        let update2 = updateSourceFile "M.hs"
                                   (BSLC.pack "module M.1.2.3.8.T where")
        updateSessionD session update2 1
        assertSourceErrors' session ["parse error on input `.'"]
    )
  , ( "Two calls to runStmt"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "echo :: IO ()"
                    , "echo = getLine >>= putStrLn"
                    , "echoReverse :: IO ()"
                    , "echoReverse = getLine >>= putStrLn . reverse"
                    ])
        updateSessionD session upd 1
        assertNoErrors session

        do runActions <- runStmt session "M" "echo"
           supplyStdin runActions (BSSC.pack "ECHO!\n")
           (output, result) <- runWaitAll runActions
           assertEqual "" RunOk result
           assertEqual "" (BSLC.pack "ECHO!\n") output

        do runActions <- runStmt session "M" "echoReverse"
           supplyStdin runActions (BSSC.pack "!OHCE\n")
           (output, result) <- runWaitAll runActions
           assertEqual "" RunOk result
           assertEqual "" (BSLC.pack "ECHO!\n") output
    )
  , ( "Two calls to runExe"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "main :: IO ()"
                    , "main = getLine >>= putStrLn . reverse"
                    ])
        updateSessionD session upd 1
        assertNoErrors session

        let m = "M"
            updExe = buildExe [] [(Text.pack m, "M.hs")]
        updateSessionD session updExe 2

        do runActions <- runExe session "M"
           supplyStdin runActions (BSSC.pack "!OHCE\n")
           (output, result) <- runWaitAll runActions
           assertEqual "" result ExitSuccess
           assertEqual "" (BSLC.pack "ECHO!\n") output

        do runActions <- runExe session "M"
           supplyStdin runActions (BSSC.pack "!OHCE\n")
           (output, result) <- runWaitAll runActions
           assertEqual "" result ExitSuccess
           assertEqual "" (BSLC.pack "ECHO!\n") output
    )
  , ( "Make sure we can terminate the IDE session when code is running"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "echo :: IO ()"
                    , "echo = (getLine >>= putStrLn) >> echo"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        _runActions <- runStmt session "M" "echo"
        return ()
     )
  , ( "Make sure we can terminate the IDE session when exe is running"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "main :: IO ()"
                    , "main = (getLine >>= putStrLn) >> main"
                    ])
        updateSessionD session upd 1

        let m = "M"
            updExe = buildExe [] [(Text.pack m, "M.hs")]
        updateSessionD session updExe 2

        assertNoErrors session
        _runActions <- runExe session "M"
        return ()
     )
  , ( "getSourceErrors during run"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "{-# OPTIONS_GHC -Wall #-}"
                    , "module M where"
                    , "loop = loop"
                    , "main :: IO ()"
                    , "main = loop"
                   ])
        updateSessionD session upd 1
        assertSourceErrors' session ["Top-level binding with no type signature"]

        let m = "M"
            updExe = buildExe [] [(Text.pack m, "M.hs")]
        updateSessionD session updExe 2

        msgs1 <- getSourceErrors session
        _ract <- runStmt session "M" "loop"
        msgs2 <- getSourceErrors session
        assertEqual "Running code does not affect getSourceErrors" msgs1 msgs2

        _runActionsExe <- runExe session m

        msgs3 <- getSourceErrors session
        assertEqual "Running exes does not affect getSourceErrors" msgs1 msgs3
    )
  , ( "getLoadedModules during run"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "{-# OPTIONS_GHC -Wall #-}"
                    , "module M where"
                    , "loop = loop"
                    ])
        updateSessionD session upd 1
        mods <- getLoadedModules session
        assertEqual "" [Text.pack "M"] mods
        _runActions <- runStmt session "M" "loop"
        mods' <- getLoadedModules session
        assertEqual "Running code does not affect getLoadedModules" mods mods'
    )
  , ( "Call runWait after termination (normal termination)"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "hello :: IO ()"
                    , "hello = putStrLn \"Hello World\""
                    , "main :: IO ()"
                    , "main = hello"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        assertEqual "" RunOk result
        assertEqual "" (BSLC.pack "Hello World\n") output
        result' <- runWait runActions
        assertEqual "" result' (Right RunOk)

        let m = "M"
            updExe = buildExe [] [(Text.pack m, "M.hs")]
        updateSessionD session updExe 2
        runActionsExe <- runExe session m
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "Hello World\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe

        result2 <- runWait runActionsExe
        assertEqual "" result2 (Right ExitSuccess)
    )
  , ( "Call runWait after termination (interrupted)"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import Control.Concurrent (threadDelay)"
                    , "loop :: IO ()"
                    , "loop = threadDelay 100000 >> loop"
                    , "main :: IO ()"
                    , "main = loop"
                    ])
        updateSessionD session upd 1
        assertNoErrors session

        let m = "M"
            updExe = buildExe [] [(Text.pack m, "M.hs")]
        updateSessionD session updExe 2
        runActionsExe <- runExe session m
        threadDelay 1000000
        interrupt runActionsExe
        resOrEx <- runWait runActionsExe
        case resOrEx of
          Right result -> assertEqual "after runExe" (ExitFailure 2) result
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
        result' <- runWait runActionsExe
        assertEqual "" result' (Right $ ExitFailure 2)

        runActions <- runStmt session "M" "loop"
        threadDelay 1000000
        interrupt runActions
        resOrEx2 <- runWait runActions
        case resOrEx2 of
          Right result -> assertBool "" (isAsyncException result)
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
        resOrEx' <- runWait runActions
        case resOrEx' of
          Right result -> assertBool "" (isAsyncException result)
          _ -> assertFailure $ "Unexpected run result in repeat call: " ++ show resOrEx'
    )
  , ( "Call runWait after termination (restarted session)"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import Control.Concurrent (threadDelay)"
                    , "loop :: IO ()"
                    , "loop = threadDelay 100000 >> loop"
                    , "main :: IO ()"
                    , "main = loop"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "loop"
        threadDelay 1000000
        restartSession session Nothing
        forceCancel runActions
        resOrEx2 <- runWait runActions
        case resOrEx2 of
          Right RunForceCancelled -> return ()
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx2
        resOrEx' <- runWait runActions
        case resOrEx' of
          Right RunForceCancelled -> return ()
          _ -> assertFailure $ "Unexpected run result in repeat call: " ++ show resOrEx'

        updateSessionD session mempty 1  -- needed to load the code again

        let m = "M"
            updExe = buildExe [] [(Text.pack m, "M.hs")]
        updateSessionD session updExe 2
        runActionsExe <- runExe session m
        threadDelay 1000000
        restartSession session Nothing
        -- restartSession would not suffice, since session restart
        -- doesn't stop the exe, so we need to interrupt manually.
        interrupt runActionsExe
        resOrEx <- runWait runActionsExe
        case resOrEx of
          Right result -> assertEqual "after runExe" (ExitFailure 2) result
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
        result' <- runWait runActionsExe
        assertEqual "" result' (Right $ ExitFailure 2)
    )
  , ( "Using something from a different package (no \"Loading package\" msg)"
      -- We pick something from the haskell platform but that doesn't come with ghc itself
      -- https://github.com/haskell/haskell-platform/blob/2012.4.0.0/haskell-platform.cabal
    , withSession (withOpts []) $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSL8.fromString . unlines $
                    [ "module M where"
                    , "import Control.Monad.IO.Class" -- From transformers
                    , "hello :: IO ()"
                    , "hello = liftIO $ print 5"
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        assertEqual "" RunOk result
        assertEqual "" (BSL8.fromString "5\n") output
    )
  , ( "Build .cabal from TH with a wrong libname and don't fail"
    , withSession (withOpts ["-XTemplateHaskell"]) $ \session -> do
        withCurrentDirectory "test" $ do
          (originalUpdate, lm) <- getModulesFrom session "TH"
          let update = originalUpdate <> updateCodeGeneration True
          updateSessionD session update (length lm)
        assertNoErrors session
        dotCabalFromName <- getDotCabal session
        let dotCabal = dotCabalFromName "--///fo/name" $ Version [-1, -9] []
        assertBool ".cabal not empty" $ not $ BSLC.null dotCabal
    )
  , ( "Parse ghc 'Compiling' messages"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "printA :: IO ()"
                    , "printA = putStr \"A\""
                    ])
               <> (updateSourceFile "B.hs" . BSLC.pack . unlines $
                    [ "module B where"
                    , "import A"
                    , "printAB :: IO ()"
                    , "printAB = printA >> putStr \"B\""
                    ])

        progressUpdatesRef <- newIORef []
        updateSession session upd $ \p -> do
          progressUpdates <- readIORef progressUpdatesRef
          writeIORef progressUpdatesRef (progressUpdates ++ [p])
        assertNoErrors session

        let abstract (Progress {..}) = (progressStep, progressNumSteps, Text.unpack `liftM` progressParsedMsg)
        progressUpdates <- readIORef progressUpdatesRef
        assertEqual "" [(1, 2, Just "Compiling A"), (2, 2, Just "Compiling B")]
                       (map abstract progressUpdates)

    )
  , ( "Parse ghc 'Compiling' messages (with TH)"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE TemplateHaskell #-}"
                    , "module A where"
                    , "import Language.Haskell.TH"
                    , "foo :: Q Exp"
                    , "foo = [| True |]"
                    ])
               <> (updateSourceFile "Main.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE TemplateHaskell #-}"
                    , "module Main where"
                    , "import A"
                    , "main :: IO ()"
                    , "main = print $foo"
                    ])

        let abstract (Progress {..}) = (progressStep, progressNumSteps, Text.unpack `liftM` progressParsedMsg)
        progressUpdatesRef <- newIORef []
        updateSession session upd $ \p -> do
          progressUpdates <- readIORef progressUpdatesRef
          writeIORef progressUpdatesRef (progressUpdates ++ [p])
        assertNoErrors session

        do progressUpdates <- readIORef progressUpdatesRef
           assertEqual "" [(1, 2, Just "Compiling A"), (2, 2, Just "Compiling Main")]
                          (map abstract progressUpdates)

        -- Now we touch A, triggering recompilation of both A and B
        -- This will cause ghc to report "[TH]" as part of the progress message
        -- (at least on the command line). It doesn't seem to happen with the
        -- API; but just in case, we check that we still get the right messages
        -- (and not, for instance, '[TH]' as the module name).

        let upd2 = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE TemplateHaskell #-}"
                    , "module A where"
                    , "import Language.Haskell.TH"
                    , "foo :: Q Exp"
                    , "foo = [| False |]"
                    ])
        writeIORef progressUpdatesRef []
        updateSession session upd2 $ \p -> do
          progressUpdates <- readIORef progressUpdatesRef
          writeIORef progressUpdatesRef (progressUpdates ++ [p])
        assertNoErrors session

        do progressUpdates <- readIORef progressUpdatesRef
           assertEqual "" [(1, 2, Just "Compiling A"), (2, 2, Just "Compiling Main")]
                          (map abstract progressUpdates)
    )
  , ( "getLoadedModules while configGenerateModInfo off"
    , withSession (withModInfo False) $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "hello :: IO ()"
                    , "hello = putStrLn \"Hello World\""
                    ])
        updateSessionD session upd 1
        assertNoErrors session
        assertLoadedModules session "" ["M"]
    )
  , ( "Quickfix for Updating static files never triggers recompilation"
    , withSession defaultSession $ \session -> do
        let upd = updateDataFile "A.foo" (BSLC.pack "unboundVarName")
               <> (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE TemplateHaskell #-}"
                    , "module A where"
                    , "import Language.Haskell.TH.Syntax"
                    , "goodVarName = 42"
                    , "foo :: Int"
                    , "foo = $(do"
                    , "          qAddDependentFile \"A.foo\""
                    , "          s <- qRunIO (readFile \"A.foo\")"
                    , "          return $ VarE (mkName s))"
                    ])
        updateSessionD session upd 1
        assertOneError session
        let upd2 = updateDataFile "A.foo" (BSLC.pack "goodVarName")
        updateSessionD session upd2 1
        assertNoErrors session
    )
  , ( "Quickfix for Updating static files never triggers --- illegal var name"
    , withSession defaultSession $ \session -> do
        let upd = updateDataFile "A.foo" (BSLC.pack "42 is a wrong var name")
               <> (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE TemplateHaskell #-}"
                    , "module A where"
                    , "import Language.Haskell.TH.Syntax"
                    , "goodVarName = 42"
                    , "foo :: Int"
                    , "foo = $(do"
                    , "          qAddDependentFile \"A.foo\""
                    , "          s <- qRunIO (readFile \"A.foo\")"
                    , "          return $ VarE (mkName s))"
                    ])
        updateSessionD session upd 1
        assertOneError session
        let upd2 = updateDataFile "A.foo" (BSLC.pack "goodVarName")
        updateSessionD session upd2 1
        assertNoErrors session
    )
  , ( "Quickfix for Updating static files never triggers --- missing file"
    , withSession defaultSession $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "{-# LANGUAGE TemplateHaskell #-}"
                    , "module A where"
                    , "import Language.Haskell.TH.Syntax"
                    , "foo :: String"
                    , "foo = $(do"
                    , "          qAddDependentFile \"A.foo\""
                    , "          x <- qRunIO (readFile \"A.foo\")"
                    , "          lift x)"
                    ])
        updateSessionD session upd 1
        assertOneError session
        let upd2 = updateDataFile "A.foo" (BSLC.pack "fooString")
        updateSessionD session upd2 1
        assertNoErrors session
    )
  , ("Issue #119"
    , withSession defaultSession $ \sess -> do
        distDir <- getDistDir sess

        let cb = \_ -> return ()
            update = flip (updateSession sess) cb

        update $ updateCodeGeneration True
        update $ updateStdoutBufferMode (RunLineBuffering Nothing)
        update $ updateStderrBufferMode (RunLineBuffering Nothing)

        -- Compile code and execute

        update $ updateSourceFile "src/Main.hs" . BSLC.pack $
            "main = putStrLn \"Version 1\""
        assertNoErrors sess

        update $ buildExe [] [(Text.pack "Main", "src/Main.hs")]
        out1 <- readProcess (distDir </> "build" </> "Main" </> "Main") [] []
        assertEqual "" "Version 1\n" out1
        runActionsExe <- runExe sess "Main"
        (outExe, statusExe) <- runWaitAll runActionsExe
        assertEqual "Output from runExe"
                    "Version 1\n"
                    outExe
        assertEqual "after runExe" ExitSuccess statusExe

        -- Update the code and execute again

        update $ updateSourceFile "src/Main.hs" . BSLC.pack $
            "main = putStrLn \"Version 2\""
        assertNoErrors sess

        update $ buildExe [] [(Text.pack "Main", "src/Main.hs")]
        out2 <- readProcess (distDir </> "build" </> "Main" </> "Main") [] []
        assertEqual "" "Version 2\n" out2
        runActionsExe2 <- runExe sess "Main"
        (outExe2, statusExe2) <- runWaitAll runActionsExe2
        assertEqual "Output from runExe"
                    "Version 2\n"
                    outExe2
        assertEqual "after runExe" ExitSuccess statusExe2
    )
  , ( "Issue #125: Hang when snippet calls createProcess with close_fds set to True"
    , withSession defaultSession $ \sess -> do
        let cb     = \_ -> return ()
            update = flip (updateSession sess) cb

        update $ updateCodeGeneration True
        update $ updateStdoutBufferMode (RunLineBuffering Nothing)
        update $ updateStderrBufferMode (RunLineBuffering Nothing)
        update $ updateDynamicOpts ["-Wall", "-Werror"]

        update $ updateSourceFile "src/Main.hs" $ BSLC.pack $ unlines [
            "module Main where"

          , "import System.Process"
          , "import System.IO"

          , "main :: IO ()"
          , "main = do"
          , "    (_,Just maybeOut,_,pr) <- createProcess $ (shell \"foo\")"
          , "        { cmdspec      = ShellCommand \"echo 123\""
          , "        , cwd          = Nothing"
          , "        , env          = Nothing"
          , "        , std_in       = CreatePipe"
          , "        , std_out      = CreatePipe"
          , "        , std_err      = CreatePipe"
          , "        , close_fds    = True"
          , "        , create_group = False"
          , "        }"
          , "    putStr =<< hGetContents maybeOut"
          , "    terminateProcess pr"
          ]

        assertNoErrors sess
        mRunActions <- timeout 2000000 $ runStmt sess "Main" "main"
        case mRunActions of
          Just runActions -> do
            mRunResult <- timeout 2000000 $ runWaitAll runActions
            case mRunResult of
              Just (output, RunOk) -> assertEqual "" (BSLC.pack "123\n") output
              Nothing -> assertFailure "Timeout in runWaitAll"
              _       -> assertFailure "Unexpected run result"
          Nothing ->
            assertFailure "Timeout in runStmt"
    )
{-
  , ( "Debugging 1: Setting and clearing breakpoints"
    , withSession defaultSession $ \session -> do
        updateSessionD session qsort 1
        assertNoErrors session

        expTypes <- getExpTypes session
        let (modName, mouseSpan) = mkSpan "Main" (2, 16, 2, 16)
            fullSpan = fst . last $ expTypes modName mouseSpan
            (_, wrongSpan) = mkSpan "Main" (0, 0, 0, 0)

        r1 <- setBreakpoint session modName fullSpan True
        r2 <- setBreakpoint session modName fullSpan False
        r3 <- setBreakpoint session modName fullSpan False
        r4 <- setBreakpoint session modName wrongSpan True
        assertEqual "original value of breakpoint"  (Just False) r1
        assertEqual "breakpoint successfully set"   (Just True)  r2
        assertEqual "breakpoint successfully unset" (Just False) r3
        assertEqual "invalid breakpoint"            Nothing      r4
    )
  , ( "Debugging 2: Running until breakpoint, then resuming"
    , withSession defaultSession $ \session -> do
        updateSessionD session qsort 1
        assertNoErrors session

        expTypes <- getExpTypes session
        let (modName, mouseSpan) = mkSpan "Main" (2, 16, 2, 16)
            fullSpan = fst . last $ expTypes modName mouseSpan

        Just False <- setBreakpoint session modName fullSpan True

        let inputList :: [Integer]
            inputList = [8, 4, 0, 3, 1, 23, 11, 18]

        outputs <- forM (mark inputList) $ \(i, isFirst, _isLast) -> do
           runActions <- if isFirst then runStmt session "Main" "main"
                                    else resume session
           (output, RunBreak) <- runWaitAll runActions
           assertBreak session "Main"
                               "Main.hs@2:16-2:48"
                               "[a]"
                               [ ("_result" , "[Integer]" , "_")
                               , ("a"       , "Integer"   , show i)
                               , ("left"    , "[Integer]" , "_")
                               , ("right"   , "[Integer]" , "_")
                               ]
           return output

        do runActions <- resume session
           (finalOutput, finalResult) <- runWaitAll runActions
           let output = BSL.concat $ outputs ++ [finalOutput]
           assertEqual "" RunOk finalResult
           assertEqual "" (show (sort inputList) ++ "\n") (BSLC.unpack output)
           mBreakInfo <- getBreakInfo session
           assertEqual "" Nothing mBreakInfo
    )
  , ( "Debugging 3: Printing and forcing"
    , withSession defaultSession $ \session -> do
        updateSessionD session qsort 1
        assertNoErrors session

        expTypes <- getExpTypes session
        let (modName, mouseSpan) = mkSpan "Main" (2, 16, 2, 16)
            fullSpan = fst . last $ expTypes modName mouseSpan

        Just False <- setBreakpoint session modName fullSpan True
        runActions <- runStmt session "Main" "main"
        (_output, RunBreak) <- runWaitAll runActions

        printed <- printVar session (Text.pack "left") True False
        forced  <- printVar session (Text.pack "left") True True
        assertEqual "" [(Text.pack "left", Text.pack "[Integer]", Text.pack "(_t1::[Integer])")] printed
        assertEqual "" [(Text.pack "left", Text.pack "[Integer]", Text.pack "[4, 0, 3, 1]")] forced
    )
-}
  , ( "ghc qAddDependentFile patch (#118)"
    , withSession defaultSession $ \session -> do
        let cb     = \_ -> return ()
            update = flip (updateSession session) cb

        let mainContents = BSLC.pack $ unlines
                [ "{-# LANGUAGE TemplateHaskell #-}"
                , "import Language.Haskell.TH.Syntax"
                , "main = print ($(do"
                , "  qAddDependentFile \"foo.hamlet\""
                , "  s <- qRunIO $ readFile \"foo.hamlet\""
                , "  lift $ (read s :: Int)"
                , "  ) :: Int)"
                ]

        update $ mconcat
            [ updateSourceFile "Main.hs" mainContents
            , updateDataFile "foo.hamlet" (BSLC.pack "invalid")
            ]

        -- Error message expected, invalid data file
        assertOneError session

        update $ updateDataFile "foo.hamlet" (BSLC.pack "42")
        assertNoErrors session

        update $ updateSourceFile "Main.hs" $ mainContents `BSLC.append` (BSLC.pack "\n\n-- Trigger a recompile")
        assertNoErrors session

        update $ updateDataFile "foo.hamlet" (BSLC.pack "invalid")
        assertOneError session
    )
  , ( "Paths in type errors (#32)"
    , withSession defaultSession $ \session -> do
        let upd = (updateSourceFile "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "f x = show . read"
                    ])
        updateSessionD session upd 1
        errs <- getSourceErrors session
        let none p = all (not . p)
        let containsFullPath e =
              "session." `isInfixOf` Text.unpack (errorMsg e)
        _fixme session "#32" $ assertBool "" (none containsFullPath errs)
    )
  , ( "Updating dependent data files multiple times per second (#134)"
    , withSession defaultSession $ \session -> do
        let cb     = \_ -> return ()
            update = flip (updateSession session) cb
            str i  = BSLC.pack $ show (i :: Int) ++ "\n"

        let mainContents = BSLC.pack $ unlines
                [ "{-# LANGUAGE TemplateHaskell #-}"
                , "import Language.Haskell.TH.Syntax"
                , "main = print ($(do"
                , "  qAddDependentFile \"foo.hamlet\""
                , "  s <- qRunIO $ readFile \"foo.hamlet\""
                , "  lift $ (read s :: Int)"
                , "  ) :: Int)"
                ]

        update $ mconcat
            [ updateSourceFile "Main.hs" mainContents
            , updateDataFile "foo.hamlet" (str 1)
            , updateCodeGeneration True
            ]
        assertNoErrors session

        do runActions <- runStmt session "Main" "main"
           (output, result) <- runWaitAll runActions
           assertEqual "" RunOk result
           assertEqual "" (str 1) output

        forM_ [2 .. 99] $ \i -> do
          update $ updateDataFile "foo.hamlet" (str i)
          runActions <- runStmt session "Main" "main"
          (output, result) <- runWaitAll runActions
          assertEqual "" RunOk result
          assertEqual "" (str i) output
    )
  , ( "Dynamically setting/unsetting -Werror (#115)"
    , withSession defaultSession $ \session -> do
        let upd1 = (updateDynamicOpts ["-Wall", "-Werror"])
                <> (updateSourceFile "src/Main.hs" . BSLC.pack $ unlines [
                       "module Main where"
                     , "main = putStrLn \"Hello 1\""
                     ])

        updateSessionD session upd1 1
        errs1 <- getSourceErrors session
        unless (any (== KindError) $ map errorKind errs1) $
          assertFailure $ "Expected some errors in " ++ show3errors errs1

        let upd2 = (updateDynamicOpts ["-Wall"])
                <> (updateSourceFile "src/Main.hs" . BSLC.pack $ unlines [
                       "module Main where"
                     , "main = putStrLn \"Hello 2\""
                     ])

        updateSessionD session upd2 1
        errs2 <- getSourceErrors session
        when (any (== KindError) $ map errorKind errs2) $
          assertFailure $ "Expected only warnings in " ++ show3errors errs2
    )
  , ( "Invalid GHC option and option warnings (#185-1) "
    , withSession defaultSession $ \session -> do
        let upd = updateDynamicOpts ["-fglasgow-exts","-thisOptionDoesNotExist"]
        updateSessionD session upd 0
        errs <- getSourceErrors session
        -- We expect two warnings (one deprecated, one unrecognized)
        assertEqual "" 2 (length errs)
    )
  , ( "GHC bug #8333 (#145)"
    , withSession defaultSession $ \session -> do
        let upd1 = (updateCodeGeneration True)
                <> (updateDynamicOpts ["-XScopedTypeVariables", "-O"])
                <> (updateSourceFile "Main.hs" . BSLC.pack $
                     "main = let (x :: String) = \"hello\" in putStrLn x")
        updateSessionD session upd1 1
        assertNoErrors session
    )
  , ( "GHC API expects 'main' to be present in 'Main' (#170)"
    , withSession defaultSession $ \session -> do
        let update = updateDataFile "Data/Monoid.hs" (BSLC.pack "module Data.Monoid where\nfoo = doesnotexist")
                     <> updateSourceFile "Main.hs" (BSLC.pack "module Main where\nimport Data.Monoid\nmain2222 = return ()")
        updateSessionD session update 2
        assertOneError session
    )
  , ( "buildExe doesn't expect 'Main.main' to be present nor to be in IO (#170)"
    , withSession defaultSession $ \session -> do
        let update = updateDataFile "Data/Monoid.hs" (BSLC.pack "module Data.Monoid where\nfoo = doesnotexist")
                     <> updateSourceFile "Main.hs" (BSLC.pack "module Main where\nimport Data.Monoid\nmain = return ()")
        updateSessionD session update 2
        assertNoErrors session
        let updE = buildExe [] [(Text.pack "Main", "Main.hs")]
        updateSessionD session updE 1
        status <- getBuildExeStatus session
        assertEqual "buildExe doesn't know 'main' is in IO"
          (Just $ ExitFailure 1) status
    )
  , ( "Data files should not leak into compilation if referenced (#169)"
    , withSession defaultSession $ \session -> do
        let update = updateDataFile "Data/Monoid.hs" (BSLC.pack "module Data.Monoid where\nfoo = doesnotexist")
                     <> updateSourceFile "Main.hs" (BSLC.pack "module Main where\nimport Data.Monoid\nmain = return ()")
        updateSessionD session update 2
        assertNoErrors session
    )
  , ( "Data files should not leak in exe building (#169)"
    , withSession defaultSession $ \session -> do
        let update = updateDataFile "Data/Monoid.hs" (BSLC.pack "module Data.Monoid where\nfoo = doesnotexist")
                     <> updateSourceFile "Main.hs" (BSLC.pack "module Main where\nimport Data.Monoid\nmain :: IO()\nmain = return ()")
        updateSessionD session update 2
        assertNoErrors session
        let updE = buildExe [] [(Text.pack "Main", "Main.hs")]
        updateSessionD session updE 2
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "" "" buildStderr
        status <- getBuildExeStatus session
        assertEqual "after exe build" (Just ExitSuccess) status

        let updDoc = buildDoc
        updateSessionD session updDoc 1
        statusDoc <- getBuildDocStatus session
        assertEqual "after doc build" (Just ExitSuccess) statusDoc
    )
  , ( "Data files should not leak in exe building (#169), with a extra modules"
    , withSession defaultSession $ \session -> do
        let update = updateDataFile "Data/Monoid.hs" (BSLC.pack "module Data.Monoid where\nfoo = doesnotexist")
                     <> updateSourceFile "Main.hs" (BSLC.pack "module Main where\nimport Data.Monoid\nmain :: IO()\nmain = return ()")
                     <> updateSourceFile "NonMain.hs" (BSLC.pack "module NonMain where\nimport Data.Monoid\nmain :: IO()\nmain = return ()")
                     <> updateSourceFile "NonMain2.hs" (BSLC.pack "module NonMain2 where\nimport Data.Monoid\nmain :: IO()\nmain = return ()")
        updateSessionD session update 4
        assertNoErrors session
        let updE = buildExe [] [(Text.pack "Main", "Main.hs")]
        updateSessionD session updE 4
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "" "" buildStderr
        status <- getBuildExeStatus session
        assertEqual "after exe build" (Just ExitSuccess) status

        let updDoc = buildDoc
        updateSessionD session updDoc 1
        statusDoc <- getBuildDocStatus session
        assertEqual "after doc build" (Just ExitSuccess) statusDoc
    )
  , ( "Data files should not leak in exe building (#169), with a non-'Main' main module"
    , withSession defaultSession $ \session -> do
        let update = updateDataFile "Data/Monoid.hs" (BSLC.pack "module Data.Monoid where\nfoo = doesnotexist")
                     <> updateSourceFile "Main.hs" (BSLC.pack "module Main where\nimport Data.Monoid\nmain :: IO()\nmain = return ()")
                     <> updateSourceFile "NonMain.hs" (BSLC.pack "module NonMain where\nimport Data.Monoid\nmain :: IO()\nmain = return ()")
                     <> updateSourceFile "NonMain2.hs" (BSLC.pack "module NonMain2 where\nimport Data.Monoid\nmain :: IO()\nmain = return ()")
        updateSessionD session update 4
        assertNoErrors session
        let updE = buildExe [] [(Text.pack "NonMain", "NonMain.hs")]
        updateSessionD session updE 4
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "" "" buildStderr
        status <- getBuildExeStatus session
        assertEqual "after exe build" (Just ExitSuccess) status

        let updDoc = buildDoc
        updateSessionD session updDoc 1
        statusDoc <- getBuildDocStatus session
        assertEqual "after doc build" (Just ExitSuccess) statusDoc
    )
  , ( "Start server without bracket (need MANUAL check that server will die; #194)"
    , do let (initParams, config) = defaultSession (defaultSessionInitParams, defaultSessionConfig)
         _session <- initSession initParams config
         putStrLn "Server started.. waiting 2sec"
         threadDelay 2000000
         putStrLn "PLEASE VERIFY THAT SERVER HAS TERMINATED"
    )
  , ( "Test NondecreasingIndentation: GHC API should fail without -XNondecreasingIndentation"
    , withSession defaultSession $ \session -> do
        let src = "module Main where\n\
                  \main = do\n\
                  \    let foo = do\n\
                  \        putStrLn \"hello\"\n\
                  \    foo"
            upd = updateSourceFile "src/Main.hs" src
        updateSessionD session upd 1
        -- The error messages are different in 7.4 and 7.8.
        assertOneError session
    )
  , ( "Test NondecreasingIndentation: buildExe should fail without -XNondecreasingIndentation"
    , withSession defaultSession $ \session -> do
        let src = "module Main where\n\
                  \main = do\n\
                  \    let foo = do\n\
                  \        putStrLn \"hello\"\n\
                  \    foo"
            upd = updateSourceFile "src/Main.hs" src
                  <> updateDynamicOpts ["-XHaskell98"]
        updateSessionD session upd 1
        assertNoErrors session

        let m = "Main"
            updE = buildExe ["-XHaskell2010"] [(m, "src/Main.hs")]
        updateSessionD session updE 1
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        -- The error messages are different in 7.4 and 7.8.
        assertEqual "buildStderr empty" False (null buildStderr)
    )
  , ( "Test NondecreasingIndentation: both should pass with -XNondecreasingIndentation"
    , withSession defaultSession $ \session -> do
        let src = "module Main where\n\
                  \main = do\n\
                  \    let foo = do\n\
                  \        putStrLn \"hello\"\n\
                  \    foo"
            upd = updateSourceFile "src/Main.hs" src
                  <> updateDynamicOpts ["-XNondecreasingIndentation"]
        updateSessionD session upd 1
        assertNoErrors session

        let m = "Main"
            updE = buildExe [] [(m, "src/Main.hs")]
        updateSessionD session updE 1
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "buildStderr empty" True (null buildStderr)
    )
  , ( "Test NondecreasingIndentation: both should pass with -XNondecreasingIndentation in SessionConfig"
    , withSession (withOpts ["-XNondecreasingIndentation"]) $ \session -> do
        let src = "module Main where\n\
                  \main = do\n\
                  \    let foo = do\n\
                  \        putStrLn \"hello\"\n\
                  \    foo"
            upd = updateSourceFile "src/Main.hs" src
        updateSessionD session upd 1
        assertNoErrors session

        let m = "Main"
            updE = buildExe [] [(m, "src/Main.hs")]
        updateSessionD session updE 1
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "buildStderr empty" True (null buildStderr)
    )
  , ( "Test NondecreasingIndentation: both should pass with -XHaskell98"
    , withSession defaultSession $ \session -> do
        let src = "module Main where\n\
                  \main = do\n\
                  \    let foo = do\n\
                  \        putStrLn \"hello\"\n\
                  \    foo"
            upd = updateSourceFile "src/Main.hs" src
                  <> updateDynamicOpts ["-XHaskell98"]
        updateSessionD session upd 1
        assertNoErrors session

        let m = "Main"
            updE = buildExe [] [(m, "src/Main.hs")]
        updateSessionD session updE 1
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "buildStderr empty" True (null buildStderr)
    )
  , ( "Test NondecreasingIndentation: both should pass with -XHaskell98 in SessionConfig"
    , withSession (withOpts ["-XHaskell98"]) $ \session -> do
        let src = "module Main where\n\
                  \main = do\n\
                  \    let foo = do\n\
                  \        putStrLn \"hello\"\n\
                  \    foo"
            upd = updateSourceFile "src/Main.hs" src
        updateSessionD session upd 1
        assertNoErrors session

        let m = "Main"
            updE = buildExe [] [(m, "src/Main.hs")]
        updateSessionD session updE 1
        distDir <- getDistDir session
        buildStderr <- readFile $ distDir </> "build/ide-backend-exe.stderr"
        assertEqual "buildStderr empty" True (null buildStderr)
    )
  , ( "Concurrent snippets 1: Run same snippet multiple times"
    , withSession defaultSession $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateStdoutBufferMode $ RunLineBuffering Nothing)
               <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import Control.Monad"
                    , "echo :: IO ()"
                    , "echo = do str <- getLine"
                    , "          replicateM_ 100 $ putStrLn str"
                    ])
        updateSessionD session upd 1
        assertNoErrors session

        -- Execute all snippets, but leave them waiting for input
        snippets <- forM ["foo", "bar", "baz", "Foo", "Bar", "Baz", "FOO", "BAR", "BAZ"] $ \str -> do
          let expectedResult = BSLC.concat (replicate 100 (BSLC.pack (str ++ "\n")))
          runActions <- runStmt session "M" "echo"
          return (runActions, str, expectedResult)

        -- Start all snippets and collect all their output concurrently
        testResults <- forM snippets $ \(runActions, str, _expectedResult) -> do
          testResult <- newEmptyMVar
          _ <- forkIO $ do
            supplyStdin runActions (BSSC.pack (str ++ "\n"))
            putMVar testResult =<< runWaitAll' runActions
          return testResult

        -- Wait for all test results, and compare against expected results
        forM_ (zip snippets testResults) $ \((_runActions, _str, expectedResult), testResult) -> do
          (output, result) <- takeMVar testResult
          assertEqual "" RunOk result
          assertEqual "" expectedResult output
    )
  , ( "Concurrent snippets 2: Execute different snippets concurrently"
    , withSession defaultSession $ \session -> do
        -- Execute all snippets, but leave them waiting for input
        snippets <- forM ["foo", "bar", "baz", "Foo", "Bar", "Baz", "FOO", "BAR", "BAZ"] $ \str -> do
          let upd = (updateCodeGeneration True)
                 <> (updateStdoutBufferMode $ RunLineBuffering Nothing)
                 <> (updateSourceFile "M.hs" . BSLC.pack . unlines $
                      [ "module M where"
                      , "import Control.Monad"
                      , "echo :: IO ()"
                      , "echo = do _waiting <- getLine"
                      , "          replicateM_ 100 $ putStrLn " ++ show str
                      ])
          updateSessionD session upd 1
          assertNoErrors session

          let expectedResult = BSLC.concat (replicate 100 (BSLC.pack (str ++ "\n")))
          runActions <- runStmt session "M" "echo"
          return (runActions, expectedResult)

        -- Start all snippets and collect all their output concurrently
        testResults <- forM snippets $ \(runActions, _expectedResult) -> do
          testResult <- newEmptyMVar
          _ <- forkIO $ do
            supplyStdin runActions (BSSC.pack "\n")
            putMVar testResult =<< runWaitAll' runActions
          return testResult

        -- Wait for all test results, and compare against expected results
        forM_ (zip snippets testResults) $ \((_runActions, expectedResult), testResult) -> do
          (output, result) <- takeMVar testResult
          assertEqual "" RunOk result
          assertEqual "" expectedResult output
    )
  , ( "Missing location information (#213)"
    , withSession defaultSession $ \session -> do
        let upd = updateSourceFile "Main.hs" $ BSLC.unlines
                [ "import DoesNotExist1"
                , "import DoesNotExist2"
                ]

        updateSession session upd print
        version <- getGhcVersion session
        case version of
          GHC742 -> -- 7.4.2 just reports the first module error
            assertSourceErrors session [
                [(Just "Main.hs", "Could not find module")]
              ]
          GHC78 -> -- 7.8 reports both; make sure we have location info (#213)
            assertSourceErrors session [
                [(Just "Main.hs", "Could not find module")]
              , [(Just "Main.hs", "Could not find module")]
              ]
    )
  , ( "Changing linker flags (#214)"
    , withSession defaultSession $ \sess -> do
        let upd =
                updateCodeGeneration True <>
                updateDynamicOpts ["-lz"] <>
                updateSourceFile "Main.hs" "import GHC.Prim" <>
                updateSourceFile "foo.c" (BSLC.unlines
                    [ "#include <zlib.h>"
                    , "int streaming_commons_inflate_init2(z_stream *stream, int window_bits) {"
                    , "return inflateInit2(stream, window_bits);}"
                    ])

        updateSession sess upd print
        assertNoErrors sess
    )
  , ( "runStmt gets corrupted by async exceptions (#219)"
    , withSession defaultSession $ \sess -> do
        updateSession sess
            (updateSourceFile "Main.hs" "main = return ()" <> updateCodeGeneration True)
            print
        assertNoErrors sess

        _ <- timeout 1 $ runStmt sess "Main" "main"

        runActions <- runStmt sess "Main" "main"
        (_output, result) <- runWaitAll runActions
        assertEqual "" RunOk result
    )
  , ( "Calling forceCancel can have detrimental side-effects (#220)"
    , withSession defaultSession $ \sess -> do
        updateSession sess
            (updateSourceFile "Main.hs" "main = return ()" <> updateCodeGeneration True)
            print
        assertNoErrors sess

        replicateM_ 100 $ do
            runActions <- runStmt sess "Main" "main"
            (_output, result) <- runWaitAll runActions
            assertEqual "" RunOk result
            forceCancel runActions
    )
  ]

runWaitAll' :: forall a. RunActions a -> IO (BSL.ByteString, a)
runWaitAll' RunActions{runWait} = go []
  where
    go :: [BSS.ByteString] -> IO (BSL.ByteString, a)
    go acc = do
      resp <- runWait
      case resp of
        Left  bs        -> go (bs : acc)
        Right runResult -> return (BSL.fromChunks (reverse acc), runResult)


{-
qsort :: IdeSessionUpdate ()
qsort = (updateSourceFile "Main.hs" . BSLC.pack . unlines $ [
          --          1         2         3         4         5
          -- 12345678901234567890123456789012345678901234567890123456
            "qsort [] = [] "
          , "qsort (a:as) = qsort left ++ [a] ++ qsort right"
          , "  where (left,right) = (filter (<=a) as, filter (>a) as)"
          , ""
          , "main = print (qsort [8, 4, 0, 3, 1, 23, 11, 18])"
          ])
     <> (updateCodeGeneration True)
-}

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










{-
assertBreak :: IdeSession
            -> String                     -- ^ Module
            -> String                     -- ^ Location
            -> String                     -- ^ Result type
            -> [(String, String, String)] -- ^ Var, type, value
            -> Assertion
assertBreak session mod loc resTy vars = do
  Just BreakInfo{..} <- getBreakInfo session
  assertEqual      "module name" mod   (Text.unpack breakInfoModule)
  assertEqual      "location"    loc   (show breakInfoSpan)
  assertAlphaEquiv "result type" resTy (Text.unpack breakInfoResultType)
  assertEqual      "number of local vars" (length vars) (length breakInfoVariableEnv)
  forM_ (zip vars breakInfoVariableEnv) $ \((var, typ, val), (var', typ', val')) -> do
    assertEqual      "var name" var (Text.unpack var')
    assertAlphaEquiv "var type" typ (Text.unpack typ')
    assertEqual      "var val"  val (Text.unpack val')
-}


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

{------------------------------------------------------------------------------
  Aux
------------------------------------------------------------------------------}


{-
-- | Mark each element of the list with booleans indicating whether it's the
-- first and/or the last element in the list
mark :: [a] -> [(a, Bool, Bool)]
mark []     = []
mark [x]    = [(x, True, True)]
mark (x:xs) = (x, True, False) : aux xs
  where
    aux []     = error "impossible"
    aux [y]    = [(y, False, True)]
    aux (y:ys) = (y, False, False) : aux ys
-}

{------------------------------------------------------------------------------
  Known problems
------------------------------------------------------------------------------}

-- | Which ghc versions are affected by these problems?
-- ([] if the bug is unrelated to the GHC version)
knownProblems :: [(String, [GhcVersion])]
knownProblems = [
    -- https://github.com/fpco/ide-backend/issues/32
    -- TODO: In 7.8 the error message does not include a filepath at all,
    -- so the error does not crop up. I don't know if this is true for _all_
    -- errors or just for this particular one (I tried a few but didn't see
    -- filepaths in any of them).
    ("#32", [GHC742])
  ]

_fixme :: IdeSession -> String -> IO () -> IO ()
_fixme session bug io = do
  version <- getGhcVersion session
  let mAllAffected = lookup bug knownProblems
      isAffected   = case mAllAffected of
                       Nothing          -> False
                       Just allAffected -> null allAffected
                                        || version `elem` allAffected

  mErr <- Ex.catch (io >> return Nothing) $ \e ->
            case Ex.fromException e of
              Just (HUnitFailure err) -> return (Just err)
              Nothing -> return (Just $ show e)

  case mErr of
    Just err -> unless isAffected $
                  Ex.throwIO . userError $ "Unexpected failure: " ++ err
    Nothing  -> when isAffected $
                  Ex.throwIO . userError $ "Unexpected success"
                                        ++ " (expected " ++ bug ++ ")"
