{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Prelude hiding (span, mod)
import Control.Concurrent (threadDelay)
import qualified Control.Exception as Ex
import Control.Monad (liftM, void, forM_)
import qualified Data.ByteString.Char8 as BSSC (pack, unpack)
import qualified Data.ByteString.Lazy.Char8 as BSLC (pack)
import qualified Data.ByteString.Lazy.UTF8 as BSL8 (fromString)
import Data.List (sort, isPrefixOf, isSuffixOf)
import qualified Data.List as List
import Data.Monoid (mconcat, mempty, (<>))
import Data.IORef (IORef, newIORef, writeIORef, readIORef)
import System.Directory
import System.Environment (getArgs)
import System.Exit (ExitCode (..))
import System.FilePath (dropExtension, (</>), takeBaseName)
import System.FilePath.Find (always, extension, find)
import System.IO.Temp (withTempDirectory)
import System.Random (randomRIO)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import qualified Data.Text as Text

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertBool, assertEqual, assertFailure, (@?=))

import IdeSession
import TestTools
import IdeSession.Debug
import IdeSession.GHC.Run (hsExtensions)
import qualified IdeSession.Strict.Map as StrictMap

-- Tests using various functions of the IdeSession API
-- and a variety of small test Haskell projects.

-- | Update the session with all modules of the given directory.
getModulesFrom :: IdeSession -> FilePath -> IO (IdeSessionUpdate, [FilePath])
getModulesFrom session originalSourcesDir = do
  sourcesDir <- getSourcesDir session
  debug dVerbosity $ "\nCopying files from: " ++ originalSourcesDir
                     ++ " to: " ++ sourcesDir
  -- Send the source files from 'originalSourcesDir' to 'configSourcesDir'
  -- using the IdeSession's update mechanism.
  originalFiles <- find always
                        ((`elem` hsExtensions) `liftM` extension)
                        originalSourcesDir
  let originalUpdate = updateCodeGeneration False
                    <> (mconcat $ map updateModuleFromFile originalFiles)
  return (originalUpdate, originalFiles)

getModules :: IdeSession -> IO (IdeSessionUpdate, [FilePath])
getModules session = do
  sourcesDir <- getSourcesDir session
  getModulesFrom session sourcesDir

loadModulesFrom :: IdeSession -> FilePath -> IO ()
loadModulesFrom session originalSourcesDir = do
  (originalUpdate, lm) <- getModulesFrom session originalSourcesDir
  updateSessionD session originalUpdate (length lm)

-- | Run the specified action with a new IDE session, configured to use a
-- temporary directory
withConfiguredSession :: [String] -> (IdeSession -> IO a) -> IO a
withConfiguredSession opts io = do
  slashTmp <- getTemporaryDirectory
  withTempDirectory slashTmp "ide-backend-test." $ \configDir -> do
    let sessionConfig = SessionConfig{ configDir
                                     , configStaticOpts = opts
                                     , configInProcess  = False
                                     , configGenerateModInfo = True
                                     }
    withSession sessionConfig io

-- | Run the specified action with a new IDE session
withSession :: SessionConfig -> (IdeSession -> IO a) -> IO a
withSession config = Ex.bracket (initSession config) shutdownSession

-- | Like 'withSession', but with a monadic configuration
withSession' :: IO SessionConfig -> (IdeSession -> IO a) -> IO a
withSession' config' io = do
  config <- config'
  withSession config io

-- Set of api calls and checks to perform on each project.
--
-- TODO: we need much more tests to recover the functionality of the old,
-- undreadable set, and then we need to much more to test all API functions.
-- E.g., check that the values of Progress do not exceeed the number of files.
-- Also, check ModuleDelete and all the DataFileChange constructors,
-- getSourceModule an getDataFile.
multipleTests :: [(String, IdeSession -> IdeSessionUpdate -> [String] -> Assertion)]
multipleTests =
  [ ( "Overwrite with error"
    , \session originalUpdate lm -> do
        updateSessionD session originalUpdate (length lm)
        msgs <- getSourceErrors session
        -- No errors in the original test code.
        assertNoErrors msgs
        -- Overwrite one of the copied files.
        (_, ms) <- getModules session
        let update = loadModule (head ms) "a = unknownX"
        updateSessionD session update 1  -- at most 1 recompiled
        msgs2 <- getSourceErrors session
        -- Error reported due to the overwrite.
        case msgs2 of
          [SourceError _ _ msg] ->
            assertEqual "" "Not in scope: `unknownX'" (Text.unpack msg)
          _ ->
            assertFailure $ "Unexpected source errors: " ++ show3errors msgs2
    )
  , ( "Overwrite with the same module name in all files"
    , \session originalUpdate lm -> do
        let upd m =
              updateModule m (BSLC.pack "module Wrong where\na = 1")
            update = originalUpdate <> mconcat (map upd lm)
        updateSessionD session update 2
        msgs <- getSourceErrors session
        if length lm >= 2
          then case msgs of
            [SourceError _ (TextSpan _) s ] ->
              assertBool "Wrong error message"
              $ isPrefixOf "module `main:Wrong' is defined in multiple files" (Text.unpack s)
            _ -> assertFailure $ "Unexpected source errors: " ++ show3errors msgs
          else assertNoErrors msgs
    )
  , ( "Overwrite modules many times"
    , \session originalUpdate lm0 -> do
        let doubleUpdate = mempty <> originalUpdate <> originalUpdate <> mempty
        -- Updates are idempotent, so no errors and no recompilation.
        updateSessionD session doubleUpdate (length lm0)
        msgs <- getSourceErrors session
        assertNoErrors msgs
        -- Overwrite one of the copied files with an error.
        (_, lm) <- getModules session
        let update1 =
              updateCodeGeneration False
              <> loadModule (head lm) "a = unknownX"
        updateSessionD session update1 1
        msgs1 <- getSourceErrors session
        case msgs1 of
          [SourceError _ _ msg] ->
            assertEqual "" "Not in scope: `unknownX'" (Text.unpack msg)
          _ ->
            assertFailure $ "Unexpected source errors: " ++ show3errors msgs1
        updateSessionD session mempty 1  -- was an error, so trying again
        msgs2 <- getSourceErrors session
        case msgs2 of
          [SourceError _ _ msg] ->
            assertEqual "" "Not in scope: `unknownX'" (Text.unpack msg)
          _ ->
            assertFailure $ "Unexpected source errors: " ++ show3errors msgs2
        -- Overwrite all files, many times, with correct code eventually.
        let upd m = loadModule m "x = unknownX"
                    <> loadModule m "y = 2"
                    <> updateCodeGeneration True
            update2 = mconcat $ map upd lm
        updateSessionD session update2 (length lm)
        msgs4 <- getSourceErrors session
        assertNoErrors msgs4
        -- Overwrite again with the error.
        updateSessionD session update1 1  -- drop bytecode, don't recompile
        msgs5 <- getSourceErrors session
        case msgs5 of
          [SourceError _ _ msg] ->
            assertEqual "" "Not in scope: `unknownX'" (Text.unpack msg)
          _ ->
            assertFailure $ "Unexpected source errors: " ++ show3errors msgs5
        assertRaises "runStmt session Main main"
          (== userError "Cannot run before the code is generated.")
          (runStmt session "Main" "main")
      )
    , ( "Run the sample code; succeed or raise an exception"
      , \session originalUpdate lm -> do
        updateSessionD session originalUpdate (length lm)
        let update = updateCodeGeneration True
        updateSessionD session update (length lm)  -- all recompiled
        msgs <- getSourceErrors session
        assertNoErrors msgs
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
              <> updateModule
                   "TotallyMain.hs"
                   (BSLC.pack
                      "module TotallyMain where\nmain = print \"test run\"")
              <> mconcat (map upd lm)
        let update2 = update <> updateCodeGeneration True
        -- Compile from scratch, generating code from the start.
        updateSessionD session update2 (length lm + 1)
        msgs <- getSourceErrors session
        assertNoErrors msgs
        runActions <- runStmt session "TotallyMain" "main"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" output (BSLC.pack "\"test run\"\n")
          RunProgException _ex ->
            assertFailure "Unexpected exception raised by the running code."
          RunGhcException ex  ->
            assertFailure $ "Manually corrected code not run successfully: "
                            ++ show ex
          RunForceCancelled ->
            assertFailure "Unexpected force-cancel"
      )
    , ( "Make sure deleting modules removes them from the directory"
      , \session originalUpdate lm -> do
        updateSessionD session originalUpdate (length lm)
        let updateDel = mconcat $ map updateModuleDelete lm
        updateSessionD session updateDel 0
        msgs <- getSourceErrors session
        assertNoErrors msgs
        -- The updates cancel each other out.
        updateSessionD session (originalUpdate <> updateDel) 0
        let update2 = updateCodeGeneration True
        updateSessionD session update2 0  -- 0: nothing to generate code from
        msgs2 <- getSourceErrors session
        assertNoErrors msgs2
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
        restartSession session
        updateSessionD session mempty (length lm)  -- all compiled anew
        msgs0 <- getSourceErrors session
        assertNoErrors msgs0
        mex2 <- Ex.try $ runStmt session "Main" "main"
        case mex2 of
          Right runActions -> void $ runWaitAll runActions  -- now runWaitAll
          Left ex -> assertEqual "runStmt" (userError "Module \"Main\" not successfully loaded, when trying to run code.") ex
        restartSession session
        let update2 = mconcat $ map updateModuleDelete lm
        updateSessionD session update2 0  -- if any file missing, would yell
        msgs <- getSourceErrors session
        assertNoErrors msgs
        let update3 = updateCodeGeneration True
        updateSessionD session update3 0  -- 0: nothing to generate code from
        exitCodeBefore <- getGhcExitCode serverBefore
        assertEqual "exitCodeBefore" (Just (ExitFailure 1)) exitCodeBefore
      )
  ]

syntheticTests :: [(String, Assertion)]
syntheticTests =
  [ ( "Maintain list of compiled modules I"
    , withConfiguredSession defOpts $ \session -> do
        let assEq name goodMods =
              assertEqual name (sort (map Text.pack goodMods))
                =<< (liftM StrictMap.keys $ getLoadedModules session)
        updateSessionD session (loadModule "XXX.hs" "a = 5") 1
        assEq "XXX" ["XXX"]
        updateSessionD session (loadModule "A.hs" "a = 5") 1
        assEq "[m1]" ["A", "XXX"]
        updateSessionD session (loadModule "A2.hs" "import A\na2 = A.a") 1
        assEq "[m1, m2]" ["A", "A2", "XXX"]
        updateSessionD session (loadModule "A3.hs" "") 1
        assEq "[m1, m2, m3]" ["A", "A2", "A3", "XXX"]
        updateSessionD session (loadModule "Wrong.hs" "import A4\na2 = A4.a + 1") 1
        assEq "wrong1" ["A", "A2", "A3", "XXX"]
        updateSessionD session (loadModule "Wrong.hs" "import A\na2 = A.a + c") 1
        assEq "wrong2" ["A", "A2", "A3", "XXX"]
        updateSessionD session (loadModule "A.hs" "a = c") 1
        -- Module "A" is compiled before "Wrong", fails, so it's invalidated
        -- and all modules that depend on it are invalidated. Module "Wrong"
        -- is never compiled.
        assEq "wrong3" ["A3", "XXX"]
    )
  , ( "Maintain list of compiled modules II"
    , withConfiguredSession defOpts $ \session -> do
        let assEq name goodMods =
              assertEqual name (sort (map Text.pack goodMods))
                =<< (liftM StrictMap.keys $ getLoadedModules session)
        updateSessionD session (loadModule "XXX.hs" "a = 5") 1
        assEq "XXX" ["XXX"]
        updateSessionD session (loadModule "A.hs" "a = 5") 1
        assEq "[m1]" ["A", "XXX"]
        updateSessionD session (loadModule "A2.hs" "import A\na2 = A.a") 1
        assEq "[m1, m2]" ["A", "A2", "XXX"]
        updateSessionD session (loadModule "A3.hs" "") 1
        assEq "[m1, m2, m3]" ["A", "A2", "A3", "XXX"]
        updateSessionD session (loadModule "Wrong.hs" "import A4\na2 = A4.a + 1") 1
        assEq "wrong1" ["A", "A2", "A3", "XXX"]
        -- This has to be disabled to get the different outcome below:
          -- updateSessionD session (loadModule m4 "import A\na2 = A.a + c") 1
          -- assEq "wrong2" [m1, m2, m3, xxx]
        -- We get this differemnt outcome both in original 7.4.2
        -- and after the GHC#7231 fix. It's probably caused by target
        -- Wrong place before or after target "A" depending on what kind
        -- of error Wrong had. This is strange, but not incorrect.
        updateSessionD session (loadModule "A.hs" "a = c") 1
        -- Module "Wrong" is compiled first here, fails, so module "A"
        -- is never comipiled, so it's not invalidated.
        assEq "wrong3" ["A", "A2", "A3", "XXX"]
    )
  , ( "Maintain list of compiled modules III"
    , withConfiguredSession defOpts $ \session -> do
        let assEq name goodMods =
              assertEqual name (sort (map Text.pack goodMods))
                =<< (liftM StrictMap.keys $ getLoadedModules session)
        updateSessionD session (loadModule "A.hs" "a = 5") 1
        assEq "1 [A]" ["A"]
        updateSessionD session (loadModule "A.hs" "a = 5 + True") 1
        assEq "1 []" []
        updateSessionD session (loadModule "A.hs" "a = 5") 1
        assEq "2 [A]" ["A"]
        updateSessionD session (loadModule "A.hs" "a = 5 + wrong") 1
        assEq "2 []" []
        updateSessionD session (loadModule "A.hs" "a = 5") 1
        assEq "3 [A]" ["A"]
        updateSessionD session (loadModule "A.hs" "import WRONG\na = 5") 1
        assEq "3 [A]; wrong imports do not unload old modules" ["A"]
        updateSessionD session (loadModule "A.hs" "a = 5 + True") 1
        assEq "3 []" []
    )
  , ( "Duplicate shutdown"
    , withConfiguredSession defOpts $ \session ->
        -- withConfiguredSession will shutdown the session as well
        shutdownSession session
    )
  , ( "Permit a session within a session and duplicated shutdownSession"
    , withConfiguredSession defOpts $ \session -> do
        loadModulesFrom session "test/ABnoError"
        config <- getSessionConfig session
        let tweakConfig :: Int -> SessionConfig -> IO SessionConfig
            tweakConfig n cfg@SessionConfig{configDir} = do
              let newDir = configDir </> "new" ++ show n
              createDirectory newDir
              return cfg { configDir = newDir }
        withSession' (tweakConfig 2 config) $ \s2 -> do
         withSession' (tweakConfig 3 config) $ \s3 -> do
          withSession' (tweakConfig 4 config) $ \_s4 -> do
           let update2 = loadModule "M.hs" "a = unknownX"
           updateSessionD s2 update2 1
           msgs2 <- getSourceErrors s2
           assertOneError msgs2
           withSession' (tweakConfig 5 config) $ \s5 -> do
            let update3 = loadModule "M.hs" "a = 3"
            updateSessionD s3 update3 1
            msgs3 <- getSourceErrors s3
            assertNoErrors msgs3
            shutdownSession s5 -- <-- duplicate "nested" shutdown
    )
  , ( "Compile a project: A depends on B, error in A"
    , withConfiguredSession defOpts $ \session -> do
        loadModulesFrom session "test/AerrorB"
        msgs <- getSourceErrors session
        case msgs of
          [] -> assertFailure $ "Missing source errors"
          [SourceError _ (ProperSpan (SourceSpan fn _ _ _ _)) s] -> do
            assertBool "Wrong file reported for the error"
              $ isSuffixOf "A.hs" fn
            assertBool "Wrong error message"
              $ isPrefixOf "No instance for (Num (IO ()))" (Text.unpack s)
          _ -> assertFailure $ "Unexpected source errors: " ++ show3errors msgs
     )
  , ( "Compile a project: A depends on B, error in B"
    , withConfiguredSession defOpts $ \session -> do
        loadModulesFrom session "test/ABerror"
        msgs <- getSourceErrors session
        case msgs of
          [] -> assertFailure $ "Missing source errors"
          [SourceError _ (ProperSpan (SourceSpan fn _ _ _ _)) s] -> do
            assertBool "Wrong file reported for the error"
              $ isSuffixOf "B.hs" fn
            assertBool "Wrong error message"
              $ isPrefixOf "No instance for (Num (IO ()))" (Text.unpack s)
          _ -> assertFailure $ "Unexpected source errors: " ++ show3errors msgs
    )
  , ( "Compile and run a project with some .lhs files"
    , withConfiguredSession defOpts $ \session -> do
        loadModulesFrom session "test/compiler/utils"
        msgs <- getSourceErrors session
        assertNoErrors msgs
        let update2 = updateCodeGeneration True
        updateSessionD session update2 3
        msgs2 <- getSourceErrors session
        assertNoErrors msgs2
        runActions <- runStmt session "Maybes" "main"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" output (BSLC.pack "False\n")
          _ -> assertFailure "Unexpected snippet run result"
    )
  , ( "Reject a program requiring -XNamedFieldPuns, then set the option"
    , let packageOpts = [ "-hide-all-packages"
                        , "-package mtl"
                        , "-package base"
                        , "-package array"
                        , "-package bytestring"
                        , "-package containers"
                        , "-package binary"
                        ]
      in withConfiguredSession packageOpts $ \session -> do
        let update = updateDataFileFromFile "EventLogFormat.h"
                                            "test/Puns/EventLogFormat.h"
        updateSessionD session update 0
        loadModulesFrom session "test/Puns"
        -- Exact errors depend on order of loaded modules, etc.
        -- Anyway, right not the test is partially bit-rotted,
        -- because the .h files are no longer available in the source dir
        -- and the data dir can't be set to the source dir. See #10.
        msgs <- getSourceErrors session
        assertSomeErrors msgs
        let punOpts = packageOpts ++ [ "-XNamedFieldPuns", "-XRecordWildCards"]
            update2 = updateGhcOptions (Just punOpts)
        (_, lm) <- getModules session
        updateSessionD session update2 (length lm)
        msgs2 <- getSourceErrors session
        assertSomeErrors msgs2
-- TODO: the hack with supplying .h as a data file no longer works;
-- we need a proper support for .h
--      assertNoErrors msgs2
    )
  , ( "Test CWD by reading a data file"
    , withConfiguredSession defOpts $ \session -> do
        let update = updateDataFile "datafile.dat"
                                    (BSLC.pack "test data content")
        updateSessionD session update 0
        let update2 = loadModule "Main.hs"
              "main = readFile \"datafile.dat\" >>= putStrLn"
        updateSessionD session update2 1
        msgs <- getSourceErrors session
        assertNoErrors msgs
        let update3 = updateCodeGeneration True
        updateSessionD session update3 1
        runActions <- runStmt session "Main" "main"
        (output, _) <- runWaitAll runActions
        assertEqual "compare test data content"
          (BSLC.pack "test data content\n") output
        let update4 = updateDataFile "datafile.dat"
                                     (BSLC.pack "new content")
                      <> update2
        updateSessionD session update4 1
        runActions2 <- runStmt session "Main" "main"
        (output2, _) <- runWaitAll runActions2
        assertEqual "compare new content"
          (BSLC.pack "new content\n") output2
    )
{- Now that we always load the RTS, we're never in this situation
  , ("Reject getSourceErrors without updateSession"
    , withConfiguredSession defOpts $ \session ->
        assertRaises "getSourceErrors session"
          (== userError "This session state does not admit queries.")
          (getSourceErrors session)
    )
-}
  , ("Reject updateSession after shutdownSession"
    , withConfiguredSession defOpts $ \session -> do
        shutdownSession session
        assertRaises "updateSessionD session mempty"
          (== userError "Session already shut down.")
          (updateSessionD session mempty 0)
    )
  , ("Reject getSourceErrors after shutdownSession"
    , withConfiguredSession defOpts $ \session -> do
        shutdownSession session
        assertRaises "getSourceErrors session"
          (== userError "Session already shut down.")
          (getSourceErrors session)
    )
  , ("Reject runStmt after shutdownSession"
    , withConfiguredSession defOpts $ \session -> do
        shutdownSession session
        assertRaises "runStmt session Main main"
          (== userError "Session already shut down.")
          (runStmt session "Main" "main")
    )
  , ( "Test recursive modules"
    , withConfiguredSession defOpts $ \session -> do
        loadModulesFrom session "test/bootMods"
        msgs <- getSourceErrors session
        -- Fails, because special support is needed, similar to .h files.
        -- Proabably, the .hs-boot files should be copied to the src dir,
        -- but not made GHC.load targets.
        assertOneError msgs
    )
  , ( "Test TH; code generation on"
    , let packageOpts = defOpts ++ ["-package template-haskell"]
      in withConfiguredSession packageOpts $ \session -> do
        (originalUpdate, lm) <- getModulesFrom session "test/TH"
        let update = originalUpdate <> updateCodeGeneration True
        updateSessionD session update (length lm)
        msgs <- getSourceErrors session
        assertNoErrors msgs
        runActions <- runStmt session "TH" "main"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" output (BSLC.pack "(True,43)\n")
          _ -> assertFailure "Unexpected snippet run result"
    )
  , ( "Test CPP: ifdefed module header"
    , let packageOpts = defOpts ++ ["-XCPP"]
      in withConfiguredSession packageOpts $ \session -> do
        let update = updateModule "M.hs" $ BSLC.pack $ unlines
              [ "#if __GLASGOW_HASKELL__ < 600"
              , "module Bad where"
              , "import Data.List"
              , "#else"
              , "module Good where"
              , "import Data.Monoid"
              , "#endif"
              , "x = mappend [] []"
              ]
        updateSessionD session update 1
        msgs <- getSourceErrors session
        assertNoErrors msgs
        cache <- getExplicitSharingCache session
        idMaps' <- getLoadedModules session
        let idMaps = removeExplicitSharing cache idMaps'
        let idMapGood = idMaps Map.! Text.pack "Good"
        assertBool "Good header accepted" $
          not $ Map.null $ idMapToMap idMapGood
        let idMapBad = Map.lookup (Text.pack "Bad") idMaps
        assertBool "Bad header ignored" $ isNothing idMapBad
    )
  , ( "Reject a wrong CPP directive"
    , let packageOpts = [ "-hide-all-packages"
                        , "-XCPP"
                        ]
      in withConfiguredSession packageOpts $ \session -> do
        let update = loadModule "M.hs" "#ifdef"
                     <> updateCodeGeneration True
        updateSessionD session update 1
        msgs <- getSourceErrors session
        -- Due to a GHC bug there are now 2 errors. TODO; when it's fixed,
        -- assert a single specific error here.
        assertSomeErrors msgs
        assertRaises "runStmt session Main main"
          (== userError "Module \"Main\" not successfully loaded, when trying to run code.")
          (runStmt session "Main" "main")
    )
  , ( "Reject a module with mangled header"
    , withConfiguredSession defOpts $ \session -> do
        let update = updateModule "M.hs"
                                  (BSLC.pack "module very-wrong where")
        updateSessionD session update 1
        msgs <- getSourceErrors session
        case msgs of
          [SourceError _ _ msg] ->
            assertEqual "" "parse error on input `very'\n" (Text.unpack msg)
          _ ->
            assertFailure $ "Unexpected source errors: " ++ show3errors msgs
        let update2 = updateModule "M.hs"
                                   (BSLC.pack "module M.1.2.3.8.T where")
        updateSessionD session update2 1
        msgs2 <- getSourceErrors session
        case msgs2 of
          [SourceError _ _ msg] ->
            assertEqual "" "parse error on input `.'\n" (Text.unpack msg)
          _ ->
            assertFailure $ "Unexpected source errors: " ++ show3errors msgs2
    )
  , ( "Interrupt runStmt (after 1 sec)"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import Control.Concurrent (threadDelay)"
                    , "loop :: IO ()"
                    , "loop = threadDelay 100000 >> loop"
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session "M" "loop"
        threadDelay 1000000
        interrupt runActions
        resOrEx <- runWait runActions
        case resOrEx of
          Right (RunProgException "AsyncException: user interrupt") -> return ()
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
    )
  , ( "Interrupt runStmt (immediately)"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import Control.Concurrent (threadDelay)"
                    , "loop :: IO ()"
                    , "loop = threadDelay 100000 >> loop"
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session "M" "loop"
        interrupt runActions
        resOrEx <- runWait runActions
        case resOrEx of
          Right (RunProgException "AsyncException: user interrupt") -> return ()
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
    )
  , ( "Interrupt runStmt (black hole; after 1 sec)"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "loop :: IO ()"
                    , "loop = loop"
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session "M" "loop"
        threadDelay 1000000
        interrupt runActions
        resOrEx <- runWait runActions
        case resOrEx of
          Right (RunProgException "AsyncException: user interrupt") -> return ()
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
    )
  , ( "Capture stdout (single putStrLn)"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "hello :: IO ()"
                    , "hello = putStrLn \"Hello World\""
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" (BSLC.pack "Hello World\n") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Capture stdout (single putStr)"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "hello :: IO ()"
                    , "hello = putStr \"Hello World\""
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" (BSLC.pack "Hello World") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Capture stdout (single putStr with delay)"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import Control.Concurrent (threadDelay)"
                    , "import System.IO"
                    , "hello :: IO ()"
                    , "hello = hSetBuffering stdout LineBuffering >> putStr \"hello\" >> threadDelay 1000000 >> putStr \"hi\""
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" (BSLC.pack "hellohi") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Capture stdout (multiple putStrLn)"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "hello :: IO ()"
                    , "hello = do putStrLn \"Hello World 1\""
                    , "           putStrLn \"Hello World 2\""
                    , "           putStrLn \"Hello World 3\""
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" (BSLC.pack "Hello World 1\nHello World 2\nHello World 3\n") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Capture stdout (mixed putStr and putStrLn)"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "hello :: IO ()"
                    , "hello = do putStrLn \"Hello World 1\""
                    , "           putStr   \"Hello World 2\""
                    , "           putStrLn \"Hello World 3\""
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" (BSLC.pack "Hello World 1\nHello World 2Hello World 3\n") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Capture stdin (simple echo process)"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "echo :: IO ()"
                    , "echo = getLine >>= putStrLn"
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session "M" "echo"
        supplyStdin runActions (BSSC.pack "ECHO!\n")
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" (BSLC.pack "ECHO!\n") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Capture stdin (infinite echo process)"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import System.IO"
                    , "import Control.Monad"
                    , "echo :: IO ()"
                    , "echo = do hSetBuffering stdout LineBuffering"
                    , "          forever $ getLine >>= putStrLn"
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session "M" "echo"

        do supplyStdin runActions (BSSC.pack "ECHO 1!\n")
           result <- runWait runActions
           assertEqual "" (Left (BSSC.pack "ECHO 1!\n")) result

        do supplyStdin runActions (BSSC.pack "ECHO 2!\n")
           result <- runWait runActions
           assertEqual "" (Left (BSSC.pack "ECHO 2!\n")) result

        do interrupt runActions
           result <- runWait runActions
           case result of
             Right (RunProgException "AsyncException: user interrupt") -> return ()
             _ -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Two calls to runStmt"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "echo :: IO ()"
                    , "echo = getLine >>= putStrLn"
                    , "echoReverse :: IO ()"
                    , "echoReverse = getLine >>= putStrLn . reverse"
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs

        do runActions <- runStmt session "M" "echo"
           supplyStdin runActions (BSSC.pack "ECHO!\n")
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "ECHO!\n") output
             _       -> assertFailure $ "Unexpected run result: " ++ show result

        do runActions <- runStmt session "M" "echoReverse"
           supplyStdin runActions (BSSC.pack "!OHCE\n")
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "ECHO!\n") output
             _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Make sure we can terminate the IDE session when code is running"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "echo :: IO ()"
                    , "echo = (getLine >>= putStrLn) >> echo"
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        _runActions <- runStmt session "M" "echo"
        return ()
     )
  , ( "Capture stderr"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import System.IO"
                    , "hello :: IO ()"
                    , "hello = hPutStrLn stderr \"Hello World\""
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" (BSLC.pack "Hello World\n") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Merge stdout and stderr"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import System.IO"
                    , "hello :: IO ()"
                    , "hello = do hPutStrLn stderr \"Hello World 1\""
                    , "           hPutStrLn stdout \"Hello World 2\""
                    , "           hPutStr   stderr \"Hello World 3\""
                    , "           hPutStr   stdout \"Hello World 4\""
                    , "           hPutStrLn stderr \"Hello World 5\""
                    , "           hPutStrLn stdout \"Hello World 6\""
                    , "           hPutStr   stderr \"Hello World 7\""
                    , "           hPutStr   stdout \"Hello World 8\""
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        let expectedOutput = "Hello World 1\n"
                          ++ "Hello World 2\n"
                          ++ "Hello World 3"
                          ++ "Hello World 4"
                          ++ "Hello World 5\n"
                          ++ "Hello World 6\n"
                          ++ "Hello World 7"
                          ++ "Hello World 8"
        case result of
          RunOk _ -> assertEqual "" (BSLC.pack expectedOutput) output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Set environment variables"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import System.Environment (getEnv)"
                    , "printFoo :: IO ()"
                    , "printFoo = getEnv \"Foo\" >>= putStr"
                    , "printBar :: IO ()"
                    , "printBar = getEnv \"Bar\" >>= putStr"
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs

        -- At the start, both Foo and Bar are undefined
        do runActions <- runStmt session "M" "printFoo"
           (_, result) <- runWaitAll runActions
           case result of
             RunProgException ex -> assertEqual "" ex "IOException: Foo: getEnv: does not exist (no environment variable)"
             _ -> assertFailure $ "Unexpected result " ++ show result
        do runActions <- runStmt session "M" "printBar"
           (_, result) <- runWaitAll runActions
           case result of
             RunProgException ex -> assertEqual "" ex "IOException: Bar: getEnv: does not exist (no environment variable)"
             _ -> assertFailure $ "Unexpected result " ++ show result

        -- Update Foo, leave Bar undefined
        updateSession session (updateEnv "Foo" (Just "Value1")) (\_ -> return ())
        do runActions <- runStmt session "M" "printFoo"
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "Value1") output
             _       -> assertFailure $ "Unexpected result " ++ show result
        do runActions <- runStmt session "M" "printBar"
           (_, result) <- runWaitAll runActions
           case result of
             RunProgException ex -> assertEqual "" ex "IOException: Bar: getEnv: does not exist (no environment variable)"
             _ -> assertFailure $ "Unexpected result " ++ show result

        -- Update Bar, leave Foo defined
        updateSession session (updateEnv "Bar" (Just "Value2")) (\_ -> return ())
        do runActions <- runStmt session "M" "printFoo"
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "Value1") output
             _       -> assertFailure $ "Unexpected result " ++ show result
        do runActions <- runStmt session "M" "printBar"
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "Value2") output
             _       -> assertFailure $ "Unexpected result " ++ show result

        -- Unset Foo, leave Bar defined
        updateSession session (updateEnv "Foo" Nothing) (\_ -> return ())
        do runActions <- runStmt session "M" "printFoo"
           (_, result) <- runWaitAll runActions
           case result of
             RunProgException ex -> assertEqual "" ex "IOException: Foo: getEnv: does not exist (no environment variable)"
             _ -> assertFailure $ "Unexpected result " ++ show result
        do runActions <- runStmt session "M" "printBar"
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "Value2") output
             _       -> assertFailure $ "Unexpected result " ++ show result
    )
  , ( "Update during run"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "loop :: IO ()"
                    , "loop = loop"
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        _runActions <- runStmt session "M" "loop"
        assertRaises ""
          (== userError "Cannot update session in running mode")
          (updateSessionD session upd 1)
    )
  , ( "getSourceErrors during run"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "{-# OPTIONS_GHC -Wall #-}"
                    , "module M where"
                    , "loop = loop"
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        case msgs of
          -- We expect a 'top-level identifier without type' warning
          [SourceError KindWarning _ _] -> return ()
          _ -> assertFailure $ "Unexpected source errors: " ++ show3errors msgs
        _runActions <- runStmt session "M" "loop"
        msgs' <- getSourceErrors session
        assertEqual "Running code does not affect getSourceErrors" msgs msgs'
    )
  , ( "getLoadedModules during run"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "{-# OPTIONS_GHC -Wall #-}"
                    , "module M where"
                    , "loop = loop"
                    ])
        updateSessionD session upd 1
        mods <- liftM StrictMap.keys $ getLoadedModules session
        assertEqual "" [Text.pack "M"] mods
        _runActions <- runStmt session "M" "loop"
        mods' <- liftM StrictMap.keys $ getLoadedModules session
        assertEqual "Running code does not affect getLoadedModules" mods mods'
    )
  , ( "Interrupt, then capture stdout"
    , withConfiguredSession defOpts $ \session -> do
        updateSession session (updateCodeGeneration True) (\_ -> return ())
        let upd1 = updateModule "Main.hs" . BSLC.pack . unlines $
                     [ "import Control.Monad"
                     , "main = forever $ print 1"
                     ]
            upd2 = updateModule "Main.hs" . BSLC.pack . unlines $
                     [ "main = print 1234" ]

        do updateSessionD session upd1 1
           runActions <- runStmt session "Main" "main"
           interrupt runActions
           randomRIO (0, 1000000) >>= threadDelay -- Wait between 0 and 1sec
           void $ runWaitAll runActions

        do updateSessionD session upd2 1
           runActions <- runStmt session "Main" "main"
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "1234\n") output
             _       -> assertFailure $ "Unexpected result: " ++ show result
    )
  , ( "Restart session (snippet doesn't swallow exceptions; after .1 sec)"
    , restartRun [ "module M where"
                 , "loop :: IO ()"
                 , "loop = loop"
                 ] (ExitFailure 1)
    )
  , ( "Restart session (snippet swallows all exceptions; after .1 sec)"
    , restartRun [ "module M where"
                 , ""
                 , "import qualified Control.Exception as Ex"
                 , "import Control.Concurrent (threadDelay)"
                 , ""
                 , "innerLoop :: IO ()"
                 , "innerLoop = threadDelay 10000 >> innerLoop"
                 , ""
                 , "loop :: IO ()"
                 , "loop = Ex.catch innerLoop $ \\e -> let _ = e :: Ex.SomeException in loop"
                 ] (ExitFailure 1)
    )
  , ( "Restart session (black hole, swallow all exceptions; after .1 sec)"
    , restartRun [ "module M where"
                 , ""
                 , "import qualified Control.Exception as Ex"
                 , "import Control.Concurrent (threadDelay)"
                 , ""
                 , "innerLoop :: IO ()"
                 , "innerLoop = innerLoop"
                 , ""
                 , "loop :: IO ()"
                 , "loop = Ex.catch innerLoop $ \\e -> let _ = e :: Ex.SomeException in loop"
                 ] (ExitFailure 1)
    )
  , ( "Restart session (evil snippet with infinite stack of exception handlers; after .1 sec)"
    , restartRun [ "module M where"
                 , ""
                 , "import qualified Control.Exception as Ex"
                 , ""
                 , "loop :: IO ()"
                 , "loop = Ex.catch loop $ \\e -> let _ = e :: Ex.SomeException in loop"
                 ] (ExitFailure 1)
    )
  , ( "Make sure environment is restored after session restart"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import System.Environment (getEnv)"
                    , "printFoo :: IO ()"
                    , "printFoo = getEnv \"Foo\" >>= putStr"
                    ])

        -- Set environment
        updateSession session (updateEnv "Foo" (Just "Value1")) (\_ -> return ())

        -- Compile and run the code on the first server
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertNoErrors msgs
        do runActions <- runStmt session "M" "printFoo"
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "Value1") output
             _       -> assertFailure $ "Unexpected result " ++ show result

        -- Start a new server
        serverBefore <- getGhcServer session
        restartSession session

        -- Compile the code on the new server
        updateSessionD session upd 1
        msgs2 <- getSourceErrors session
        assertNoErrors msgs2

        -- Make sure the old server exited
        exitCodeBefore <- getGhcExitCode serverBefore
        assertEqual "exitCodeBefore" (Just (ExitFailure 1)) exitCodeBefore

        -- Make sure the new server is still alive
        serverAfter <- getGhcServer session
        exitCodeAfter <- getGhcExitCode serverAfter
        assertEqual "exitCodeAfter" Nothing exitCodeAfter

        -- Make sure environment is restored
        do runActions <- runStmt session "M" "printFoo"
           (output, result) <- runWaitAll runActions
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "Value1") output
             _       -> assertFailure $ "Unexpected result " ++ show result
    )
  , ( "Buffer modes: RunNoBuffering"
    , testBufferMode RunNoBuffering
    )
  , ( "Buffer modes: RunLineBuffering, no timeout"
    , testBufferMode (RunLineBuffering Nothing)
    )
  , ( "Buffer modes: RunBlockBuffering, no timeout"
    , testBufferMode (RunBlockBuffering (Just 5) Nothing)
    )
  , ( "Buffer modes: RunLineBuffering, with timeout"
    , testBufferMode (RunLineBuffering (Just 1000000))
    )
  , ( "Buffer modes: RunBlockBuffering, with timeout"
    , testBufferMode (RunBlockBuffering (Just 4) (Just 1000000))
    )
  , ( "Buffer modes: RunBlockBuffering, buffer never fills, with timeout"
    , testBufferMode (RunBlockBuffering (Just 4096) (Just 1000000))
    )
  , ( "Use relative path in SessionConfig"
    , do withTempDirectory "." "ide-backend-test." $ \fullPath -> do
           relativePath <- makeRelativeToCurrentDirectory fullPath
           let sessionConfig = SessionConfig{ configDir        = relativePath
                                            , configStaticOpts = defOpts
                                            , configInProcess  = False
                                            , configGenerateModInfo = True
                                            }
           withSession sessionConfig $ \session -> do
             let upd = (updateCodeGeneration True)
                    <> (updateModule "M.hs" . BSLC.pack . unlines $
                         [ "module M where"
                         , "hello :: IO ()"
                         , "hello = putStr \"Hello World\""
                         ])
             updateSessionD session upd 1
             msgs <- getSourceErrors session
             assertEqual "This should compile without errors" [] msgs
             runActions <- runStmt session "M" "hello"
             (output, result) <- runWaitAll runActions
             case result of
               RunOk _ -> assertEqual "" (BSLC.pack "Hello World") output
               _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Call runWait after termination (normal termination)"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "hello :: IO ()"
                    , "hello = putStrLn \"Hello World\""
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" (BSLC.pack "Hello World\n") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
        result' <- runWait runActions
        case result' of
          Right (RunOk _) -> return ()
          _ -> assertFailure $ "Unexpected run result in repeat call: " ++ show result'
    )
  , ( "Call runWait after termination (interrupted)"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import Control.Concurrent (threadDelay)"
                    , "loop :: IO ()"
                    , "loop = threadDelay 100000 >> loop"
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session "M" "loop"
        threadDelay 1000000
        interrupt runActions
        resOrEx <- runWait runActions
        case resOrEx of
          Right (RunProgException "AsyncException: user interrupt") -> return ()
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
        resOrEx' <- runWait runActions
        case resOrEx' of
          Right (RunProgException "AsyncException: user interrupt") -> return ()
          _ -> assertFailure $ "Unexpected run result in repeat call: " ++ show resOrEx'
    )
  , ( "Call runWait after termination (restarted session)"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import Control.Concurrent (threadDelay)"
                    , "loop :: IO ()"
                    , "loop = threadDelay 100000 >> loop"
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session "M" "loop"
        threadDelay 1000000
        restartSession session
        resOrEx <- runWait runActions
        case resOrEx of
          Right RunForceCancelled -> return ()
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx
        resOrEx' <- runWait runActions
        case resOrEx' of
          Right RunForceCancelled -> return ()
          _ -> assertFailure $ "Unexpected run result in repeat call: " ++ show resOrEx'
    )
  , ( "Call runWait after termination (started new snippet in meantime)"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                    [ "module M where"
                    , "import Control.Concurrent"
                    , "hello :: IO ()"
                    , "hello = putStrLn \"Hello World\""
                    , "slowHello :: IO ()"
                    , "slowHello = threadDelay 2000000 >> putStrLn \"Oh, hello\""
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs

        -- Start first snippet and wait for it to terminate
        runActions1 <- runStmt session "M" "hello"
        do (output, result) <- runWaitAll runActions1
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "Hello World\n") output
             _       -> assertFailure $ "Unexpected run result: " ++ show result

        -- Start second snippet
        runActions2 <- runStmt session "M" "slowHello"

        -- While it is running, call runWait again on the old runActions, make
        -- sure it's still the same
        do result <- runWait runActions1
           case result of
             Right (RunOk _) -> return ()
             _ -> assertFailure $ "Unexpected run result in repeat call: " ++ show result

        -- Make sure that a call to 'runStmt' throws an exception
        -- (because we are still in running state)
        assertRaises "runStmt during running code"
          (== userError "Cannot run code concurrently")
          (runStmt session "M" "hello")

        -- Now call runWait on the *new* runActions and make sure we
        -- get the right result
        do (output, result) <- runWaitAll runActions2
           case result of
             RunOk _ -> assertEqual "" (BSLC.pack "Oh, hello\n") output
             _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Don't recompile unnecessarily (single module)"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "a :: IO ()"
                    , "a = print 'a'"
                    ])

        counter <- newCounter

        updateSession session upd (\_ -> incCounter counter)
        assertCounter counter 1

        resetCounter counter
        updateSession session upd (\_ -> incCounter counter)
        assertCounter counter 0
    )
  , ( "Don't recompile unnecessarily (A depends on B)"
    , withConfiguredSession defOpts $ \session -> do
        -- 'updA' is defined so that the interface of 'updA n' is different
        -- to the interface of 'updA m' (with n /= m)
        let updA n = updateModule "A.hs" . BSLC.pack . unlines $
                       [ "module A where"
                       , "import B"
                       ]
                      ++
                       [ "a" ++ show i ++ " = b" ++ show i
                       | i <- [0 .. n :: Int]
                       ]
        let updB n = updateModule "B.hs" . BSLC.pack . unlines $
                       [ "module B where"
                       ]
                      ++
                       [ "b" ++ show i ++ " = return () :: IO ()"
                       | i <- [0 .. n :: Int]
                       ]
        let upd = updateCodeGeneration True <> updA 0 <> updB 0

        counter <- newCounter

        -- Initial compilation needs to recompile for A and B
        updateSession session upd (\_ -> incCounter counter)
        assertCounter counter 2

        -- Overwriting B with the same code requires no compilation at all
        resetCounter counter
        updateSession session (updB 0) (\_ -> incCounter counter)
        assertCounter counter 0

        -- Nor does overwriting A with the same code
        resetCounter counter
        updateSession session (updA 0) (\_ -> incCounter counter)
        assertCounter counter 0

        -- Giving B a new interface means both A and B need to be recompiled
        resetCounter counter
        updateSession session (updB 1) (\_ -> incCounter counter)
        assertCounter counter 2

        -- Changing the interface of A only requires recompilation of A
        resetCounter counter
        updateSession session (updA 1) (\_ -> incCounter counter)
        assertCounter counter 1
    )
  , ( "First snippet closes stdin; next snippet unaffected"
    , withConfiguredSession defOpts $ \session -> do
        let updates2 = mconcat
                [ updateCodeGeneration True
                , updateModule "Main.hs" (BSLC.pack "import System.IO\nmain = hClose stdin")
                ]
        updateSession session updates2 $ const $ return ()
        ra2 <- runStmt session "Main" "main"
        out2b <- runWait ra2
        case out2b of
          Right (RunOk _) -> return ()
          _ -> assertFailure $ "Unexpected result " ++ show out2b

        let updates3 =
              updateModule "Main.hs" (BSLC.pack "main = getLine >>= putStrLn")
        updateSession session updates3 $ const $ return ()
        ra3 <- runStmt session "Main" "main"
        supplyStdin ra3 (BSSC.pack "Michael\n")
        (output, out3b) <- runWaitAll ra3
        case out3b of
          RunOk _ -> assertEqual "" (BSLC.pack "Michael\n") output
          _ -> assertFailure $ "Unexpected result " ++ show out3b
    )
  , ( "First snippet closes stdin (interrupted 'interact'); next snippet unaffected"
    , withConfiguredSession defOpts $ \session -> do
        let updates2 = mconcat
                [ updateCodeGeneration True
                , updateModule "Main.hs" (BSLC.pack "main = getContents >>= putStr")
                , updateStdoutBufferMode $ RunLineBuffering Nothing
                ]
        updateSession session updates2 $ const $ return ()
        ra2 <- runStmt session "Main" "main"
        supplyStdin ra2 (BSSC.pack "hello\n")
        out2a <- runWait ra2
        out2a @?= Left (BSSC.pack "hello\n")
        interrupt ra2
        out2b <- runWait ra2
        out2b @?= Right (RunProgException "AsyncException: user interrupt")

        let updates3 = mconcat
                [ updateCodeGeneration True
                , updateModule "Main.hs" (BSLC.pack "main = putStrLn \"Hi!\" >> getLine >> return ()")
                , updateStdoutBufferMode $ RunLineBuffering Nothing
                ]
        updateSession session updates3 $ const $ return ()
        ra3 <- runStmt session "Main" "main"
        out3a <- runWait ra3
        out3a @?= Left (BSSC.pack "Hi!\n")
        supplyStdin ra3 (BSSC.pack "Michael\n")
        out3b <- runWait ra3
        case out3b of
          Right (RunOk _) -> return ()
          _ -> assertFailure $ "Unexpected result " ++ show out3b
    )
  , ( "First snippet closes stdout; next snippet unaffected"
    , withConfiguredSession defOpts $ \session -> do
        let updates2 = mconcat
                [ updateCodeGeneration True
                , updateModule "Main.hs" (BSLC.pack "import System.IO\nmain = hClose stdout")
                ]
        updateSession session updates2 $ const $ return ()
        ra2 <- runStmt session "Main" "main"
        out2b <- runWait ra2
        case out2b of
          Right (RunOk _) -> return ()
          _ -> assertFailure $ "Unexpected result " ++ show out2b

        let updates3 =
              updateModule "Main.hs" (BSLC.pack "main = getLine >>= putStrLn")
        updateSession session updates3 $ const $ return ()
        ra3 <- runStmt session "Main" "main"
        supplyStdin ra3 (BSSC.pack "Michael\n")
        (output, out3b) <- runWaitAll ra3
        case out3b of
          RunOk _ -> assertEqual "" (BSLC.pack "Michael\n") output
          _ -> assertFailure $ "Unexpected result " ++ show out3b
    )
  , ( "First snippet closes stderr; next snippet unaffected"
    , withConfiguredSession defOpts $ \session -> do
        let updates2 = mconcat
                [ updateCodeGeneration True
                , updateModule "Main.hs" (BSLC.pack "import System.IO\nmain = hClose stderr")
                ]
        updateSession session updates2 $ const $ return ()
        ra2 <- runStmt session "Main" "main"
        out2b <- runWait ra2
        case out2b of
          Right (RunOk _) -> return ()
          _ -> assertFailure $ "Unexpected result " ++ show out2b

        let updates3 =
              updateModule "Main.hs" (BSLC.pack "import System.IO\nmain = getLine >>= hPutStrLn stderr")
        updateSession session updates3 $ const $ return ()
        ra3 <- runStmt session "Main" "main"
        supplyStdin ra3 (BSSC.pack "Michael\n")
        (output, out3b) <- runWaitAll ra3
        case out3b of
          RunOk _ -> assertEqual "" (BSLC.pack "Michael\n") output
          _ -> assertFailure $ "Unexpected result " ++ show out3b
    )
  , ( "Snippet closes stderr, using timeout buffering"
    , withConfiguredSession defOpts $ \session -> do
        let upd = mconcat [
                      updateCodeGeneration True
                    , updateStdoutBufferMode $ RunLineBuffering Nothing
                    , updateStderrBufferMode $ RunBlockBuffering (Just 4096) (Just 250000)
                    , updateModule "Main.hs" . BSLC.pack . unlines $ [
                          "import Control.Concurrent"
                        , "import Control.Monad"
                        , "import System.IO"
                        , "main :: IO ()"
                        , "main = do"
                        , "  hClose stderr"
                        , "  forM_ [1 :: Int .. 3] $ \\i -> do"
                        , "    print i"
                        , "    threadDelay 500000"
                        ]
                    ]
        updateSessionD session upd 1
        ra <- runStmt session "Main" "main"
        forM_ [1 :: Int .. 3] $ \i -> do
          result <- runWait ra
          result @?= Left (BSSC.pack $ show i ++ "\n")

        finalResult <- runWait ra
        case finalResult of
          Right (RunOk _) -> return ()
          _ -> assertFailure $ "Unexpected result " ++ show finalResult
     )
  , ( "Make sure encoding is UTF8"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSL8.fromString . unlines $
                    [ "module M where"
                    , "hello :: IO ()"
                    , "hello = putStrLn \"你好\""
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" (BSL8.fromString "你好\n") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Using something from a different package (no \"Loading package\" msg)"
      -- We pick something from the haskell platform but that doesn't come with ghc itself
      -- https://github.com/haskell/haskell-platform/blob/2012.4.0.0/haskell-platform.cabal
    , withConfiguredSession [] $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSL8.fromString . unlines $
                    [ "module M where"
                    , "import Control.Monad.IO.Class" -- From transformers
                    , "hello :: IO ()"
                    , "hello = liftIO $ print 5"
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" (BSL8.fromString "5\n") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
    )
  , ( "Using the FFI (expected failure)"
    , withConfiguredSession defOpts $ \session -> do
        let upd = mconcat [
                updateCodeGeneration True
              , updateModuleFromFile "test/FFI/Main.hs"
              ]
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertOneError msgs
        {-
        assertEqual "This should compile without errors" [] msgs
        runActions <- runStmt session "M" "hello"
        (output, result) <- runWaitAll runActions
        case result of
          RunOk _ -> assertEqual "" (BSL8.fromString "5\n") output
          _       -> assertFailure $ "Unexpected run result: " ++ show result
        -}
    )
  , ( "Type information 1: Local identifiers and Prelude"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "a = (5 :: Int)"
                    , "b = a + 6"
                    , "c = True"
                    , "d = foldr"
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        cache <- getExplicitSharingCache session
        idMaps' <- getLoadedModules session
        let idMaps = removeExplicitSharing cache idMaps'
        let idMapA = idMaps Map.! Text.pack "A"
        let expectedIdMapA = [
                "(A.hs@2:1-2:2,a (VarName) :: GHC.Types.Int (binding occurrence))"
              , "(A.hs@3:1-3:2,b (VarName) :: GHC.Types.Int (binding occurrence))"
              , "(A.hs@3:5-3:6,a (VarName) :: GHC.Types.Int (defined at A.hs@2:1-2:2))"
              , "(A.hs@3:7-3:8,+ (VarName) :: forall a. GHC.Num.Num a => a -> a -> a (defined in base-4.5.1.0:GHC.Num at <no location info>; imported from base-4.5.1.0:Prelude at A.hs@1:8-1:9))"
              , "(A.hs@4:1-4:2,c (VarName) :: GHC.Types.Bool (binding occurrence))"
              -- TODO: We should have a type for True
              , "(A.hs@4:5-4:9,True (DataName) (wired in to the compiler))"
              , "(A.hs@5:1-5:2,d (VarName) :: forall a b. (a -> b -> b) -> b -> [a] -> b (binding occurrence))"
              , "(A.hs@5:5-5:10,foldr (VarName) :: forall a1 b1. (a1 -> b1 -> b1) -> b1 -> [a1] -> b1 (defined in base-4.5.1.0:GHC.Base at <no location info>; imported from base-4.5.1.0:Prelude at A.hs@1:8-1:9))"
              ]
        let actualIdMapA = lines (show idMapA)
        assertSameSet "actualIdMapA" expectedIdMapA actualIdMapA
        {- TODO: reenable
        assertEqual "Haddock link for A.b should be correct"
                    "main/latest/doc/html/A.html#v:b" $
                    haddockLink (idMapToMap idMapB Map.! SourceSpan "B.hs" 5 8 5 9)
        -}
    )
  , ( "Type information 2: Simple ADTs"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "data T = MkT"
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        cache <- getExplicitSharingCache session
        idMaps' <- getLoadedModules session
        let idMaps = removeExplicitSharing cache idMaps'
        let idMapA = idMaps Map.! Text.pack "A"
        let expectedIdMapA = [
                "(A.hs@2:10-2:13,MkT (DataName) :: A.T (binding occurrence))"
              , "(A.hs@2:6-2:7,T (TcClsName) (binding occurrence))"
              ]
        let actualIdMapA = lines (show idMapA)
        assertSameSet "actualIdMapA" expectedIdMapA actualIdMapA
    )
  , ( "Type information 3: Polymorphism"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "data TMaybe a = TNothing | TJust a"
                    , ""
                    , "f1 x = x"
                    , "f2 = \\x -> x"
                    , ""
                    , "g1 x y = x"
                    , "g2 = \\x y -> x"
                    , ""
                    , "h1 = h1go True False"
                    , "  where"
                    , "    h1go x y = x"
                    , ""
                    , "h2 = h2go True False"
                    , "  where"
                    , "    h2go = \\x y -> x"
                    ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        cache <- getExplicitSharingCache session
        idMaps' <- getLoadedModules session
        let idMaps = removeExplicitSharing cache idMaps'
        let idMapA = idMaps Map.! Text.pack "A"
        let expectedIdMapA = [
                "(A.hs@2:13-2:14,a (TvName) (binding occurrence))"
              , "(A.hs@2:17-2:25,TNothing (DataName) :: forall a. A.TMaybe a (binding occurrence))"
              , "(A.hs@2:28-2:33,TJust (DataName) :: forall a. a -> A.TMaybe a (binding occurrence))"
              , "(A.hs@2:34-2:35,a (TvName) (defined at A.hs@2:13-2:14))"
              , "(A.hs@2:6-2:12,TMaybe (TcClsName) (binding occurrence))"
              , "(A.hs@4:1-4:3,f1 (VarName) :: forall t. t -> t (binding occurrence))"
              , "(A.hs@4:4-4:5,x (VarName) :: t (binding occurrence))"
              , "(A.hs@4:8-4:9,x (VarName) :: t (defined at A.hs@4:4-4:5))"
              , "(A.hs@5:1-5:3,f2 (VarName) :: forall t. t -> t (binding occurrence))"
              , "(A.hs@5:12-5:13,x (VarName) :: t (defined at A.hs@5:7-5:8))"
              , "(A.hs@5:7-5:8,x (VarName) :: t (binding occurrence))"
              , "(A.hs@7:1-7:3,g1 (VarName) :: forall t t1. t -> t1 -> t (binding occurrence))"
              , "(A.hs@7:10-7:11,x (VarName) :: t (defined at A.hs@7:4-7:5))"
              , "(A.hs@7:4-7:5,x (VarName) :: t (binding occurrence))"
              , "(A.hs@7:6-7:7,y (VarName) :: t1 (binding occurrence))"
              , "(A.hs@8:1-8:3,g2 (VarName) :: forall t t1. t -> t1 -> t (binding occurrence))"
              , "(A.hs@8:14-8:15,x (VarName) :: t (defined at A.hs@8:7-8:8))"
              , "(A.hs@8:7-8:8,x (VarName) :: t (binding occurrence))"
              , "(A.hs@8:9-8:10,y (VarName) :: t1 (binding occurrence))"
              , "(A.hs@10:1-10:3,h1 (VarName) :: GHC.Types.Bool (binding occurrence))"
              , "(A.hs@10:11-10:15,True (DataName) (wired in to the compiler))"
              , "(A.hs@10:16-10:21,False (DataName) (wired in to the compiler))"
              , "(A.hs@10:6-10:10,h1go (VarName) :: forall t t1. t -> t1 -> t (defined at A.hs@12:5-12:9))"
              , "(A.hs@12:10-12:11,x (VarName) :: t (binding occurrence))"
              , "(A.hs@12:12-12:13,y (VarName) :: t1 (binding occurrence))"
              , "(A.hs@12:16-12:17,x (VarName) :: t (defined at A.hs@12:10-12:11))"
              , "(A.hs@12:5-12:9,h1go (VarName) :: forall t t1. t -> t1 -> t (binding occurrence))"
              , "(A.hs@14:1-14:3,h2 (VarName) :: GHC.Types.Bool (binding occurrence))"
              , "(A.hs@14:11-14:15,True (DataName) (wired in to the compiler))"
              , "(A.hs@14:16-14:21,False (DataName) (wired in to the compiler))"
              , "(A.hs@14:6-14:10,h2go (VarName) :: forall t t1. t -> t1 -> t (defined at A.hs@16:5-16:9))"
              , "(A.hs@16:13-16:14,x (VarName) :: t (binding occurrence))"
              , "(A.hs@16:15-16:16,y (VarName) :: t1 (binding occurrence))"
              , "(A.hs@16:20-16:21,x (VarName) :: t (defined at A.hs@16:13-16:14))"
              , "(A.hs@16:5-16:9,h2go (VarName) :: forall t t1. t -> t1 -> t (binding occurrence))"
              ]
        let actualIdMapA = lines (show idMapA)
        assertSameSet "actualIdMapA" expectedIdMapA actualIdMapA
    )
  , ( "Type information 4: Multiple modules"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "data T = MkT"
                    ])
                    -- Make sure that an occurrence of MkT in a second module
                    -- doesn't cause us to lose type information we learned
                    -- while processing the first
               <> (updateModule "B.hs" . BSLC.pack . unlines $
                    [ "module B where"
                    , "import A"
                    , "foo = MkT"
                    ])
        updateSessionD session upd 2
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        cache <- getExplicitSharingCache session
        idMaps' <- getLoadedModules session
        let idMaps = removeExplicitSharing cache idMaps'
        let idMapA = idMaps Map.! Text.pack "A"
        let idMapB = idMaps Map.! Text.pack "B"
        let expectedIdMapA = [
                "(A.hs@2:10-2:13,MkT (DataName) :: A.T (binding occurrence))"
              , "(A.hs@2:6-2:7,T (TcClsName) (binding occurrence))"
              ]
        let expectedIdMapB = [
                "(B.hs@3:1-3:4,foo (VarName) :: A.T (binding occurrence))"
              , "(B.hs@3:7-3:10,MkT (DataName) :: A.T (defined in main:A at A.hs@2:10-2:13; imported from main:A at B.hs@2:1-2:9))"
              ]
        let actualIdMapA = lines (show idMapA)
        let actualIdMapB = lines (show idMapB)
        assertSameSet "actualIdMapA" expectedIdMapA actualIdMapA
        assertSameSet "actualIdMapB" expectedIdMapB actualIdMapB
    )
  , ( "Type information 5: External packages, type sigs, scoped type vars, kind sigs"
    , let opts = defOpts ++ [
                     "-package parallel"
                   , "-XScopedTypeVariables"
                   , "-XKindSignatures"
                   ]
      in withConfiguredSession opts $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"

                    , "import Control.Parallel"

                    , "e = True `pseq` False"

                    , "f :: a -> a"
                    , "f x = x"

                    , "g :: forall a. a -> a"
                    , "g x = x"

                    , "h :: forall a. a -> a"
                    , "h x = y"
                    , "  where"
                    , "    y :: a"
                    , "    y = x"

                    , "i :: forall (t :: * -> *) a. t a -> t a"
                    , "i x = x"
                    ])
        updateSessionD session upd 2
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        cache <- getExplicitSharingCache session
        idMaps' <- getLoadedModules session
        let idMaps = removeExplicitSharing cache idMaps'
        let idMapA = idMaps Map.! Text.pack "A"
        let expectedIdMapA = [
                "(A.hs@3:1-3:2,e (VarName) :: GHC.Types.Bool (binding occurrence))"
              , "(A.hs@3:10-3:16,pseq (VarName) :: forall a b. a -> b -> b (defined in parallel-3.2.0.3:Control.Parallel at <no location info>; imported from parallel-3.2.0.3:Control.Parallel at A.hs@2:1-2:24))"
              , "(A.hs@3:17-3:22,False (DataName) (wired in to the compiler))"
              , "(A.hs@3:5-3:9,True (DataName) (wired in to the compiler))"
              , "(A.hs@4:1-4:2,f (VarName) :: forall a. a -> a (defined at A.hs@5:1-5:2))"
              , "(A.hs@4:11-4:12,a (TvName) (defined at A.hs@4:6-4:7))"
              , "(A.hs@4:6-4:7,a (TvName) (defined at A.hs@4:6-4:7))"
              , "(A.hs@5:1-5:2,f (VarName) :: forall a. a -> a (binding occurrence))"
              , "(A.hs@5:3-5:4,x (VarName) :: a (binding occurrence))"
              , "(A.hs@5:7-5:8,x (VarName) :: a (defined at A.hs@5:3-5:4))"
              , "(A.hs@6:1-6:2,g (VarName) :: forall a. a -> a (defined at A.hs@7:1-7:2))"
              , "(A.hs@6:13-6:14,a (TvName) (binding occurrence))"
              , "(A.hs@6:16-6:17,a (TvName) (defined at A.hs@6:13-6:14))"
              , "(A.hs@6:21-6:22,a (TvName) (defined at A.hs@6:13-6:14))"
              , "(A.hs@7:1-7:2,g (VarName) :: forall a. a -> a (binding occurrence))"
              , "(A.hs@7:3-7:4,x (VarName) :: a (binding occurrence))"
              , "(A.hs@7:7-7:8,x (VarName) :: a (defined at A.hs@7:3-7:4))"
              , "(A.hs@8:1-8:2,h (VarName) :: forall a. a -> a (defined at A.hs@9:1-9:2))"
              , "(A.hs@8:13-8:14,a (TvName) (binding occurrence))"
              , "(A.hs@8:16-8:17,a (TvName) (defined at A.hs@8:13-8:14))"
              , "(A.hs@8:21-8:22,a (TvName) (defined at A.hs@8:13-8:14))"
              , "(A.hs@9:1-9:2,h (VarName) :: forall a. a -> a (binding occurrence))"
              , "(A.hs@9:3-9:4,x (VarName) :: a (binding occurrence))"
              , "(A.hs@9:7-9:8,y (VarName) :: a (defined at A.hs@12:5-12:6))"
              , "(A.hs@11:10-11:11,a (TvName) (defined at A.hs@8:13-8:14))"
              , "(A.hs@11:5-11:6,y (VarName) :: a (defined at A.hs@12:5-12:6))"
              , "(A.hs@12:5-12:6,y (VarName) :: a (binding occurrence))"
              , "(A.hs@12:9-12:10,x (VarName) :: a (defined at A.hs@9:3-9:4))"
              , "(A.hs@13:1-13:2,i (VarName) :: forall (t :: * -> *) a. t a -> t a (defined at A.hs@14:1-14:2))"
              , "(A.hs@13:13-13:26,t (TvName) (binding occurrence))"
              , "(A.hs@13:27-13:28,a (TvName) (binding occurrence))"
              , "(A.hs@13:30-13:31,t (TvName) (defined at A.hs@13:13-13:26))"
              , "(A.hs@13:32-13:33,a (TvName) (defined at A.hs@13:27-13:28))"
              , "(A.hs@13:37-13:38,t (TvName) (defined at A.hs@13:13-13:26))"
              , "(A.hs@13:39-13:40,a (TvName) (defined at A.hs@13:27-13:28))"
              , "(A.hs@14:1-14:2,i (VarName) :: forall (t :: * -> *) a. t a -> t a (binding occurrence))"
              , "(A.hs@14:3-14:4,x (VarName) :: t a (binding occurrence))"
              , "(A.hs@14:7-14:8,x (VarName) :: t a (defined at A.hs@14:3-14:4))"
              ]
        let actualIdMapA = lines (show idMapA)
        assertSameSet "actualIdMapA" expectedIdMapA actualIdMapA
    )
  , ( "Type information 6: Reusing type variables"
    , withConfiguredSession ("-XScopedTypeVariables" : defOpts) $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"

                    , "f1 (x, y) = x"
                    , "f2 (x, y) = x"

                    , "f3 (x, y) = f4 (x, y)"
                    , "  where"
                    , "    f4 (x, y) = x"

                    , "f5 :: forall t t1. (t, t1) -> t"
                    , "f5 (x, y) = f6 (x, y)"
                    , "  where"
                    , "    f6 :: forall t2. (t, t2) -> t"
                    , "    f6 (x, y) = x"
                    ])
        updateSessionD session upd 2
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        cache <- getExplicitSharingCache session
        idMaps' <- getLoadedModules session
        let idMaps = removeExplicitSharing cache idMaps'
        let idMap = idMaps Map.! Text.pack "A"
        let expectedIdMap = [
                "(A.hs@2:1-2:3,f1 (VarName) :: forall t t1. (t, t1) -> t (binding occurrence))"
              , "(A.hs@2:13-2:14,x (VarName) :: t (defined at A.hs@2:5-2:6))"
              , "(A.hs@2:5-2:6,x (VarName) :: t (binding occurrence))"
              , "(A.hs@2:8-2:9,y (VarName) :: t1 (binding occurrence))"
              , "(A.hs@3:1-3:3,f2 (VarName) :: forall t t1. (t, t1) -> t (binding occurrence))"
              , "(A.hs@3:13-3:14,x (VarName) :: t (defined at A.hs@3:5-3:6))"
              , "(A.hs@3:5-3:6,x (VarName) :: t (binding occurrence))"
              , "(A.hs@3:8-3:9,y (VarName) :: t1 (binding occurrence))"
              , "(A.hs@4:1-4:3,f3 (VarName) :: forall t t1. (t, t1) -> t (binding occurrence))"
              , "(A.hs@4:13-4:15,f4 (VarName) :: forall t2 t3. (t2, t3) -> t2 (defined at A.hs@6:5-6:7))"
              , "(A.hs@4:17-4:18,x (VarName) :: t (defined at A.hs@4:5-4:6))"
              , "(A.hs@4:20-4:21,y (VarName) :: t1 (defined at A.hs@4:8-4:9))"
              , "(A.hs@4:5-4:6,x (VarName) :: t (binding occurrence))"
              , "(A.hs@4:8-4:9,y (VarName) :: t1 (binding occurrence))"
              , "(A.hs@6:12-6:13,y (VarName) :: t3 (binding occurrence))"
              , "(A.hs@6:17-6:18,x (VarName) :: t2 (defined at A.hs@6:9-6:10))"
              , "(A.hs@6:5-6:7,f4 (VarName) :: forall t2 t3. (t2, t3) -> t2 (binding occurrence))"
              , "(A.hs@6:9-6:10,x (VarName) :: t2 (binding occurrence))"
              , "(A.hs@7:1-7:3,f5 (VarName) :: forall t t1. (t, t1) -> t (defined at A.hs@8:1-8:3))"
              , "(A.hs@7:14-7:15,t (TvName) (binding occurrence))"
              , "(A.hs@7:16-7:18,t1 (TvName) (binding occurrence))"
              , "(A.hs@7:21-7:22,t (TvName) (defined at A.hs@7:14-7:15))"
              , "(A.hs@7:24-7:26,t1 (TvName) (defined at A.hs@7:16-7:18))"
              , "(A.hs@7:31-7:32,t (TvName) (defined at A.hs@7:14-7:15))"
              , "(A.hs@8:1-8:3,f5 (VarName) :: forall t t1. (t, t1) -> t (binding occurrence))"
              , "(A.hs@8:13-8:15,f6 (VarName) :: forall t2. (t, t2) -> t (defined at A.hs@11:5-11:7))"
              , "(A.hs@8:17-8:18,x (VarName) :: t (defined at A.hs@8:5-8:6))"
              , "(A.hs@8:20-8:21,y (VarName) :: t1 (defined at A.hs@8:8-8:9))"
              , "(A.hs@8:5-8:6,x (VarName) :: t (binding occurrence))"
              , "(A.hs@8:8-8:9,y (VarName) :: t1 (binding occurrence))"
              , "(A.hs@10:18-10:20,t2 (TvName) (binding occurrence))"
              , "(A.hs@10:23-10:24,t (TvName) (defined at A.hs@7:14-7:15))"
              , "(A.hs@10:26-10:28,t2 (TvName) (defined at A.hs@10:18-10:20))"
              , "(A.hs@10:33-10:34,t (TvName) (defined at A.hs@7:14-7:15))"
              , "(A.hs@10:5-10:7,f6 (VarName) :: forall t2. (t, t2) -> t (defined at A.hs@11:5-11:7))"
              , "(A.hs@11:12-11:13,y (VarName) :: t2 (binding occurrence))"
              , "(A.hs@11:17-11:18,x (VarName) :: t (defined at A.hs@11:9-11:10))"
              , "(A.hs@11:5-11:7,f6 (VarName) :: forall t2. (t, t2) -> t (binding occurrence))"
              , "(A.hs@11:9-11:10,x (VarName) :: t (binding occurrence))"
              ]
        let actualIdMap = lines (show idMap)
        assertSameSet "" expectedIdMap actualIdMap
    )
  , ( "Type information 7: Qualified imports"
    , withConfiguredSession defOpts $ \session -> do
        let upd = (updateModule "A.hs" . BSLC.pack . unlines $
                    [ "module A where"
                    , "import Data.Maybe"
                    , "import qualified Data.List"
                    , "import qualified Data.Function as F"
                    , "foo = (fromJust, Data.List.and, F.on)"
                    ])
        updateSessionD session upd 2
        msgs <- getSourceErrors session
        assertEqual "This should compile without errors" [] msgs
        cache <- getExplicitSharingCache session
        idMaps'  <- getLoadedModules session
        let idMaps = removeExplicitSharing cache idMaps'
        let idMap = idMaps Map.! (Text.pack "A")
        let expectedIdMap = [
                "(A.hs@5:1-5:4,foo (VarName) :: forall a b c a1. (Data.Maybe.Maybe a -> a, [GHC.Types.Bool] -> GHC.Types.Bool, (b -> b -> c) -> (a1 -> b) -> a1 -> a1 -> c) (binding occurrence))"
              , "(A.hs@5:18-5:31,and (VarName) :: [GHC.Types.Bool] -> GHC.Types.Bool (defined in base-4.5.1.0:GHC.List at <no location info>; imported from base-4.5.1.0:Data.List as 'Data.List.' at A.hs@3:1-3:27))"
              , "(A.hs@5:33-5:37,on (VarName) :: forall b1 c1 a2. (b1 -> b1 -> c1) -> (a2 -> b1) -> a2 -> a2 -> c1 (defined in base-4.5.1.0:Data.Function at <no location info>; imported from base-4.5.1.0:Data.Function as 'F.' at A.hs@4:1-4:36))"
              , "(A.hs@5:8-5:16,fromJust (VarName) :: forall a2. Data.Maybe.Maybe a2 -> a2 (defined in base-4.5.1.0:Data.Maybe at <no location info>; imported from base-4.5.1.0:Data.Maybe at A.hs@2:1-2:18))"
              ]
        let actualIdMap = lines (show idMap)
        assertSameSet "" expectedIdMap actualIdMap
    )
  , ( "Test internal consistency of local id markers"
    , withConfiguredSession ("-package pretty" : defOpts) $ \session -> do
        let upd = (updateModule "M.hs" . BSLC.pack . unlines $
              [ "module M where"
              , "import qualified Text.PrettyPrint as Disp"
              , "class Text a where"
              , "  disp  :: a -> Disp.Doc"
              , "display :: Text a => a -> String"
              , "display = Disp.renderStyle style . disp"
              , "  where style = Disp.Style {}"
              ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertOneError msgs
    )
  , ( "Test internal consistency of imported id markers"
    , withConfiguredSession ("-package pretty" : defOpts) $ \session -> do
        let upd = (updateModule "M.hs" . BSLC.pack . unlines $
              [ "module M where"
              , "import qualified Text.PrettyPrint as Disp"
              , "class Text a where"
              , "  disp  :: a -> Disp.Doc"
              , "display :: Text a => a -> String"
              , "display = Disp.renderStyle astyle . disp"
              , "  where astyle = Disp.Style {"
              , "          Disp.mode            = Disp.PageMode,"
              , "          Disp.lineLength      = 79,"
              , "          Disp.ribbonsPerLine  = 1.0"
              , "        }"
              ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertNoErrors msgs
    )
  , ( "Autocomplete 1: Imports for partial module"
    , withConfiguredSession ("-XPackageImports" : defOpts) $ \session -> do
        let upd = (updateModule "M.hs" . BSLC.pack . unlines $
              [ "module M where"
              , "import Control.Monad"
              , "import Control.Category hiding (id)"
              , "import qualified Control.Arrow as A (second)"
              , "import qualified \"base\" Data.List"
              , "foo ="
              ])
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        case msgs of
          [SourceError KindError _ msg] | "parse error" `isPrefixOf` (Text.unpack msg) -> return ()
          _ -> assertFailure $ "Unexpected source errors: " ++ show3errors msgs
        imports <- getImports session
        assertSameSet "imports: " (imports Map.! Text.pack "M") [
            Import {
                importModule    = Text.pack "Prelude"
              , importPackage   = Nothing
              , importQualified = False
              , importImplicit  = True
              , importAs        = Nothing
              , importHiding    = Nothing
              }
          , Import {
                importModule    = Text.pack "Control.Monad"
              , importPackage   = Nothing
              , importQualified = False
              , importImplicit  = False
              , importAs        = Nothing
              , importHiding    = Nothing
              }
          , Import {
                importModule    = Text.pack "Control.Category"
              , importPackage   = Nothing
              , importQualified = False
              , importImplicit  = False
              , importAs        = Nothing
              , importHiding    = Just (True, [Text.pack "id"])
              }
          , Import {
                importModule     = Text.pack "Control.Arrow"
              , importPackage    = Nothing
              , importQualified  = True
              , importImplicit   = False
              , importAs         = Just (Text.pack "A")
              , importHiding     = Just (False, [Text.pack "second"])
              }
          , Import {
                importModule    = Text.pack "Data.List"
              , importPackage   = Just (Text.pack "base")
              , importQualified = True
              , importImplicit  = False
              , importAs        = Nothing
              , importHiding    = Nothing
              }
          ]
        autocomplete <- getAutocompletion session
        let completeFo = autocomplete (Text.pack "M") "fo"
        assertSameSet "fo: " (map idInfoQN completeFo) [
            "foldM"
          , "foldM_"
          , "forM"
          , "forM_"
          , "forever"
          , "Data.List.foldl'"
          , "Data.List.foldl1"
          , "Data.List.foldl1'"
          , "Data.List.foldr"
          , "Data.List.foldl"
          , "Data.List.foldr1"
          ]
        let completeControlMonadFo = autocomplete (Text.pack "M") "Data.List.fo"
        assertSameSet "Data.List.fo: " (map idInfoQN completeControlMonadFo) [
            "Data.List.foldl'"
          , "Data.List.foldl1"
          , "Data.List.foldl1'"
          , "Data.List.foldr"
          , "Data.List.foldl"
          , "Data.List.foldr1"
          ]
        let completeSec = autocomplete (Text.pack "M") "sec"
        assertSameSet "sec: " (map idInfoQN completeSec) [
            "A.second"
          ]
    )
    -- TODO: Autocomplete test that checks import errors
    -- - Explicitly importing somthing that wasn't exported
    -- - Explicitly hiding something that wasn't exported
    -- - Use of PackageImports without the flag
  ]

assertSameSet :: (Ord a, Show a) => String -> [a] -> [a] -> Assertion
assertSameSet header xs ys = assertSameList header (sort xs) (sort ys)

assertSameList :: (Ord a, Show a) => String -> [a] -> [a] -> Assertion
assertSameList header xs ys =
  case diff xs ys of
    [] -> return ()
    ds -> assertFailure (header ++ unlines ds)

-- | Compare two lists, both assumed sorted
--
-- @diff expected actual@ returns a list of differences between two lists,
-- or an empty lists if the input lists are identical
diff :: (Ord a, Show a) => [a] -> [a] -> [String]
diff [] [] = []
diff xs [] = map (\x -> "Missing "    ++ show x) xs
diff [] ys = map (\y -> "Unexpected " ++ show y) ys
diff (x:xs) (y:ys)
  | x <  y    = ("Missing "    ++ show x) : diff xs (y:ys)
  | x >  y    = ("Unexpected " ++ show y) : diff (x:xs) ys
  | otherwise = diff xs ys

defOpts :: [String]
defOpts = [ "-hide-all-packages", "-package base" ]

-- Set of projects and options to use for them.
projects :: [(String, FilePath, [String])]
projects =
  [ ("A depends on B, throws exception", "test/ABnoError", defOpts)
  , ( "Cabal code"
    , "test/Cabal"
    , [ "-hide-all-packages"
      , "-package base"
      , "-package deepseq"
      , "-package filepath"
      , "-package directory"
      , "-package process"
      , "-package old-time"
      , "-package containers"
      , "-package array"
      , "-package pretty"
      , "-package bytestring"
      , "-package unix"
      ]
    )
  , ("A single file with a code to run in parallel"
    , "test/MainModule"
    , [ "-hide-all-packages"
      , "-package base"
      , "-package parallel"
      , "-package old-time"
      ])
  ]

-- Driver
tests :: [Test]
tests =
  let groupProject ((featureName, check), k) =
        testGroup featureName $ map (caseFeature featureName check k) projects
      caseFeature featureName check k
                  (projectName, originalSourcesDir, opts) = do
        let caseName = projectName ++ " (" ++ show k ++ ")"
        testCase caseName $ do
          debug dVerbosity $ featureName ++ " / " ++ caseName ++ ":"
          withConfiguredSession opts $ \session -> do
            (originalUpdate, lm) <- getModulesFrom session originalSourcesDir
            check session originalUpdate lm
  in [ testGroup "Full integration tests on multiple projects"
       $ map groupProject $ zip multipleTests [1 :: Int ..]
     , testGroup "Synthetic integration tests"
       $ map (uncurry testCase) syntheticTests
     ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    "--server" : opts -> ghcServer opts  -- @opts@ are GHC static flags
    _ -> defaultMain tests


-- Extra debug facilities. Normally turned off.

displayCounter :: Int -> Progress -> Assertion
displayCounter i p = do
  debug dVerbosity $ show p
  assertBool (show p ++ " exceeds " ++ show i) (progressStep p <= i)

updateSessionD :: IdeSession -> IdeSessionUpdate -> Int -> IO ()
updateSessionD session update i = do
  updateSession session update (displayCounter i)
  msgs <- getSourceErrors session
  debug dVerbosity $ "getSourceErrors after update: "
                     ++ List.intercalate "\n" (map show msgs)

-- Extra test tools.
--

loadModule :: FilePath -> String -> IdeSessionUpdate
loadModule file contents =
    let mod =  "module " ++ mname file ++ " where\n" ++ contents
    in updateModule file (BSLC.pack mod)
  where
    -- This is a hack: construct a module name from a filename
    mname :: FilePath -> String
    mname path = case "test/" `substr` path of
      Just rest -> dotToSlash . dropExtension . dropFirstPathComponent $ rest
      Nothing   -> takeBaseName path

    dropFirstPathComponent :: FilePath -> FilePath
    dropFirstPathComponent = tail . dropWhile (/= '/')

    dotToSlash :: String -> String
    dotToSlash = map $ \c -> if c == '/' then '.' else c

    -- | Specification:
    --
    -- > bs `substr` (as ++ bs ++ cs) == Just cs
    -- > bs `substr` _                == Nothing
    substr :: Eq a => [a] -> [a] -> Maybe [a]
    substr needle haystack
      | needle `isPrefixOf` haystack = Just $ drop (length needle) haystack
      | otherwise = case haystack of
                      []              -> Nothing
                      (_ : haystack') -> substr needle haystack'

assertNoErrors :: [SourceError] -> Assertion
assertNoErrors msgs =
  assertBool ("Unexpected errors: " ++ show3errors msgs)
    $ null msgs

assertSomeErrors :: [SourceError] -> Assertion
assertSomeErrors msgs = do
  assertBool "Type error lost" $ length msgs >= 1

assertOneError :: [SourceError] -> Assertion
assertOneError msgs = do
  assertSomeErrors msgs
  assertBool ("Too many type errors: " ++ show3errors msgs)
    $ length msgs <= 1

show3errors :: [SourceError] -> String
show3errors msgs =
  let shown = List.intercalate "\n" (map show $ take 3 $ msgs)
      more | length msgs > 3 = "\n... and more ..."
           | otherwise       = ""
  in shown ++ more

restartRun :: [String] -> ExitCode -> Assertion
restartRun code exitCode =
      withConfiguredSession defOpts $ \session -> do
        let upd = (updateCodeGeneration True)
               <> (updateModule "M.hs" . BSLC.pack . unlines $
                     code)

        -- Compile and run the code on the first server
        updateSessionD session upd 1
        msgs <- getSourceErrors session
        assertNoErrors msgs
        runActionsBefore <- runStmt session "M" "loop"

        -- Start a new server
        threadDelay 100000
        serverBefore <- getGhcServer session
        restartSession session

        -- Compile the code on the new server
        updateSessionD session upd 1
        msgs2 <- getSourceErrors session
        assertNoErrors msgs2

        -- Make sure the old server exited
        exitCodeBefore <- getGhcExitCode serverBefore
        assertEqual "exitCodeBefore" (Just exitCode) exitCodeBefore

        -- Make sure the new server is still alive
        serverAfter <- getGhcServer session
        exitCodeAfter <- getGhcExitCode serverAfter
        assertEqual "exitCodeAfter" Nothing exitCodeAfter

        -- Just one more extra perverse test, since we have the setup ready.
        resOrEx <- runWait runActionsBefore
        case resOrEx of
          Right RunForceCancelled -> return ()
          _ -> assertFailure $ "Unexpected run result: " ++ show resOrEx

testBufferMode :: RunBufferMode -> Assertion
testBufferMode bufferMode =
  withConfiguredSession defOpts $ \session -> do
    let upd = (updateCodeGeneration True)
           <> (updateStdoutBufferMode bufferMode)
           <> (updateStderrBufferMode bufferMode)
           <> (updateModule "M.hs" . BSLC.pack . unlines $
                [ "module M where"
                , "import Control.Concurrent"
                , "import Control.Monad"
                , "printCs :: IO ()"
                , "printCs = do"
                , "  threadDelay 500000"
                , "  replicateM_ 5 $ do"
                , "    forM_ ['1' .. '9'] $ \\ch -> do"
                , "      threadDelay 100000"
                , "      putChar ch"
                , "    threadDelay 100000"
                , "    putChar '\\n'"
                ])

    updateSessionD session upd 1
    msgs <- getSourceErrors session
    assertNoErrors msgs

    runActions <- runStmt session "M" "printCs"
    let go acc = do ret <- runWait runActions
                    case ret of
                      Left bs -> do
                        go (BSSC.unpack bs : acc)
                      Right (RunOk _) ->
                        verify bufferMode (reverse acc)
                      Right res ->
                        assertFailure $ "Program terminated abnormally: " ++ show res
    go []
  where
    verify :: RunBufferMode -> [String] -> Assertion
    verify RunNoBuffering outp =
      assertEqual "" (chunk 1 total) outp
    verify (RunLineBuffering Nothing) outp =
      assertEqual "" (chunkOn '\n' total) outp
    verify (RunBlockBuffering (Just blockSize) Nothing) outp =
      assertEqual "" (chunk blockSize total) outp
    verify (RunLineBuffering (Just 1000000)) outp = do
      -- We don't want to be *too* precise, but we should expect 10 chunks,
      -- half of which should end on a linebreak. And of course they should
      -- total to the right thing :)
      let (withBreak, withoutBreak) = List.partition ((== '\n') . last) outp
      assertEqual "" 5 (length withBreak)
      assertEqual "" 5 (length withoutBreak)
      assertEqual "" total (concat outp)
    verify (RunBlockBuffering (Just 4) (Just 1000000)) outp = do
      -- As above, we don't want to be too precise. Certaily no chunks should
      -- be larger than 4, and "some" should be smaller
      assertBool "" (all ((<= 4) . length) outp)
      assertBool "" (any ((< 4)  . length) outp)
      assertEqual "" total (concat outp)
    verify (RunBlockBuffering (Just 4096) (Just 1000000)) outp = do
      assertEqual "" 6 (length outp)
      assertEqual "" total (concat outp)
    verify mode _outp =
      assertFailure $ "Unsupported mode " ++ show mode

    total :: String
    total = concat $ replicate 5 "123456789\n"

    chunk :: Int -> [a] -> [[a]]
    chunk _ [] = []
    chunk n xs = let (firstChunk, rest) = splitAt n xs
                 in firstChunk : chunk n rest

    chunkOn :: Eq a => a -> [a] -> [[a]]
    chunkOn _ [] = []
    chunkOn x xs = let (firstChunk, rest) = List.span (/= x) xs
                   in case rest of
                        (x' : rest') -> (firstChunk ++ [x']) : chunkOn x rest'
                        []           -> [firstChunk]

{------------------------------------------------------------------------------
  Aux
------------------------------------------------------------------------------}

newtype Counter = Counter (IORef Int)

newCounter :: IO Counter
newCounter = do
  c <- newIORef 0
  return (Counter c)

resetCounter :: Counter -> IO ()
resetCounter (Counter c) = writeIORef c 0

incCounter :: Counter -> IO ()
incCounter (Counter c) = readIORef c >>= writeIORef c . (+ 1)

assertCounter :: Counter -> Int -> Assertion
assertCounter (Counter c) i = do
  j <- readIORef c
  assertEqual "" i j
