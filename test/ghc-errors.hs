module Main (main) where

import System.Environment (getArgs)
import System.FilePath ((</>), takeExtension, dropExtension)
import System.Directory
import System.Unix.Directory (withTemporaryDirectory)
import qualified Data.List as List
import Data.Monoid ((<>), mconcat, mempty)
import Data.ByteString.Lazy.Char8 (pack)
import Control.Exception (bracket)
import Data.Char (toUpper)

import Test.Framework (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (Assertion, assertBool, assertEqual)

import IdeSession
import GhcServer
import Common
import TestTools

-- Tests using various functions of the IdeSession API
-- and a variety of small test Haskell projects.

-- | Update the session with all modules the given directory
loadModulesFrom :: IdeSession -> FilePath -> IO ()
loadModulesFrom session originalSourcesDir = do
  debug dVerbosity $ "\nCopying files from: " ++ originalSourcesDir
                     ++ " to: " ++ configSourcesDir (getSessionConfig session)
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
  updateSessionD session originalUpdate (length originalFiles)

-- | Run the specified action with a new IDE session, configured to use a
-- temporary directory
withConfiguredSession :: [String] -> (IdeSession -> IO a) -> IO a
withConfiguredSession opts io =
  withTemporaryDirectory "ide-backend-test" $ \configSourcesDir -> do
    let sessionConfig = SessionConfig{ configSourcesDir
                                     , configWorkingDir = configSourcesDir
                                     , configDataDir    = configSourcesDir
                                     , configTempDir    = "."
                                     , configStaticOpts = opts
                                     }
    withSession sessionConfig io

-- | Run the specified action with a new IDE session
withSession :: SessionConfig -> (IdeSession -> IO a) -> IO a
withSession config = bracket (initSession config) shutdownSession

-- | Like 'withSession', but with a monadic configuration
withSession' :: IO SessionConfig -> (IdeSession -> IO a) -> IO a
withSession' config' io = do
  config <- config'
  withSession config io

-- Set of api calls and checks to perform on each project.
--
-- TODO: we need much more tests to recover the functionality of the old,
-- undreadable set, and then we need to much more to test all API functions.
-- E.g., check that the values of PCounter do not exceeed the number of files.
-- Also, check ModuleDelete and all the DataFileChange constructors,
-- getSourceModule an getDataFile.
multipleTests :: [(String, IdeSession -> Assertion)]
multipleTests =
  [ ("Just typecheck", \_ -> return ())
  , ("Overwrite with error"
    , \session -> do
        -- Overwrite one of the copied files.
        (m, _) <- getModules session
        let update = loadModule m "a = unknownX"
        updateSessionD session update 1  -- at most 1 recompiled
        msgs <- getSourceErrors session
        assertSomeErrors msgs
    )
  , ("Overwrite with the same module name in all files"
    , \session -> do
        (_, lm) <- getModules session
        let upd m =
              updateModule (ModulePut m (pack "module Wrong where\na = 1"))
            update = mconcat $ map upd lm
        updateSessionD session update 2
        msgs <- getSourceErrors session
        if length lm >= 2
          then assertSomeErrors msgs
          else assertNoErrors msgs
    )
  , ("Overwrite modules many times"
    , \session -> do
        -- Overwrite one of the copied files with an error.
        (m1, lm) <- getModules session
        let update1 =
              updateModule (ChangeCodeGeneration False)
              <> loadModule m1 "a = unknownX"
        updateSessionD session update1 1
        updateSessionD session mempty 1  -- was an error, so trying again
        msgs2 <- getSourceErrors session
        assertSomeErrors msgs2
        -- Overwrite all files, many times, with correct modules.
        let upd m = loadModule m "x = unknownX"
                    <> loadModule m "y = 2"
                    <> updateModule (ChangeCodeGeneration True)
            update2 = mconcat $ map upd lm
        updateSessionD session update2 (length lm)
        msgs4 <- getSourceErrors session
        assertNoErrors msgs4
        -- Overwrite again with the error.
        updateSessionD session update1 1  -- drop bytecode, don't recompile
        msgs5 <- getSourceErrors session
        assertOneError msgs5
        assertRaises "runStmt session Main main"
          (== userError "Cannot run before the code is generated.")
          (runStmt session "Main" "main")
      )
    , ("Run the sample code; don't fail without an explanation"
      , \session -> do
        (_, lm) <- getModules session
        let update = updateModule (ChangeCodeGeneration True)
        updateSessionD session update (length lm)  -- all recompiled
        mRunActions <- runStmt session "Main" "main"
        case mRunActions of
          Left errs -> assertBool "No errors detected, but the run failed" $ length errs >= 1
          Right runActions -> do
           resOrEx <- runWait runActions
           assertBool "No errors detected, but the run failed" $
             case resOrEx of
               Right (RunOk _ident)     -> True
               Right (RunException _ex) -> False
               Left _                   -> False
      )
    , ("Run automatically corrected code; don't fail at all"
      , \session -> do
        (_, lm) <- getModules session
        let upd m = loadModule m "x = 1"
            update =
              updateModule (ModulePut
                              (ModuleName "Main")
                              (pack "module Main where\nmain = print \"running automatically generated trivial code\""))
              <> mconcat (map upd lm)
        updateSessionD session update (length lm + 1)
        updateSessionD session mempty 0
        let update2 = updateModule (ChangeCodeGeneration True)
        updateSessionD session update2 (length lm + 1)
        Right runActions <- runStmt session "Main" "main"
        resOrEx <- runWait runActions
        assertBool ("Manually corrected code not run successfully") $
          case resOrEx of
            Right (RunOk _ident)     -> True
            Right (RunException _ex) -> False
            Left _                   -> False
      )
    , ("Make sure deleting modules removes them from the directory"
      , \session -> do
        (_, lm) <- getModules session
        let update = mconcat $ map (updateModule . ModuleDelete) lm
        updateSessionD session update 0
        msgs <- getSourceErrors session
        assertNoErrors msgs
        shutdownSession session
        -- Start new session in the same directory; should not throw an error
        let config = getSessionConfig session
        withSession config $ \_ -> return ()
      )
  ]

syntheticTests :: [(String, Assertion)]
syntheticTests =
  [ ( "Maintain list of compiled modules"
    , withConfiguredSession defOpts $ \session -> do
        let m = ModuleName "A"
        updateSessionD session (loadModule m "") 1
        assertEqual "" [m] =<< getLoadedModules session
    )
  , ( "Duplicate shutdown"
    , withConfiguredSession defOpts $ \session ->
        -- withConfiguredSession will shutdown the session as well
        shutdownSession session
    )
  , ( "Permit a session within a session and duplicated shutdownSession"
    , withConfiguredSession defOpts $ \session -> do
        loadModulesFrom session "test/ABnoError"
        let config = getSessionConfig session
            tweakConfig :: Int -> SessionConfig -> IO SessionConfig
            tweakConfig n cfg@SessionConfig{configSourcesDir} = do
              let newDir = configSourcesDir </> "new" ++ show n
              createDirectory newDir
              return cfg { configSourcesDir = newDir
                         , configWorkingDir = newDir
                         , configDataDir = newDir }
        withSession' (tweakConfig 2 config) $ \s2 -> do
         withSession' (tweakConfig 3 config) $ \s3 -> do
          withSession' (tweakConfig 4 config) $ \_s4 -> do
           let update2 = loadModule (ModuleName "M") "a = unknownX"
           updateSessionD s2 update2 1
           msgs2 <- getSourceErrors s2
           assertOneError msgs2
           withSession' (tweakConfig 5 config) $ \s5 -> do
            let update3 = loadModule (ModuleName "M") "a = 3"
            updateSessionD s3 update3 1
            msgs3 <- getSourceErrors s3
            assertNoErrors msgs3
            shutdownSession s5 -- <-- duplicate "nested" shutdown
    )
  , ( "Compile a project: A depends on B, error in A"
    , withConfiguredSession defOpts $ \session -> do
        loadModulesFrom session "test/AerrorB"
        msgs <- getSourceErrors session
        assertOneError msgs
    )
  , ( "Compile a project: A depends on B, error in B"
    , withConfiguredSession defOpts $ \session -> do
        loadModulesFrom session "test/ABerror"
        msgs <- getSourceErrors session
        assertOneError msgs
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
        loadModulesFrom session "test/Puns"
        msgs <- getSourceErrors session
        assertSomeErrors msgs
        let punOpts = packageOpts ++ [ "-XNamedFieldPuns", "-XRecordWildCards"]
            update2 = updateModule (ChangeOptions $ Just punOpts)
        (_, lm) <- getModules session
        updateSessionD session update2 (length lm)
        msgs2 <- getSourceErrors session
        assertNoErrors msgs2
    )
  , ("Reject getSourceErrors without updateSession"
    , withConfiguredSession defOpts $ \session ->
        assertRaises "getSourceErrors session"
          (== userError "This session state does not admit queries.")
          (getSourceErrors session)
    )
  , ("Reject initSession with a non-empty source directory"
    , withConfiguredSession defOpts $ \session -> do
        loadModulesFrom session "test/ABnoError"
        shutdownSession session
        let config = getSessionConfig session
        assertRaises "initSession config"
          (== userError
            ("Directory " ++ configSourcesDir config ++ " is not empty."))
          (initSession config)
    )
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
  , ( "Reject a wrong CPP directive"
    , let packageOpts = [ "-hide-all-packages"
                        , "-XCPP"
                        ]
      in withConfiguredSession packageOpts $ \session -> do
        let update = loadModule (ModuleName "M") "#ifdef"
                     <> updateModule (ChangeCodeGeneration True)
        updateSessionD session update 1
        msgs <- getSourceErrors session
        assertSomeErrors msgs
        Right runActions <- runStmt session "Main" "main"
        resOrEx <- runWait runActions
        assertBool "This run has to fail" $
          case resOrEx of
            Right (RunOk _ident)    -> False
            Right (RunException ex) ->
              ex == "Cannot run before GHC session is initialized."
            Left _                  -> False
    )
  , ( "Reject a module with mangled header"
    , withConfiguredSession defOpts $ \session -> do
        let update = updateModule (ModulePut (ModuleName "M")
                                   $ pack "module very-wrong where")
        updateSessionD session update 1
        msgs <- getSourceErrors session
        assertOneError msgs
    )
  ]

defOpts :: [String]
defOpts = [ "-no-user-package-conf" ]

-- Set of projects and options to use for them.
projects :: [(String, FilePath, [String])]
projects =
  [ ("A depends on B, no errors", "test/ABnoError", defOpts)
  , ("Our own code, package 'ghc' missing", ".", [])
  , ( "A subdirectory of Cabal code"
    , "test/Cabal.Distribution.PackageDescription"
    , defOpts
    )
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
  let groupProject ((featureName, check), k) =
        testGroup featureName $ map (caseFeature featureName check k) projects
      caseFeature featureName check k
                  (projectName, originalSourcesDir, opts) = do
        let caseName = projectName ++ " (" ++ show k ++ ")"
        testCase caseName $ do
          debug dVerbosity $ featureName ++ " / " ++ caseName ++ ":"
          withConfiguredSession opts $ \session -> do
            loadModulesFrom session originalSourcesDir
            check session
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

displayCounter :: Int -> PCounter -> Assertion
displayCounter i n = do
  debug dVerbosity $ "PCounter: " ++ (show n) ++ ". "
  assertBool ("PCounter " ++ show n ++ " exceeds " ++ show i) (n <= i)

updateSessionD :: IdeSession -> IdeSessionUpdate -> Int -> IO ()
updateSessionD session update i = do
  updateSession session update (displayCounter i)
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
      -- Is there a ready function for that in GHC API?
      mFixed = capitalize $ map (\c -> if c == '-' then '_' else c) n
      name = dropExtension mFixed
  in updateModule . ModulePut m . pack
     $ "module " ++ name ++ " where\n" ++ contents

capitalize :: String -> String
capitalize s = case s of
  []       -> []
  c : rest -> toUpper c : rest

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
  let shown = List.intercalate "\n" (map formatSourceError $ take 3 $ msgs)
      more | length msgs > 3 = "\n... and more ..."
           | otherwise       = ""
  in shown ++ more
