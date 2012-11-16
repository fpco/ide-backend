module Main where

import Control.Monad
import qualified Control.Exception as Ex
import System.Environment
import System.FilePath ((</>), takeExtension, dropExtension)
import System.Directory
import System.Unix.Directory (withTemporaryDirectory)
import qualified Data.List as List
import Data.Monoid ((<>), mempty, mconcat)
import qualified Data.ByteString.Lazy.Char8 as BS
import Text.JSON as JSON
import Text.JSON.Pretty (pp_value)
import Text.PrettyPrint (render)
import System.IO
  ( stderr
  , hSetBuffering
  , BufferMode(LineBuffering)
  )

import IdeSession
import GhcServer
import Progress

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  args <- getArgs
  case args of
    ["--server"] -> do
      let opts = []  -- GHC static flags; set them in sessionConfig?
      createGhcServer opts
    _ -> do
      test "test/ABnoError"
      test "test/ABerror"
      test "test/AerrorB"
      test "."

test :: FilePath -> IO ()
test originalSourcesDir =
  withTemporaryDirectory "ide-backend-test" $ check originalSourcesDir

check :: FilePath -> FilePath -> IO ()
check originalSourcesDir configSourcesDir = do
  putStrLn $ "Copying files from: " ++ originalSourcesDir ++ "\n\n"
          ++ "Temporary test directory: " ++ configSourcesDir ++ "\n\n"
  -- Init session.
  let sessionConfig = SessionConfig{ configSourcesDir
                                   , configWorkingDir = configSourcesDir
                                   , configDataDir    = configSourcesDir
                                   , configTempDir    = configSourcesDir
                                   }
  sP <- initSession sessionConfig
  -- Copy some source files from 'originalSourcesDir' to 'configSourcesDir'.
  -- HACK: here we fake module names, guessing them from file names.
  cnts <- getDirectoryContents originalSourcesDir
  let originalFiles = filter ((`elem` [".hs"]) . takeExtension) cnts
      originalModules =
        map (\ f -> (ModuleName $ dropExtension f, f)) originalFiles
      upd (m, f) = updateModule $ ModuleSource m $ originalSourcesDir </> f
      originalUpdate = mconcat $ map upd originalModules
      displayCounter :: PCounter -> IO ()
      displayCounter n = putStr (show n)
  s0 <- updateSession sP originalUpdate (progressWaitConsume displayCounter)
  msgs0 <- getSourceErrors s0
  putStrLn $ "Error 0:\n" ++ List.intercalate "\n\n"
    (map formatErrorMessagesJSON msgs0) ++ "\n"
  -- Overwrite some copied files.
  let overName = case originalModules of
        [] -> ModuleName "testEmptyDirModule"
        (m, _) : _ -> m
      update1 =
        (updateModule $ ModulePut overName (BS.pack "module M where\n2"))
        <> (updateModule $ ModulePut overName (BS.pack "module M where\nx = a2"))
      update2 =
        (updateModule $ ModulePut overName (BS.pack "module M where\n4"))
        <> (updateModule $ ModulePut overName (BS.pack "module M where\nx = a4"))
  -- Test the computations.
  s2 <- updateSession s0 update1 (progressWaitConsume displayCounter)
  msgs2 <- getSourceErrors s2
  putStrLn $ "Error 2:\n" ++ List.intercalate "\n\n"
    (map formatErrorMessagesJSON msgs2) ++ "\n"
  shouldFail "updateSession s0 update2 (progressWaitConsume displayCounter)"
            $ updateSession s0 update2 (progressWaitConsume displayCounter)
  s4 <- updateSession s2 update2 (progressWaitConsume displayCounter)
  msgs4 <- getSourceErrors s4
  putStrLn $ "Error 4:\n" ++ List.intercalate "\n\n"
    (map formatErrorMessagesJSON msgs4) ++ "\n"
  msgs2' <- getSourceErrors s2
  putStrLn $ "Error 2 again:\n" ++ List.intercalate "\n\n"
    (map formatErrorMessagesJSON msgs2') ++ "\n"
-- Can't do the following until we have each runGHC session spawned in
-- a differen process.
--
--  shutdownSession s4
--  s10 <- initSession sessionConfig
  let s10 = s4
  s11 <- updateSession s10 mempty (progressWaitConsume displayCounter)
  msgs11 <- getSourceErrors s11
  putStrLn $ "Error 11:\n" ++ List.intercalate "\n\n"
    (map formatErrorMessagesJSON msgs11) ++ "\n"
  shouldFail "shutdownSession s10"
            $ shutdownSession s10
  shutdownSession s11

shouldFail :: String -> IO a -> IO ()
shouldFail descr x = do
  let logException e = do
        putStrLn $ "Correctly rejected: " ++ descr ++
                   "\nwith exception msg: " ++ show (e :: Ex.ErrorCall) ++ "\n"
        return True
  failed <- Ex.catch (x >> return False) logException
  unless failed $ error $ "should fail: " ++ descr

-- Hacks retained just to pretty-print error messages.
formatErrorMessagesJSON :: SourceError -> String
formatErrorMessagesJSON err =
  render $ pp_value $ errorMessageToJSON err

errorMessageToJSON :: SourceError -> JSValue
errorMessageToJSON (SrcError errKind file (stline, stcol)
                                          (endline, endcol) msgstr) =
  JSObject $
    toJSObject
      [ ("kind",      showJSON (toJSString (show errKind)))
      , ("file",      showJSON (toJSString file))
      , ("startline", showJSON stline)
      , ("startcol",  showJSON stcol)
      , ("endline",   showJSON endline)
      , ("endcol",    showJSON endcol)
      , ("message",   showJSON (toJSString msgstr))
      ]
errorMessageToJSON (OtherError msgstr) =
  JSObject $
    toJSObject
      [ ("kind",      showJSON (toJSString "message"))
      , ("message",   showJSON (toJSString msgstr))
      ]