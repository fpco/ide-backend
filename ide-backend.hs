module Main where

import Control.Monad
import qualified Control.Exception as Ex
import System.Environment
import System.FilePath ((</>), takeExtension, dropExtension)
import System.Directory
import System.Unix.Directory (withTemporaryDirectory)
import qualified Data.List as List
import Data.Monoid (mconcat)
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

--- A sample program using the library. It type-checks all files
--- in the given directory and prints out the list of errors
--- in JSON format.

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  args <- getArgs
  case args of
    "--server" : opts -> createGhcServer opts  -- @opts@ are GHC static flags
    _ -> do
      let originalSourcesDir = case args of
            [dir] -> dir
            [] -> "."
            _ -> fail "usage: programName [source-dir]"
      withTemporaryDirectory "ide-backend-test" $ check originalSourcesDir

check :: FilePath -> FilePath -> IO ()
check originalSourcesDir configSourcesDir = do
  putStrLn $ "Copying files from: " ++ originalSourcesDir ++ "\n\n"
          ++ "Temporary directory: " ++ configSourcesDir ++ "\n\n"
  -- Init session.
  let sessionConfig = SessionConfig{ configSourcesDir
                                   , configWorkingDir = configSourcesDir
                                   , configDataDir    = configSourcesDir
                                   , configTempDir    = "."
                                   , configStaticOpts = []
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
      displayCounter n = putStr (show n)  -- or just putStr "#"
  s0 <- updateSession sP originalUpdate (progressWaitConsume displayCounter)
  msgs0 <- getSourceErrors s0
  putStrLn $ "Errors :\n" ++ List.intercalate "\n\n"
    (map formatErrorMessagesJSON msgs0) ++ "\n"
  shutdownSession s0

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
