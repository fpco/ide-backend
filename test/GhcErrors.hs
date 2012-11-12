module Main where

import Control.Monad
import Control.Exception
import System.Environment
import System.FilePath (combine, takeExtension)
import System.Directory
import System.Unix.Directory (withTemporaryDirectory)
import qualified Data.List as List
import Data.Monoid ((<>), mempty)
import qualified Data.ByteString.Lazy.Char8 as BS
import Text.JSON as JSON
import Text.JSON.Pretty (pp_value)
import Text.PrettyPrint (render)

import IdeSession

main :: IO ()
main = do
 args <- getArgs
 let originalSourcesDir = case args of
       [dir] -> dir
       [] -> "."
       _ -> fail "usage: GhcErrors [source-dir]"
 withTemporaryDirectory "ide-backend-test" $ \ configSourcesDir -> do
  putStrLn $ "Copying files from: " ++ originalSourcesDir ++ "\n\n"
          ++ "Temporary test directory: " ++ configSourcesDir ++ "\n\n"

  -- Copy some source files from 'originalSourcesDir' to 'configSourcesDir'.
  cnts <- getDirectoryContents originalSourcesDir
  let files = filter ((`elem` [".hs"]) . takeExtension) cnts
      copy file = copyFile (combine originalSourcesDir file)
                           (combine configSourcesDir file)
  mapM_ copy files
  -- Init session.
  let sessionConfig = SessionConfig{ configSourcesDir
                                   , configWorkingDir = configSourcesDir
                                   , configDataDir    = configSourcesDir
                                   , configTempDir    = configSourcesDir
                                   }
  s0 <- initSession sessionConfig
  -- Overwrite something.
  let update1 = if originalSourcesDir /= "." then mempty else
        (updateModule $ ModulePut "ide-backend.hs" (BS.pack "2"))
        <> (updateModule $ ModulePut "ide-backend.hs" (BS.pack "x = a2"))
      update2 = if originalSourcesDir /= "." then mempty else
        (updateModule $ ModulePut "ide-backend.hs" (BS.pack "4"))
        <> (updateModule $ ModulePut "ide-backend.hs" (BS.pack "x = a4"))
  -- Test the computations.
  progress1 <- updateSession s0 update1
  s2 <- progressWaitCompletion progress1
  msgs2 <- getSourceErrors s2
  putStrLn $ "Error 2:\n" ++ List.intercalate "\n\n"
    (map formatErrorMessagesJSON msgs2) ++ "\n"
  shouldFail "updateSession s0 update2"
            $ updateSession s0 update2
  progress3 <- updateSession s2 update2
  s4 <- progressWaitCompletion progress3
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
  progress <- updateSession s10 mempty
  s11 <- progressWaitCompletion progress
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
                   "\nwith exception msg: " ++ show (e :: ErrorCall) ++ "\n"
        return True
  failed <- catch (x >> return False) logException
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
