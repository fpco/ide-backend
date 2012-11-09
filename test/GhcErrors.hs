module Main where

import Data.IORef
import System.Environment
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Monoid ((<>), mempty)
import System.Random (randomIO)
import qualified Data.ByteString.Lazy.Char8 as BS
import Text.JSON as JSON
import Text.JSON.Pretty (pp_value)
import Text.PrettyPrint (render)

import IdeSession

main :: IO ()
main = do
  args <- getArgs
  let configSourcesDir = case args of
        [dir] -> dir
        [] -> "."
        _ -> fail "usage: ide-backend [source-dir]"
  configFilesystem <- newIORef $ Map.empty
  let sessionConfig = SessionConfig{..}
  -- Two sample scenarios:
  b <- randomIO
  if b
    then do
      s0 <- initSession sessionConfig
      let update1 =
            (updateModule $ ModulePut "ide-backend.hs" (BS.pack "2"))
            <> (updateModule $ ModulePut "ide-backend.hs" (BS.pack "x = a2"))
          update2 =
            (updateModule $ ModulePut "ide-backend.hs" (BS.pack "4"))
            <> (updateModule $ ModulePut "ide-backend.hs" (BS.pack "x = a4"))
      progress1 <- updateSession s0 update1
      s2 <- progressWaitCompletion progress1
      msgs2 <- getSourceErrors s2
      putStrLn $ "Error 2:\n" ++ List.intercalate "\n\n"
        (map formatErrorMessagesJSON msgs2) ++ "\n"
      progress3 <- updateSession s2 update2  -- s0 should fail
      s4 <- progressWaitCompletion progress3
      msgs4 <- getSourceErrors s4
      putStrLn $ "Error 4:\n" ++ List.intercalate "\n\n"
        (map formatErrorMessagesJSON msgs4) ++ "\n"
      msgs2' <- getSourceErrors s2
      putStrLn $ "Error 2 again:\n" ++ List.intercalate "\n\n"
        (map formatErrorMessagesJSON msgs2') ++ "\n"
      shutdownSession s4
    else do
      s0 <- initSession sessionConfig
      progress <- updateSession s0 mempty
      s1 <- progressWaitCompletion progress
      msgs1 <- getSourceErrors s1
      putStrLn $ "Error 1:\n" ++ List.intercalate "\n\n"
        (map formatErrorMessagesJSON msgs1) ++ "\n"
      shutdownSession s1  -- s0 should fail

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
