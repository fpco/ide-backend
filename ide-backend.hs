module Main where

import Data.IORef
import System.Environment
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Monoid (mempty)
import Text.JSON as JSON
import Text.JSON.Pretty (pp_value)
import Text.PrettyPrint (render)

import IdeSession

-- A sample program using the library. Naively, separately, type-checks
-- all programs in the given directory and prints out the list of errors
-- in JSON format.

main :: IO ()
main = do
  args <- getArgs
  let configSourcesDir = case args of
        [dir] -> dir
        [] -> "."
        _ -> fail "usage: ide-backend [source-dir]"
  configFilesystem <- newIORef $ Map.empty
  let sessionConfig = SessionConfig{..}
  s0 <- initSession sessionConfig
  progress1 <- updateSession s0 mempty
  s2 <- progressWaitCompletion progress1
  msgs2 <- getSourceErrors s2
  putStrLn $ "Errors:\n" ++ List.intercalate "\n\n"
    (map formatErrorMessagesJSON msgs2) ++ "\n"
  shutdownSession s2

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
