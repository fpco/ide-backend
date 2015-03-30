module Debug (
    dVerbosity
  , debugFile
  , debug
  ) where

import Control.Monad (when)
import qualified Data.ByteString.Char8 as BS
import System.IO (stderr, hFlush)

dVerbosity :: Int
dVerbosity = 3

debugFile :: Maybe FilePath
debugFile = Nothing -- Just "debug.log"

debug :: Int -> String -> IO ()
debug verbosity msg = when (verbosity >= 3) $ do
  case debugFile of
    Nothing      -> return ()
    Just logName -> appendFile logName $ msg ++ "\n"
  when (verbosity >= 4) $ do
    BS.hPutStrLn stderr $ BS.pack $ "debug: " ++ msg
    hFlush stderr
