{-# LANGUAGE TemplateHaskell #-}
-- | The API for controlling a running snippet of code.
module RunAPI
  ( RunResult(..)
  , RunActions(..)
-- TODO: a bug in haddock ignores the 3 ops:  , RunActions(interrupt, runWait, supplyStdin)
  , runWaitAll
  , afterRunActions
  ) where

import Data.Aeson.TH (deriveJSON)
import qualified Data.ByteString as BSS (ByteString)
import qualified Data.ByteString.Lazy as BSL (ByteString, fromChunks)

-- | The outcome of running code
data RunResult =
    -- | The code terminated okay
    RunOk String
    -- | The code threw an exception
  | RunProgException String
    -- | GHC itself threw an exception when we tried to run the code
  | RunGhcException String
  deriving (Show, Eq)

$(deriveJSON id ''RunResult)

-- | Handles to the running code, through which one can interact with the code.
data RunActions = RunActions {
    runWait     :: IO (Either BSS.ByteString RunResult)
  , interrupt   :: IO ()
  , supplyStdin :: BSS.ByteString -> IO ()
  }

-- | Repeatedly call 'runWait' until we receive a 'Right' result, while
-- collecting all 'Left' results
runWaitAll :: RunActions -> IO (BSL.ByteString, RunResult)
runWaitAll RunActions{runWait} = go []
  where
    go :: [BSS.ByteString] -> IO (BSL.ByteString, RunResult)
    go acc = do
      resp <- runWait
      case resp of
        Left  bs        -> go (bs : acc)
        Right runResult -> return (BSL.fromChunks (reverse acc), runResult)

-- | Register a callback to be invoked when the program terminates
afterRunActions :: RunActions -> (RunResult -> IO ()) -> RunActions
afterRunActions runActions callback = runActions {
    runWait = do result <- runWait runActions
                 case result of
                   Left bs -> return (Left bs)
                   Right r -> callback r >> return (Right r)
  }
