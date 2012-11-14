{-# LANGUAGE TemplateHaskell #-}
-- | Implementation of the server that controls the long-running GHC instance.
-- | This is the place where the GHC-specific part joins the part
-- implementing the general RPC infrastructure.
--
-- The modules importing any GHC internals, as well as the modules
-- implementing the  RPC infrastructure, should be accessible to the rest
-- of the program only indirectly, through the @GhcServer@ module.
module GhcServer
  ( -- * Types involved in the communication
    GhcRequest(..), GhcResponse(..)
  , Progress(..)  -- probably move somewhere outside
  , fmap2Progress
    -- * A handle to the server
  , GhcServer
    -- * Server-side operations
  , createGhcServer
    -- * Client-side operations
  , forkGhcServer
  , rpcGhcServer
  , shutdownGhcServer
  ) where

-- getExecutablePath is in base only for >= 4.6
import System.Environment.Executable (getExecutablePath)
import System.FilePath ((</>), takeExtension)
import System.Directory
import Data.Aeson.TH (deriveJSON)
import System.IO
  ( stdin
  , stdout
  , stderr
  )
import Control.Concurrent.MVar
  ( MVar
  , newMVar
  , readMVar
  , swapMVar
  )
import Control.Monad (void)

import RpcServer
import Common
import GhcRun

data GhcRequest  = ReqCompute FilePath | ReqErrors
  deriving Show
data GhcResponse = RespWorking | RespDone [SourceError]
  deriving Show

$(deriveJSON id ''GhcRequest)
$(deriveJSON id ''GhcResponse)

data GhcState = GhcState
  { lOpts    :: LeftoverOpts
  , mvErrors :: MVar (Maybe [SourceError])
  }
type GhcServer = RpcServer GhcRequest GhcResponse

-- * Server-side operations

-- TODO: Perhaps fork checkModule in a thread, rewrite collectSrcError to place
-- errors in a channel and not in an IORef, read from the channel and return
-- errors one by one in Progress, as soon as they appear? The problem
-- is that there are usually very few errors and found quickly one after
-- another, so perhaps instead return the number of files already type-checked
-- in Progress and only eventually return the errors? With warnings,
-- sensind them one by one, or file after file, makes sense.
-- Anyway, is running checkModule in a thread correct vs the spec,
-- assuming at most one copy is running at the same time?
ghcServerEngine :: GhcState -> GhcRequest
                -> IO (Progress GhcResponse GhcResponse)
ghcServerEngine GhcState{lOpts, mvErrors} (ReqCompute configSourcesDir) = do
  -- TODO: Fork a thread doing the following (and ensure with mvars
  -- that at most one copy runs at the same time).
  void $ swapMVar mvErrors Nothing  -- invalidate previous result
  -- Optional semantics: do not invalidate previous results until
  -- we finish the new one. Extra state components are then needed
  -- to communicate progress.
  cnts <- getDirectoryContents configSourcesDir
  let files = map (configSourcesDir </>)
              $ filter ((`elem` [".hs"]) . takeExtension) cnts
  errs <- checkModule files Nothing lOpts
  void $ swapMVar mvErrors $ Just errs
  -- TODO: The following will make sense when the above runs in a thread.
  return $ Progress $ return $ Left RespWorking
ghcServerEngine GhcState{mvErrors} ReqErrors = do
  merrs <- readMVar mvErrors
  case merrs of
    Nothing   -> return $ Progress $ return $ Left $ RespWorking
    Just errs -> return $ Progress $ return $ Left $ RespDone errs

createGhcServer :: [String] -> IO ()
createGhcServer opts = do
  lOpts <- submitOpts opts
  mvErrors <- newMVar Nothing
  rpcServer stdin stdout stderr (ghcServerEngine GhcState{..})

-- * Client-side operations

forkGhcServer :: IO GhcServer
forkGhcServer = do
  prog <- getExecutablePath
  forkRpcServer prog ["--server"]

-- This could already produce values of a stronger type
-- @Progress () [SourceError]@ here, but in the future
-- we may use some RpcServer operations stronger than @rpc@
-- and they tend to use @Progress a a@.
rpcGhcServer :: GhcServer -> FilePath -> IO (Progress GhcResponse GhcResponse)
rpcGhcServer gs configSourcesDir = do
  resp <- rpc gs $ ReqCompute configSourcesDir
  p resp
 where
  p :: GhcResponse -> IO (Progress GhcResponse GhcResponse)
  p resp1 = do
    case resp1 of
      RespWorking -> return $ Progress $ do
        resp2 <- rpc gs ReqErrors
        progress <- p resp2
        return $ Right (resp1, progress)
      RespDone _ -> return $ Progress $ return $ Left resp1

shutdownGhcServer :: GhcServer -> IO ()
shutdownGhcServer gs = shutdown gs
