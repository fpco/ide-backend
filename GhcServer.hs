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

import RpcServer
import Common
import GhcRun

data GhcRequest  = ReqCompute FilePath
  deriving Show
data GhcResponse = RespDone [SourceError]
  deriving Show
type GhcServer = RpcServer GhcRequest GhcResponse

$(deriveJSON id ''GhcRequest)
$(deriveJSON id ''GhcResponse)

-- * Server-side operations

ghcServerEngine :: GhcState -> GhcRequest
                -> IO (Progress GhcResponse GhcResponse)
ghcServerEngine ghcState (ReqCompute configSourcesDir) = do
  cnts <- getDirectoryContents configSourcesDir
  let files = map (configSourcesDir </>)
              $ filter ((`elem` [".hs"]) . takeExtension) cnts
  allErrs <- checkModule files Nothing ghcState
  return $ Progress $ return $ Left $ RespDone allErrs

createGhcServer :: [String] -> IO ()
createGhcServer opts = do
  ideGhcState <- optsToGhcState opts
  rpcServer stdin stdout stderr (ghcServerEngine ideGhcState)

-- * Client-side operations

forkGhcServer :: IO GhcServer
forkGhcServer = do
  prog <- getExecutablePath
  forkRpcServer prog ["--server"]

rpcGhcServer :: GhcServer -> FilePath -> IO [SourceError]
rpcGhcServer gs configSourcesDir = do
  RespDone allErrs <- rpc gs $ ReqCompute configSourcesDir
  return allErrs

shutdownGhcServer :: GhcServer -> IO ()
shutdownGhcServer gs = shutdown gs
