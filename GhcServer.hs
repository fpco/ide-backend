{-# LANGUAGE TemplateHaskell #-}
module GhcServer
  ( GhcRequest(..), GhcResponse(..), GhcServer
  , ghcServer
  , optsToGhcState
  ) where

import System.FilePath ((</>), takeExtension)
import System.Directory
import Data.Aeson.TH (deriveJSON)

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


ghcServer :: GhcState -> GhcRequest
          -> IO (Progress GhcResponse GhcResponse)
ghcServer ghcState (ReqCompute configSourcesDir) = do
  cnts <- getDirectoryContents configSourcesDir
  let files = map (configSourcesDir </>)
              $ filter ((`elem` [".hs"]) . takeExtension) cnts
  allErrs <- checkModule files Nothing ghcState
  return $ Progress $ return $ Left $ RespDone allErrs
