{-# LANGUAGE NamedFieldPuns, RecordWildCards, TemplateHaskell #-}
module Main where

import System.IO.Temp (withSystemTempDirectory)
import Network.URI (unEscapeString)
import Data.Maybe (fromJust)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Aeson (encode)
import Data.Aeson.TH (deriveJSON)
import Network.Shed.Httpd

import IdeSession
import ModuleName as MN

data IdeResponse = IdeResponse {
    errors :: [SourceError]
  , idMap  :: IdMap
  }

$(deriveJSON id ''IdeResponse)

server :: IdeSession -> Request -> IO Response
server ideSession Request{reqBody} = do
  let upd = updateModule (fromJust (MN.fromString "M")) (pack reqBody)
  updateSession ideSession upd (const $ return ())
  errors <- getSourceErrors ideSession
  idMap  <- getIdMap ideSession
  return Response {
      resCode    = 200
    , resHeaders = [ ("Access-Control-Allow-Origin", "*")
                   , ("Access-Control-Allow-Headers", "Content-Type")
                   ]
    , resBody    = unpack (encode IdeResponse {..})
    }

main :: IO Server
main = do
  withSystemTempDirectory "protoide" $ \tempDir -> do
    let cfg = SessionConfig {
                  configDir        = tempDir
                , configStaticOpts = []
                }
    ideSession <- initSession cfg
    initServer 8080 (server ideSession)
