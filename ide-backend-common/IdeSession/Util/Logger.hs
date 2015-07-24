{-# LANGUAGE TemplateHaskell #-}

module IdeSession.Util.Logger
  ( LogFunc
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logOther
  ) where

import qualified Control.Monad.Logger as L
import           Data.Text (Text)
import           Language.Haskell.TH

type LogFunc = L.Loc -> L.LogSource -> L.LogLevel -> L.LogStr -> IO ()

logDebug :: Q Exp
logDebug = [| \msg -> L.runLoggingT ($(L.logDebug) msg) $(varE (mkName "logFunc")) |]

logInfo :: Q Exp
logInfo = [| \msg -> L.runLoggingT ($(L.logInfo) msg) $(varE (mkName "logFunc")) |]

logWarn :: Q Exp
logWarn = [| \msg -> L.runLoggingT ($(L.logWarn) msg) $(varE (mkName "logFunc")) |]

logError :: Q Exp
logError = [| \msg -> L.runLoggingT ($(L.logError) msg) $(varE (mkName "logFunc")) |]

logOther :: Text -> Q Exp
logOther level = [| \msg -> L.runLoggingT $(L.logOther level) $(varE (mkName "logFunc")) |]
