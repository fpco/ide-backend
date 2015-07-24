{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, TupleSections, OverloadedStrings #-}
-- | Invoke the executable that calls cabal functions and communicate
-- with it via RPC.
module IdeSession.ExeCabalClient (
    invokeExeCabal
  ) where

import Data.Monoid ((<>))
import qualified Data.Text as Text
import System.Exit (ExitCode(..))

import IdeSession.Cabal
import IdeSession.Config
import IdeSession.GHC.API
import IdeSession.RPC.Client (RpcServer, RpcConversation(..), forkRpcServer, rpcConversation, shutdown, findProgram)
import IdeSession.State
import IdeSession.Types.Progress
import IdeSession.Types.Public (UpdateStatus(..))
import IdeSession.Util

-- | Invoke the executable that processes our custom functions that use
-- the machinery of the cabal library.
invokeExeCabal :: IdeStaticInfo -> IdeCallbacks -> ExeCabalRequest -> (UpdateStatus -> IO ())
               -> IO ExitCode
invokeExeCabal ideStaticInfo@IdeStaticInfo{..} ideCallbacks args callback = do
  let logFunc = ideCallbacksLogFunc ideCallbacks
  mLoc <- findProgram logFunc searchPath ide_backend_exe_cabal
  case mLoc of
    Nothing ->
      fail $ "Could not find ide-backend-exe-cabal"
    Just prog -> do
      env <- envWithPathOverride configExtraPathDirs
      let cwd = case args of
            ReqExeDoc{} -> ideSessionDir
            _ -> ideDataDir ideStaticInfo
      server <- forkRpcServer prog [] (Just cwd) env
      exitCode <- rpcRunExeCabal server args callback
      shutdown server  -- not long-running
      return exitCode
  where
    (searchPath,ide_backend_exe_cabal) = configIdeBackendExeCabal

    SessionConfig{..} = ideConfig

rpcRunExeCabal :: RpcServer -> ExeCabalRequest -> (UpdateStatus -> IO ())
               -> IO ExitCode
rpcRunExeCabal server req callback =
  rpcConversation server $ \RpcConversation{..} -> do
    put req

    let go = do response <- get
                case response of
                  ExeCabalProgress pcounter -> do
                    callback (UpdateStatusProgress pcounter)
                    go
                  ExeCabalDone ec@ExitSuccess -> do
                    callback UpdateStatusDone
                    return ec
                  ExeCabalDone ec@(ExitFailure code) -> do
                    callback $ UpdateStatusFailed $
                      "Cabal exited with code " <> Text.pack (show code)
                    return ec

    go
