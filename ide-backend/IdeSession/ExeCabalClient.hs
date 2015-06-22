{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, TupleSections #-}
-- | Invoke the executable that calls cabal functions and communicate
-- with it via RPC.
module IdeSession.ExeCabalClient (
    invokeExeCabal
  ) where

import System.Exit (ExitCode)

import Distribution.Verbosity (normal)
import Distribution.Simple.Program.Find (
    ProgramSearchPath
  , findProgramOnSearchPath
  , ProgramSearchPathEntry(..)
  )

import IdeSession.Cabal
import IdeSession.Config
import IdeSession.GHC.API
import IdeSession.RPC.Client (RpcServer, RpcConversation(..), forkRpcServer, rpcConversation, shutdown)
import IdeSession.State
import IdeSession.Types.Progress
import IdeSession.Util

-- | Invoke the executable that processes our custom functions that use
-- the machinery of the cabal library.
invokeExeCabal :: IdeStaticInfo -> ExeCabalRequest -> (Progress -> IO ())
               -> IO ExitCode
invokeExeCabal ideStaticInfo@IdeStaticInfo{..} args callback = do
  mLoc <- findProgramOnSearchPath normal searchPath "ide-backend-exe-cabal"
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
    searchPath :: ProgramSearchPath
    searchPath = ProgramSearchPathDefault
               : map ProgramSearchPathDir configExtraPathDirs

    SessionConfig{..} = ideConfig

rpcRunExeCabal :: RpcServer -> ExeCabalRequest -> (Progress -> IO ())
               -> IO ExitCode
rpcRunExeCabal server req callback =
  rpcConversation server $ \RpcConversation{..} -> do
    put req

    let go = do response <- get
                case response of
                  ExeCabalProgress pcounter -> callback pcounter >> go
                  ExeCabalDone exitCode     -> return exitCode

    go
