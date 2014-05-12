module Main where

import System.Environment (getArgs)
import System.Exit (ExitCode)

import IdeSession.Cabal
import IdeSession.RPC.Server

main :: IO ()
main = do
  args <- getArgs
  rpcServer exeCabalEngine args

exeCabalEngine :: RpcConversation -> IO ()
exeCabalEngine RpcConversation{..} = do
    -- Start handling RPC calls
    let go = do
          req <- get
          case req of
            ReqExeCabalRun args -> do
             exitCode <- runExeCabal args
             put $ ExeCabalDone exitCode
          go

    go

-- | Run the cabal functions inside the executable.
runExeCabal :: ExeArgs -> IO ExitCode
runExeCabal args =
  case args of
    ExeBuild buildExeArgs modArgs ->
      configureAndBuild buildExeArgs modArgs
    ExeDoc buildExeArgs ->
      configureAndHaddock buildExeArgs
    ExeCc runCcArgs ->
      runComponentCc runCcArgs
