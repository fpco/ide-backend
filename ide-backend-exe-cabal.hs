module Main where

import Control.Concurrent.Async (async, wait)
import qualified Control.Exception as Ex
import Control.Monad (unless)
import qualified Data.ByteString as BSS
import System.Environment (getArgs)
import System.Exit (ExitCode)
import qualified System.IO as IO
import System.IO.Error (isEOFError)
import System.Posix.IO.ByteString

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
runExeCabal args = do
   -- Create pipe
  (stdOutputRd, stdOutputWr) <- createPipe

  -- Backup stdout, then replace stdout with the pipe's write end
  stdOutputBackup <- dup stdOutput
  _ <- dupTo stdOutputWr stdOutput
  closeFd stdOutputWr

  -- Convert the read end to a handle
  stdOutputRdHandle <- fdToHandle stdOutputRd
  IO.hSetBuffering stdOutputRdHandle IO.LineBuffering

  let stdoutLog = case args of
        ExeBuild buildExeArgs _ -> beStdoutLog buildExeArgs
        ExeDoc buildExeArgs -> beStdoutLog buildExeArgs
        ExeCc runCcArgs -> rcStdoutLog runCcArgs

  stdoutThread <- async $ readStdout stdOutputRdHandle stdoutLog

  exitCode <- case args of
    ExeBuild buildExeArgs modArgs ->
      configureAndBuild buildExeArgs modArgs
    ExeDoc buildExeArgs ->
      configureAndHaddock buildExeArgs
    ExeCc runCcArgs ->
      runComponentCc runCcArgs

  -- Restore stdout
  dupTo stdOutputBackup stdOutput >> closeFd stdOutputBackup

  -- Closing the write end of the stdout pipe will cause the stdout
  -- thread to terminate after it processed all remaining output;
  -- wait for this to happen
  wait stdoutThread

  return exitCode

readStdout :: IO.Handle -> FilePath -> IO ()
readStdout stdOutputRdHandle stdoutLog = do
  logHandle <- IO.openFile stdoutLog IO.WriteMode
--  let go = do bs <- BSS.hGetSome stdOutputRdHandle 4096
--              unless (BSS.null bs) $ BSS.hPut logHandle bs >> go
  let go = do
        res <- Ex.try $ BSS.hGetLine stdOutputRdHandle
        case res of
          Left ex -> if isEOFError ex then return () else Ex.throw ex
          Right bs ->do
            BSS.hPut logHandle bs
            go
  go
  IO.hClose logHandle
