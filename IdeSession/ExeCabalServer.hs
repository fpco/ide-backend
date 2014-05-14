-- | Run Cabal functions upon receiving RPC requests, log output and provide
-- Progress updates.
module IdeSession.ExeCabalServer (
    exeCabalEngine
  ) where

import Control.Concurrent.Async (async, wait)
import qualified Control.Exception as Ex
import qualified Data.ByteString as BSS
import Data.Text.Encoding as E
import System.Exit (ExitCode)
import qualified System.IO as IO
import System.IO.Error (isEOFError)
import System.Posix.IO.ByteString

import IdeSession.Cabal
import IdeSession.RPC.Server
import IdeSession.Util
import IdeSession.Types.Progress

-- | Handle RPC requests by calling Cabal functions, keeping track
-- of progress and passing the results.
exeCabalEngine :: RpcConversation -> IO ()
exeCabalEngine conv@RpcConversation{..} = do
    -- Start handling RPC calls
    let go = do
          req <- get
          exitCode <- runExeCabal conv req
          put $ ExeCabalDone exitCode
          go

    go

-- | Run the cabal functions inside the executable.
runExeCabal :: RpcConversation -> ExeCabalRequest -> IO ExitCode
runExeCabal conv req = do
   -- Create pipe
  (stdOutputRd, stdOutputWr) <- createPipe

  -- Backup stdout, then replace stdout with the pipe's write end
  IO.hFlush IO.stdout
  stdOutputBackup <- dup stdOutput
  _ <- dupTo stdOutputWr stdOutput
  closeFd stdOutputWr

  -- Convert the read end to a handle
  stdOutputRdHandle <- fdToHandle stdOutputRd
  IO.hSetBuffering stdOutputRdHandle IO.LineBuffering

  let stdoutLog = case req of
        ReqExeBuild buildExeArgs _ -> beStdoutLog buildExeArgs
        ReqExeDoc buildExeArgs -> beStdoutLog buildExeArgs
        ReqExeCc runCcArgs -> rcStdoutLog runCcArgs
  stdoutThread <- async $ readStdout conv stdOutputRdHandle stdoutLog

  exitCode <- case req of
    ReqExeBuild buildExeArgs modArgs ->
      configureAndBuild buildExeArgs modArgs
    ReqExeDoc buildExeArgs ->
      configureAndHaddock buildExeArgs
    ReqExeCc runCcArgs ->
      runComponentCc runCcArgs

  -- Restore stdout
  IO.hFlush IO.stdout
  dupTo stdOutputBackup stdOutput >> closeFd stdOutputBackup

  -- Closing the write end of the stdout pipe will cause the stdout
  -- thread to terminate after it processed all remaining output;
  -- wait for this to happen
  wait stdoutThread

  return exitCode

readStdout :: RpcConversation -> IO.Handle -> FilePath -> IO ()
readStdout RpcConversation{..} stdOutputRdHandle stdoutLog = do
  logHandle <- IO.openFile stdoutLog IO.WriteMode
  let go = do
        res <- Ex.try $ BSS.hGetLine stdOutputRdHandle
        case res of
          Left ex -> if isEOFError ex then return () else Ex.throw ex
          Right bs -> do
            progressCallback bs
            BSS.hPut logHandle bs
            BSS.hPut logHandle (BSS.singleton (0x0a))  -- \n
            go
  go
  IO.hClose logHandle
 where
    progressCallback :: BSS.ByteString -> IO ()
    progressCallback ghcMsg = do
      let ghcMsg' = E.decodeUtf8 ghcMsg
      case parseProgressMessage ghcMsg' of
        Right (step, numSteps, msg) ->
          put $ ExeCabalProgress $ Progress {
               progressStep      = step
             , progressNumSteps  = numSteps
             , progressParsedMsg = Just msg
             , progressOrigMsg   = Just ghcMsg'
             }
        _ ->
          -- Ignore messages we cannot parse
          return ()
