-- | Run Cabal functions upon receiving RPC requests, log output and provide
-- Progress updates.
module IdeSession.ExeCabalServer (
    exeCabalEngine
  ) where

import Control.Concurrent.Async (async, wait)
import System.Exit (ExitCode)
import System.IO (IOMode(..))
import System.IO.Error (isEOFError)
import qualified Control.Exception  as Ex
import qualified Data.ByteString    as BSS
import qualified Data.Text.Encoding as E
import qualified System.IO          as IO

import IdeSession.Cabal
import IdeSession.RPC.Server
import IdeSession.Types.Progress
import IdeSession.Util
import IdeSession.Util.PortableIO

-- | Handle RPC requests by calling Cabal functions, keeping track
-- of progress and passing the results.
exeCabalEngine :: FilePath -> RpcConversation -> IO ()
exeCabalEngine _errorLog conv@RpcConversation{..} = do
    -- Start handling RPC calls
    let go = do
          req <- get
          exitCode <- runExeCabal conv req
          put $ ExeCabalDone exitCode
          go  -- not long-running, but we can't exit or RPC would crash

    go

-- | Run the cabal functions inside the executable.
runExeCabal :: RpcConversation -> ExeCabalRequest -> IO ExitCode
runExeCabal conv req = do
   -- Create pipe
  (stdOutputRd, stdOutputWr) <- createPipe

  -- Backup stdout, then replace stdout with the pipe's write end
  -- TODO fix swizzleStdout
  -- (exitCode, stdoutThread) <- swizzleStdout stdOutputWr $ do
  (exitCode, stdoutThread) <- do
    -- Convert the read end to a handle
    stdOutputRdHandle <- fdToHandle stdOutputRd ReadMode
    IO.hSetBuffering stdOutputRdHandle IO.LineBuffering

    let stdoutLog = case req of
          ReqExeBuild buildExeArgs _ -> beStdoutLog buildExeArgs
          ReqExeDoc   buildExeArgs   -> beStdoutLog buildExeArgs
          ReqExeCc    runCcArgs      -> rcStdoutLog runCcArgs
          ReqExeLic   licenseArgs    -> liStdoutLog licenseArgs
    stdoutThread <- async $ readStdout conv stdOutputRdHandle stdoutLog

    exitCode <- case req of
      ReqExeBuild buildExeArgs modArgs ->
        configureAndBuild buildExeArgs modArgs
      ReqExeDoc buildExeArgs ->
        configureAndHaddock buildExeArgs
      ReqExeCc runCcArgs ->
        runComponentCc runCcArgs
      ReqExeLic licenseArgs ->
        buildLicsFromPkgs True licenseArgs

    return (exitCode, stdoutThread)

  -- Closing the write end of the stdout pipe will cause the stdout
  -- thread to terminate after it processed all remaining output;
  -- wait for this to happen
  closeFd stdOutputWr
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
