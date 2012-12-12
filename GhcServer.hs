{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
-- | Implementation of the server that controls the long-running GHC instance.
-- This is the place where the GHC-specific part joins the part
-- implementing the general RPC infrastructure.
--
-- The modules importing any GHC internals, as well as the modules
-- implementing the  RPC infrastructure, should be accessible to the rest
-- of the program only indirectly, through the @GhcServer@ module.
module GhcServer
  ( -- * Types involved in the communication
    PCounter
    -- * A handle to the server
  , GhcServer
    -- * Server-side operations
  , ghcServer
    -- * Client-side operations
  , forkGhcServer
  , rpcCompile
  , rpcRun
  , shutdownGhcServer
  ) where

-- getExecutablePath is in base only for >= 4.6
import System.Environment.Executable (getExecutablePath)
import Data.Aeson.TH (deriveJSON)
import Data.IORef
import Control.Monad (forever)

import System.IO (stdout, hFlush)
import System.Posix (Fd)
import System.Posix.IO.ByteString
import Data.ByteString.Char8 (pack)

import RpcServer
import Common
import GhcRun

data GhcRequest
  = ReqCompile (Maybe [String]) FilePath Bool
  | ReqRun     String String
  deriving Show
-- data GhcResponse = RespWorking PCounter | RespDone RunOutcome
--   deriving Show
data GhcCompileResponse =
    GhcCompileProgress PCounter
  | GhcCompileDone [SourceError]
data GhcRunResponse =
    GhcRunDone RunResult

$(deriveJSON id ''GhcRequest)
$(deriveJSON id ''GhcCompileResponse)
$(deriveJSON id ''GhcRunResponse)

type GhcServer = RpcServer

--------------------------------------------------------------------------------
-- Server-side operations                                                     --
--------------------------------------------------------------------------------

ghcServer :: [String] -> IO ()
ghcServer fdsAndOpts = do
  let (opts, markerAndFds) = span (/= "--ghc-opts-end") fdsAndOpts
  rpcServer (tail markerAndFds) (ghcServerEngine opts)

-- | The GHC server engine proper.
--
-- This function runs in end endless loop inside the @Ghc@ monad, making
-- incremental compilation possible.
ghcServerEngine :: [String] -> RpcConversation  -> IO ()
ghcServerEngine staticOpts conv@RpcConversation{..} = do
  -- Submit static opts and get back leftover dynamic opts.
  dOpts <- submitStaticOpts staticOpts

  -- Start handling requests. From this point on we don't leave the GHC monad.
  runFromGhc . forever $ do
    req <- liftIO $ get
    case req of
      ReqCompile opts dir genCode ->
        ghcHandleCompile conv dOpts opts dir genCode
      ReqRun m fun ->
        ghcHandleRun conv m fun

-- | Handle a compile or type check request
ghcHandleCompile :: RpcConversation
                 -> DynamicOpts        -- ^ "Old" dynamic flags
                 -> Maybe [String]     -- ^ "New" dynamic flags
                 -> FilePath           -- ^ Source directory
                 -> Bool               -- ^ Should we generate code
                 -> Ghc ()
ghcHandleCompile RpcConversation{..} dOpts ideNewOpts configSourcesDir ideGenerateCode = do
    errsRef <- liftIO $ newIORef []
    counter <- liftIO $ newIORef 1  -- Progress counter goes from 1..n
    errs    <- surpressGhcStdout $ compileInGhc configSourcesDir
                                                dynOpts
                                                ideGenerateCode
                                                verbosity
                                                errsRef
                                                (progressCallback counter)
                                                (\_ -> return ()) -- TODO: log?
    liftIO $ debug dVerbosity $ "returned from compileInGhc with " ++ show errs
    liftIO $ put $ GhcCompileDone errs
  where
    dynOpts :: DynamicOpts
    dynOpts = maybe dOpts optsToDynFlags ideNewOpts

    -- Let GHC API print "compiling M ... done." for each module.
    verbosity :: Int
    verbosity = 1

    -- TODO: verify that _ is the "compiling M" message
    progressCallback :: IORef Int -> String -> IO ()
    progressCallback counter _ghcMsg = do
      oldCounter <- readIORef counter
      modifyIORef counter (+1)
      put $ GhcCompileProgress oldCounter

-- | Handle a run request
ghcHandleRun :: RpcConversation
             -> String            -- ^ Module
             -> String            -- ^ Function
             -> Ghc ()
ghcHandleRun RpcConversation{..} m fun = do
  runOutcome <- runInGhc (m, fun)
  liftIO $ debug dVerbosity $ "returned from runInGhc with " ++ show runOutcome
  liftIO $ put $ GhcRunDone runOutcome

--------------------------------------------------------------------------------
-- Client-side operations                                                     --
--------------------------------------------------------------------------------

forkGhcServer :: [String] -> IO GhcServer
forkGhcServer opts = do
  prog <- getExecutablePath
  forkRpcServer prog $ ["--server"] ++ opts ++ ["--ghc-opts-end"]

-- | Compile or typecheck
rpcCompile :: GhcServer           -- ^ GHC server
           -> Maybe [String]      -- ^ Options
           -> FilePath            -- ^ Source directory
           -> Bool                -- ^ Should we generate code?
           -> (PCounter -> IO ()) -- ^ Progress callback
           -> IO [SourceError]
rpcCompile server opts dir genCode callback =
  rpcConversation server $ \RpcConversation{..} -> do
    put (ReqCompile opts dir genCode)

    let go = do response <- get
                case response of
                  GhcCompileProgress pcounter -> callback pcounter >> go
                  GhcCompileDone     errors   -> return errors

    go

-- | Run code
rpcRun :: GhcServer       -- ^ GHC server
       -> String          -- ^ Module
       -> String          -- ^ Function
       -> IO RunResult
rpcRun server m fun = rpc server (ReqRun m fun)

shutdownGhcServer :: GhcServer -> IO ()
shutdownGhcServer gs = shutdown gs

--------------------------------------------------------------------------------
-- Auxiliary                                                                  --
--------------------------------------------------------------------------------

surpressGhcStdout :: Ghc a -> Ghc a
surpressGhcStdout p = do
  stdOutputBackup <- liftIO $ surpressStdOutput
  x <- p
  liftIO $ restoreStdOutput stdOutputBackup
  return x

type StdOutputBackup = Fd

surpressStdOutput :: IO StdOutputBackup
surpressStdOutput = do
  hFlush stdout
  stdOutputBackup <- dup stdOutput
  closeFd stdOutput
  -- Will use next available file descriptor: that is, stdout
  _ <- openFd (pack "/dev/null") WriteOnly Nothing defaultFileFlags
  return stdOutputBackup

restoreStdOutput :: StdOutputBackup -> IO ()
restoreStdOutput stdOutputBackup = do
  hFlush stdout
  closeFd stdOutput
  dup stdOutputBackup
  closeFd stdOutputBackup
