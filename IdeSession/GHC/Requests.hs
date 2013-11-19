-- | GHC requests
--
-- GHC requests use "IdeSession.Types.Public" types.
{-# LANGUAGE DeriveDataTypeable #-}
module IdeSession.GHC.Requests (
    GhcRequest(..)
  , GhcRunRequest(..)
  , RunCmd(..)
  ) where

import Data.Binary
import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import Control.Applicative ((<$>), (<*>))

import IdeSession.Types.Public

data GhcRequest
  = ReqCompile {
        reqCompileOptions   :: Maybe [String]
      , reqCompileSourceDir :: FilePath
      , reqCompileDistDir   :: FilePath
      , reqCompileGenCode   :: Bool
      , reqCompileTargets   :: Maybe [ModuleName]
      }
  | ReqRun {
        reqRunCmd :: RunCmd
      }
  | ReqSetEnv {
        reqSetEnv :: [(String, Maybe String)]
      }
  | ReqSetArgs {
        reqSetArgs :: [String]
      }
  | ReqBreakpoint {
        reqBreakpointModule :: ModuleName
      , reqBreakpointSpan   :: SourceSpan
      , reqBreakpointValue  :: Bool
      }
  | ReqPrint {
        reqPrintVars  :: Name
      , reqPrintBind  :: Bool
      , reqPrintForce :: Bool
      }
  | ReqLoad {
        reqLoadPath   :: FilePath
      , reqLoadUnload :: Bool
      }
    -- | For debugging only! :)
  | ReqCrash {
        reqCrashDelay :: Maybe Int
      }
  deriving Typeable

data RunCmd =
    RunStmt {
        runCmdModule   :: String
      , runCmdFunction :: String
      , runCmdStdout   :: RunBufferMode
      , runCmdStderr   :: RunBufferMode
      }
  | Resume

data GhcRunRequest =
    GhcRunInput ByteString
  | GhcRunInterrupt
  | GhcRunAckDone
  deriving Typeable

instance Binary GhcRequest where
  put ReqCompile{..} = do
    putWord8 0
    put reqCompileOptions
    put reqCompileSourceDir
    put reqCompileDistDir
    put reqCompileGenCode
    put reqCompileTargets
  put ReqRun{..} = do
    putWord8 1
    put reqRunCmd
  put ReqSetEnv{..} = do
    putWord8 2
    put reqSetEnv
  put ReqSetArgs{..} = do
    putWord8 3
    put reqSetArgs
  put ReqBreakpoint{..} = do
    putWord8 4
    put reqBreakpointModule
    put reqBreakpointSpan
    put reqBreakpointValue
  put ReqPrint{..} = do
    putWord8 5
    put reqPrintVars
    put reqPrintBind
    put reqPrintForce
  put ReqLoad{..} = do
    putWord8 6
    put reqLoadPath
    put reqLoadUnload
  put ReqCrash{..} = do
    putWord8 255
    put reqCrashDelay

  get = do
    header <- getWord8
    case header of
      0   -> ReqCompile    <$> get <*> get <*> get <*> get <*> get
      1   -> ReqRun        <$> get
      2   -> ReqSetEnv     <$> get
      3   -> ReqSetArgs    <$> get
      4   -> ReqBreakpoint <$> get <*> get <*> get
      5   -> ReqPrint      <$> get <*> get <*> get
      6   -> ReqLoad       <$> get <*> get
      255 -> ReqCrash      <$> get
      _   -> fail "GhcRequest.get: invalid header"

instance Binary RunCmd where
  put (RunStmt {..}) = do
    putWord8 0
    put runCmdModule
    put runCmdFunction
    put runCmdStdout
    put runCmdStderr
  put Resume = do
    putWord8 1

  get = do
    header <- getWord8
    case header of
      0 -> RunStmt <$> get <*> get <*> get <*> get
      1 -> return Resume
      _ -> fail "RunCmd.get: invalid header"

instance Binary GhcRunRequest where
  put (GhcRunInput bs) = putWord8 0 >> put bs
  put GhcRunInterrupt  = putWord8 1
  put GhcRunAckDone    = putWord8 2

  get = do
    header <- getWord8
    case header of
      0 -> GhcRunInput <$> get
      1 -> return GhcRunInterrupt
      2 -> return GhcRunAckDone
      _ -> fail "GhcRunRequest.get: invalid header"
