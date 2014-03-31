-- | GHC requests
--
-- GHC requests use "IdeSession.Types.Public" types.
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module IdeSession.GHC.Requests (
    GhcInitRequest(..)
  , GhcRequest(..)
  , GhcRunRequest(..)
  , RunCmd(..)
  ) where

import Data.Binary
import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import Control.Applicative ((<$>), (<*>))

import Text.Show.Pretty (PrettyVal(..))
import GHC.Generics

import IdeSession.Types.Public

data GhcInitRequest = GhcInitRequest {
    ghcInitClientApiVersion   :: Int
  , ghcInitGenerateModInfo    :: Bool
  , ghcInitOpts               :: [String]
  , ghcInitUserPackageDB      :: Bool
  , ghcInitSpecificPackageDBs :: [String]
  , ghcInitSourceDir          :: FilePath
  , ghcInitDistDir            :: FilePath
  }
  deriving (Typeable, Generic)

data GhcRequest
  = ReqCompile {
        reqCompileGenCode   :: Bool
      , reqCompileTargets   :: Targets
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
  | ReqSetGhcOpts {
        reqSetGhcOpts :: [String]
      }
    -- | For debugging only! :)
  | ReqCrash {
        reqCrashDelay :: Maybe Int
      }
  deriving (Typeable, Generic)

data RunCmd =
    RunStmt {
        runCmdModule   :: String
      , runCmdFunction :: String
      , runCmdStdout   :: RunBufferMode
      , runCmdStderr   :: RunBufferMode
      }
  | Resume
  deriving (Typeable, Generic)

instance PrettyVal GhcInitRequest
instance PrettyVal GhcRequest
instance PrettyVal RunCmd

data GhcRunRequest =
    GhcRunInput ByteString
  | GhcRunInterrupt
  | GhcRunAckDone
  deriving Typeable

instance Binary GhcInitRequest where
  put (GhcInitRequest{..}) = do
    -- Note: we intentionally write the API version first. This makes it
    -- possible (in theory at least) to have some form of backwards API
    -- compatibility.
    put ghcInitClientApiVersion
    put ghcInitGenerateModInfo
    put ghcInitOpts
    put ghcInitUserPackageDB
    put ghcInitSpecificPackageDBs
    put ghcInitSourceDir
    put ghcInitDistDir

  get = GhcInitRequest <$> get
                       <*> get
                       <*> get
                       <*> get
                       <*> get
                       <*> get
                       <*> get

instance Binary GhcRequest where
  put ReqCompile{..} = do
    putWord8 0
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
  put ReqSetGhcOpts{..} = do
    putWord8 7
    put reqSetGhcOpts
  put ReqCrash{..} = do
    putWord8 255
    put reqCrashDelay

  get = do
    header <- getWord8
    case header of
      0   -> ReqCompile     <$> get <*> get
      1   -> ReqRun         <$> get
      2   -> ReqSetEnv      <$> get
      3   -> ReqSetArgs     <$> get
      4   -> ReqBreakpoint  <$> get <*> get <*> get
      5   -> ReqPrint       <$> get <*> get <*> get
      6   -> ReqLoad        <$> get <*> get
      7   -> ReqSetGhcOpts  <$> get
      255 -> ReqCrash       <$> get
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
