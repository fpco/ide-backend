{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, RecordWildCards #-}

-- | GHC requests
--
-- GHC requests use "IdeSession.Types.Public" types.
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

-- | Initial handshake with the ghc server
--
-- Ideally we'd send over the entire IdeStaticInfo but this includes some
-- Cabal fields, and the ghc server does -not- compile against Cabal
-- (although this isn't so important anymore now that we use Cabal-ide-backend)
data GhcInitRequest = GhcInitRequest {
    ghcInitClientApiVersion   :: Int
  , ghcInitGenerateModInfo    :: Bool
  , ghcInitOpts               :: [String]
  , ghcInitUserPackageDB      :: Bool
  , ghcInitSpecificPackageDBs :: [String]
  , ghcInitSessionDir         :: FilePath
  , ghcInitSourceDir          :: FilePath
  , ghcInitDistDir            :: !FilePath
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
        reqLoad :: [FilePath]
      }
  | ReqUnload {
        reqUnload :: [FilePath]
      }
  | ReqSetGhcOpts {
        reqSetGhcOpts :: [String]
      }
    -- | For debugging only! :)
  | ReqCrash {
        reqCrashDelay :: Maybe Int
      }
  deriving (Typeable, Generic, Show)

data RunCmd =
    RunStmt {
        runCmdModule   :: String
      , runCmdFunction :: String
      , runCmdStdout   :: RunBufferMode
      , runCmdStderr   :: RunBufferMode
      , runCmdPty      :: Bool
      }
  | Resume
  deriving (Typeable, Generic, Show)

instance PrettyVal GhcInitRequest
instance PrettyVal GhcRequest
instance PrettyVal RunCmd

data GhcRunRequest =
    GhcRunInput ByteString
  | GhcRunInterrupt
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
    put ghcInitSessionDir
    put ghcInitDistDir

  get = GhcInitRequest <$> get
                       <*> get
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
    put reqLoad
  put ReqUnload{..} = do
    putWord8 7
    put reqUnload
  put ReqSetGhcOpts{..} = do
    putWord8 8
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
      6   -> ReqLoad        <$> get
      7   -> ReqUnload      <$> get
      8   -> ReqSetGhcOpts  <$> get
      255 -> ReqCrash       <$> get
      _   -> fail "GhcRequest.get: invalid header"

instance Binary RunCmd where
  put (RunStmt {..}) = do
    putWord8 2
    put runCmdModule
    put runCmdFunction
    put runCmdStdout
    put runCmdStderr
    put runCmdPty
  put Resume = do
    putWord8 1

  get = do
    header <- getWord8
    case header of
      -- Still respond to requests that use the old binary format.
      0 -> RunStmt <$> get <*> get <*> get <*> get <*> return False
      1 -> return Resume
      2 -> RunStmt <$> get <*> get <*> get <*> get <*> get
      _ -> fail "RunCmd.get: invalid header"

instance Binary GhcRunRequest where
  put (GhcRunInput bs) = putWord8 0 >> put bs
  put GhcRunInterrupt  = putWord8 1

  get = do
    header <- getWord8
    case header of
      0 -> GhcRunInput <$> get
      1 -> return GhcRunInterrupt
      _ -> fail "GhcRunRequest.get: invalid header"
