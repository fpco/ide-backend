-- | GHC requests
--
-- GHC requests use "IdeSession.Types.Public" types.
{-# LANGUAGE DeriveDataTypeable #-}
module IdeSession.GHC.Requests (
    GhcRequest(..)
  , GhcRunRequest(..)
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
      , reqCompileGenCode   :: Bool
      }
  | ReqRun {
        reqRunModule   :: String
      , reqRunFun      :: String
      , reqRunOutBMode :: RunBufferMode
      , reqRunErrBMode :: RunBufferMode
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
    -- | For debugging only! :)
  | ReqCrash {
         reqCrashDelay :: Maybe Int
       }
  deriving Typeable

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
    put reqCompileGenCode
  put ReqRun{..} = do
    putWord8 1
    put reqRunModule
    put reqRunFun
    put reqRunOutBMode
    put reqRunErrBMode
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
  put ReqCrash{..} = do
    putWord8 5
    put reqCrashDelay

  get = do
    header <- getWord8
    case header of
      0 -> ReqCompile <$> get <*> get <*> get
      1 -> ReqRun     <$> get <*> get <*> get <*> get
      2 -> ReqSetEnv  <$> get
      3 -> ReqSetArgs <$> get
      4 -> ReqBreakpoint <$> get <*> get <*> get
      5 -> ReqCrash   <$> get
      _ -> fail "GhcRequest.get: invalid header"

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
