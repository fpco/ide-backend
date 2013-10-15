-- | Types for the messages to and fro the GHC server
--
-- It is important that none of the types here rely on the GHC library.
{-# LANGUAGE TemplateHaskell #-}
module IdeSession.GHC.API (
    -- * Requests
    GhcRequest(..)
  , GhcRunRequest(..)
  , RunBufferMode(..)
    -- * Responses
  , GhcCompileResponse(..)
  , GhcCompileResult(..)
  , GhcRunResponse(..)
  , RunResult(..)
    -- * Configuration
  , ideBackendApiVersion
  , hsExtensions
  , cabalMacrosLocation
  ) where

import Data.Binary
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Control.Applicative ((<$>), (<*>))
import System.FilePath ((</>))

import IdeSession.Types.Private
import IdeSession.Types.Progress
import IdeSession.Strict.Container
import IdeSession.Util (Diff)

-- | For detecting runtime version mismatch between the server and the library
--
-- We use a Unix timestamp for this so that these API versions have some
-- semantics (http://www.epochconverter.com/, GMT).
ideBackendApiVersion :: Int
ideBackendApiVersion = 1381839823

{------------------------------------------------------------------------------
  Requests
------------------------------------------------------------------------------}

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
    -- | For debugging only! :)
  | ReqCrash {
         reqCrashDelay :: Maybe Int
       }

data GhcRunRequest =
    GhcRunInput ByteString
  | GhcRunInterrupt
  | GhcRunAckDone

-- | Buffer modes for running code
--
-- Note that 'NoBuffering' means that something like 'putStrLn' will do a
-- syscall per character, and each of these characters will be read and sent
-- back to the client. This results in a large overhead.
--
-- When using 'LineBuffering' or 'BlockBuffering', 'runWait' will not report
-- any output from the snippet until it outputs a linebreak/fills the buffer,
-- respectively (or does an explicit flush). However, you can specify a timeout
-- in addition to the buffering mode; if you set this to @Just n@, the buffer
-- will be flushed every @n@ microseconds.
--
-- NOTE: This is duplicated in the IdeBackendRTS (defined in IdeSession)
data RunBufferMode =
    RunNoBuffering
  | RunLineBuffering  { runBufferTimeout   :: Maybe Int }
  | RunBlockBuffering { runBufferBlockSize :: Maybe Int
                      , runBufferTimeout   :: Maybe Int
                      }
  deriving Show

{------------------------------------------------------------------------------
  Responses
------------------------------------------------------------------------------}

data GhcCompileResponse =
    GhcCompileProgress Progress
  | GhcCompileDone GhcCompileResult

data GhcCompileResult = GhcCompileResult {
    ghcCompileErrors   :: Strict [] SourceError
  , ghcCompileLoaded   :: Strict [] ModuleName
  , ghcCompileImports  :: Strict (Map ModuleName) (Diff (Strict [] Import))
  , ghcCompileAuto     :: Strict (Map ModuleName) (Diff (Strict [] IdInfo))
  , ghcCompileSpanInfo :: Strict (Map ModuleName) (Diff IdList)
  , ghcCompileExpTypes :: Strict (Map ModuleName) (Diff [(SourceSpan, Text)])
  , ghcCompilePkgDeps  :: Strict (Map ModuleName) (Diff (Strict [] PackageId))
  , ghcCompileCache    :: ExplicitSharingCache
  }

data GhcRunResponse =
    GhcRunOutp ByteString
  | GhcRunDone RunResult

-- | The outcome of running code
data RunResult =
    -- | The code terminated okay
    RunOk String
    -- | The code threw an exception
  | RunProgException String
    -- | GHC itself threw an exception when we tried to run the code
  | RunGhcException String
    -- | The session was restarted
  | RunForceCancelled
  deriving (Show, Eq)

{------------------------------------------------------------------------------
  Binary instances
------------------------------------------------------------------------------}

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
  put ReqCrash{..} = do
    putWord8 4
    put reqCrashDelay

  get = do
    header <- getWord8
    case header of
      0 -> ReqCompile <$> get <*> get <*> get
      1 -> ReqRun     <$> get <*> get <*> get <*> get
      2 -> ReqSetEnv  <$> get
      3 -> ReqSetArgs <$> get
      4 -> ReqCrash   <$> get
      _ -> fail "GhcRequest.get: invalid header"

instance Binary GhcCompileResponse where
  put (GhcCompileProgress progress) = putWord8 0 >> put progress
  put (GhcCompileDone result)       = putWord8 1 >> put result

  get = do
    header <- getWord8
    case header of
      0 -> GhcCompileProgress <$> get
      1 -> GhcCompileDone     <$> get
      _ -> fail "GhcCompileRespone.get: invalid header"

instance Binary GhcCompileResult where
  put GhcCompileResult{..} = do
    put ghcCompileErrors
    put ghcCompileLoaded
    put ghcCompileImports
    put ghcCompileAuto
    put ghcCompileSpanInfo
    put ghcCompileExpTypes
    put ghcCompilePkgDeps
    put ghcCompileCache

  get = GhcCompileResult <$> get <*> get <*> get
                         <*> get <*> get <*> get
                         <*> get <*> get

instance Binary GhcRunResponse where
  put (GhcRunOutp bs) = putWord8 0 >> put bs
  put (GhcRunDone r)  = putWord8 1 >> put r

  get = do
    header <- getWord8
    case header of
      0 -> GhcRunOutp <$> get
      1 -> GhcRunDone <$> get
      _ -> fail "GhcRunResponse.get: invalid header"

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

instance Binary RunResult where
  put (RunOk str)            = putWord8 0 >> put str
  put (RunProgException str) = putWord8 1 >> put str
  put (RunGhcException str)  = putWord8 2 >> put str
  put RunForceCancelled      = putWord8 3

  get = do
    header <- getWord8
    case header of
      0 -> RunOk <$> get
      1 -> RunProgException <$> get
      2 -> RunGhcException <$> get
      3 -> return RunForceCancelled
      _ -> fail "RunResult.get: invalid header"

instance Binary RunBufferMode where
  put RunNoBuffering        = putWord8 0
  put RunLineBuffering{..}  = do putWord8 1
                                 put runBufferTimeout
  put RunBlockBuffering{..} = do putWord8 2
                                 put runBufferBlockSize
                                 put runBufferTimeout

  get = do
    header <- getWord8
    case header of
      0 -> return RunNoBuffering
      1 -> RunLineBuffering <$> get
      2 -> RunBlockBuffering <$> get <*> get
      _ -> fail "RunBufferMode.get: invalid header"

{------------------------------------------------------------------------------
  JSON instances (for the convenience of client code)
------------------------------------------------------------------------------}

$(deriveJSON defaultOptions ''RunResult)
$(deriveJSON defaultOptions ''RunBufferMode)

{------------------------------------------------------------------------------
  Configuration
------------------------------------------------------------------------------}

-- | These source files are type-checked.
hsExtensions :: [FilePath]
hsExtensions = [".hs", ".lhs"]
-- Boot files are not so simple. They should probably be copied to the src dir,
-- but not made proper targets. This is probably similar to .h files.
-- hsExtentions = [".hs", ".lhs", ".hs-boot", ".lhs-boot", ".hi-boot"]

cabalMacrosLocation :: FilePath -> FilePath
cabalMacrosLocation ideSourcesDir = ideSourcesDir </> "cabal_macros.h"
