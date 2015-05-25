{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, RecordWildCards #-}

-- | Responses from the GHC server
--
-- The server responds with "IdeSession.Types.Private" types
module IdeSession.GHC.Responses (
    GhcInitResponse(..)
  , GhcCompileResponse(..)
  , GhcCompileResult(..)
  , GhcRunResponse(..)
  , GhcVersion(..)
  ) where

import Data.Binary
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Control.Applicative ((<$>), (<*>))

import IdeSession.Types.Private
import IdeSession.Types.Progress
import IdeSession.Strict.Container
import IdeSession.Util (Diff)

import Text.Show.Pretty
import GHC.Generics

data GhcInitResponse = GhcInitResponse {
    ghcInitVersion :: GhcVersion
  }
  deriving (Typeable, Generic)

data GhcCompileResponse =
    GhcCompileProgress Progress
  | GhcCompileDone GhcCompileResult
  deriving (Typeable, Generic)

-- NOTE: These fields cannot be made strict (at least, not easily)
data GhcCompileResult = GhcCompileResult {
    ghcCompileErrors   :: Strict [] SourceError
  , ghcCompileLoaded   :: Strict [] ModuleName
  , ghcCompileCache    :: ExplicitSharingCache
  -- Computed from the GhcSummary (independent of the plugin, and hence
  -- available even when the plugin does not run)
  , ghcCompileFileMap  :: Strict (Map FilePath) ModuleId
  , ghcCompileImports  :: Strict (Map ModuleName) (Diff (Strict [] Import))
  , ghcCompileAuto     :: Strict (Map ModuleName) (Diff (Strict [] IdInfo))
  -- Computed by the plugin
  , ghcCompileSpanInfo :: Strict (Map ModuleName) (Diff IdList)
  , ghcCompilePkgDeps  :: Strict (Map ModuleName) (Diff (Strict [] PackageId))
  , ghcCompileExpTypes :: Strict (Map ModuleName) (Diff [(SourceSpan, Text)])
  , ghcCompileUseSites :: Strict (Map ModuleName) (Diff UseSites)
  }
  deriving (Typeable, Generic)

data GhcRunResponse =
    GhcRunOutp ByteString
  | GhcRunDone RunResult
  deriving (Typeable, Generic)

-- | GHC version
--
-- NOTE: Defined in such a way that the Ord instance makes sense.
data GhcVersion = GHC_7_4 | GHC_7_8 | GHC_7_10
  deriving (Typeable, Show, Eq, Ord, Generic)

instance PrettyVal GhcInitResponse
instance PrettyVal GhcCompileResponse
instance PrettyVal GhcCompileResult
instance PrettyVal GhcRunResponse
instance PrettyVal GhcVersion

instance Binary GhcInitResponse where
  put (GhcInitResponse{..}) = do
    put ghcInitVersion
  get = GhcInitResponse <$> get

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
    put ghcCompileCache
    put ghcCompileFileMap
    put ghcCompileImports
    put ghcCompileAuto
    put ghcCompileSpanInfo
    put ghcCompilePkgDeps
    put ghcCompileExpTypes
    put ghcCompileUseSites

  get = GhcCompileResult <$> get <*> get <*> get
                         <*> get <*> get <*> get
                         <*> get <*> get <*> get <*> get

instance Binary GhcRunResponse where
  put (GhcRunOutp bs) = putWord8 0 >> put bs
  put (GhcRunDone r)  = putWord8 1 >> put r

  get = do
    header <- getWord8
    case header of
      0 -> GhcRunOutp <$> get
      1 -> GhcRunDone <$> get
      _ -> fail "GhcRunResponse.get: invalid header"

instance Binary GhcVersion where
  put GHC_7_4  = putWord8 0
  put GHC_7_8  = putWord8 1
  put GHC_7_10 = putWord8 2

  get = do
    header <- getWord8
    case header of
      0 -> return GHC_7_4
      1 -> return GHC_7_8
      2 -> return GHC_7_10
      _ -> fail "GhcVersion.get: invalid header"
