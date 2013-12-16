-- | Responses from the GHC server
--
-- The server responds with "IdeSession.Types.Private" types
{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
module IdeSession.GHC.Responses (
    GhcCompileResponse(..)
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

data GhcCompileResponse =
    GhcCompileProgress Progress
  | GhcCompileDone GhcCompileResult
  deriving (Typeable, Generic)

data GhcCompileResult = GhcCompileResult {
    ghcCompileErrors   :: Strict [] SourceError
  , ghcCompileLoaded   :: Strict [] ModuleName
  , ghcCompileCache    :: ExplicitSharingCache
  -- Computed from the GhcSummary (independent of the plugin, and hence
  -- available even when the plugin does not run)
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
data GhcVersion = GHC742 | GHC78
  deriving (Typeable, Show, Eq, Generic)

instance PrettyVal GhcCompileResponse
instance PrettyVal GhcCompileResult
instance PrettyVal GhcRunResponse
instance PrettyVal GhcVersion

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
    put ghcCompileImports
    put ghcCompileAuto
    put ghcCompileSpanInfo
    put ghcCompilePkgDeps
    put ghcCompileExpTypes
    put ghcCompileUseSites

  get = GhcCompileResult <$> get <*> get <*> get
                         <*> get <*> get <*> get
                         <*> get <*> get <*> get

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
  put GHC742 = putWord8 0
  put GHC78  = putWord8 1

  get = do
    header <- getWord8
    case header of
      0 -> return GHC742
      1 -> return GHC78
      _ -> fail "GhcVersion.get: invalid header"
