{-# LANGUAGE TypeFamilies, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances #-}
-- | Translation from the private to the public types
module IdeSession.Types.Translation (
    XShared
  , ExplicitSharingCache(..)
  , ExplicitSharing(..)
  , showNormalized
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Map as Map
import Data.Aeson (FromJSON(..), ToJSON(..))

import qualified IdeSession.Types.Public  as Public
import qualified IdeSession.Types.Private as Private

-- TODO: Use IdeSession.Strict.IntMap
data ExplicitSharingCache = ExplicitSharingCache {
    filePathCache :: !(IntMap FilePath)
  , idPropCache   :: !(IntMap Private.IdProp)
  }

-- | The associated type with explicit sharing
type family XShared a

-- | The inverse of MShared, only for decidability of type checking
type family MShared a

type instance XShared Public.IdProp         = Private.IdProp
type instance XShared Public.IdInfo         = Private.IdInfo
type instance XShared Public.IdScope        = Private.IdScope
type instance XShared Public.SourceSpan     = Private.SourceSpan
type instance XShared Public.EitherSpan     = Private.EitherSpan
type instance XShared Public.SourceError    = Private.SourceError
type instance XShared Public.IdMap          = Private.IdMap
type instance XShared Public.LoadedModules  = Private.LoadedModules
type instance XShared Public.ModuleId       = Private.ModuleId
type instance XShared Public.PackageId      = Private.PackageId

type instance MShared Private.IdProp        = Public.IdProp
type instance MShared Private.IdInfo        = Public.IdInfo
type instance MShared Private.IdScope       = Public.IdScope
type instance MShared Private.SourceSpan    = Public.SourceSpan
type instance MShared Private.EitherSpan    = Public.EitherSpan
type instance MShared Private.SourceError   = Public.SourceError
type instance MShared Private.IdMap         = Public.IdMap
type instance MShared Private.LoadedModules = Public.LoadedModules
type instance MShared Private.ModuleId      = Public.ModuleId
type instance MShared Private.PackageId     = Public.PackageId

class MShared (XShared a) ~ a => ExplicitSharing a where
  removeExplicitSharing :: ExplicitSharingCache -> XShared a -> a

showNormalized :: forall a. (Show a, ExplicitSharing a)
               => ExplicitSharingCache -> XShared a -> String
showNormalized cache x = show (removeExplicitSharing cache x :: a)

instance ExplicitSharing Public.IdProp where
  removeExplicitSharing _cache Private.IdProp{..} = Public.IdProp {
      Public.idName  = idName
    , Public.idSpace = idSpace
    , Public.idType  = idType
    }

instance ExplicitSharing Public.IdInfo where
  removeExplicitSharing cache Private.IdInfo{..} = Public.IdInfo {
      Public.idProp  = removeExplicitSharing cache (idPropCache cache IntMap.! Private.idPropPtr idProp)
    , Public.idScope = removeExplicitSharing cache idScope
    }

instance ExplicitSharing Public.ModuleId where
  removeExplicitSharing cache Private.ModuleId{..} = Public.ModuleId {
      Public.moduleName    = moduleName
    , Public.modulePackage = removeExplicitSharing cache modulePackage
    }

instance ExplicitSharing Public.PackageId where
  removeExplicitSharing _cache Private.PackageId{..} = Public.PackageId {
      Public.packageName    = packageName
    , Public.packageVersion = packageVersion
    }

instance ExplicitSharing Public.IdScope where
  removeExplicitSharing cache idScope = case idScope of
    Private.Binder -> Public.Binder
    Private.Local {..} -> Public.Local {
        Public.idDefSpan = removeExplicitSharing cache idDefSpan
      }
    Private.Imported {..} -> Public.Imported {
        Public.idDefSpan      = removeExplicitSharing cache idDefSpan
      , Public.idDefinedIn    = removeExplicitSharing cache idDefinedIn
      , Public.idImportedFrom = removeExplicitSharing cache idImportedFrom
      , Public.idImportSpan   = removeExplicitSharing cache idImportSpan
      , Public.idImportQual   = idImportQual
      }
    Private.WiredIn -> Public.WiredIn

instance ExplicitSharing Public.SourceSpan where
  removeExplicitSharing cache Private.SourceSpan{..} = Public.SourceSpan {
      Public.spanFilePath   = filePathCache cache IntMap.! Private.filePathPtr spanFilePath
    , Public.spanFromLine   = spanFromLine
    , Public.spanFromColumn = spanFromColumn
    , Public.spanToLine     = spanToLine
    , Public.spanToColumn   = spanToColumn
    }

instance ExplicitSharing Public.EitherSpan where
  removeExplicitSharing cache eitherSpan = case eitherSpan of
    Private.ProperSpan sourceSpan ->
      Public.ProperSpan (removeExplicitSharing cache sourceSpan)
    Private.TextSpan str ->
      Public.TextSpan str

instance ExplicitSharing Public.SourceError where
  removeExplicitSharing cache Private.SourceError{..} = Public.SourceError {
      Public.errorKind = errorKind
    , Public.errorSpan = removeExplicitSharing cache errorSpan
    , Public.errorMsg  = errorMsg
    }

instance ExplicitSharing Public.IdMap where
  removeExplicitSharing cache = Public.IdMap
                              . Map.map (removeExplicitSharing cache)
                              . Map.mapKeys (removeExplicitSharing cache)
                              . Private.idMapToMap

instance ExplicitSharing Public.LoadedModules where
  removeExplicitSharing = Map.map . removeExplicitSharing

{------------------------------------------------------------------------------
  JSON
------------------------------------------------------------------------------}

instance FromJSON ExplicitSharingCache where
  parseJSON = fmap aux . parseJSON
    where
      aux :: ( [(Int, FilePath)]
             , [(Int, Private.IdProp)]
             )
          -> ExplicitSharingCache
      aux (fpCache, idCache) = ExplicitSharingCache {
          filePathCache = IntMap.fromList fpCache
        , idPropCache   = IntMap.fromList idCache
        }

instance ToJSON ExplicitSharingCache where
  toJSON = toJSON . aux
    where
      aux :: ExplicitSharingCache
          -> ( [(Int, FilePath)]
             , [(Int, Private.IdProp)]
             )
      aux ExplicitSharingCache {..} = (
          IntMap.toList filePathCache
        , IntMap.toList idPropCache
        )
