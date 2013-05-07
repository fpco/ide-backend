{-# LANGUAGE TypeFamilies, ScopedTypeVariables, TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
-- | Translation from the private to the public types
module IdeSession.Types.Translation (
    XShared
  , ExplicitSharing(..)
  , IntroduceSharing(..)
  , showNormalized
  ) where

import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as BSSC
import Data.Binary (Binary)

import IdeSession.Strict.Container
import qualified IdeSession.Types.Public  as Public
import qualified IdeSession.Types.Private as Private
import qualified IdeSession.Strict.IntMap as StrictIntMap
import qualified IdeSession.Strict.Map    as StrictMap

-- | The associated type with explicit sharing
type family XShared a

-- | The inverse of MShared, only for decidability of type checking
type family MShared a

type instance XShared Public.IdProp          = Private.IdProp
type instance XShared Public.IdInfo          = Private.IdInfo
type instance XShared Public.IdScope         = Private.IdScope
type instance XShared Public.SourceSpan      = Private.SourceSpan
type instance XShared Public.EitherSpan      = Private.EitherSpan
type instance XShared Public.SourceError     = Private.SourceError
type instance XShared Public.IdMap           = Private.IdMap
type instance XShared Public.LoadedModules   = Private.LoadedModules
type instance XShared Public.ModuleId        = Private.ModuleId
type instance XShared Public.PackageId       = Private.PackageId
type instance XShared Public.ImportEntities  = Private.ImportEntities
type instance XShared Public.Import          = Private.Import

type instance MShared Private.IdProp         = Public.IdProp
type instance MShared Private.IdInfo         = Public.IdInfo
type instance MShared Private.IdScope        = Public.IdScope
type instance MShared Private.SourceSpan     = Public.SourceSpan
type instance MShared Private.EitherSpan     = Public.EitherSpan
type instance MShared Private.SourceError    = Public.SourceError
type instance MShared Private.IdMap          = Public.IdMap
type instance MShared Private.LoadedModules  = Public.LoadedModules
type instance MShared Private.ModuleId       = Public.ModuleId
type instance MShared Private.PackageId      = Public.PackageId
type instance MShared Private.ImportEntities = Public.ImportEntities
type instance MShared Private.Import         = Public.Import

{------------------------------------------------------------------------------
  Removing explicit sharing
------------------------------------------------------------------------------}

-- | Many of the public data types that we export in "IdeSession" have a
-- corresponding private @XShared@ version. For instance, we have @IdProp@ and
-- @XShared IdProp@, @SourceError@ and @XShared SourceError@, etc. These
-- @XShared@ types are abstract; what's important is only that they can be
-- serialized (support @FromJSON@ and @ToJSON@). The main difference between
-- the public and the private data types is that the private data types use
-- explicit sharing. This is important for serialization, because there is
-- quite a bit of sharing in the type information that we collect and losing
-- this would be a significant performance hit. (The other difference is that
-- the private data types use specialized types that guarantee strictness.)
--
-- The @MShared (XShared a) ~ a@ condition on the @ExplicitSharing@ type class
-- is there for technical reasons only (it convinces GHC that the @XShared@
-- type family is a bijection).
class (MShared (XShared a) ~ a, Binary (XShared a)) => ExplicitSharing a where
  removeExplicitSharing :: Private.ExplicitSharingCache -> XShared a -> a

showNormalized :: forall a. (Show a, ExplicitSharing a)
               => Private.ExplicitSharingCache -> XShared a -> String
showNormalized cache x = show (removeExplicitSharing cache x :: a)

instance ExplicitSharing Public.IdProp where
  removeExplicitSharing _cache Private.IdProp{..} = Public.IdProp {
      Public.idName  = idName
    , Public.idSpace = idSpace
    , Public.idType  = toLazyMaybe idType
    }

instance ExplicitSharing Public.IdInfo where
  removeExplicitSharing cache Private.IdInfo{..} = Public.IdInfo {
      Public.idProp  = removeExplicitSharing cache (Private.idPropCache cache StrictIntMap.! Private.idPropPtr idProp)
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
    , Public.packageVersion = toLazyMaybe packageVersion
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
      Public.spanFilePath   = BSSC.unpack $ Private.filePathCache cache StrictIntMap.! Private.filePathPtr spanFilePath
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

{-
instance ExplicitSharing Public.IdMap where
  removeExplicitSharing cache = Public.IdMap
                              . toLazyMap
                              . StrictMap.map (removeExplicitSharing cache)
                              . StrictMap.mapKeys (removeExplicitSharing cache)
                              . Private.idMapToMap
-}

{-
instance ExplicitSharing Public.LoadedModules where
  removeExplicitSharing cache = Map.map (removeExplicitSharing cache)
                              . toLazyMap
-}

instance ExplicitSharing Public.ImportEntities where
  removeExplicitSharing _cache entities = case entities of
    Private.ImportAll          -> Public.ImportAll
    Private.ImportHiding names -> Public.ImportHiding (toLazyList names)
    Private.ImportOnly   names -> Public.ImportOnly (toLazyList names)

instance ExplicitSharing Public.Import where
  removeExplicitSharing cache Private.Import{..} = Public.Import {
      Public.importModule     = removeExplicitSharing cache $ importModule
    , Public.importPackage    = toLazyMaybe importPackage
    , Public.importQualified  = importQualified
    , Public.importImplicit   = importImplicit
    , Public.importAs         = toLazyMaybe importAs
    , Public.importEntities   = removeExplicitSharing cache $ importEntities
    }

{------------------------------------------------------------------------------
  Introducing explicit sharing
------------------------------------------------------------------------------}

-- | Introduce explicit sharing
--
-- This provides the opposite translation to removeExplicitSharing. Note however
-- that this is a partial function -- we never extend the cache, so if a
-- required value is missing from the cache we return @Nothing@.
class IntroduceSharing a where
  introduceExplicitSharing :: Private.ExplicitSharingCache -> a -> Maybe (XShared a)

instance IntroduceSharing Public.SourceSpan where
  introduceExplicitSharing cache Public.SourceSpan{..} = do
    ptr <- StrictIntMap.reverseLookup (Private.filePathCache cache)
                                      (BSSC.pack spanFilePath)
    return Private.SourceSpan {
        Private.spanFilePath   = Private.FilePathPtr ptr
      , Private.spanFromLine   = spanFromLine
      , Private.spanFromColumn = spanFromColumn
      , Private.spanToLine     = spanToLine
      , Private.spanToColumn   = spanToColumn
      }

