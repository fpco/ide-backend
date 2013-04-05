{-# LANGUAGE TemplateHaskell #-}
-- | The private types
module IdeSession.Types.Private (
    -- * Types without a public counterpart
    FilePathPtr(..)
  , IdPropPtr(..)
    -- * Types with a public counterpart
  , Public.IdNameSpace(..)
  , IdInfo(..)
  , IdProp(..)
  , IdScope(..)
  , SourceSpan(..)
  , EitherSpan(..)
  , SourceError(..)
  , Public.SourceErrorKind(..)
  , Public.ModuleName
  , ModuleId(..)
  , PackageId(..)
  , IdMap(..)
  , LoadedModules
  , Public.Import(..)
    -- * Cache
  , ExplicitSharingCache(..)
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.TH (deriveJSON)
import qualified Data.IntMap as IntMap

import qualified IdeSession.Types.Public as Public

-- TODOs:
-- 1. Get rid of strings
-- 2. Replace Map with StrictMap
-- 3. Replace Maybe with StrictMaybe
-- 4. Replace [] with StrictList (or perhaps even arrays)
-- 5. Replace IntMap with StrictIntMap

newtype FilePathPtr = FilePathPtr { filePathPtr :: Int }
  deriving (Eq, Ord)

newtype IdPropPtr = IdPropPtr { idPropPtr :: Int }
  deriving (Eq, Ord)

data IdInfo = IdInfo {
    idProp  :: {-# UNPACK #-} !IdPropPtr
  , idScope :: !IdScope
  }

data IdProp = IdProp {
    idName  :: !String
  , idSpace :: !Public.IdNameSpace
  , idType  :: !(Maybe String)
  }
  deriving (Eq)

data IdScope =
    -- | This is a binding occurrence (@f x = ..@, @\x -> ..@, etc.)
    Binder
    -- | Defined within this module
  | Local {
        idDefSpan :: !EitherSpan
      }
    -- | Imported from a different module
  | Imported {
        idDefSpan      :: !EitherSpan
      , idDefinedIn    :: {-# UNPACK #-} !ModuleId
      , idImportedFrom :: {-# UNPACK #-} !ModuleId
      , idImportSpan   :: !EitherSpan
        -- | Qualifier used for the import
        --
        -- > IMPORTED AS                       idImportQual
        -- > import Data.List                  ""
        -- > import qualified Data.List        "Data.List."
        -- > import qualified Data.List as L   "L."
      , idImportQual   :: !String
      }
    -- | Wired into the compiler (@()@, @True@, etc.)
  | WiredIn
  deriving (Eq)

data SourceSpan = SourceSpan
  { spanFilePath   :: {-# UNPACK #-} !FilePathPtr
  , spanFromLine   :: {-# UNPACK #-} !Int
  , spanFromColumn :: {-# UNPACK #-} !Int
  , spanToLine     :: {-# UNPACK #-} !Int
  , spanToColumn   :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Ord)

data EitherSpan =
    ProperSpan {-# UNPACK #-} !SourceSpan
  | TextSpan !String
  deriving (Eq)

data SourceError = SourceError
  { errorKind :: !Public.SourceErrorKind
  , errorSpan :: !EitherSpan
  , errorMsg  :: !String
  }
  deriving (Eq)

data ModuleId = ModuleId
  { moduleName    :: !Public.ModuleName
  , modulePackage :: {-# UNPACK #-} !PackageId
  }
  deriving (Eq)

data PackageId = PackageId
  { packageName    :: !String
  , packageVersion :: !(Maybe String)
  }
  deriving (Eq)

newtype IdMap = IdMap { idMapToMap :: Map (SourceSpan) (IdInfo) }

type LoadedModules = Map Public.ModuleName (IdMap)

{------------------------------------------------------------------------------
  Cache
------------------------------------------------------------------------------}

data ExplicitSharingCache = ExplicitSharingCache {
    filePathCache :: !(IntMap FilePath)
  , idPropCache   :: !(IntMap IdProp)
  }

{------------------------------------------------------------------------------
  JSON
------------------------------------------------------------------------------}

$(deriveJSON id ''FilePathPtr)
$(deriveJSON id ''SourceSpan)
$(deriveJSON id ''EitherSpan)
$(deriveJSON id ''SourceError)
$(deriveJSON id ''IdInfo)
$(deriveJSON id ''IdScope)
$(deriveJSON id ''IdPropPtr)
$(deriveJSON id ''ModuleId)
$(deriveJSON id ''PackageId)
$(deriveJSON id ''IdProp)

instance FromJSON IdMap where
  parseJSON = fmap (IdMap . Map.fromList) . parseJSON

instance ToJSON IdMap where
  toJSON = toJSON . Map.toList . idMapToMap

instance FromJSON ExplicitSharingCache where
  parseJSON = fmap aux . parseJSON
    where
      aux :: ( [(Int, FilePath)]
             , [(Int, IdProp)]
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
             , [(Int, IdProp)]
             )
      aux ExplicitSharingCache {..} = (
          IntMap.toList filePathCache
        , IntMap.toList idPropCache
        )
