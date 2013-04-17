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
  , ImportEntities(..)
  , Import(..)
    -- * Cache
  , ExplicitSharingCache(..)
  ) where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Aeson.TH (deriveJSON)

import qualified IdeSession.Types.Public as Public
import IdeSession.Strict.Container

newtype FilePathPtr = FilePathPtr { filePathPtr :: Int }
  deriving (Eq, Ord)

newtype IdPropPtr = IdPropPtr { idPropPtr :: Int }

data IdInfo = IdInfo {
    idProp  :: {-# UNPACK #-} !IdPropPtr
  , idScope :: !IdScope
  }

data IdProp = IdProp {
    idName  :: !Text
  , idSpace :: !Public.IdNameSpace
  , idType  :: !(Strict Maybe Text)
  }

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
      , idImportQual   :: !Text
      }
    -- | Wired into the compiler (@()@, @True@, etc.)
  | WiredIn

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
  | TextSpan !Text

data SourceError = SourceError
  { errorKind :: !Public.SourceErrorKind
  , errorSpan :: !EitherSpan
  , errorMsg  :: !Text
  }

data ModuleId = ModuleId
  { moduleName    :: !Public.ModuleName
  , modulePackage :: {-# UNPACK #-} !PackageId
  }
  deriving Eq

data PackageId = PackageId
  { packageName    :: !Text
  , packageVersion :: !(Strict Maybe Text)
  }
  deriving Eq

newtype IdMap = IdMap { idMapToMap :: Strict (Map SourceSpan) IdInfo }

type LoadedModules = Strict (Map Public.ModuleName) IdMap

data ImportEntities =
    ImportOnly   !(Strict [] Text)
  | ImportHiding !(Strict [] Text)
  | ImportAll
  deriving Eq

data Import = Import {
    importModule    :: !ModuleId
  -- | Used only for ghc's PackageImports extension
  , importPackage   :: !(Strict Maybe Text)
  , importQualified :: !Bool
  , importImplicit  :: !Bool
  , importAs        :: !(Strict Maybe Public.ModuleName)
  , importEntities  :: !ImportEntities
  }
  deriving Eq

{------------------------------------------------------------------------------
  Cache
------------------------------------------------------------------------------}

-- TODO: Since the ExplicitSharingCache contains internal types, resolving
-- references to the cache means we lose implicit sharing because we need
-- to translate on every lookup. To avoid this, we'd have to introduce two
-- versions of the cache and translate the entire cache first.
data ExplicitSharingCache = ExplicitSharingCache {
    filePathCache :: !(Strict IntMap ByteString)
  , idPropCache   :: !(Strict IntMap IdProp)
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
$(deriveJSON id ''IdMap)
$(deriveJSON id ''ImportEntities)
$(deriveJSON id ''Import)
$(deriveJSON id ''ExplicitSharingCache)
