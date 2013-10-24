{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
-- | The private types
module IdeSession.Types.Private (
    -- * Types without a public counterpart
    FilePathPtr(..)
  , IdPropPtr(..)
  , UseSites
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
  , IdList
  , IdMap(..)
  , ExpMap(..)
  , SpanInfo(..)
  , ImportEntities(..)
  , Import(..)
  , RunResult(..)
    -- * Cache
  , ExplicitSharingCache(..)
    -- * Util
  , mkIdMap
  , mkExpMap
  , dominators
  ) where

import Prelude hiding (span)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first)
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Typeable (Typeable)

import qualified IdeSession.Types.Public as Public
import IdeSession.Strict.Container
import IdeSession.Strict.IntervalMap (StrictIntervalMap, Interval(..))
import qualified IdeSession.Strict.IntervalMap as IntervalMap

newtype FilePathPtr = FilePathPtr { filePathPtr :: Int }
  deriving (Eq, Ord, Show)

newtype IdPropPtr = IdPropPtr { idPropPtr :: Int }
  deriving (Eq, Ord, Show)

data IdInfo = IdInfo {
    idProp  :: {-# UNPACK #-} !IdPropPtr
  , idScope :: !IdScope
  }
  deriving Show

data IdProp = IdProp {
    idName       :: !Text
  , idSpace      :: !Public.IdNameSpace
  , idType       :: !(Strict Maybe Text)
  , idDefinedIn  :: {-# UNPACK #-} !ModuleId
  , idDefSpan    :: !EitherSpan
  , idHomeModule :: !(Strict Maybe ModuleId)
  }
  deriving Show

data IdScope =
    -- | This is a binding occurrence (@f x = ..@, @\x -> ..@, etc.)
    Binder
    -- | Defined within this module
  | Local
    -- | Imported from a different module
  | Imported {
        idImportedFrom :: {-# UNPACK #-} !ModuleId
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
  deriving Show

data SourceSpan = SourceSpan
  { spanFilePath   :: {-# UNPACK #-} !FilePathPtr
  , spanFromLine   :: {-# UNPACK #-} !Int
  , spanFromColumn :: {-# UNPACK #-} !Int
  , spanToLine     :: {-# UNPACK #-} !Int
  , spanToColumn   :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Ord, Show)

data EitherSpan =
    ProperSpan {-# UNPACK #-} !SourceSpan
  | TextSpan !Text
  deriving Show

data SourceError = SourceError
  { errorKind :: !Public.SourceErrorKind
  , errorSpan :: !EitherSpan
  , errorMsg  :: !Text
  }
  deriving (Show)

data ModuleId = ModuleId
  { moduleName    :: !Public.ModuleName
  , modulePackage :: {-# UNPACK #-} !PackageId
  }
  deriving (Show, Eq)

data PackageId = PackageId
  { packageName    :: !Text
  , packageVersion :: !(Strict Maybe Text)
  }
  deriving (Show, Eq, Ord)

-- | Used before we convert it to an IdMap
type IdList = [(SourceSpan, SpanInfo)]

data SpanInfo =
   SpanId IdInfo
 | SpanQQ IdInfo
 deriving Show

newtype IdMap = IdMap { idMapToMap :: StrictIntervalMap (FilePathPtr, Int, Int) SpanInfo }
  deriving Show

newtype ExpMap = ExpMap { expMapToMap :: StrictIntervalMap (FilePathPtr, Int, Int) Text }
  deriving Show

type UseSites = Strict (Map IdPropPtr) [SourceSpan]

data ImportEntities =
    ImportOnly   !(Strict [] Text)
  | ImportHiding !(Strict [] Text)
  | ImportAll
  deriving (Show, Eq)

data Import = Import {
    importModule    :: !ModuleId
  -- | Used only for ghc's PackageImports extension
  , importPackage   :: !(Strict Maybe Text)
  , importQualified :: !Bool
  , importImplicit  :: !Bool
  , importAs        :: !(Strict Maybe Public.ModuleName)
  , importEntities  :: !ImportEntities
  }
  deriving (Show, Eq)

-- | The outcome of running code
data RunResult =
    -- | The code terminated okay
    RunOk String
    -- | The code threw an exception
  | RunProgException String
    -- | GHC itself threw an exception when we tried to run the code
  | RunGhcException String
  deriving (Typeable, Show, Eq)

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
  deriving Show

{------------------------------------------------------------------------------
  Binary instances
------------------------------------------------------------------------------}

instance Binary FilePathPtr where
  put = put . filePathPtr
  get = FilePathPtr <$> get

instance Binary SourceSpan where
  put SourceSpan{..} = do
    put spanFilePath
    put spanFromLine
    put spanFromColumn
    put spanToLine
    put spanToColumn
  get = SourceSpan <$> get <*> get <*> get <*> get <*> get

instance Binary EitherSpan where
  put (ProperSpan span) = putWord8 0 >> put span
  put (TextSpan text)   = putWord8 1 >> put text

  get = do
    header <- getWord8
    case header of
      0 -> ProperSpan <$> get
      1 -> TextSpan <$> get
      _ -> fail "EitherSpan.get: invalid header"

instance Binary SourceError where
  put SourceError{..} = do
    put errorKind
    put errorSpan
    put errorMsg

  get = SourceError <$> get <*> get <*> get

instance Binary IdInfo where
  put IdInfo{..} = put idProp >> put idScope
  get = IdInfo <$> get <*> get

instance Binary IdScope where
  put Binder       = putWord8 0
  put Local        = do putWord8 1
  put Imported{..} = do putWord8 2
                        put idImportedFrom
                        put idImportSpan
                        put idImportQual
  put WiredIn      = putWord8 3

  get = do
    header <- getWord8
    case header of
      0 -> return Binder
      1 -> return Local
      2 -> Imported <$> get <*> get <*> get
      3 -> return WiredIn
      _ -> fail "IdScope.get: invalid header"

instance Binary IdPropPtr where
  put = put . idPropPtr
  get = IdPropPtr <$> get

instance Binary ModuleId where
  put ModuleId{..} = put moduleName >> put modulePackage
  get = ModuleId <$> get <*> get

instance Binary PackageId where
  put PackageId{..} = put packageName >> put packageVersion
  get = PackageId <$> get <*> get

instance Binary IdProp where
  put IdProp{..} = do
    put idName
    put idSpace
    put idType
    put idDefinedIn
    put idDefSpan
    put idHomeModule

  get = IdProp <$> get <*> get <*> get <*> get <*> get <*> get

{-
instance Binary IdMap where
  put = put . idMapToMap
  get = IdMap <$> get
-}

instance Binary ImportEntities where
  put (ImportOnly names)   = putWord8 0 >> put names
  put (ImportHiding names) = putWord8 1 >> put names
  put ImportAll            = putWord8 2

  get = do
    header <- getWord8
    case header of
      0 -> ImportOnly   <$> get
      1 -> ImportHiding <$> get
      2 -> return ImportAll
      _ -> fail "ImportEntities.get: invalid header"

instance Binary Import where
  put Import{..} = do
    put importModule
    put importPackage
    put importQualified
    put importImplicit
    put importAs
    put importEntities

  get = Import <$> get <*> get <*> get <*> get <*> get <*> get

instance Binary ExplicitSharingCache where
  put ExplicitSharingCache{..} = do
    put filePathCache
    put idPropCache

  get = ExplicitSharingCache <$> get <*> get

instance Binary SpanInfo where
  put (SpanId idInfo) = putWord8 0 >> put idInfo
  put (SpanQQ idInfo) = putWord8 1 >> put idInfo

  get = do
    header <- getWord8
    case header of
      0 -> SpanId <$> get
      1 -> SpanQQ <$> get
      _ -> fail "SpanInfo.get: invalid header"

instance Binary RunResult where
  put (RunOk str)            = putWord8 0 >> put str
  put (RunProgException str) = putWord8 1 >> put str
  put (RunGhcException str)  = putWord8 2 >> put str

  get = do
    header <- getWord8
    case header of
      0 -> RunOk <$> get
      1 -> RunProgException <$> get
      2 -> RunGhcException <$> get
      _ -> fail "RunResult.get: invalid header"

{------------------------------------------------------------------------------
  Util
------------------------------------------------------------------------------}

mkIdMap :: IdList -> IdMap
mkIdMap = IdMap . IntervalMap.fromList . map (first spanToInterval)

mkExpMap :: [(SourceSpan, Text)] -> ExpMap
mkExpMap = ExpMap . IntervalMap.fromList . map (first spanToInterval)

dominators :: SourceSpan -> StrictIntervalMap (FilePathPtr, Int, Int) a -> [(SourceSpan, a)]
dominators span ivalmap =
    map (\(ival, idInfo) -> (intervalToSpan ival, idInfo))
        (IntervalMap.dominators (spanToInterval span) ivalmap)

spanToInterval :: SourceSpan -> Interval (FilePathPtr, Int, Int)
spanToInterval SourceSpan{..} =
  Interval (spanFilePath, spanFromLine, spanFromColumn)
           (spanFilePath, spanToLine, spanToColumn)

intervalToSpan :: Interval (FilePathPtr, Int, Int) -> SourceSpan
intervalToSpan (Interval (spanFilePath, spanFromLine, spanFromColumn)
                         (_,            spanToLine, spanToColumn)) =
  SourceSpan{..}
