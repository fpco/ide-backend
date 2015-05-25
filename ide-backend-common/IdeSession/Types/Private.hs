{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, DeriveDataTypeable, DeriveGeneric, RecordWildCards #-}

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
  , BreakInfo(..)
    -- * Cache
  , ExplicitSharingCache(..)
  , unionCache
    -- * Util
  , mkIdMap
  , mkExpMap
  , dominators
  ) where

import Prelude hiding (span, mod)
import Data.Text (Text)
import Data.ByteString (ByteString)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first)
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import qualified IdeSession.Types.Public as Public
import IdeSession.Strict.Container
import IdeSession.Strict.IntervalMap (StrictIntervalMap, Interval(..))
import qualified IdeSession.Strict.IntervalMap as IntervalMap
import qualified IdeSession.Strict.IntMap      as IntMap
import IdeSession.Util.PrettyVal

newtype FilePathPtr = FilePathPtr { filePathPtr :: Int }
  deriving (Eq, Ord, Show, Generic)

newtype IdPropPtr = IdPropPtr { idPropPtr :: Int }
  deriving (Eq, Ord, Show, Generic)

data IdInfo = IdInfo {
    idProp  :: {-# UNPACK #-} !IdPropPtr
  , idScope :: !IdScope
  }
  deriving (Show, Typeable, Generic)

data IdProp = IdProp {
    idName       :: !Text
  , idSpace      :: !Public.IdNameSpace
  , idType       :: !(Strict Maybe Public.Type)
  , idDefinedIn  :: {-# UNPACK #-} !ModuleId
  , idDefSpan    :: !EitherSpan
  , idHomeModule :: !(Strict Maybe ModuleId)
  }
  deriving (Show, Generic)

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
  deriving (Show, Generic)

data SourceSpan = SourceSpan
  { spanFilePath   :: {-# UNPACK #-} !FilePathPtr
  , spanFromLine   :: {-# UNPACK #-} !Int
  , spanFromColumn :: {-# UNPACK #-} !Int
  , spanToLine     :: {-# UNPACK #-} !Int
  , spanToColumn   :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Ord, Show, Generic)

data EitherSpan =
    ProperSpan {-# UNPACK #-} !SourceSpan
  | TextSpan !Text
  deriving (Show, Generic)

data SourceError = SourceError
  { errorKind :: !Public.SourceErrorKind
  , errorSpan :: !EitherSpan
  , errorMsg  :: !Text
  }
  deriving (Show, Generic)

data ModuleId = ModuleId
  { moduleName    :: !Public.ModuleName
  , modulePackage :: {-# UNPACK #-} !PackageId
  }
  deriving (Show, Eq, Generic)

data PackageId = PackageId
  { packageName    :: !Text
  , packageVersion :: !(Strict Maybe Text)
  , packageKey     :: !Text
  }
  deriving (Show, Eq, Ord, Generic)

-- | Used before we convert it to an IdMap
type IdList = [(SourceSpan, SpanInfo)]

data SpanInfo =
   SpanId IdInfo
 | SpanQQ IdInfo
   -- We use 'SpanInSplice' for prioritization only (see 'internalGetSpanInfo').
   -- It gets translated to 'Public.SpanId'
 | SpanInSplice IdInfo
 deriving (Show, Generic)

newtype IdMap = IdMap { idMapToMap :: StrictIntervalMap (FilePathPtr, Int, Int) SpanInfo }
  deriving (Show, Generic)

newtype ExpMap = ExpMap { expMapToMap :: StrictIntervalMap (FilePathPtr, Int, Int) Text }
  deriving (Show, Generic)

type UseSites = Strict (Map IdPropPtr) [SourceSpan]

data ImportEntities =
    ImportOnly   !(Strict [] Text)
  | ImportHiding !(Strict [] Text)
  | ImportAll
  deriving (Show, Eq, Generic)

data Import = Import {
    importModule    :: !ModuleId
  -- | Used only for ghc's PackageImports extension
  , importPackage   :: !(Strict Maybe Text)
  , importQualified :: !Bool
  , importImplicit  :: !Bool
  , importAs        :: !(Strict Maybe Public.ModuleName)
  , importEntities  :: !ImportEntities
  }
  deriving (Show, Eq, Generic)

-- | The outcome of running code
data RunResult =
    -- | The code terminated okay
    RunOk
    -- | The code threw an exception
  | RunProgException String
    -- | GHC itself threw an exception when we tried to run the code
  | RunGhcException String
    -- | Execution was paused because of a breakpoint
  | RunBreak BreakInfo
  deriving (Typeable, Show, Generic)

-- | Information about a triggered breakpoint
data BreakInfo = BreakInfo {
    breakInfoModule      :: Public.ModuleName
  , breakInfoSpan        :: SourceSpan
  , breakInfoResultType  :: Public.Type
  , breakInfoVariableEnv :: Public.VariableEnv
  }
  deriving (Typeable, Show, Generic)

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
  deriving (Show, Generic)

unionCache :: ExplicitSharingCache -> ExplicitSharingCache -> ExplicitSharingCache
unionCache a b = ExplicitSharingCache {
    filePathCache = IntMap.union (filePathCache a) (filePathCache b)
  , idPropCache   = IntMap.union (idPropCache   a) (idPropCache   b)
  }

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
  put PackageId{..} = do
    put packageName
    put packageVersion
    put packageKey
  get = PackageId <$> get <*> get <*> get

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
  put (SpanId idInfo)       = putWord8 0 >> put idInfo
  put (SpanQQ idInfo)       = putWord8 1 >> put idInfo
  put (SpanInSplice idInfo) = putWord8 2 >> put idInfo

  get = do
    header <- getWord8
    case header of
      0 -> SpanId       <$> get
      1 -> SpanQQ       <$> get
      2 -> SpanInSplice <$> get
      _ -> fail "SpanInfo.get: invalid header"

instance Binary RunResult where
  put RunOk                  = putWord8 0
  put (RunProgException str) = putWord8 1 >> put str
  put (RunGhcException str)  = putWord8 2 >> put str
  put (RunBreak info)        = putWord8 3 >> put info

  get = do
    header <- getWord8
    case header of
      0 -> return RunOk
      1 -> RunProgException <$> get
      2 -> RunGhcException <$> get
      3 -> RunBreak <$> get
      _ -> fail "RunResult.get: invalid header"

instance Binary BreakInfo where
  put (BreakInfo{..}) = do
    put breakInfoModule
    put breakInfoSpan
    put breakInfoResultType
    put breakInfoVariableEnv

  get = BreakInfo <$> get <*> get <*> get <*> get

{------------------------------------------------------------------------------
  PrettyVal instances (these rely on Generics)
------------------------------------------------------------------------------}

instance PrettyVal FilePathPtr
instance PrettyVal IdPropPtr
instance PrettyVal IdInfo
instance PrettyVal IdProp
instance PrettyVal IdScope
instance PrettyVal SourceSpan
instance PrettyVal EitherSpan
instance PrettyVal SourceError
instance PrettyVal ModuleId
instance PrettyVal PackageId
instance PrettyVal SpanInfo
instance PrettyVal IdMap
instance PrettyVal ExpMap
instance PrettyVal ImportEntities
instance PrettyVal Import
instance PrettyVal RunResult
instance PrettyVal BreakInfo
instance PrettyVal ExplicitSharingCache

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
