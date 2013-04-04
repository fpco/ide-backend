{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies, TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}
-- | Common types and utilities
module Common
  ( -- * Common data types
    IdNameSpace(..)
  , IdInfo(..)
  , IdProp(..)
  , IdScope(..)
  , SourceSpan(..)
  , EitherSpan(..)
  , SourceError(..)
  , SourceErrorKind(..)
  , ModuleName
  , ModuleId(..)
  , PackageId(..)
  , IdMap(..)
  , LoadedModules
    -- * Explicit sharing
  , FilePathPtr(..)
  , IdPropPtr(..)
  , XIdInfo(..)
  , XIdScope(..)
  , XSourceSpan(..)
  , XEitherSpan(..)
  , XSourceError(..)
  , XIdMap(..)
  , XLoadedModules
  , XShared
    -- * Normalization
  , ExplicitSharingCache(..)
  , ExplicitSharing(..)
  , showNormalized
    -- * Progress
  , Progress(..)
  , initialProgress
  , updateProgress
  , progressStep
    -- * Configuration
  , hsExtensions
    -- * Debugging
  , dVerbosity
  , debugFile
  , debug
    -- * Util
  , applyMapDiff
  , showExWithClass
  , accessorName
  , lookup'
  ) where

import Prelude hiding (span)
import qualified Control.Exception as Ex
import Control.Monad (when)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson.TH (deriveJSON)
import Data.Typeable (typeOf)
import qualified Data.ByteString.Char8 as BS
import System.IO (hFlush, stderr)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Accessor (Accessor, accessor)

{------------------------------------------------------------------------------
  Common data types

  These are the normalized data types (without explicit sharing).
------------------------------------------------------------------------------}

-- This type is abstract in GHC. One more reason to define our own.
data IdNameSpace =
    VarName    -- ^ Variables, including real data constructors
  | DataName   -- ^ Source data constructors
  | TvName     -- ^ Type variables
  | TcClsName  -- ^ Type constructors and classes
  deriving (Show, Eq)

-- | Information about identifiers
data IdInfo = IdInfo { idProp  :: IdProp
                     , idScope :: IdScope
                     }

-- | Identifier info that is independent of the usage site
data IdProp = IdProp
  { -- | The base name of the identifer at this location. Module prefix
    -- is not included.
    idName  :: String
    -- | Namespace this identifier is drawn from
  , idSpace :: IdNameSpace
    -- | The type
    -- We don't always know this; in particular, we don't know kinds because
    -- the type checker does not give us LSigs for top-level annotations)
  , idType  :: Maybe String
  }
  deriving (Eq)

-- TODO: Ideally, we would have
-- 1. SourceSpan for Local rather than EitherSpan
-- 2. Don't have Maybe String or Maybe Package in the import case
--    (under which circumstances do we not get package information? -- the unit
--    tests give us examples)
-- 3. SourceSpan for idImportSpan
-- 4. Have a idImportedFromPackage, but unfortunately ghc doesn't give us
--    this information (it's marked as a TODO in RdrName.lhs)
data IdScope =
    -- | This is a binding occurrence (@f x = ..@, @\x -> ..@, etc.)
    Binder
    -- | Defined within this module
  | Local {
        idDefSpan :: EitherSpan
      }
    -- | Imported from a different module
  | Imported {
        idDefSpan      :: EitherSpan
      , idDefinedIn    :: ModuleId
      , idImportedFrom :: ModuleId
      , idImportSpan   :: EitherSpan
        -- | Qualifier used for the import
        --
        -- > IMPORTED AS                       idImportQual
        -- > import Data.List                  ""
        -- > import qualified Data.List        "Data.List."
        -- > import qualified Data.List as L   "L."
      , idImportQual   :: String
      }
    -- | Wired into the compiler (@()@, @True@, etc.)
  | WiredIn
  deriving (Eq)

data SourceSpan = SourceSpan
  { spanFilePath   :: FilePath
  , spanFromLine   :: Int
  , spanFromColumn :: Int
  , spanToLine     :: Int
  , spanToColumn   :: Int
  }
  deriving (Eq, Ord)

data EitherSpan =
    ProperSpan SourceSpan
  | TextSpan String
  deriving (Eq)

-- | An error or warning in a source module.
--
-- Most errors are associated with a span of text, but some have only a
-- location point.
--
data SourceError = SourceError
  { errorKind :: SourceErrorKind
  , errorSpan :: EitherSpan
  , errorMsg  :: String
  }
  deriving (Show, Eq)

-- | Severity of an error.
data SourceErrorKind = KindError | KindWarning
  deriving (Show, Eq)

type ModuleName = String

data ModuleId = ModuleId
  { moduleName    :: ModuleName
  , modulePackage :: PackageId
  }
  deriving (Eq)

data PackageId = PackageId
  { packageName    :: String
  , packageVersion :: Maybe String
  }
  deriving (Eq)

data IdMap = IdMap { idMapToMap :: Map SourceSpan IdInfo }

type LoadedModules = Map Common.ModuleName IdMap

{------------------------------------------------------------------------------
  Show instances
------------------------------------------------------------------------------}

instance Show SourceSpan where
  show (SourceSpan{..}) =
       spanFilePath ++ "@"
    ++ show spanFromLine ++ ":" ++ show spanFromColumn ++ "-"
    ++ show spanToLine   ++ ":" ++ show spanToColumn

instance Show IdProp where
  show (IdProp {..}) =
       idName ++ " "
    ++ "(" ++ show idSpace ++ ")"
    ++ (case idType of Just typ -> " :: " ++ typ; Nothing -> [])

-- TODO: If these Maybes stay, we should have a prettier Show instance
-- (but hopefully they will go)
instance Show IdScope where
  show Binder          = "binding occurrence"
  show (Local {..})    = "defined at " ++ show idDefSpan
  show WiredIn         = "wired in to the compiler"
  show (Imported {..}) =
          "defined in "
        ++ show idDefinedIn
        ++ " at " ++ show idDefSpan ++ ";"
        ++ " imported from " ++ show idImportedFrom
        ++ (if null idImportQual then [] else " as '" ++ idImportQual ++ "'")
        ++ " at "++ show idImportSpan

instance Show EitherSpan where
  show (ProperSpan srcSpan) = show srcSpan
  show (TextSpan str)       = str

instance Show ModuleId where
  show (ModuleId mo pkg) = show pkg ++ ":" ++ mo

instance Show PackageId where
  show (PackageId name (Just version)) = name ++ "-" ++ version
  show (PackageId name Nothing)        = name

instance Show IdInfo where
  show IdInfo{..} = show idProp ++ " (" ++ show idScope ++ ")"

instance Show IdMap where
  show =
    let showIdInfo(span, idInfo) = "(" ++ show span ++ "," ++ show idInfo ++ ")"
    in unlines . map showIdInfo . Map.toList . idMapToMap

{------------------------------------------------------------------------------
  Versions of the above with explicit sharing

  These data types don't have Show instances (we'd have to show them with
  the sharing pointers in them. See 'showNormalized' instead.
------------------------------------------------------------------------------}

newtype FilePathPtr = FilePathPtr { filePathPtr :: Int }
  deriving (Eq, Ord)

newtype IdPropPtr = IdPropPtr { idPropPtr :: Int }
  deriving (Eq, Ord)

-- | Information about identifiers
data XIdInfo = XIdInfo { xIdProp  :: IdPropPtr
                       , xIdScope :: XIdScope
                       }

data XIdScope =
    -- | This is a binding occurrence (@f x = ..@, @\x -> ..@, etc.)
    XBinder
    -- | Defined within this module
  | XLocal {
        xIdDefSpan :: XEitherSpan
      }
    -- | Imported from a different module
  | XImported {
        xIdDefSpan      :: XEitherSpan
      , xIdDefinedIn    :: ModuleId
      , xIdImportedFrom :: ModuleId
      , xIdImportSpan   :: XEitherSpan
        -- | Qualifier used for the import
        --
        -- > IMPORTED AS                       idImportQual
        -- > import Data.List                  ""
        -- > import qualified Data.List        "Data.List."
        -- > import qualified Data.List as L   "L."
      , xIdImportQual   :: String
      }
    -- | Wired into the compiler (@()@, @True@, etc.)
  | XWiredIn
  deriving (Eq)

data XSourceSpan = XSourceSpan
  { xSpanFilePath   :: FilePathPtr
  , xSpanFromLine   :: Int
  , xSpanFromColumn :: Int
  , xSpanToLine     :: Int
  , xSpanToColumn   :: Int
  }
  deriving (Eq, Ord)

data XEitherSpan =
    XProperSpan XSourceSpan
  | XTextSpan String
  deriving (Eq)

-- | An error or warning in a source module.
--
-- Most errors are associated with a span of text, but some have only a
-- location point.
--
data XSourceError = XSourceError
  { xErrorKind :: SourceErrorKind
  , xErrorSpan :: XEitherSpan
  , xErrorMsg  :: String
  }
  deriving (Eq)

data XIdMap = XIdMap { xIdMapToMap :: Map (XSourceSpan) (XIdInfo) }

type XLoadedModules = Map Common.ModuleName (XIdMap)

{------------------------------------------------------------------------------
  Normalization (turning explicit sharing into implicit sharing)
------------------------------------------------------------------------------}

data ExplicitSharingCache = ExplicitSharingCache {
    filePathCache :: IntMap FilePath
  , idPropCache   :: IntMap IdProp
  }

-- | The associated type with explicit sharing
type family XShared a

-- | The inverse of MShared, only for decidability of type checking
type family MShared a

type instance XShared FilePath      = FilePathPtr
type instance XShared IdProp        = IdPropPtr
type instance XShared IdInfo        = XIdInfo
type instance XShared IdScope       = XIdScope
type instance XShared SourceSpan    = XSourceSpan
type instance XShared EitherSpan    = XEitherSpan
type instance XShared SourceError   = XSourceError
type instance XShared IdMap         = XIdMap
type instance XShared LoadedModules = XLoadedModules

type instance MShared FilePathPtr    = FilePath
type instance MShared IdPropPtr      = IdProp
type instance MShared XIdInfo        = IdInfo
type instance MShared XIdScope       = IdScope
type instance MShared XSourceSpan    = SourceSpan
type instance MShared XEitherSpan    = EitherSpan
type instance MShared XSourceError   = SourceError
type instance MShared XIdMap         = IdMap
type instance MShared XLoadedModules = LoadedModules

class MShared (XShared a) ~ a => ExplicitSharing a where
  removeExplicitSharing :: ExplicitSharingCache -> XShared a -> a

showNormalized :: forall a. (Show a, ExplicitSharing a)
               => ExplicitSharingCache -> XShared a -> String
showNormalized cache x = show (removeExplicitSharing cache x :: a)

instance ExplicitSharing FilePath where
  removeExplicitSharing cache ptr = filePathCache cache IntMap.! filePathPtr ptr

instance ExplicitSharing IdProp where
  removeExplicitSharing cache ptr = idPropCache cache IntMap.! idPropPtr ptr

instance ExplicitSharing IdInfo where
  removeExplicitSharing cache XIdInfo{..} = IdInfo {
      idProp  = removeExplicitSharing cache xIdProp
    , idScope = removeExplicitSharing cache xIdScope
    }

instance ExplicitSharing IdScope where
  removeExplicitSharing cache xIdScope = case xIdScope of
    XBinder        -> Binder
    XLocal {..}    -> Local {
                          idDefSpan      = removeExplicitSharing cache xIdDefSpan
                        }
    XImported {..} -> Imported {
                          idDefSpan      = removeExplicitSharing cache xIdDefSpan
                        , idDefinedIn    = xIdDefinedIn
                        , idImportedFrom = xIdImportedFrom
                        , idImportSpan   = removeExplicitSharing cache xIdImportSpan
                        , idImportQual   = xIdImportQual
                        }
    XWiredIn       -> WiredIn

instance ExplicitSharing SourceSpan where
  removeExplicitSharing cache XSourceSpan{..} = SourceSpan {
      spanFilePath   = removeExplicitSharing cache xSpanFilePath
    , spanFromLine   = xSpanFromLine
    , spanFromColumn = xSpanFromColumn
    , spanToLine     = xSpanToLine
    , spanToColumn   = xSpanToColumn
    }

instance ExplicitSharing EitherSpan where
  removeExplicitSharing cache xEitherSpan = case xEitherSpan of
    XProperSpan xSourceSpan -> ProperSpan (removeExplicitSharing cache xSourceSpan)
    XTextSpan str           -> TextSpan str

instance ExplicitSharing SourceError where
  removeExplicitSharing cache XSourceError{..} = SourceError {
      errorKind = xErrorKind
    , errorSpan = removeExplicitSharing cache xErrorSpan
    , errorMsg  = xErrorMsg
    }

instance ExplicitSharing IdMap where
  removeExplicitSharing cache = IdMap
                  . Map.map (removeExplicitSharing cache)
                  . Map.mapKeys (removeExplicitSharing cache)
                  . xIdMapToMap

instance ExplicitSharing LoadedModules where
  removeExplicitSharing = Map.map . removeExplicitSharing

{------------------------------------------------------------------------------
  JSON instances

  Note that most of the data types without explicit sharing do NOT have JSON
  instances. The whole point is that we maintain sharing when through
  serialization.

  We could give generic JSON instances for Map and IntMap, but these would be
  orphan instances (grr).
------------------------------------------------------------------------------}

-- Types without an explicit sharing version
$(deriveJSON id ''SourceErrorKind)
$(deriveJSON id ''ModuleId)
$(deriveJSON id ''PackageId)

-- Types with explicit sharing
$(deriveJSON id ''XIdScope)
$(deriveJSON id ''XSourceSpan)
$(deriveJSON id ''XEitherSpan)
$(deriveJSON id ''XSourceError)
$(deriveJSON id ''XIdInfo)

instance FromJSON XIdMap where
  parseJSON = fmap aux . parseJSON
    where
      aux :: [(XSourceSpan, XIdInfo)] -> XIdMap
      aux = XIdMap . Map.fromList

instance ToJSON XIdMap where
  toJSON = toJSON . aux
    where
      aux :: XIdMap -> [(XSourceSpan, XIdInfo)]
      aux = Map.toList . xIdMapToMap

-- Types that we ship as part of the explicit sharing cache
$(deriveJSON id ''IdNameSpace)
$(deriveJSON id ''IdProp)
$(deriveJSON id ''IdPropPtr)
$(deriveJSON id ''FilePathPtr)

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

{------------------------------------------------------------------------------
  Progress
------------------------------------------------------------------------------}

-- | This type represents intermediate progress information during compilation.
newtype Progress = Progress Int
  deriving (Show, Eq, Ord)

$(deriveJSON id ''Progress)

initialProgress :: Progress
initialProgress = Progress 1  -- the progress indicates a start of a step

updateProgress :: String -> Progress -> Progress
updateProgress _msg (Progress n) = Progress (n + 1)

-- | The step number of the progress. Usually corresponds to the number
-- of files already processed.
progressStep :: Progress -> Int
progressStep (Progress n) = n

{------------------------------------------------------------------------------
  Configuration
------------------------------------------------------------------------------}

-- | These source files are type-checked.
hsExtensions :: [FilePath]
hsExtensions = [".hs", ".lhs"]
-- Boot files are not so simple. They should probably be copied to the src dir,
-- but not made proper targets. This is probably similar to .h files.
-- hsExtentions = [".hs", ".lhs", ".hs-boot", ".lhs-boot", ".hi-boot"]

{------------------------------------------------------------------------------
  Debugging
------------------------------------------------------------------------------}

dVerbosity :: Int
dVerbosity = 3

debugFile :: Maybe FilePath
debugFile = Nothing -- Just "debug.log"

debug :: MonadIO m => Int -> String -> m ()
debug verbosity msg =
  when (verbosity >= 3) $ do
    case debugFile of
      Nothing -> return ()
      Just logName ->
        liftIO $ appendFile logName $ msg ++ "\n"
    when (verbosity >= 4) $ liftIO $ do
      BS.hPutStrLn stderr $ BS.pack $ "debug: " ++ msg
      hFlush stderr

{------------------------------------------------------------------------------
  Util
------------------------------------------------------------------------------}

applyMapDiff :: Ord k => Map k (Maybe v) -> Map k v -> Map k v
applyMapDiff diff m =
  let f m2 (k, v) = Map.alter (const v) k m2
  in List.foldl' f m $ Map.toList diff

-- | Show an exception together with its most precise type tag.
showExWithClass :: Ex.SomeException -> String
showExWithClass (Ex.SomeException ex) = show (typeOf ex) ++ ": " ++ show ex

-- | Translate record field '_name' to the accessor 'name'
accessorName :: String -> Maybe String
accessorName ('_' : str) = Just str
accessorName _           = Nothing

-- | Prelude.lookup as an accessor
lookup' :: Eq a => a -> Accessor [(a, b)] (Maybe b)
lookup' key =
    accessor (lookup key) $ \mVal list ->
      case mVal of
        Nothing  -> delete key list
        Just val -> override key val list
  where
    override :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
    override a b [] = [(a, b)]
    override a b ((a', b') : xs)
      | a == a'   = (a, b) : xs
      | otherwise = (a', b') : override a b xs

    delete :: Eq a => a -> [(a, b)] -> [(a, b)]
    delete _ [] = []
    delete a ((a', b') : xs)
      | a == a'   = xs
      | otherwise = (a', b') : delete a xs
