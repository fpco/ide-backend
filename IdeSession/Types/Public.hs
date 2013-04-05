{-# LANGUAGE TemplateHaskell #-}
-- | The public types
module IdeSession.Types.Public (
    -- * Types
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
  , Import(..)
    -- * Util
  , idInfoQN
--, idInfoAtLocation
  , haddockLink
  ) where

import Prelude hiding (span)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Aeson.TH (deriveJSON)

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

-- | Identifiers in Haskell are drawn from a number of different name spaces
data IdNameSpace =
    VarName    -- ^ Variables, including real data constructors
  | DataName   -- ^ Source data constructors
  | TvName     -- ^ Type variables
  | TcClsName  -- ^ Type constructors and classes
  deriving (Show, Eq)

-- | Information about identifiers
data IdInfo = IdInfo {
    idProp  :: {-# UNPACK #-} !IdProp
  , idScope :: !IdScope
  }

-- | Identifier info that is independent of the usage site
data IdProp = IdProp {
    -- | The base name of the identifer at this location. Module prefix
    -- is not included.
    idName  :: !String
    -- | Namespace this identifier is drawn from
  , idSpace :: !IdNameSpace
    -- | The type
    -- We don't always know this; in particular, we don't know kinds because
    -- the type checker does not give us LSigs for top-level annotations)
  , idType  :: !(Maybe String)
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
  { spanFilePath   :: !FilePath
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

-- | An error or warning in a source module.
--
-- Most errors are associated with a span of text, but some have only a
-- location point.
data SourceError = SourceError
  { errorKind :: !SourceErrorKind
  , errorSpan :: !EitherSpan
  , errorMsg  :: !String
  }
  deriving (Show, Eq)

-- | Severity of an error.
data SourceErrorKind = KindError | KindWarning
  deriving (Show, Eq)

type ModuleName = String

data ModuleId = ModuleId
  { moduleName    :: !ModuleName
  , modulePackage :: {-# UNPACK #-} !PackageId
  }
  deriving (Eq)

data PackageId = PackageId
  { packageName    :: !String
  , packageVersion :: !(Maybe String)
  }
  deriving (Eq)

newtype IdMap = IdMap { idMapToMap :: Map SourceSpan IdInfo }

type LoadedModules = Map ModuleName IdMap

data Import = Import {
    importModule    :: ModuleName
  -- | Used only for ghc's PackageImports extension
  , importPackage   :: Maybe String
  , importQualified :: Bool
  , importImplicit  :: Bool
  , importAs        :: Maybe ModuleName
  -- | @Just (True, ..)@ for @import M hiding (..)@,
  -- @Just (False, ..)@ for @import M (..)@, or
  -- @Nothing@ otherwise.
  , importHiding    :: Maybe (Bool, [String])
  }
  deriving (Show, Eq, Ord)


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
  JSON instances

  We only have JSON instances for those types that are shared between
  the public and private types. Generally speaking, the public types are
  not meant to be serialized.
------------------------------------------------------------------------------}

$(deriveJSON id ''IdNameSpace)
$(deriveJSON id ''SourceErrorKind)
$(deriveJSON id ''Import)

{------------------------------------------------------------------------------
  Util
------------------------------------------------------------------------------}

-- | Construct qualified name following Haskell's scoping rules
idInfoQN :: IdInfo -> String
idInfoQN IdInfo{idProp = IdProp{idName}, idScope} =
  case idScope of
    Binder                 -> idName
    Local{}                -> idName
    Imported{idImportQual} -> idImportQual ++ idName
    WiredIn                -> idName

-- | Show approximately what Haddock adds to documentation URLs.
haddockSpaceMarks :: IdNameSpace -> String
haddockSpaceMarks VarName   = "v"
haddockSpaceMarks DataName  = "v"
haddockSpaceMarks TvName    = "t"
haddockSpaceMarks TcClsName = "t"

-- | Show approximately a haddock link (without haddock root) for an id.
-- This is an illustration and a test of the id info, but under ideal
-- conditions could perhaps serve to link to documentation without
-- going via Hoogle.
haddockLink :: IdProp -> IdScope -> String
haddockLink IdProp{..} idScope =
  case idScope of
    Imported{idImportedFrom} ->
         dashToSlash (modulePackage idImportedFrom)
      ++ "/doc/html/"
      ++ dotToDash (moduleName idImportedFrom) ++ ".html#"
      ++ haddockSpaceMarks idSpace ++ ":"
      ++ idName
    _ -> "<local identifier>"
 where
   dotToDash = map (\c -> if c == '.' then '-' else c)
   dashToSlash p = case packageVersion p of
     Nothing -> packageName p ++ "/latest"
     Just version -> packageName p ++ "/" ++ version

{-
idInfoAtLocation :: Int -> Int -> IdMap -> [(SourceSpan, IdInfo)]
idInfoAtLocation line col = filter inRange . idMapToList
  where
    inRange :: (SourceSpan, a) -> Bool
    inRange (SourceSpan{..}, _) =
      (line   > spanFromLine || (line == spanFromLine && col >= spanFromColumn)) &&
      (line   < spanToLine   || (line == spanToLine   && col <= spanToColumn))
-}

