{-# LANGUAGE TemplateHaskell, DeriveDataTypeable, DeriveGeneric #-}
-- | The public types
module IdeSession.Types.Public (
    -- * Types
    IdNameSpace(..)
  , Type
  , Name
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
--  , IdMap(..)
--  , LoadedModules
  , ImportEntities(..)
  , Import(..)
  , SpanInfo(..)
  , RunBufferMode(..)
  , RunResult(..)
  , BreakInfo(..)
  , Value
  , VariableEnv
  , Targets(..)
    -- * Util
  , idInfoQN
--, idInfoAtLocation
  , haddockLink
  ) where

import Prelude hiding (span)
import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import IdeSession.Util () -- Binary instance for Text
import IdeSession.Util.PrettyVal

{------------------------------------------------------------------------------
  Types
------------------------------------------------------------------------------}

-- | Identifiers in Haskell are drawn from a number of different name spaces
data IdNameSpace =
    VarName    -- ^ Variables, including real data constructors
  | DataName   -- ^ Source data constructors
  | TvName     -- ^ Type variables
  | TcClsName  -- ^ Type constructors and classes
  deriving (Show, Eq, Generic)

-- | Information about identifiers
data IdInfo = IdInfo {
    idProp  :: {-# UNPACK #-} !IdProp
  , idScope :: !IdScope
  }
  deriving (Eq, Generic)

-- | Variable name
type Name = Text

-- | For now we represent types in pretty-printed form
type Type = Text

-- | Identifier info that is independent of the usage site
data IdProp = IdProp {
    -- | The base name of the identifer at this location. Module prefix
    -- is not included.
    idName  :: !Name
    -- | Namespace this identifier is drawn from
  , idSpace :: !IdNameSpace
    -- | The type
    -- We don't always know this; in particular, we don't know kinds because
    -- the type checker does not give us LSigs for top-level annotations)
  , idType  :: !(Maybe Type)
    -- | Module the identifier was defined in
  , idDefinedIn :: {-# UNPACK #-} !ModuleId
    -- | Where in the module was it defined (not always known)
  , idDefSpan :: !EitherSpan
    -- | Haddock home module
  , idHomeModule :: !(Maybe ModuleId)
  }
  deriving (Eq, Generic)

-- TODO: Ideally, we would have
-- 1. SourceSpan for Local rather than EitherSpan
-- 2. SourceSpan for idImportSpan
-- 3. Have a idImportedFromPackage, but unfortunately ghc doesn't give us
--    this information (it's marked as a TODO in RdrName.lhs)
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
  deriving (Eq, Generic)

data SourceSpan = SourceSpan
  { spanFilePath   :: !FilePath
  , spanFromLine   :: {-# UNPACK #-} !Int
  , spanFromColumn :: {-# UNPACK #-} !Int
  , spanToLine     :: {-# UNPACK #-} !Int
  , spanToColumn   :: {-# UNPACK #-} !Int
  }
  deriving (Eq, Ord, Generic)

data EitherSpan =
    ProperSpan {-# UNPACK #-} !SourceSpan
  | TextSpan !Text
  deriving (Eq, Generic)

-- | An error or warning in a source module.
--
-- Most errors are associated with a span of text, but some have only a
-- location point.
data SourceError = SourceError
  { errorKind :: !SourceErrorKind
  , errorSpan :: !EitherSpan
  , errorMsg  :: !Text
  }
  deriving (Show, Eq, Generic)

-- | Severity of an error.
data SourceErrorKind = KindError | KindWarning | KindServerDied
  deriving (Show, Eq, Generic)

type ModuleName = Text

data ModuleId = ModuleId
  { moduleName    :: !ModuleName
  , modulePackage :: {-# UNPACK #-} !PackageId
  }
  deriving (Eq, Ord, Generic)

data PackageId = PackageId
  { packageName    :: !Text
  , packageVersion :: !(Maybe Text)
  }
  deriving (Eq, Ord, Generic)

{-
newtype IdMap = IdMap { idMapToMap :: Map SourceSpan IdInfo }

type LoadedModules = Map ModuleName IdMap
-}

data ImportEntities =
    ImportOnly   ![Text]
  | ImportHiding ![Text]
  | ImportAll
  deriving (Show, Eq, Ord, Generic)

data Import = Import {
    importModule    :: !ModuleId
  -- | Used only for ghc's PackageImports extension
  , importPackage   :: !(Maybe Text)
  , importQualified :: !Bool
  , importImplicit  :: !Bool
  , importAs        :: !(Maybe ModuleName)
  , importEntities  :: !ImportEntities
  }
  deriving (Show, Eq, Ord, Generic)

-- | Returned then the IDE asks "what's at this particular location?"
data SpanInfo =
    -- | Identifier
    SpanId IdInfo
    -- | Quasi-quote. The 'IdInfo' field gives the quasi-quoter
  | SpanQQ IdInfo
  deriving (Generic)

-- | Buffer modes for running code
--
-- Note that 'NoBuffering' means that something like 'putStrLn' will do a
-- syscall per character, and each of these characters will be read and sent
-- back to the client. This results in a large overhead.
--
-- When using 'LineBuffering' or 'BlockBuffering', 'runWait' will not report
-- any output from the snippet until it outputs a linebreak/fills the buffer,
-- respectively (or does an explicit flush). However, you can specify a timeout
-- in addition to the buffering mode; if you set this to @Just n@, the buffer
-- will be flushed every @n@ microseconds.
--
-- NOTE: This is duplicated in the IdeBackendRTS (defined in IdeSession)
data RunBufferMode =
    RunNoBuffering
  | RunLineBuffering  { runBufferTimeout   :: Maybe Int }
  | RunBlockBuffering { runBufferBlockSize :: Maybe Int
                      , runBufferTimeout   :: Maybe Int
                      }
  deriving (Typeable, Show, Generic)

-- | The outcome of running code
data RunResult =
    -- | The code terminated okay
    RunOk
    -- | The code threw an exception
  | RunProgException String
    -- | GHC itself threw an exception when we tried to run the code
  | RunGhcException String
    -- | The session was restarted
  | RunForceCancelled
    -- | Execution was paused because of a breakpoint
  | RunBreak
  deriving (Typeable, Show, Eq, Generic)

-- | Information about a triggered breakpoint
data BreakInfo = BreakInfo {
    -- | Module containing the breakpoint
    breakInfoModule :: ModuleName
    -- | Location of the breakpoint
  , breakInfoSpan :: SourceSpan
    -- | Type of the result
  , breakInfoResultType :: Type
    -- | Local variables and their values
  , breakInfoVariableEnv :: VariableEnv
  }
  deriving (Typeable, Show, Eq, Generic)

-- | We present values only in pretty-printed form
type Value = Text

-- | Variables during execution (in debugging mode)
type VariableEnv = [(Name, Type, Value)]

data Targets = TargetsInclude [FilePath] | TargetsExclude [FilePath]
  deriving (Typeable, Generic)

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
       Text.unpack idName ++ " "
    ++ "(" ++ show idSpace ++ ")"
    ++ (case idType of Just typ -> " :: " ++ Text.unpack typ; Nothing -> [])
    ++ " defined in "
    ++ show idDefinedIn
    ++ " at " ++ show idDefSpan
    ++ (case idHomeModule of Just home -> " (home " ++ show home ++ ")"
                             Nothing   -> "")

instance Show IdScope where
  show Binder          = "binding occurrence"
  show Local           = "defined locally"
  show WiredIn         = "wired in to the compiler"
  show (Imported {..}) =
           "imported from " ++ show idImportedFrom
        ++ (if Text.null idImportQual
              then []
              else " as '" ++ Text.unpack idImportQual ++ "'")
        ++ " at "++ show idImportSpan

instance Show EitherSpan where
  show (ProperSpan srcSpan) = show srcSpan
  show (TextSpan str)       = Text.unpack str

instance Show ModuleId where
  show (ModuleId mo pkg) = show pkg ++ ":" ++ Text.unpack mo

instance Show PackageId where
  show (PackageId name (Just version)) =
    Text.unpack name ++ "-" ++ Text.unpack version
  show (PackageId name Nothing) =
    Text.unpack name

instance Show IdInfo where
  show IdInfo{..} = show idProp ++ " (" ++ show idScope ++ ")"

instance Show SpanInfo where
  show (SpanId idInfo) = show idInfo
  show (SpanQQ idInfo) = "quasi-quote with quoter " ++ show idInfo

{-
instance Show IdMap where
  show =
    let showIdInfo(span, idInfo) = "(" ++ show span ++ "," ++ show idInfo ++ ")"
    in unlines . map showIdInfo . Map.toList . idMapToMap
-}

{------------------------------------------------------------------------------
  Binary instances

  We only have Binary instances for those types that are shared between
  the public and private types, and for the "small" types that are the result of
  IDE session queries. We don't want Binary instances for entire LoadedModules
  maps and other "large" types.
------------------------------------------------------------------------------}

instance Binary IdNameSpace where
  put VarName   = putWord8 0
  put DataName  = putWord8 1
  put TvName    = putWord8 2
  put TcClsName = putWord8 3

  get = do
    header <- getWord8
    case header of
      0 -> return VarName
      1 -> return DataName
      2 -> return TvName
      3 -> return TcClsName
      _ -> fail "IdNameSpace.get: invalid header"

instance Binary SourceErrorKind where
  put KindError      = putWord8 0
  put KindWarning    = putWord8 1
  put KindServerDied = putWord8 2

  get = do
    header <- getWord8
    case header of
      0 -> return KindError
      1 -> return KindWarning
      2 -> return KindServerDied
      _ -> fail "SourceErrorKind.get: invalid header"

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

instance Binary SourceError where
  put SourceError{..} = do
    put errorKind
    put errorSpan
    put errorMsg

  get = SourceError <$> get <*> get <*> get

instance Binary IdProp where
  put IdProp{..} = do
    put idName
    put idSpace
    put idType
    put idDefinedIn
    put idDefSpan
    put idHomeModule

  get = IdProp <$> get <*> get <*> get <*> get <*> get <*> get

instance Binary IdScope where
  put Binder       = putWord8 0
  put Local        = putWord8 1
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

instance Binary SourceSpan where
  put (SourceSpan{..}) = do
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

instance Binary ModuleId where
  put ModuleId{..} = put moduleName >> put modulePackage
  get = ModuleId <$> get <*> get

instance Binary PackageId where
  put PackageId{..} = put packageName >> put packageVersion
  get = PackageId <$> get <*> get

instance Binary IdInfo where
  put IdInfo{..} = put idProp >> put idScope
  get = IdInfo <$> get <*> get

instance Binary RunBufferMode where
  put RunNoBuffering        = putWord8 0
  put RunLineBuffering{..}  = do putWord8 1
                                 put runBufferTimeout
  put RunBlockBuffering{..} = do putWord8 2
                                 put runBufferBlockSize
                                 put runBufferTimeout

  get = do
    header <- getWord8
    case header of
      0 -> return RunNoBuffering
      1 -> RunLineBuffering <$> get
      2 -> RunBlockBuffering <$> get <*> get
      _ -> fail "RunBufferMode.get: invalid header"

instance Binary Targets where
  put (TargetsInclude l) = do
    putWord8 0
    put l
  put (TargetsExclude l) = do
    putWord8 1
    put l

  get = do
    header <- getWord8
    case header of
      0 -> TargetsInclude <$> get
      1 -> TargetsExclude <$> get
      _ -> fail "Targets.get: invalid header"

{------------------------------------------------------------------------------
  JSON instances

  We provide these for the convenience of client code only; we don't use them
  internally.
------------------------------------------------------------------------------}

$(deriveJSON defaultOptions ''IdNameSpace)
$(deriveJSON defaultOptions ''SourceErrorKind)
$(deriveJSON defaultOptions ''ImportEntities)
$(deriveJSON defaultOptions ''Import)
$(deriveJSON defaultOptions ''SourceError)
$(deriveJSON defaultOptions ''IdProp)
$(deriveJSON defaultOptions ''IdScope)
$(deriveJSON defaultOptions ''SourceSpan)
$(deriveJSON defaultOptions ''EitherSpan)
$(deriveJSON defaultOptions ''ModuleId)
$(deriveJSON defaultOptions ''PackageId)
$(deriveJSON defaultOptions ''IdInfo)
$(deriveJSON defaultOptions ''SpanInfo)
$(deriveJSON defaultOptions ''BreakInfo)
$(deriveJSON defaultOptions ''RunResult)
$(deriveJSON defaultOptions ''RunBufferMode)

{------------------------------------------------------------------------------
  PrettyVal instances (these rely on Generics)
------------------------------------------------------------------------------}

instance PrettyVal IdNameSpace
instance PrettyVal IdInfo
instance PrettyVal IdProp
instance PrettyVal IdScope
instance PrettyVal SourceSpan
instance PrettyVal EitherSpan
instance PrettyVal SourceError
instance PrettyVal SourceErrorKind
instance PrettyVal ModuleId
instance PrettyVal PackageId
instance PrettyVal ImportEntities
instance PrettyVal Import
instance PrettyVal SpanInfo
instance PrettyVal RunBufferMode
instance PrettyVal RunResult
instance PrettyVal BreakInfo
instance PrettyVal Targets

{------------------------------------------------------------------------------
  Util
------------------------------------------------------------------------------}

-- | Construct qualified name following Haskell's scoping rules
idInfoQN :: IdInfo -> String
idInfoQN IdInfo{idProp = IdProp{idName}, idScope} =
  case idScope of
    Binder                 -> Text.unpack idName
    Local{}                -> Text.unpack idName
    Imported{idImportQual} -> Text.unpack idImportQual ++ Text.unpack idName
    WiredIn                -> Text.unpack idName

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
      ++ dotToDash (Text.unpack $ moduleName idImportedFrom) ++ ".html#"
      ++ haddockSpaceMarks idSpace ++ ":"
      ++ Text.unpack idName
    _ -> "<local identifier>"
 where
   dotToDash = map (\c -> if c == '.' then '-' else c)
   dashToSlash p = case packageVersion p of
     Nothing      -> Text.unpack (packageName p) ++ "/latest"
     Just version -> Text.unpack (packageName p) ++ "/" ++ Text.unpack version

{-
idInfoAtLocation :: Int -> Int -> IdMap -> [(SourceSpan, IdInfo)]
idInfoAtLocation line col = filter inRange . idMapToList
  where
    inRange :: (SourceSpan, a) -> Bool
    inRange (SourceSpan{..}, _) =
      (line   > spanFromLine || (line == spanFromLine && col >= spanFromColumn)) &&
      (line   < spanToLine   || (line == spanToLine   && col <= spanToColumn))
-}
