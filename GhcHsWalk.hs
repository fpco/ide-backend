{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module GhcHsWalk
  ( IdMap(..)
  , IdInfo(..)
  , IdNameSpace(..)
  , IsBinder(..)
  , extractIdsPlugin
  ) where

import Prelude hiding (span, id)
import Control.Monad.Writer (MonadWriter, WriterT, execWriterT, tell)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.IORef
import System.FilePath (takeFileName)
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.TH (deriveJSON)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid

import Common
import GhcRun (extractSourceSpan)

import GHC hiding (idType)
import TcRnTypes
import Outputable
import HscPlugin
import DynFlags
import Var hiding (idInfo)
import qualified Name as Name
import qualified Module as Module
import MonadUtils (MonadIO(..))
import Bag

{------------------------------------------------------------------------------
  Environment mapping source locations to info
------------------------------------------------------------------------------}

-- This type is abstract in GHC. One more reason to define our own.
data IdNameSpace =
    VarName    -- ^ Variables, including real data constructors
  | DataName   -- ^ Source data constructors
  | TvName     -- ^ Type variables
  | TcClsName  -- ^ Type constructors and classes
  deriving (Show, Eq)

data IsBinder = Binding | NonBinding
  deriving (Show, Eq)

-- | Information about identifiers
data IdInfo = IdInfo
  { -- | The base name of the identifer at this location. Module prefix
    -- is not included.
    idName :: String
    -- | The module prefix of the identifier. Empty, if a local variable.
  , idModule :: Maybe String
    -- | Package the identifier comes from. Empty, if a local variable.
  , idPackage :: Maybe String
    -- | Namespace of the identifier.
  , idSpace :: IdNameSpace
    -- | The type
    -- We don't always know this; in particular, we don't know kinds because
    -- the type checker does not give us LSigs for top-level annotations)
  , idType :: Maybe String
    -- | Where was this identifier defined?
  , idDefSpan :: Either SourceSpan String
    -- | Is this a binding occurrence?
  , idIsBinder :: IsBinder
  }
  deriving (Show, Eq)

data IdMap = IdMap { idMapToMap :: Map SourceSpan IdInfo }

$(deriveJSON (\x -> x) ''IdNameSpace)
$(deriveJSON (\x -> x) ''IsBinder)
$(deriveJSON (\x -> x) ''IdInfo)

instance FromJSON IdMap where
  parseJSON = fmap (IdMap . Map.fromList) . parseJSON

instance ToJSON IdMap where
  toJSON = toJSON . Map.toList . idMapToMap

idMapToList :: IdMap -> [(SourceSpan, IdInfo)]
idMapToList = Map.toList . idMapToMap

instance Show IdMap where
  show = unlines . map pp . idMapToList
    where
      ppDash n m | m - n <= 1 = show n
                 | otherwise = show n ++ "-" ++ show (m - 1)
      ppSpan (Left (fn, (stL, stC), (endL, endC))) =
        fn ++ ":" ++ ppDash stL endL ++ ":" ++ ppDash stC endC
      ppSpan (Right s) = s

      pp (sp, IdInfo{..}) =
        takeFileName (ppSpan $ Left sp)
        ++ " (" ++ show idSpace
        ++ (case idIsBinder of Binding -> ", binder): " ; _ -> "): ")
        ++ maybe "" (++ "/") idPackage
        ++ maybe "" (++ ".") idModule
        ++ idName ++ " :: "
        ++ (case idType of Nothing -> " (type unknown)" ; Just tp -> tp)
        ++ " (" ++ takeFileName (ppSpan idDefSpan) ++ ")"

instance Monoid IdMap where
  mempty = IdMap Map.empty
  (IdMap a) `mappend` (IdMap b) = IdMap (Map.unionWith combineIdInfo a b)

combineIdInfo :: IdInfo -> IdInfo -> IdInfo
combineIdInfo _ _ = error "This should not (yet) happen"

fromGhcNameSpace :: Name.NameSpace -> IdNameSpace
fromGhcNameSpace ns =
  if ns == Name.varName then VarName
  else if ns == Name.dataName then DataName
  else if ns == Name.tvName then TvName
  else if ns == Name.tcName then TcClsName
  else error "fromGhcNameSpace"

{------------------------------------------------------------------------------
  Extract an IdMap from information returned by the ghc type checker
------------------------------------------------------------------------------}

-- Define type synonym to avoid orphan instances
newtype ExtractIdsT m a = ExtractIdsT { runExtractIdsT :: WriterT IdMap m a }
  deriving (Functor, Monad, MonadWriter IdMap, MonadTrans)

execExtractIdsT :: Monad m => ExtractIdsT m () -> m IdMap
execExtractIdsT = execWriterT . runExtractIdsT

instance (HasDynFlags m, Monad m) => HasDynFlags (ExtractIdsT m) where
  getDynFlags = lift getDynFlags

-- This is not the standard MonadIO, but the MonadIO from GHC!
instance MonadIO m => MonadIO (ExtractIdsT m) where
  liftIO = lift . liftIO

debugPP :: (MonadIO m, HasDynFlags m, Outputable a) => String -> a -> m ()
debugPP header val = do
  dynFlags <- getDynFlags
  liftIO $ appendFile "/tmp/ghc.log" (header ++ showSDoc dynFlags (ppr val) ++ "\n")

record :: (HasDynFlags m, Monad m, ConstructIdInfo id)
       => SrcSpan -> IsBinder -> id -> ExtractIdsT m ()
record span isBinder id = do
  dynFlags <- getDynFlags
  sourceSpan <- case extractSourceSpan span of
    Left sourceSpan -> return sourceSpan
    Right unhelpful -> fail $ "Id without sourcespan: " ++ unhelpful
  tell . IdMap . Map.singleton sourceSpan $ constructIdInfo dynFlags isBinder id

extractIdsPlugin :: IORef [IdMap] -> HscPlugin
extractIdsPlugin symbolRef = HscPlugin $ \env -> do
  identMap <- execExtractIdsT $ extractIds (tcg_binds env)

  debugPP "tcg_rn_decls" (tcg_rn_decls env)

  liftIO $ appendFile "/tmp/ghc.log" (show identMap)
  liftIO $ modifyIORef symbolRef (identMap :)

  return env

{-------------------------------------------------------------------------------
  ConstructIdInfo
-------------------------------------------------------------------------------}

class ConstructIdInfo id where
  constructIdInfo :: DynFlags -> IsBinder -> id -> IdInfo

instance ConstructIdInfo Id where
  constructIdInfo dynFlags idIsBinder id = IdInfo{..}
    where
      nameStruct = Var.varName id
      occStruct = Name.nameOccName nameStruct
      moduleStruct = Name.nameModule_maybe nameStruct
      idName    = Name.occNameString occStruct
      idModule  =
        fmap (Module.moduleNameString . Module.moduleName) moduleStruct
      idPackage =
        fmap (Module.packageIdString . Module.modulePackageId) moduleStruct
      idSpace   = fromGhcNameSpace $ Name.occNameSpace occStruct
      idType    = Just $ showSDoc dynFlags (ppr $ Var.varType id)
      idDefSpan = extractSourceSpan (Name.nameSrcSpan nameStruct)

{------------------------------------------------------------------------------
  ExtractIds
------------------------------------------------------------------------------}

class ExtractIds a where
  extractIds :: (Functor m, MonadIO m, HasDynFlags m) => a -> ExtractIdsT m ()

instance ConstructIdInfo id => ExtractIds (LHsBinds id) where
  extractIds = mapM_ extractIds . bagToList

instance ConstructIdInfo id => ExtractIds (LHsBind id) where
  extractIds (L _span bind@(FunBind {})) = do
    record (getLoc (fun_id bind)) Binding (unLoc (fun_id bind))
    debugPP "wrapper" (fun_co_fn bind)
    extractIds (fun_matches bind)
  extractIds (L _span _bind@(PatBind {})) =
    fail "extractIds: unsupported PatBind"
  extractIds (L _span _bind@(VarBind {})) =
    fail "extractIds: unsupported VarBind"
  extractIds (L _span bind@(AbsBinds {})) =
    extractIds (abs_binds bind)

instance ConstructIdInfo id => ExtractIds (MatchGroup id) where
  extractIds (MatchGroup matches _postTcType) = do
    mapM_ extractIds matches
    -- We ignore the postTcType, as it doesn't have location information

instance ConstructIdInfo id => ExtractIds (LMatch id) where
  extractIds (L _span (Match _pats _type rhss)) =
    -- TODO: process _pats
    extractIds rhss

instance ConstructIdInfo id => ExtractIds (GRHSs id) where
  extractIds (GRHSs rhss _binds) = do
    mapM_ extractIds rhss
    -- TODO: deal with the where clause (`binds`)

instance ConstructIdInfo id => ExtractIds (LGRHS id) where
  extractIds (L _span (GRHS _guards rhs)) = extractIds rhs

instance ConstructIdInfo id => ExtractIds (HsLocalBinds id) where
  extractIds EmptyLocalBinds =
    return ()
  extractIds (HsValBinds (ValBindsIn _ _)) =
    fail "extractIds: Unexpected ValBindsIn (after renamer these should not exist)"
  extractIds (HsValBinds (ValBindsOut binds _sigs)) =
    mapM_ (extractIds . snd) binds -- "fst" is 'rec flag'
  extractIds (HsIPBinds _) =
    fail "extractIds: unsupported HsIPBinds"

instance ConstructIdInfo id => ExtractIds (LHsExpr id) where
  extractIds (L _ (HsPar expr)) =
    extractIds expr
  extractIds (L _ (ExprWithTySigOut expr _type)) = do
    extractIds expr
    debugPP "ExprWithTySigOut" _type
  extractIds (L _ (HsOverLit _ )) =
    return ()
  extractIds (L _ (OpApp left op _fix right)) = do
    extractIds left
    extractIds op
    extractIds right
  extractIds (L span (HsVar id)) =
    record span NonBinding id
  extractIds (L span (HsWrap _wrapper expr)) =
    extractIds (L span expr)
  extractIds (L _ (HsLet binds expr)) = do
    extractIds binds
    extractIds expr

  -- We should ignore unrecognized expressions rather than throw an error
  -- However, for writing this code in the first place it's useful to know
  -- which constructor we fail to support
  extractIds (L _ (HsIPVar _ )) = fail "extractIds: unsupported HsIPVar"
  extractIds (L _ (HsLit _)) = fail "extractIds: unsupported HsLit"
  extractIds (L _ (HsLam _)) = fail "extractIds: unsupported HsLam"
  extractIds (L _ (HsLamCase _ _ )) = fail "extractIds: unsupported HsLamCase"
  extractIds (L _ (HsApp _ _)) = fail "extractIds: unsupported HsApp"
  extractIds (L _ (NegApp _ _)) = fail "extractIds: unsupported NegApp"
  extractIds (L _ (SectionL _ _)) = fail "extractIds: unsupported SectionL"
  extractIds (L _ (SectionR _ _)) = fail "extractIds: unsupported SectionR"
  extractIds (L _ (ExplicitTuple _ _)) = fail "extractIds: unsupported ExplicitTuple"
  extractIds (L _ (HsCase _ _)) = fail "extractIds: unsupported HsCase"
  extractIds (L _ (HsIf _ _ _ _)) = fail "extractIds: unsupported HsIf"
  extractIds (L _ (HsMultiIf _ _)) = fail "extractIds: unsupported HsMultiIf"
  extractIds (L _ (HsDo _ _ _)) = fail "extractIds: unsupported HsDo"
  extractIds (L _ (ExplicitList _ _)) = fail "extractIds: unsupported ExplicitList"
  extractIds (L _ (ExplicitPArr _ _)) = fail "extractIds: unsupported ExplicitPArr"
  extractIds (L _ (RecordCon _ _ _)) = fail "extractIds: unsupported RecordCon"
  extractIds (L _ (RecordUpd _ _ _ _ _)) = fail "extractIds: unsupported RecordUpd"
  extractIds (L _ (ExprWithTySig _ _)) = fail "extractIds: unsupported ExprWithTySig"
  extractIds (L _ (ArithSeq _ _ )) = fail "extractIds: unsupported ArithSeq"
  extractIds (L _ (PArrSeq _ _)) = fail "extractIds: unsupported PArrSeq"
  extractIds (L _ (HsSCC _ _)) = fail "extractIds: unsupported HsSCC"
  extractIds (L _ (HsCoreAnn _ _)) = fail "extractIds: unsupported HsCoreAnn"
  extractIds (L _ (HsBracket _)) = fail "extractIds: unsupported HsBracket"
  extractIds (L _ (HsBracketOut _ _)) = fail "extractIds: unsupported HsBracketOut"
  extractIds (L _ (HsSpliceE _)) = fail "extractIds: unsupported HsSpliceE"
  extractIds (L _ (HsQuasiQuoteE _ )) = fail "extractIds: unsupported HsQuasiQuoteE"
  extractIds (L _ (HsProc _ _)) = fail "extractIds: unsupported HsProc"
  extractIds (L _ (HsArrApp _ _ _ _ _)) = fail "extractIds: unsupported HsArrApp"
  extractIds (L _ (HsArrForm _ _ _)) = fail "extractIds: unsupported HsArrForm"
  extractIds (L _ (HsTick _ _)) = fail "extractIds: unsupported HsTick"
  extractIds (L _ (HsBinTick _ _ _)) = fail "extractIds: unsupported HsBinTick"
  extractIds (L _ (HsTickPragma _ _)) = fail "extractIds: unsupported HsTickPragma"
  extractIds (L _ (EWildPat)) = fail "extractIds: unsupported EWildPat"
  extractIds (L _ (EAsPat _ _)) = fail "extractIds: unsupported EAsPat"
  extractIds (L _ (EViewPat _ _)) = fail "extractIds: unsupported EViewPat"
  extractIds (L _ (ELazyPat _)) = fail "extractIds: unsupported ELazyPat"
  extractIds (L _ (HsType _ )) = fail "extractIds: unsupported HsType"
