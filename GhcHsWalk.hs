{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module GhcHsWalk
  ( IdMap(..)
  , IdInfo(..)
  , IdNameSpace(..)
  , IsBinder(..)
  , extractIdsPlugin
  , haddockLink
  ) where

import Prelude hiding (span, id, mod)
import Control.Monad (forM_)
import Control.Monad.Writer (MonadWriter, WriterT, execWriterT, tell)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.IORef
import System.FilePath (takeFileName)
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.TH (deriveJSON)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
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

-- Right-biased union (information provided by the type checker replaces
-- information provided by the renamer)
instance Monoid IdMap where
  mempty = IdMap Map.empty
  (IdMap a) `mappend` (IdMap b) = IdMap (Map.union b a)

fromGhcNameSpace :: Name.NameSpace -> IdNameSpace
fromGhcNameSpace ns =
  if ns == Name.varName then VarName
  else if ns == Name.dataName then DataName
  else if ns == Name.tvName then TvName
  else if ns == Name.tcName then TcClsName
  else error "fromGhcNameSpace"

-- | Show approximately what Haddock adds to documantation URLs.
haddockSpaceMarks :: IdNameSpace -> String
haddockSpaceMarks VarName = "v"
haddockSpaceMarks DataName = "v"
haddockSpaceMarks TvName = "t"
haddockSpaceMarks TcClsName = "t"

-- | Show approximately a haddock link (without haddock root) for an id.
-- This is an illustraction and a test of the id info, but under ideal
-- conditions could perhaps serve to link to documentation without
-- going via Hoogle.
haddockLink :: IdMap -> SourceSpan -> String
haddockLink (IdMap m) sp =
  case Map.lookup sp m of
    Nothing -> "<identifier not found>"
    Just IdInfo{..} ->
        fromMaybe "<unknown package>" idPackage ++ "/"
        ++ maybe "<unknown module>" dotToDash idModule ++ ".html#"
        ++ haddockSpaceMarks idSpace ++ ":"
        ++ idName
 where
  dotToDash = map (\c -> if c == '.' then '-' else c)

{------------------------------------------------------------------------------
  Extract an IdMap from information returned by the ghc type checker
------------------------------------------------------------------------------}

extractIdsPlugin :: IORef [IdMap] -> HscPlugin
extractIdsPlugin symbolRef = HscPlugin $ \env -> do
  identMap <- execExtractIdsT $ do extractIds (tcg_rn_decls env)
                                   extractIds (tcg_binds env)

  liftIO $ appendFile "/tmp/ghc.log" (show identMap)
  liftIO $ modifyIORef symbolRef (identMap :)

  return env

{------------------------------------------------------------------------------
  ExtractIdsT is just a wrapper around the writer monad for IdMap
  (we wrap it to avoid orphan instances). Note that MonadIO is GHC's
  MonadIO, not the standard one, and hence we need our own instance.
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

-- We should ignore unrecognized expressions rather than throw an error
-- However, for writing this code in the first place it's useful to know
-- which constructor we fail to support
unsupported :: Monad m => String -> ExtractIdsT m ()
unsupported c = fail $ "extractIds: unsupported " ++ c

{------------------------------------------------------------------------------
  ConstructIdInfo
------------------------------------------------------------------------------}

class OutputableBndr id => ConstructIdInfo id where
  constructIdInfo :: DynFlags -> IsBinder -> id -> IdInfo

instance ConstructIdInfo Id where
  constructIdInfo dynFlags idIsBinder id =
    (constructIdInfo dynFlags idIsBinder (Var.varName id)) {
        idType = Just . showSDoc dynFlags . ppr . Var.varType $ id
      }

instance ConstructIdInfo Name where
  constructIdInfo _ idIsBinder name = IdInfo{..}
    where
      occ       = Name.nameOccName name
      mod       = Name.nameModule_maybe name
      idName    = Name.occNameString occ
      idModule  = fmap (Module.moduleNameString . Module.moduleName) mod
      idPackage = fmap (Module.packageIdString . Module.modulePackageId) mod
      idSpace   = fromGhcNameSpace $ Name.occNameSpace occ
      idDefSpan = extractSourceSpan (Name.nameSrcSpan name)
      idType    = Nothing -- After renamer but before typechecker

{------------------------------------------------------------------------------
  ExtractIds
------------------------------------------------------------------------------}

class ExtractIds a where
  extractIds :: (Functor m, MonadIO m, HasDynFlags m) => a -> ExtractIdsT m ()

instance ExtractIds a => ExtractIds [a] where
  extractIds = mapM_ extractIds

instance ExtractIds a => ExtractIds (Maybe a) where
  extractIds Nothing  = return ()
  extractIds (Just x) = extractIds x

instance ConstructIdInfo id => ExtractIds (HsGroup id) where
  extractIds group = do
    -- TODO: HsGroup has lots of other fields
    extractIds (hs_valds group)

instance ConstructIdInfo id => ExtractIds (HsValBinds id) where
  extractIds (ValBindsIn {}) =
    fail "extractIds: Unexpected ValBindsIn"
  extractIds (ValBindsOut binds sigs) = do
    extractIds (map snd binds)
    extractIds sigs

instance ConstructIdInfo id => ExtractIds (LSig id) where
  extractIds (L _span (TypeSig names tp)) = do
    forM_ names $ \name -> record (getLoc name) NonBinding (unLoc name)
    extractIds tp
  extractIds (L _span (GenericSig _ _)) = unsupported "GenericSig"
  extractIds (L _span (IdSig _))        = unsupported "IdSig"
  extractIds (L _span (FixSig _))       = unsupported "FixSig"
  extractIds (L _span (InlineSig _ _))  = unsupported "InlineSig"
  extractIds (L _span (SpecSig _ _ _))  = unsupported "SpecSig"
  extractIds (L _span (SpecInstSig _))  = unsupported "SpecInstSig"

instance ConstructIdInfo id => ExtractIds (LHsType id) where
  extractIds (L _span (HsFunTy arg res)) =
    extractIds [arg, res]
  extractIds (L span (HsTyVar name)) =
    record span NonBinding name

  extractIds (L _span (HsForAllTy _ _ _ _))    = unsupported "HsForAllTy _ _ _"
  extractIds (L _span (HsAppTy _ _))           = unsupported "HsAppTy _"
  extractIds (L _span (HsListTy _))            = unsupported "HsListTy"
  extractIds (L _span (HsPArrTy _))            = unsupported "HsPArrTy"
  extractIds (L _span (HsTupleTy _ _))         = unsupported "HsTupleTy _"
  extractIds (L _span (HsOpTy _ _ _))          = unsupported "HsOpTy _ _"
  extractIds (L _span (HsParTy _))             = unsupported "HsParTy"
  extractIds (L _span (HsIParamTy _ _))        = unsupported "HsIParamTy _"
  extractIds (L _span (HsEqTy _ _))            = unsupported "HsEqTy _"
  extractIds (L _span (HsKindSig _ _))         = unsupported "HsKindSig _"
  extractIds (L _span (HsQuasiQuoteTy _))      = unsupported "HsQuasiQuoteTy"
  extractIds (L _span (HsSpliceTy _ _ _))      = unsupported "HsSpliceTy _ _"
  extractIds (L _span (HsDocTy _ _))           = unsupported "HsDocTy _"
  extractIds (L _span (HsBangTy _ _))          = unsupported "HsBangTy _"
  extractIds (L _span (HsRecTy _))             = unsupported "HsRecTy"
  extractIds (L _span (HsCoreTy _))            = unsupported "HsCoreTy"
  extractIds (L _span (HsExplicitListTy _ _))  = unsupported "HsExplicitListTy _"
  extractIds (L _span (HsExplicitTupleTy _ _)) = unsupported "HsExplicitTupleTy _"
  extractIds (L _span (HsTyLit _))             = unsupported "HsTyLit"
  extractIds (L _span (HsWrapTy _ _))          = unsupported "HsWrapTy _"

instance ConstructIdInfo id => ExtractIds (LHsBinds id) where
  extractIds = extractIds . bagToList

instance ConstructIdInfo id => ExtractIds (LHsBind id) where
  extractIds (L _span bind@(FunBind {})) = do
    record (getLoc (fun_id bind)) Binding (unLoc (fun_id bind))
    extractIds (fun_matches bind)
  extractIds (L _span _bind@(PatBind {})) =
    unsupported "PatBind"
  extractIds (L _span _bind@(VarBind {})) =
    unsupported "VarBind"
  extractIds (L _span bind@(AbsBinds {})) =
    extractIds (abs_binds bind)

instance ConstructIdInfo id => ExtractIds (MatchGroup id) where
  extractIds (MatchGroup matches _postTcType) = do
    extractIds matches
    -- We ignore the postTcType, as it doesn't have location information

instance ConstructIdInfo id => ExtractIds (LMatch id) where
  extractIds (L _span (Match _pats _type rhss)) =
    -- TODO: process _pats
    extractIds rhss

instance ConstructIdInfo id => ExtractIds (GRHSs id) where
  extractIds (GRHSs rhss _binds) = do
    extractIds rhss
    -- TODO: deal with the where clause (`binds`)

instance ConstructIdInfo id => ExtractIds (LGRHS id) where
  extractIds (L _span (GRHS _guards rhs)) = extractIds rhs

instance ConstructIdInfo id => ExtractIds (HsLocalBinds id) where
  extractIds EmptyLocalBinds =
    return ()
  extractIds (HsValBinds (ValBindsIn _ _)) =
    fail "extractIds: Unexpected ValBindsIn (after renamer these should not exist)"
  extractIds (HsValBinds (ValBindsOut binds _sigs)) =
    extractIds (map snd binds) -- "fst" is 'rec flag'
  extractIds (HsIPBinds _) =
    unsupported "HsIPBinds"

instance ConstructIdInfo id => ExtractIds (LHsExpr id) where
  extractIds (L _ (HsPar expr)) =
    extractIds expr
  extractIds (L _ (ExprWithTySig expr _type)) = do
    extractIds expr
    debugPP "ExprWithTySig" _type
  extractIds (L _ (ExprWithTySigOut expr _type)) = do
    extractIds expr
    debugPP "ExprWithTySig" _type
  extractIds (L _ (HsOverLit _ )) =
    return ()
  extractIds (L _ (OpApp left op _fix right)) = do
    extractIds [left, op, right]
  extractIds (L span (HsVar id)) =
    record span NonBinding id
  extractIds (L span (HsWrap _wrapper expr)) =
    extractIds (L span expr)
  extractIds (L _ (HsLet binds expr)) = do
    extractIds binds
    extractIds expr

  extractIds (L _ (HsIPVar _ ))          = unsupported "HsIPVar"
  extractIds (L _ (HsLit _))             = unsupported "HsLit"
  extractIds (L _ (HsLam _))             = unsupported "HsLam"
  extractIds (L _ (HsLamCase _ _ ))      = unsupported "HsLamCase"
  extractIds (L _ (HsApp _ _))           = unsupported "HsApp"
  extractIds (L _ (NegApp _ _))          = unsupported "NegApp"
  extractIds (L _ (SectionL _ _))        = unsupported "SectionL"
  extractIds (L _ (SectionR _ _))        = unsupported "SectionR"
  extractIds (L _ (ExplicitTuple _ _))   = unsupported "ExplicitTuple"
  extractIds (L _ (HsCase _ _))          = unsupported "HsCase"
  extractIds (L _ (HsIf _ _ _ _))        = unsupported "HsIf"
  extractIds (L _ (HsMultiIf _ _))       = unsupported "HsMultiIf"
  extractIds (L _ (HsDo _ _ _))          = unsupported "HsDo"
  extractIds (L _ (ExplicitList _ _))    = unsupported "ExplicitList"
  extractIds (L _ (ExplicitPArr _ _))    = unsupported "ExplicitPArr"
  extractIds (L _ (RecordCon _ _ _))     = unsupported "RecordCon"
  extractIds (L _ (RecordUpd _ _ _ _ _)) = unsupported "RecordUpd"
  extractIds (L _ (ArithSeq _ _ ))       = unsupported "ArithSeq"
  extractIds (L _ (PArrSeq _ _))         = unsupported "PArrSeq"
  extractIds (L _ (HsSCC _ _))           = unsupported "HsSCC"
  extractIds (L _ (HsCoreAnn _ _))       = unsupported "HsCoreAnn"
  extractIds (L _ (HsBracket _))         = unsupported "HsBracket"
  extractIds (L _ (HsBracketOut _ _))    = unsupported "HsBracketOut"
  extractIds (L _ (HsSpliceE _))         = unsupported "HsSpliceE"
  extractIds (L _ (HsQuasiQuoteE _ ))    = unsupported "HsQuasiQuoteE"
  extractIds (L _ (HsProc _ _))          = unsupported "HsProc"
  extractIds (L _ (HsArrApp _ _ _ _ _))  = unsupported "HsArrApp"
  extractIds (L _ (HsArrForm _ _ _))     = unsupported "HsArrForm"
  extractIds (L _ (HsTick _ _))          = unsupported "HsTick"
  extractIds (L _ (HsBinTick _ _ _))     = unsupported "HsBinTick"
  extractIds (L _ (HsTickPragma _ _))    = unsupported "HsTickPragma"
  extractIds (L _ (EWildPat))            = unsupported "EWildPat"
  extractIds (L _ (EAsPat _ _))          = unsupported "EAsPat"
  extractIds (L _ (EViewPat _ _))        = unsupported "EViewPat"
  extractIds (L _ (ELazyPat _))          = unsupported "ELazyPat"
  extractIds (L _ (HsType _ ))           = unsupported "HsType"
