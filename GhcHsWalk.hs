{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, GeneralizedNewtypeDeriving, TemplateHaskell, CPP #-}
module GhcHsWalk
  ( IdMap(..)
  , IdInfo(..)
  , IdNameSpace(..)
  , IsBinder(..)
  , extractIdsPlugin
  , haddockLink
  , idInfoAtLocation
  ) where

import Prelude hiding (span, id, mod)
import Control.Monad (forM_)
import Control.Monad.Writer (MonadWriter, WriterT, execWriterT, tell, censor)
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT, ask)
import Control.Monad.Trans.Class (MonadTrans, lift)
import Data.IORef
import System.FilePath (takeFileName)
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.TH (deriveJSON)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Monoid
import Control.Applicative ((<$>))

import Common
import GhcRun (extractSourceSpan)

import GHC hiding (idType)
import TcRnTypes
import Outputable
import HscPlugin
import Var hiding (idInfo)
import qualified Name
import qualified Module
import MonadUtils (MonadIO(..))
import Bag
import DataCon (dataConName)

#define DEBUG 1

{------------------------------------------------------------------------------
  TODO: Current known problems:

  - RECORDS

    Given

    > data T = MkT { a :: Bool, b :: Int }
    > someT = MkT { a = True, b = 5 }

    the record declaration does not have any source info at all for MkT, a or b;
    the record definition as 'a' point to the record definition rather than the
    record declaration.
------------------------------------------------------------------------------}


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
  , idDefSpan :: EitherSpan
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
      ppSpan (SourceSpan fn stL stC endL endC) =
        fn ++ ":" ++ ppDash stL endL ++ ":" ++ ppDash stC endC
      ppEitherSpan (ProperSpan sp) = ppSpan sp
      ppEitherSpan (TextSpan s) = s

      pp (sp, IdInfo{..}) =
        takeFileName (ppSpan sp)
        ++ " (" ++ show idSpace
        ++ (case idIsBinder of Binding -> ", binder): " ; _ -> "): ")
        ++ maybe "" (++ "/") idPackage
        ++ maybe "" (++ ".") idModule
        ++ idName ++ " :: "
        ++ fromMaybe "<unknown type>" idType
        ++ " (" ++ takeFileName (ppEitherSpan idDefSpan) ++ ")"

-- Right-biased union (information provided by the type checker replaces
-- information provided by the renamer)
instance Monoid IdMap where
  mempty = IdMap Map.empty
  (IdMap a) `mappend` (IdMap b) = IdMap (b `Map.union` a)

fromGhcNameSpace :: Name.NameSpace -> IdNameSpace
fromGhcNameSpace ns
  | ns == Name.varName  = VarName
  | ns == Name.dataName = DataName
  | ns == Name.tvName   = TvName
  | ns == Name.tcName   = TcClsName
  | otherwise = error "fromGhcNameSpace"

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
haddockLink :: IdInfo -> String
haddockLink IdInfo{..} =
      latest (fromMaybe "<unknown package>" idPackage) ++ "/doc/html/"
   ++ maybe "<unknown module>" dotToDash idModule ++ ".html#"
   ++ haddockSpaceMarks idSpace ++ ":"
   ++ idName
 where
   dotToDash = map (\c -> if c == '.' then '-' else c)
   latest p =
     let (afterDash, uptoDash) = break (== '-') $ reverse p
     in if null uptoDash
        then reverse afterDash ++ "/latest"
        else reverse (tail uptoDash) ++ "/" ++ reverse afterDash

idInfoAtLocation :: Int -> Int -> IdMap -> [IdInfo]
idInfoAtLocation line col = map snd . filter inRange . idMapToList
  where
    inRange :: (SourceSpan, a) -> Bool
    inRange (SourceSpan{..}, _) =
      (line   > spanFromLine || (line == spanFromLine && col >= spanFromColumn)) &&
      (line   < spanToLine   || (line == spanToLine   && col <= spanToColumn))

{------------------------------------------------------------------------------
  Extract an IdMap from information returned by the ghc type checker
------------------------------------------------------------------------------}

extractIdsPlugin :: IORef [IdMap] -> HscPlugin
extractIdsPlugin symbolRef = HscPlugin $ \dynFlags env -> do
  identMap <- execExtractIdsT dynFlags $ do
    -- Information provided by the renamer
    -- See http://www.haskell.org/pipermail/ghc-devs/2013-February/000540.html
    extractIds (tcg_rn_decls env)
    -- Information provided by the type checker
    extractIds (tcg_binds env)
  liftIO $ writeFile "/tmp/ghc.idmap" (show identMap)
  liftIO $ modifyIORef symbolRef (identMap :)
  return env

{------------------------------------------------------------------------------
  ExtractIdsT is just a wrapper around the writer monad for IdMap
  (we wrap it to avoid orphan instances). Note that MonadIO is GHC's
  MonadIO, not the standard one, and hence we need our own instance.
------------------------------------------------------------------------------}

-- Define type synonym to avoid orphan instances
newtype ExtractIdsT m a = ExtractIdsT {
      runExtractIdsT :: ReaderT DynFlags (WriterT IdMap m) a
    }
  deriving (Functor, Monad, MonadWriter IdMap, MonadReader DynFlags)

execExtractIdsT :: Monad m => DynFlags -> ExtractIdsT m () -> m IdMap
execExtractIdsT dynFlags m = execWriterT (runReaderT (runExtractIdsT m) dynFlags)

instance MonadTrans ExtractIdsT where
  lift = ExtractIdsT . lift . lift

-- This is not the standard MonadIO, but the MonadIO from GHC!
instance MonadIO m => MonadIO (ExtractIdsT m) where
  liftIO = lift . liftIO

-- In ghc 7.4 showSDoc does not take the dynflags argument; for 7.6 and up
-- it does
pretty :: (Monad m, Outputable a) => a -> ExtractIdsT m String
pretty val = do
  _dynFlags <- ask
#if __GLASGOW_HASKELL__ >= 706
  return $ showSDoc _dynFlags (ppr val)
#else
  return $ showSDoc (ppr val)
#endif

debugPP :: (MonadIO m, Outputable a) => String -> a -> ExtractIdsT m ()
#if DEBUG
debugPP header val = do
  val' <- pretty val
  liftIO $ appendFile "/tmp/ghc.log" (header ++ ": " ++ val' ++ "\n")
#else
debugPP _ _ = return ()
#endif

record :: (MonadIO m, ConstructIdInfo id)
       => SrcSpan -> IsBinder -> id -> ExtractIdsT m ()
record span isBinder id =
  case extractSourceSpan span of
    ProperSpan sourceSpan -> do
      idInfo <- constructIdInfo isBinder id
      tell . IdMap $ Map.singleton sourceSpan idInfo
    TextSpan _ ->
      debugPP "Id without source span" id

-- For debugging purposes, we also record information about the AST
ast :: Monad m => Maybe SrcSpan -> String -> ExtractIdsT m a -> ExtractIdsT m a
#if DEBUG
ast mspan info cont = do
    case extractSourceSpan <$> mspan of
      Just (ProperSpan sourceSpan) -> do
        let idInfo = IdInfo { idName     = info
                            , idModule   = Nothing
                            , idPackage  = Nothing
                            , idSpace    = VarName
                            , idType     = Nothing
                            , idDefSpan  = TextSpan "<Debugging>"
                            , idIsBinder = NonBinding
                            }
        tell . IdMap $ Map.singleton sourceSpan idInfo
      _ ->
       return ()
    censor addInfo cont
  where
    addInfo :: IdMap -> IdMap
    addInfo = IdMap . Map.map addInfo' . idMapToMap

    addInfo' :: IdInfo -> IdInfo
    addInfo' idInfo =
      if idDefSpan idInfo == TextSpan "<Debugging>"
        then idInfo { idName = info ++ "/" ++ idName idInfo }
        else idInfo
#else
ast _ _ cont = cond
#endif

unsupported :: MonadIO m => Maybe SrcSpan -> String -> ExtractIdsT m ()
#if DEBUG
unsupported mspan c = ast mspan c $ do
  prettySpan <- pretty mspan
  liftIO . appendFile "/tmp/ghc.log" $ "extractIds: unsupported " ++ c ++ " at " ++ prettySpan ++ "\n"
#else
unsupported _ = return ()
#endif

{------------------------------------------------------------------------------
  ConstructIdInfo
------------------------------------------------------------------------------}

class OutputableBndr id => ConstructIdInfo id where
  constructIdInfo :: Monad m => IsBinder -> id -> ExtractIdsT m IdInfo

instance ConstructIdInfo Id where
  constructIdInfo idIsBinder id = do
    idInfo <- constructIdInfo idIsBinder (Var.varName id)
    typ    <- pretty (Var.varType id)
    return idInfo { idType = Just typ }

instance ConstructIdInfo Name where
  constructIdInfo idIsBinder name = return IdInfo{..}
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
  extractIds :: MonadIO m => a -> ExtractIdsT m ()

instance ExtractIds a => ExtractIds [a] where
  extractIds = mapM_ extractIds

instance ExtractIds a => ExtractIds (Maybe a) where
  extractIds Nothing  = return ()
  extractIds (Just x) = extractIds x

instance ConstructIdInfo id => ExtractIds (HsGroup id) where
  extractIds group = ast Nothing "HsGroup" $ do
    -- TODO: HsGroup has lots of other fields
    extractIds (hs_valds group)
    extractIds (hs_tyclds group)

instance ConstructIdInfo id => ExtractIds (HsValBinds id) where
  extractIds (ValBindsIn {}) =
    fail "extractIds: Unexpected ValBindsIn"
  extractIds (ValBindsOut binds sigs) = ast Nothing "ValBindsOut" $ do
    extractIds (map snd binds)
    extractIds sigs

instance ConstructIdInfo id => ExtractIds (LSig id) where
  extractIds (L span (TypeSig names tp)) = ast (Just span) "TypeSig" $ do
    forM_ names $ \name -> record (getLoc name) NonBinding (unLoc name)
    extractIds tp
  extractIds (L span (GenericSig _ _)) = unsupported (Just span) "GenericSig"
  extractIds (L span (IdSig _))        = unsupported (Just span) "IdSig"
  extractIds (L span (FixSig _))       = unsupported (Just span) "FixSig"
  extractIds (L span (InlineSig _ _))  = unsupported (Just span) "InlineSig"
  extractIds (L span (SpecSig _ _ _))  = unsupported (Just span) "SpecSig"
  extractIds (L span (SpecInstSig _))  = unsupported (Just span) "SpecInstSig"

instance ConstructIdInfo id => ExtractIds (LHsType id) where
  extractIds (L span (HsFunTy arg res)) = ast (Just span) "HsFunTy" $
    extractIds [arg, res]
  extractIds (L span (HsTyVar name)) = ast (Just span) "HsTyVar" $
    record span NonBinding name
  extractIds (L span (HsForAllTy _explicitFlag tyVars ctxt body)) = ast (Just span) "hsForAllTy" $ do
    extractIds tyVars
    extractIds ctxt
    extractIds body
  extractIds (L span (HsAppTy fun arg)) = ast (Just span) "HsAppTy" $
    extractIds [fun, arg]
  extractIds (L span (HsTupleTy _tupleSort typs)) = ast (Just span) "HsTupleTy" $
    -- tupleSort is unboxed/boxed/etc.
    extractIds typs
  extractIds (L span (HsListTy typ)) = ast (Just span) "HsListTy" $
    extractIds typ
  extractIds (L span (HsPArrTy typ)) = ast (Just span) "HsPArrTy" $
    extractIds typ
  extractIds (L span (HsParTy typ)) = ast (Just span) "HsParTy" $
    extractIds typ
  extractIds (L span (HsEqTy a b)) = ast (Just span) "HsEqTy" $
    extractIds [a, b]

  extractIds (L span (HsOpTy _ _ _))          = unsupported (Just span) "HsOpTy"
  extractIds (L span (HsIParamTy _ _))        = unsupported (Just span) "HsIParamTy"
  extractIds (L span (HsKindSig _ _))         = unsupported (Just span) "HsKindSig"
  extractIds (L span (HsQuasiQuoteTy _))      = unsupported (Just span) "HsQuasiQuoteTy"
  extractIds (L span (HsSpliceTy _ _ _))      = unsupported (Just span) "HsSpliceTy"
  extractIds (L span (HsDocTy _ _))           = unsupported (Just span) "HsDocTy"
  extractIds (L span (HsBangTy _ _))          = unsupported (Just span) "HsBangTy"
  extractIds (L span (HsRecTy _))             = unsupported (Just span) "HsRecTy"
  extractIds (L span (HsCoreTy _))            = unsupported (Just span) "HsCoreTy"
  extractIds (L span (HsExplicitListTy _ _))  = unsupported (Just span) "HsExplicitListTy"
  extractIds (L span (HsExplicitTupleTy _ _)) = unsupported (Just span) "HsExplicitTupleTy"
  extractIds (L span (HsWrapTy _ _))          = unsupported (Just span) "HsWrapTy"

#if __GLASGOW_HASKELL__ >= 706
  extractIds (L span (HsTyLit _))             = unsupported (Just span) "HsTyLit"
#endif

#if __GLASGOW_HASKELL__ >= 706
instance ConstructIdInfo id => ExtractIds (LHsTyVarBndrs id) where
  extractIds (HsQTvs _kvs tvs) = ast Nothing "HsQTvs" $ do
    -- We don't have location info for the kind variables
    extractIds tvs
#endif

instance ConstructIdInfo id => ExtractIds (LHsTyVarBndr id) where
#if __GLASGOW_HASKELL__ >= 706
  extractIds (L span (UserTyVar name)) = ast (Just span) "UserTyVar" $
#else
  extractIds (L span (UserTyVar name _postTcKind)) = ast (Just span) "UserTyVar" $
#endif
    record span Binding name

#if __GLASGOW_HASKELL__ >= 706
  extractIds (L span (KindedTyVar name _kind)) = ast (Just span) "KindedTyVar" $
#else
  extractIds (L span (KindedTyVar name _kind _postTcKind)) = ast (Just span) "KindedTyVar" $
#endif
    -- TODO: deal with _kind
    record span Binding name

instance ConstructIdInfo id => ExtractIds (LHsContext id) where
  extractIds (L span typs) = ast (Just span) "LHsContext" $
    extractIds typs

instance ConstructIdInfo id => ExtractIds (LHsBinds id) where
  extractIds = extractIds . bagToList

instance ConstructIdInfo id => ExtractIds (LHsBind id) where
  extractIds (L span bind@(FunBind {})) = ast (Just span) "FunBind" $ do
    record (getLoc (fun_id bind)) Binding (unLoc (fun_id bind))
    extractIds (fun_matches bind)
  extractIds (L span _bind@(PatBind {})) = ast (Just span) "PatBind" $
    unsupported (Just span) "PatBind"
  extractIds (L span _bind@(VarBind {})) =
    unsupported (Just span) "VarBind"
  extractIds (L span bind@(AbsBinds {})) = ast (Just span) "AbsBinds" $
    extractIds (abs_binds bind)

instance ConstructIdInfo id => ExtractIds (MatchGroup id) where
  extractIds (MatchGroup matches _postTcType) = ast Nothing "MatchGroup" $
    extractIds matches
    -- We ignore the postTcType, as it doesn't have location information

instance ConstructIdInfo id => ExtractIds (LMatch id) where
  extractIds (L span (Match pats _type rhss)) = ast (Just span) "Match" $ do
    extractIds pats
    extractIds rhss

instance ConstructIdInfo id => ExtractIds (GRHSs id) where
  extractIds (GRHSs rhss binds) = ast Nothing "GRHSs" $ do
    extractIds rhss
    extractIds binds

instance ConstructIdInfo id => ExtractIds (LGRHS id) where
  extractIds (L span (GRHS _guards rhs)) = ast (Just span) "GRHS" $
    extractIds rhs

instance ConstructIdInfo id => ExtractIds (HsLocalBinds id) where
  extractIds EmptyLocalBinds =
    return ()
  extractIds (HsValBinds (ValBindsIn _ _)) =
    fail "extractIds: Unexpected ValBindsIn (after renamer these should not exist)"
  extractIds (HsValBinds (ValBindsOut binds sigs)) = ast Nothing "HsValBinds" $ do
    extractIds (map snd binds) -- "fst" is 'rec flag'
    extractIds sigs
  extractIds (HsIPBinds _) =
    unsupported Nothing "HsIPBinds"

instance ConstructIdInfo id => ExtractIds (LHsExpr id) where
  extractIds (L span (HsPar expr)) = ast (Just span) "HsPar" $
    extractIds expr
  extractIds (L span (ExprWithTySig expr _type)) = ast (Just span) "ExprWithTySig" $
    extractIds expr
  extractIds (L span (ExprWithTySigOut expr _type)) = ast (Just span) "ExprWithTySigOut" $
    extractIds expr
  extractIds (L span (HsOverLit _ )) = ast (Just span) "HsOverLit" $
    return ()
  extractIds (L span (OpApp left op _fix right)) = ast (Just span) "OpApp" $
    extractIds [left, op, right]
  extractIds (L span (HsVar id)) = ast (Just span) "HsVar" $
    record span NonBinding id
  extractIds (L span (HsWrap _wrapper expr)) = ast (Just span) "HsWrap" $
    extractIds (L span expr)
  extractIds (L span (HsLet binds expr)) = ast (Just span) "HsLet" $ do
    extractIds binds
    extractIds expr
  extractIds (L span (HsApp fun arg)) = ast (Just span) "HsApp" $
    extractIds [fun, arg]
  extractIds (L span lit@(HsLit _)) =
    -- Intentional omission of the "ast" debugging call here.
    -- The syntax "assert" is replaced by GHC by "assertError <span>", where
    -- both "assertError" and the "<span>" are assigned the source span of
    -- the original "assert". This means that the <span> (represented as an
    -- HsLit) might override "assertError" in the IdMap.
    return ()
  extractIds (L span (HsLam matches)) = ast (Just span) "HsLam" $
    extractIds matches
  extractIds (L span (HsDo _ctxt stmts _postTcType)) = ast (Just span) "HsDo" $
    -- ctxt indicates what kind of statement it is; AFAICT there is no
    -- useful information in it for us
    -- postTcType is not located
    extractIds stmts
  extractIds (L span (ExplicitList _postTcType exprs)) = ast (Just span) "ExplicitList" $
    extractIds exprs
  extractIds (L span (RecordCon con _postTcType recordBinds)) = ast (Just span) "RecordCon" $ do
    record (getLoc con) NonBinding (unLoc con)
    extractIds recordBinds
  extractIds (L span (HsCase expr matches)) = ast (Just span) "HsCase" $ do
    extractIds expr
    extractIds matches

  extractIds (L span (HsIPVar _ ))          = unsupported (Just span) "HsIPVar"
  extractIds (L span (NegApp _ _))          = unsupported (Just span) "NegApp"
  extractIds (L span (SectionL _ _))        = unsupported (Just span) "SectionL"
  extractIds (L span (SectionR _ _))        = unsupported (Just span) "SectionR"
  extractIds (L span (ExplicitTuple _ _))   = unsupported (Just span) "ExplicitTuple"
  extractIds (L span (HsIf _ _ _ _))        = unsupported (Just span) "HsIf"
  extractIds (L span (ExplicitPArr _ _))    = unsupported (Just span) "ExplicitPArr"
  extractIds (L span (RecordUpd _ _ _ _ _)) = unsupported (Just span) "RecordUpd"
  extractIds (L span (ArithSeq _ _ ))       = unsupported (Just span) "ArithSeq"
  extractIds (L span (PArrSeq _ _))         = unsupported (Just span) "PArrSeq"
  extractIds (L span (HsSCC _ _))           = unsupported (Just span) "HsSCC"
  extractIds (L span (HsCoreAnn _ _))       = unsupported (Just span) "HsCoreAnn"
  extractIds (L span (HsBracket _))         = unsupported (Just span) "HsBracket"
  extractIds (L span (HsBracketOut _ _))    = unsupported (Just span) "HsBracketOut"
  extractIds (L span (HsSpliceE _))         = unsupported (Just span) "HsSpliceE"
  extractIds (L span (HsQuasiQuoteE _ ))    = unsupported (Just span) "HsQuasiQuoteE"
  extractIds (L span (HsProc _ _))          = unsupported (Just span) "HsProc"
  extractIds (L span (HsArrApp _ _ _ _ _))  = unsupported (Just span) "HsArrApp"
  extractIds (L span (HsArrForm _ _ _))     = unsupported (Just span) "HsArrForm"
  extractIds (L span (HsTick _ _))          = unsupported (Just span) "HsTick"
  extractIds (L span (HsBinTick _ _ _))     = unsupported (Just span) "HsBinTick"
  extractIds (L span (HsTickPragma _ _))    = unsupported (Just span) "HsTickPragma"
  extractIds (L span EWildPat)              = unsupported (Just span) "EWildPat"
  extractIds (L span (EAsPat _ _))          = unsupported (Just span) "EAsPat"
  extractIds (L span (EViewPat _ _))        = unsupported (Just span) "EViewPat"
  extractIds (L span (ELazyPat _))          = unsupported (Just span) "ELazyPat"
  extractIds (L span (HsType _ ))           = unsupported (Just span) "HsType"

#if __GLASGOW_HASKELL__ >= 706
  extractIds (L span (HsLamCase _ _ ))      = unsupported "HsLamCase"
  extractIds (L span (HsMultiIf _ _))       = unsupported "HsMultiIf"
#endif

instance (ExtractIds a, ConstructIdInfo id) => ExtractIds (HsRecFields id a) where
  extractIds (HsRecFields rec_flds _rec_dotdot) = ast Nothing "HsRecFields" $
    extractIds rec_flds

instance (ExtractIds a, ConstructIdInfo id) => ExtractIds (HsRecField id a) where
  extractIds (HsRecField id arg _pun) = ast Nothing "HsRecField" $ do
    record (getLoc id) NonBinding (unLoc id)
    extractIds arg

-- The meaning of the constructors of LStmt isn't so obvious; see various
-- notes in ghc/compiler/hsSyn/HsExpr.lhs
instance ConstructIdInfo id => ExtractIds (LStmt id) where
  extractIds (L span (ExprStmt expr _seq _guard _postTcType)) = ast (Just span) "ExprStmt" $
    -- Neither _seq nor _guard are located
    extractIds expr
  extractIds (L span (BindStmt pat expr _bind _fail)) = ast (Just span) "BindStmt" $ do
    -- Neither _bind or _fail are located
    extractIds pat
    extractIds expr
  extractIds (L span (LetStmt binds)) = ast (Just span) "LetStmt" $
    extractIds binds
  extractIds (L span (LastStmt expr _return)) = ast (Just span) "LastStmt" $
    extractIds expr

  extractIds (L span (TransStmt {}))     = unsupported (Just span) "TransStmt"
  extractIds (L span (RecStmt {}))       = unsupported (Just span) "RecStmt"

#if __GLASGOW_HASKELL__ >= 706
  extractIds (L span (ParStmt _ _ _))    = unsupported (Just span) "ParStmt"
#else
  extractIds (L span (ParStmt _ _ _ _))  = unsupported (Just span) "ParStmt"
#endif

instance ConstructIdInfo id => ExtractIds (LPat id) where
  extractIds (L span (WildPat _postTcType)) = ast (Just span) "WildPat" $
    return ()
  extractIds (L span (VarPat id)) = ast (Just span) "VarPat" $
    record span Binding id
  extractIds (L span (LazyPat pat)) = ast (Just span) "LazyPat" $
    extractIds pat
  extractIds (L span (AsPat id pat)) = ast (Just span) "AsPat" $ do
    record (getLoc id) Binding (unLoc id)
    extractIds pat
  extractIds (L span (ParPat pat)) = ast (Just span) "ParPat" $
    extractIds pat
  extractIds (L span (BangPat pat)) = ast (Just span) "BangPat" $
    extractIds pat
  extractIds (L span (ListPat pats _postTcType)) = ast (Just span) "ListPat" $
    extractIds pats
  extractIds (L span (TuplePat pats _boxity _postTcType)) = ast (Just span) "TuplePat" $
    extractIds pats
  extractIds (L span (PArrPat pats _postTcType)) = ast (Just span) "PArrPat" $
    extractIds pats
  extractIds (L span (ConPatIn con details)) = ast (Just span) "ConPatIn" $ do
    record (getLoc con) NonBinding (unLoc con) -- the constructor name is non-binding
    extractIds details
  extractIds (L span (ConPatOut {pat_con, pat_args})) = ast (Just span) "ConPatOut" $ do
    record (getLoc pat_con) NonBinding (dataConName (unLoc pat_con))
    extractIds pat_args

  -- View patterns
  extractIds (L span (ViewPat _ _ _))     = unsupported (Just span) "ViewPat"
  extractIds (L span (QuasiQuotePat _))   = unsupported (Just span) "QuasiQuotePat"
  extractIds (L span (LitPat _))          = unsupported (Just span) "LitPat"
  extractIds (L span (NPat _ _ _))        = unsupported (Just span) "NPat"
  extractIds (L span (NPlusKPat _ _ _ _)) = unsupported (Just span) "NPlusKPat"
  extractIds (L span (SigPatIn _ _))      = unsupported (Just span) "SigPatIn"
  extractIds (L span (SigPatOut _ _))     = unsupported (Just span) "SigPatOut"
  extractIds (L span (CoPat _ _ _))       = unsupported (Just span) "CoPat"

instance ConstructIdInfo id => ExtractIds (HsConPatDetails id) where
  extractIds (PrefixCon args) = ast Nothing "PrefixCon" $
    extractIds args
  extractIds (RecCon rec) = ast Nothing "RecCon" $
    extractIds rec
  extractIds (InfixCon a b) = ast Nothing "InfixCon" $
    extractIds [a, b]

instance ConstructIdInfo id => ExtractIds (LTyClDecl id) where
  extractIds (L span (ForeignType {})) = unsupported (Just span) "ForeignType"
  extractIds (L span (TyFamily {}))    = unsupported (Just span) "TyFamily"
  extractIds (L span (TyData {}))      = unsupported (Just span) "TyData"
  extractIds (L span (TySynonym {}))   = unsupported (Just span) "TySynonym"
  extractIds (L span (ClassDecl {}))   = unsupported (Just span) "ClassDecl"
