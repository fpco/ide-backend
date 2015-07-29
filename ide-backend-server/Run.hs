{-# LANGUAGE CPP, TemplateHaskell, RecordWildCards, ScopedTypeVariables #-}
-- Copyright   : (c) JP Moresmau 2011,
--                   Well-Typed 2012
-- (JP Moresmau's buildwrapper package used as template for GHC API use)
--
-- | Implementation details of the calls to GHC that compute information
-- based on source files, provides progress information while doing so
-- and optionally compiles, links and executes code.
--
-- Only @IdeSession.GHC.Run@ and @IdeSession.GHC.HsWalk@ should import
-- any modules from the ghc package and the modules should not be reexported
-- anywhere else, with the exception of @IdeSession.GHC.Server@.
module Run
  ( -- * Re-exported GHC API
    Ghc
  , runFromGhc
  , liftIO
  , GhcException
  , ghandle
  , ghandleJust
  , getModuleGraph
  , moduleNameString
  , ms_mod_name, ms_hs_date
    -- * Processing source files (including compilation)
  , compileInGhc
  , collectSrcError
  , DynamicOpts
  , submitStaticOpts
  , optsToDynFlags
  , DynFlags(hooks)
  , defaultDynFlags
  , getSessionDynFlags
  , setSessionDynFlags
  , parseDynamicFlags
  , hsExtensions
  , importList
  , autocompletion
    -- * Executing snippets
  , runInGhc
  , RunCmd(..)
  , RunResult(..)
  , RunBufferMode(..)
  , breakFromSpan
  , printVars
  ) where

#define DEBUG 0

import Prelude hiding (id, mod, span)
import qualified Control.Exception as Ex
import Control.Monad (filterM, liftM, void, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Applicative ((<$>))
import Data.List ((\\))
import Data.Maybe (catMaybes, listToMaybe)
import Data.Text (Text)
import Data.Array (Array)
import qualified Data.Array as Array
import qualified Data.Text as Text
import System.FilePath.Find (find, always, extension)
import System.Process
import System.IO.Unsafe (unsafePerformIO)

#if defined(GHC_761)
-- http://hackage.haskell.org/trac/ghc/ticket/7548
-- is not fixed in this version. Compilation should fail,
-- because the problem is not minor and not easy to spot otherwise.
#error "GHC 7.6.1 is broken (#7548) and not supported"
#endif

import Bag (bagToList)
import DynFlags (defaultDynFlags)
import Exception (ghandle)
import ErrUtils (ErrMsg)
import FastString (unpackFS, mkFastString)
import GHC hiding (
    ModuleName
  , RunResult(..)
  , BreakInfo(..)
  , getBreak
  )
import GhcMonad (liftIO)
import OccName (occEnvElts)
import Outputable (PprStyle, mkUserStyle)
import RdrName (GlobalRdrEnv, GlobalRdrElt, gre_name)
import RnNames (rnImports)
import SrcLoc (mkRealSrcSpan, mkRealSrcLoc)
import TcRnMonad (initTc)
import TcRnTypes (RnM)
import RtClosureInspect (Term)
import qualified HscTypes
import qualified GHC           as GHC
import qualified Config        as GHC
import qualified Outputable    as GHC
import qualified ByteCodeInstr as GHC
import qualified Id            as GHC

#if __GLASGOW_HASKELL__ >= 706
import ErrUtils   ( MsgDoc )
#else
import ErrUtils   ( Message )
#endif

import IdeSession.GHC.API
import IdeSession.Types.Public (RunBufferMode(..), Targets(..))
import IdeSession.Types.Private
import qualified IdeSession.Types.Public as Public
import IdeSession.Util
import IdeSession.Strict.Container
import IdeSession.Strict.IORef
import qualified IdeSession.Strict.List as StrictList
import qualified IdeSession.Strict.Map  as StrictMap

import HsWalk (idInfoForName)
import Haddock
import Debug
import Conv
import FilePathCaching
import Break
import GhcShim

type DynamicOpts = [Located String]

-- | Set static flags at server startup and return dynamic flags.
submitStaticOpts :: [String] -> IO DynamicOpts
submitStaticOpts opts = do
  (dynFlags, _) <- parseStaticFlags (map noLoc opts)
  return dynFlags

-- | Create dynamic flags from their command-line names.
optsToDynFlags :: [String] -> DynamicOpts
optsToDynFlags = map noLoc

getGhcLibdir :: IO FilePath
getGhcLibdir = do
  let ghcbinary = "ghc-" ++ GHC.cProjectVersion
  out <- readProcess ghcbinary ["--print-libdir"] ""
  case lines out of
    [libdir] -> return libdir
    _        -> fail "cannot parse output of ghc --print-libdir"

runFromGhc :: Ghc a -> IO a
runFromGhc a = do
  libdir <- getGhcLibdir
  runGhc (Just libdir) a

-- | Version of handleJust for use in the GHC monad
ghandleJust :: Ex.Exception e => (e -> Maybe b) -> (b -> Ghc a) -> Ghc a -> Ghc a
ghandleJust p handler a = ghandle handler' a
  where
    handler' e = case p e of
                   Nothing -> liftIO $ Ex.throwIO e
                   Just b  -> handler b

-- | Compile a set of targets
--
-- Returns the errors, loaded modules, and mapping from filenames to modules
compileInGhc :: FilePath            -- ^ target directory, only used for 'TargetsExclude'
             -> Bool                -- ^ should we generate code
             -> Targets             -- ^ targets
             -> StrictIORef (Strict [] SourceError) -- ^ the IORef where GHC stores errors
             -> Ghc ( Strict [] SourceError
                    , [ModuleName]
                    , Strict (Map FilePath) ModuleId
                    )
compileInGhc configSourcesDir generateCode mTargets errsRef = do
    -- Reset errors storage.
    liftIO $ writeIORef errsRef StrictList.nil
    -- Compute new GHC flags.
    flags1 <- getSessionDynFlags
    -- Let GHC API print "compiling M ... done." for each module.
    let verbosityNew :: Int
        verbosityNew = 1
        (hscTargetNew, ghcLinkNew)
          | generateCode = (HscInterpreted, LinkInMemory)
          | otherwise    = (HscNothing,     NoLink)
        flagsNeedChange = hscTargetNew /= hscTarget flags1
                          || ghcLinkNew /= ghcLink flags1
                          || verbosityNew /= verbosity flags1
        flags = flags1 {
                         hscTarget = hscTargetNew,
                         ghcLink = ghcLinkNew,
                         verbosity = verbosityNew
                       }
    handleErrors flags $ do
      defaultCleanupHandler flags $ do
        -- Set up the GHC flags.
        when flagsNeedChange $ void $ setSessionDynFlags flags
        setTargets =<< computeTargets
        void $ load LoadAllTargets

    -- Collect info
    errs    <- liftIO $ readIORef errsRef
    loaded  <- getLoaded
    fileMap <- getFileMap
    return ( StrictList.reverse errs
           , loaded
           , StrictMap.fromList $ fileMap
           )
  where
    getLoaded :: Ghc [ModuleName]
    getLoaded = do
      graph <- getModuleGraph
      names <- filterM isLoaded $ map ms_mod_name graph
      return $ map (Text.pack . moduleNameString) names

    getFileMap :: Ghc [(FilePath, ModuleId)]
    getFileMap = do
        dflags <- getSessionDynFlags
        let aux :: ModSummary -> Maybe (FilePath, ModuleId)
            aux summary = do
              hs <- ml_hs_file $ ms_location summary
              return (hs, importModuleId dflags (ms_mod summary))
        graph <- getModuleGraph
        return $ catMaybes $ map aux graph

    computeTargets :: Ghc [Target]
    computeTargets = do
      targetIds <- case mTargets of
        TargetsInclude include -> return (map targetIdFromFile include)
        TargetsExclude exclude -> liftIO $ do
          allPaths <- find always ((`elem` hsExtensions) `liftM` extension)
                                  configSourcesDir
          let paths = allPaths \\ exclude
          return (map targetIdFromFile paths)
      return (map targetWithId targetIds)

    targetWithId :: TargetId -> Target
    targetWithId targetId = Target {
        targetId           = targetId
      , targetAllowObjCode = True
      , targetContents     = Nothing
      }

    targetIdFromFile :: FilePath -> TargetId
    targetIdFromFile path = TargetFile path Nothing

    sourceErrorHandler :: DynFlags -> HscTypes.SourceError -> Ghc ()
    sourceErrorHandler _flags e = liftIO $ do
      debug dVerbosity $ "handleSourceError: " ++ show e
      errs <- readIORef errsRef
      e'   <- fromHscSourceError e
      writeIORef errsRef (force e' StrictList.++ errs)

    -- A workaround for http://hackage.haskell.org/trac/ghc/ticket/7430.
    -- Some errors are reported as exceptions instead.
    ghcExceptionHandler :: DynFlags -> GhcException -> Ghc ()
    ghcExceptionHandler _flags e = liftIO $ do
      let eText   = Text.pack $ show e  -- no SrcSpan as a field in GhcException
          fromEx  = Text.pack $ "<from GhcException>"
          exError = SourceError KindError (TextSpan fromEx) eText
            -- though it may be embedded in string
      debug dVerbosity $ "handleOtherErrors: " ++ Text.unpack eText
      errs <- readIORef errsRef
      writeIORef errsRef (exError `StrictList.cons` errs)

    handleErrors :: DynFlags -> Ghc () -> Ghc ()
    handleErrors flags = ghandle (ghcExceptionHandler flags)
                       . handleSourceError (sourceErrorHandler flags)

importList :: ModSummary -> Ghc (Strict [] Import)
importList summary = do
  dflags  <- getSessionDynFlags

  -- TODO: This is lossy. We might want a more accurate data type.
  let unLIE :: LIE RdrName -> Text
      unLIE (L _ name) = Text.pack $ pretty dflags GHC.defaultUserStyle name

#if __GLASGOW_HASKELL__ >= 710
  let unLocNames :: Located [LIE name] -> [LIE name]
      unLocNames (L _ names) = names
#else
  let unLocNames :: [LIE name] -> [LIE name]
      unLocNames names = names
#endif

#if __GLASGOW_HASKELL__ >= 710
  let mkImportEntities :: Maybe (Bool, Located [LIE RdrName]) -> ImportEntities
#else
  let mkImportEntities :: Maybe (Bool, [LIE RdrName]) -> ImportEntities
#endif
      mkImportEntities Nothing               = ImportAll
      mkImportEntities (Just (True, names))  = ImportHiding (force $ map unLIE (unLocNames names))
      mkImportEntities (Just (False, names)) = ImportOnly   (force $ map unLIE (unLocNames names))

  let goImp :: Located (ImportDecl RdrName) -> Import
      goImp (L _ decl) = Import {
          importModule    = importModuleId' dflags (ideclPkgQual decl) (unLoc (ideclName decl))
        , importPackage   = force $ ((Text.pack . unpackFS) <$> ideclPkgQual decl)
        , importQualified = ideclQualified decl
        , importImplicit  = ideclImplicit decl
        , importAs        = force $ ((Text.pack . moduleNameString) <$> ideclAs decl)
        , importEntities  = mkImportEntities (ideclHiding decl)
        }

  return . force $ map goImp (ms_srcimps summary)
                ++ map goImp (ms_textual_imps summary)

autocompletion :: ModSummary -> Ghc (Strict [] IdInfo)
autocompletion summary = do
  dflags  <- getSessionDynFlags
  session <- getSession

  let pkgDeps = pkgDepsFromModSummary dflags summary
  linkEnv <- liftIO $ linkEnvForDeps dflags pkgDeps

  let eltsToAutocompleteMap :: GlobalRdrElt -> Ghc IdInfo
      eltsToAutocompleteMap elt = do
        let name          = gre_name elt
            currentModule = Nothing -- Must be imported (TH stage restriction)
        (idProp, Just idScope) <- idInfoForName dflags
                                                name
                                                UseSite
                                                (Just elt)
                                                currentModule
                                                (homeModuleFor dflags linkEnv)
        return IdInfo{..}

      autoEnvs :: ModSummary -> IO [GlobalRdrElt]
      autoEnvs ms@ModSummary{ ms_mod
                                 , ms_hsc_src
                                 , ms_srcimps
                                 , ms_textual_imps
                                 } = do
        let go :: RnM (a, GlobalRdrEnv, b, c) -> IO [GlobalRdrElt]
            go op = do
#if GHC_AFTER_710_1
              -- Not sure if this is the same thing that GHC would
              -- pass in, but this is just to initialize the state,
              -- and will be overridden before any uses.
              let srcSpan = mkRealSrcSpan loc loc
                  loc = mkRealSrcLoc (mkFastString (HscTypes.msHsFilePath ms)) 1 1
              ((_warns, _errs), res) <- initTc session ms_hsc_src False ms_mod srcSpan op
#else
              ((_warns, _errs), res) <- initTc session ms_hsc_src False ms_mod op
#endif
              case res of
                Nothing -> do
                  -- TODO: deal with import errors
#if DEBUG
                  appendFile "/tmp/ghc.importerrors" $ show
                                                     . map GHC.showSDoc
                                                     $ ErrUtils.pprErrMsgBag _errs
#endif
                  return []
                Just (_, elts, _, _) ->
                  return . concat $ occEnvElts elts
        env1 <- go $ rnImports ms_srcimps
        env2 <- go $ rnImports ms_textual_imps
        return $ env1 ++ env2

  envs    <- liftIO $ autoEnvs summary
  idIs    <- mapM eltsToAutocompleteMap envs
  return $ force idIs

-- | Run a snippet.
runInGhc :: RunCmd -> Ghc RunResult
runInGhc cmd = do
  flags <- getSessionDynFlags
  defaultCleanupHandler flags . handleErrors $ do
      runRes <- runCmd cmd
      case runRes of
        GHC.RunOk _ ->
          return RunOk
        GHC.RunException ex ->
          return $ RunProgException (showExWithClass ex)
        GHC.RunBreak _tid names mBreakInfo -> do
          Just info <- importBreakInfo mBreakInfo names
          return $ RunBreak info
  where
    handleError :: Show a => a -> Ghc RunResult
    handleError = return . RunGhcException . show

    -- "such-and-such not in scope" is reported as a source error
    -- not sure when GhcExceptions are thrown (if at all)
    handleErrors :: Ghc RunResult -> Ghc RunResult
    handleErrors = handleSourceError handleError
                 . ghandle (handleError :: GhcException -> Ghc RunResult)

-- | Auxiliary to 'runInGhc'
runCmd :: RunCmd -> Ghc GHC.RunResult
runCmd (RunStmt {..}) = do
    setContext $ [ IIDecl $ simpleImportDecl $ mkModuleName runCmdModule
                 , IIDecl $ simpleImportDecl $ mkModuleName "IdeBackendRTS"
                 ]
    runStmt expr RunToCompletion
  where
    expr :: String
    expr = fqn "run "
        ++ "(" ++ fqBMode runCmdStdout ++ ")"
        ++ "(" ++ fqBMode runCmdStderr ++ ")"
        ++ "(" ++ runCmdModule ++ "." ++ runCmdFunction ++ ")"

    fqn :: String -> String
    fqn = (++) "IdeBackendRTS."

    fqBMode :: RunBufferMode -> String
    fqBMode RunNoBuffering =
      fqn "RunNoBuffering"
    fqBMode (RunLineBuffering t) =
      fqn "RunLineBuffering (" ++ fqMInt t ++ ")"
    fqBMode (RunBlockBuffering sz t) =
      fqn "RunBlockBuffering (" ++ fqMInt sz ++ ") (" ++ fqMInt t ++ ")"

    fqMInt :: Maybe Int -> String
    fqMInt Nothing  = fqn "Nothing"
    fqMInt (Just n) = fqn "Just " ++ show n
runCmd Resume =
    resume (const True) RunToCompletion

{------------------------------------------------------------------------------
  Dealing with breakpoints
------------------------------------------------------------------------------}

-- | We store a print context whenever we hit a breakpoint so that we have
-- sufficient context for subsequent requests for pretty-printing types
printContext :: StrictIORef PprStyle
{-# NOINLINE printContext #-}
printContext = unsafePerformIO $
  newIORef (mkUserStyle alwaysQualify GHC.AllTheWay)

getPrintContext :: Ghc PprStyle
getPrintContext = liftIO $ readIORef printContext

setPrintContext :: PprStyle -> Ghc ()
setPrintContext ctxt = liftIO $ writeIORef printContext ctxt

breakFromSpan :: ModuleName        -- ^ Module containing the breakpoint
              -> Public.SourceSpan -- ^ Location of the breakpoint
              -> Bool              -- ^ New value for the breakpoint
              -> Ghc (Maybe Bool)    -- ^ Old valeu of the breakpoint (if valid)
breakFromSpan modName span newValue = runMaybeT $ do
    modInfo <- MaybeT $ getModuleInfo mod
    let ModBreaks{..} = modInfoModBreaks modInfo

    breakIndex <- MaybeT $ return $ findBreakIndex modBreaks_locs
    oldValue   <- MaybeT $ getBreak modBreaks_flags breakIndex

    lift $ setBreak modBreaks_flags breakIndex newValue
    return oldValue
  where
    findBreakIndex :: Array BreakIndex SrcSpan -> Maybe BreakIndex
    findBreakIndex breaks = listToMaybe
                          . catMaybes
                          . map matchesSpan
                          . Array.assocs
                          $ breaks

    matchesSpan :: (BreakIndex, SrcSpan) -> Maybe BreakIndex
    matchesSpan (_, UnhelpfulSpan _) = Nothing
    matchesSpan (i, RealSrcSpan span') =
      if  srcSpanStartLine span' == Public.spanFromLine   span
       && srcSpanStartCol  span' == Public.spanFromColumn span
       && srcSpanEndLine   span' == Public.spanToLine     span
       && srcSpanEndCol    span' == Public.spanToColumn   span
      then Just i
      else Nothing

    mod :: Module
    mod = mkModule mainPackageKey (mkModuleName . Text.unpack $ modName)

importBreakInfo :: Maybe GHC.BreakInfo
                -> [Name]
                -> Ghc (Maybe BreakInfo)
importBreakInfo (Just GHC.BreakInfo{..}) names = runMaybeT $ do
    modInfo          <- MaybeT $ getModuleInfo breakInfo_module
    printUnqualified <- MaybeT $ mkPrintUnqualifiedForModule modInfo

    let ModBreaks{..} = modInfoModBreaks modInfo
        srcSpan       = modBreaks_locs Array.! breakInfo_number
        pprStyle      = mkUserStyle printUnqualified GHC.AllTheWay

    lift $ do
      setPrintContext pprStyle

      localVars           <- mkTerms >>= mapM (exportVar pprStyle)
      ProperSpan srcSpan' <- liftIO $ extractEitherSpan srcSpan
      prettyResTy         <- prettyM pprStyle breakInfo_resty

      return BreakInfo {
          breakInfoModule      = mod
        , breakInfoSpan        = srcSpan'
        , breakInfoResultType  = Text.pack prettyResTy
        , breakInfoVariableEnv = localVars
        }
  where
    mkTerms :: Ghc [(Id, Term)]
    mkTerms = resolveNames names >>= evaluateIds False False

    -- we ignore the package (because it's always the home package)
    mod = Text.pack . moduleNameString . GHC.moduleName $ breakInfo_module
importBreakInfo Nothing _names = error "importBreakInfo: missing case"

printVars :: String -> Bool -> Bool -> Ghc Public.VariableEnv
printVars vars bind forceEval =
  getPrintContext            >>= \unqual ->
  parseNames vars            >>=
  resolveNames               >>=
  evaluateIds bind forceEval >>=
  mapM (exportVar unqual)

exportVar :: PprStyle -> (Id, Term) -> Ghc (Public.Name, Public.Type, Public.Value)
exportVar pprStyle (var, term) = do
    nameStr <- prettyM     pprStyle             (GHC.idName var)
    typeStr <- prettyTypeM pprStyle showForalls (GHC.idType var)
    termStr <- prettyM     pprStyle             term
    return (Text.pack nameStr, Text.pack typeStr, Text.pack termStr)
  where
    showForalls  = False

-----------------------
-- Source error conversion and collection
--

collectSrcError :: StrictIORef (Strict [] SourceError)
                -> (String -> IO ())
                -> (String -> IO ())
                -> DynFlags
                -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()
collectSrcError errsRef handlerOutput handlerRemaining flags
                severity srcspan style msg = do
  -- Prepare debug prints.
  debug dVerbosity
   $  "Severity: "   ++ show severity
   ++ "  SrcSpan: "  ++ show srcspan
-- ++ "  PprStyle: " ++ show style
   ++ "  MsgDoc: "
   ++ showSDocForUser flags (queryQual style) msg
   ++ "\n"
  -- Actually collect errors.
  collectSrcError'
    errsRef handlerOutput handlerRemaining flags severity srcspan style msg

collectSrcError' :: StrictIORef (Strict [] SourceError)
                 -> (String -> IO ())
                 -> (String -> IO ())
                 -> DynFlags
                 -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()
collectSrcError' errsRef _ _ flags severity srcspan style msg
  | Just errKind <- case severity of
                      SevWarning -> Just KindWarning
                      SevError   -> Just KindError
                      SevFatal   -> Just KindError
                      _          -> Nothing
  = do let msgstr = showSDocForUser flags (queryQual style) msg
       sp <- extractEitherSpan srcspan
       modifyIORef errsRef (StrictList.cons $ SourceError errKind sp (Text.pack msgstr))

collectSrcError' _errsRef handlerOutput _ flags SevOutput _srcspan style msg
  = let msgstr = showSDocForUser flags (queryQual style) msg
     in handlerOutput msgstr

collectSrcError' _errsRef _ handlerRemaining flags _severity _srcspan style msg
  = let msgstr = showSDocForUser flags (queryQual style) msg
     in handlerRemaining msgstr

-- TODO: perhaps make a honest SrcError from the first span from the first
-- error message and put the rest into the message string? That probably
-- entains string-search-and-replace inside the message string not to
-- duplicate the first span, because the types involved are abstract.
-- And even then, all the file paths but the first would be out of
-- our control (and so, e.g., not relative to the project root).
-- But at least the IDE could point somewhere in the code.
-- | Convert GHC's SourceError type into ours.
fromHscSourceError :: forall m. MonadFilePathCaching m => HscTypes.SourceError -> m [SourceError]
fromHscSourceError = mapM aux . bagToList . HscTypes.srcErrorMessages
  where
    aux :: ErrMsg -> m SourceError
    aux e = do
      let err   = Text.pack (show e)
          noloc = Text.pack "<no location info>"

      case sourceErrorSpan e of
        Just real -> do
          xSpan <- extractEitherSpan real
          return $ SourceError KindError xSpan err
        Nothing ->
          return $ SourceError KindError (TextSpan noloc) err

-----------------------
-- GHC version compat
--

#if __GLASGOW_HASKELL__ < 706
type MsgDoc = Message
#endif

showSDocForUser :: DynFlags -> PrintUnqualified -> MsgDoc -> String
#if __GLASGOW_HASKELL__ >= 706
showSDocForUser  flags uqual msg = GHC.showSDocForUser flags uqual msg
#else
showSDocForUser _flags uqual msg = GHC.showSDocForUser       uqual msg
#endif

showSDocDebug :: DynFlags -> MsgDoc -> String
#if __GLASGOW_HASKELL__ >= 706
showSDocDebug  flags msg = GHC.showSDocDebug flags msg
#else
showSDocDebug _flags msg = GHC.showSDocDebug        msg
#endif

queryQual :: PprStyle -> PrintUnqualified
#if __GLASGOW_HASKELL__ >= 710
queryQual = GHC.queryQual
#else
queryQual style = (GHC.qualName style, GHC.qualModule style)
#endif


-----------------------
-- Debug
--

_debugPpContext :: DynFlags -> String -> Ghc ()
_debugPpContext flags msg = do
  context <- getContext
  liftIO $ debug dVerbosity
    $ msg ++ ": " ++ showSDocDebug flags (GHC.ppr context)
