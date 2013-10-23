{-# LANGUAGE CPP, TemplateHaskell #-}
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
  ( -- * Re-expored GHC API
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
  , DynamicOpts
  , submitStaticOpts
  , optsToDynFlags
  , DynFlags(sourcePlugins)
  , defaultDynFlags
  , getSessionDynFlags
  , setSessionDynFlags
  , parseDynamicFlags
  , hsExtensions
  , importList
  , autocompletion
    -- * Executing snippets
  , runInGhc
  , RunResult(..)
  , RunBufferMode(..)
  ) where

#define DEBUG 0

import Prelude hiding (mod)

import Bag (bagToList)
import qualified Config as GHC
import DynFlags (defaultDynFlags)
import qualified ErrUtils
import Exception (ghandle)
import FastString ( unpackFS )
import qualified GHC
import GhcMonad (liftIO)
import qualified HscTypes
import Outputable ( PprStyle, qualName, qualModule )
import qualified Outputable as GHC
#if __GLASGOW_HASKELL__ >= 706
import ErrUtils   ( MsgDoc )
#else
import ErrUtils   ( Message )
#endif

#if defined(GHC_761)
-- http://hackage.haskell.org/trac/ghc/ticket/7548
-- is not fixed in this version. Compilation should fail,
-- because the problem is not minor and not easy to spot otherwise.
#else
import GHC hiding (flags, ModuleName, RunResult(..))
import GhcMonad (modifySession)
import HscTypes (HscEnv(hsc_mod_graph))
import System.Time
import RnNames (rnImports)
import TcRnMonad (initTc)
import TcRnTypes (RnM)
import OccName (occEnvElts)
import RdrName (GlobalRdrEnv, GlobalRdrElt, gre_name)
#endif

import qualified Control.Exception as Ex
import Control.Monad (filterM, liftM, void)
import Control.Applicative ((<$>))
import Data.List ((\\))
import Data.Text (Text)
import qualified Data.Text as Text
import System.FilePath.Find (find, always, extension)
import System.Process
import Control.Concurrent (MVar, ThreadId)

import IdeSession.GHC.API
import IdeSession.Types.Public (RunBufferMode(..))
import IdeSession.Types.Private
import IdeSession.Util
import IdeSession.Strict.Container
import IdeSession.Strict.IORef
import qualified IdeSession.Strict.List  as StrictList

import HsWalk (extractSourceSpan, idInfoForName, IsBinder(..))
import Haddock
import Debug
import Conv
import FilePathCaching

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

#if __GLASGOW_HASKELL__ < 706 || defined(GHC_761)
-- A workaround for http://hackage.haskell.org/trac/ghc/ticket/7478.
-- WIth some luck the fix should work for all these versions.
-- The problem is fixed for 7.6.2 onwards.
invalidateModSummaryCache :: GhcMonad m => m ()
invalidateModSummaryCache =
  modifySession $ \h -> h { hsc_mod_graph = map inval (hsc_mod_graph h) }
 where
#if __GLASGOW_HASKELL__ >= 706
  inval ms = ms { ms_hs_date = addUTCTime (-1) (ms_hs_date ms) }
#else
  tdiff = TimeDiff 0 0 0 0 0 (-1) 0
  inval ms = ms { ms_hs_date = addToClockTime tdiff (ms_hs_date ms) }
#endif
#endif

compileInGhc :: FilePath            -- ^ target directory
             -> DynamicOpts         -- ^ dynamic flags for this call
             -> Bool                -- ^ should we generate code
             -> Int                 -- ^ verbosity level
             -> StrictIORef (Strict [] SourceError) -- ^ the IORef where GHC stores errors
             -> (String -> IO ())   -- ^ handler for each SevOutput message
             -> (String -> IO ())   -- ^ handler for remaining non-error msgs
             -> Ghc (Strict [] SourceError, [ModuleName])
compileInGhc configSourcesDir dynOpts
             generateCode verbosity
             errsRef handlerOutput handlerRemaining = do
    -- Reset errors storage.
    liftIO $ writeIORef errsRef StrictList.nil
    -- Determine files to process.
    targets <- liftIO $ find always
                             ((`elem` hsExtensions) `liftM` extension)
                             configSourcesDir
    -- Compute new GHC flags.
    flags0 <- getSessionDynFlags
    (flags1, _, _) <- parseDynamicFlags flags0 dynOpts
    let (hscTarget, ghcLink) | generateCode = (HscInterpreted, LinkInMemory)
                             | otherwise    = (HscNothing,     NoLink)
        flags = flags1 {
                         hscTarget,
                         ghcLink,
                         ghcMode    = CompManager,
                         verbosity,
#if __GLASGOW_HASKELL__ >= 706
                         log_action = collectSrcError errsRef handlerOutput handlerRemaining
#else
                         log_action = collectSrcError errsRef handlerOutput handlerRemaining flags
#endif
                       }
    handleErrors flags $ do
      defaultCleanupHandler flags $ do
        -- Set up the GHC flags.
#if __GLASGOW_HASKELL__ < 706 || defined(GHC_761)
        invalidateModSummaryCache
#endif
        _ <- setSessionDynFlags flags
        -- Set up targets.
        oldTargets <- getTargets
        let targetIdFromFile file = TargetFile file Nothing
            addSingle filename = do
              addTarget Target
                { targetId           = targetIdFromFile filename
                , targetAllowObjCode = True
                , targetContents     = Nothing
                }
            fileFromTarget Target{targetId} =
              case targetId of
                TargetFile file Nothing -> file
                _ -> error "fileFromTarget: not a known target"
            oldFiles = map fileFromTarget oldTargets
        mapM_ addSingle (targets \\ oldFiles)
        mapM_ removeTarget $ map targetIdFromFile $ oldFiles \\ targets
        -- Load modules to typecheck and perhaps generate code, too.
        void $ load LoadAllTargets

    -- Collect info
    errs   <- liftIO $ readIORef errsRef
    loaded <- getModuleGraph >>= filterM isLoaded . map ms_mod_name
    return ( StrictList.reverse errs
           , map (Text.pack . moduleNameString) loaded
           )
  where
    sourceErrorHandler :: DynFlags -> HscTypes.SourceError -> Ghc ()
    sourceErrorHandler _flags e = liftIO $ do
      debug dVerbosity $ "handleSourceError: " ++ show e
      errs <- readIORef errsRef
      e'   <- fromHscSourceError e
      writeIORef errsRef (e' `StrictList.cons` errs)

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
  where
    mkImportEntities :: Maybe (Bool, [LIE RdrName]) -> ImportEntities
    mkImportEntities Nothing               = ImportAll
    mkImportEntities (Just (True, names))  = ImportHiding (force $ map unLIE names)
    mkImportEntities (Just (False, names)) = ImportOnly   (force $ map unLIE names)

    -- TODO: This is lossy. We might want a more accurate data type.
    unLIE :: LIE RdrName -> Text
    unLIE (L _ name) = Text.pack $ GHC.showSDoc (GHC.ppr name)

autocompletion :: ModSummary -> Ghc (Strict [] IdInfo)
autocompletion summary = do
  dflags  <- getSessionDynFlags
  session <- getSession

  let pkgDeps = pkgDepsFromModSummary dflags summary
  linkEnv <- liftIO $ linkEnvForDeps dflags pkgDeps

  let eltsToAutocompleteMap :: GlobalRdrElt -> IO IdInfo
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
      autoEnvs ModSummary{ ms_mod
                                 , ms_hsc_src
                                 , ms_srcimps
                                 , ms_textual_imps
                                 } = do
        let go :: RnM (a, GlobalRdrEnv, b, c) -> IO [GlobalRdrElt]
            go op = do
              ((_warns, _errs), res) <- initTc session ms_hsc_src False ms_mod op
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
  idIs    <- liftIO $ mapM eltsToAutocompleteMap envs
  return $ force idIs

-- | Run a snippet.
runInGhc :: (String, String)  -- ^ module and function to execute
         -> RunBufferMode     -- ^ Buffer mode for stdout
         -> RunBufferMode     -- ^ Buffer mode for stderr
         -> MVar (Maybe ThreadId)
         -> Ghc RunResult
runInGhc (m, fun) outBMode errBMode tidMVar = do
  flags <- getSessionDynFlags
  -- Half of a workaround for http://hackage.haskell.org/trac/ghc/ticket/7456.
  -- Set GHC verbosity to avoid stray GHC messages, e.g., from the linker.
  _ <- setSessionDynFlags (flags { verbosity = 0 })
  -- TODO: not sure if this cleanup handler is needed:
  defaultCleanupHandler flags . handleErrors $ do
-- TODO: these debug statements break tests currently:
--    _debugPpContext flags "context before setContext"
    setContext $ [ IIDecl $ simpleImportDecl $ mkModuleName m
                 , IIDecl $ simpleImportDecl $ mkModuleName "IdeBackendRTS"
                 ]
--    _debugPpContext flags "context after setContext"
--    liftIO $ writeFile "/Users/fpco/fpco/ide-backend/RunStmt.hs" expr
    handleErrors $ do
      runRes <- runStmt expr RunToCompletion tidMVar
      case runRes of
        GHC.RunOk _ ->
          return RunOk
        GHC.RunException ex ->
          return . RunProgException $ showExWithClass ex
        GHC.RunBreak{} ->
          error "checkModule: RunBreak"
  where
    expr :: String
    expr = fqn "run "
        ++ "(" ++ fqBMode outBMode ++ ")"
        ++ "(" ++ fqBMode errBMode ++ ")"
        ++ "(" ++ m ++ "." ++ fun ++ ")"

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

    handleError :: Show a => a -> Ghc RunResult
    handleError = return . RunGhcException . show

    -- "such-and-such not in scope" is reported as a source error
    -- not sure when GhcExceptions are thrown (if at all)
    handleErrors :: Ghc RunResult -> Ghc RunResult
    handleErrors = handleSourceError handleError
                 . ghandle (handleError :: GhcException -> Ghc RunResult)

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
  let showSeverity SevOutput  = "SevOutput"
#if __GLASGOW_HASKELL__ >= 706
      showSeverity SevDump    = "SevDump"
#endif
      showSeverity SevInfo    = "SevInfo"
      showSeverity SevWarning = "SevWarning"
      showSeverity SevError   = "SevError"
      showSeverity SevFatal   = "SevFatal"
  debug dVerbosity
   $  "Severity: "   ++ showSeverity severity
   ++ "  SrcSpan: "  ++ show srcspan
-- ++ "  PprStyle: " ++ show style
   ++ "  MsgDoc: "
   ++ showSDocForUser flags (qualName style,qualModule style) msg
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
  = do let msgstr = showSDocForUser flags (qualName style,qualModule style) msg
       sp <- extractSourceSpan srcspan
       modifyIORef errsRef (StrictList.cons $ SourceError errKind sp (Text.pack msgstr))

collectSrcError' _errsRef handlerOutput _ flags SevOutput _srcspan style msg
  = let msgstr = showSDocForUser flags (qualName style,qualModule style) msg
     in handlerOutput msgstr

collectSrcError' _errsRef _ handlerRemaining flags _severity _srcspan style msg
  = let msgstr = showSDocForUser flags (qualName style,qualModule style) msg
     in handlerRemaining msgstr

-- TODO: perhaps make a honest SrcError from the first span from the first
-- error message and put the rest into the message string? That probably
-- entains string-search-and-replace inside the message string not to
-- duplicate the first span, because the types involved are abstract.
-- And even then, all the file paths but the first would be out of
-- our control (and so, e.g., not relative to the project root).
-- But at least the IDE could point somewhere in the code.
-- | Convert GHC's SourceError type into ours.
fromHscSourceError :: MonadFilePathCaching m => HscTypes.SourceError -> m SourceError
fromHscSourceError e = case bagToList (HscTypes.srcErrorMessages e) of
    [errMsg] -> case ErrUtils.errMsgSpans errMsg of
      [real@RealSrcSpan{}] -> do
        xSpan <- extractSourceSpan real
        return $ SourceError KindError xSpan err
      _ ->
        return $ SourceError KindError (TextSpan noloc) err
    _ ->
      return $ SourceError KindError (TextSpan noloc) err
  where
    err   = Text.pack (show e)
    noloc = Text.pack "<no location info>"

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

-----------------------
-- Debug
--

_debugPpContext :: DynFlags -> String -> Ghc ()
_debugPpContext flags msg = do
  context <- getContext
  liftIO $ debug dVerbosity
    $ msg ++ ": " ++ showSDocDebug flags (GHC.ppr context)
