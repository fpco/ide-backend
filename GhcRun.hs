{-# LANGUAGE CPP, TemplateHaskell #-}
-- Copyright   : (c) JP Moresmau 2011,
--                   Well-Typed 2012
-- (JP Moresmau's buildwrapper package used as template for GHC API use)
--
-- | Implementation details of the calls to GHC that compute information
-- based on source files, provides progress information while doing so
-- and optionally compiles, links and executes code.
-- Only this file should import the GHC-internals modules.
module GhcRun
  ( -- * Re-expored GHC API
    Ghc
  , runFromGhc
  , liftIO
  , GhcException
  , ghandle
  , ghandleJust
    -- * Processing source files (including compilation)
  , compileInGhc
  , DynamicOpts
  , submitStaticOpts
  , optsToDynFlags
  , DynFlags(sourcePlugins)
  , defaultDynFlags
  , getSessionDynFlags
  , setSessionDynFlags
    -- * Executing snippets
  , runInGhc
  , RunResult(..)
  , RunBufferMode(..)
    -- * Information about imports
  , Import(..)
  ) where

#define DEBUG 1

import Bag (bagToList)
import qualified Config as GHC
import DynFlags (dopt_unset, defaultDynFlags)
import qualified ErrUtils
import Exception (ghandle)
import FastString ( unpackFS )
import qualified GHC
import GhcMonad (liftIO)
import qualified HscTypes
import qualified Unique as Unique
import Outputable ( PprStyle, qualName, qualModule )
import qualified Outputable as GHC
#if __GLASGOW_HASKELL__ >= 706
import ErrUtils   ( MsgDoc )
#else
import ErrUtils   ( Message )
#endif

#if __GLASGOW_HASKELL__ == 704
-- Import our own version of --make as a workaround for
-- http://hackage.haskell.org/trac/ghc/ticket/7548
import GhcMakeFixed
import GHC hiding (flags, ModuleName, RunResult(..), load)
import GhcMonad (modifySession)
import HscTypes (HscEnv(hsc_mod_graph))
import System.Time
import RnNames (rnImports)
import TcRnMonad (initTc)
import TcRnTypes (RnM)
import OccName (occEnvElts)
import RdrName (GlobalRdrEnv, GlobalRdrElt, gre_name)
#elif __GLASGOW_HASKELL__ >= 706 && !defined(GHC_761)
-- Use the default tools. They are fixed in these GHC versions.
import GHC hiding (flags, ModuleName, RunResult(..))
#else
-- Not fixed in this version and no workaround. Compilation should fail,
-- because the problem is not minor and not easy to spot otherwise.
#endif

import qualified Control.Exception as Ex
import Control.Monad (filterM, liftM, void)
import Control.Applicative ((<$>))
import Control.Arrow (second)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IORef
import Data.List ((\\))
import Data.Maybe (fromJust)
import System.FilePath.Find (find, always, extension)
import System.Process

import Common
import Data.Aeson.TH (deriveJSON)
import GhcHsWalk (extractSourceSpan, IdInfo(..), idInfoForName)

newtype DynamicOpts = DynamicOpts [Located String]

-- | The outcome of running code
data RunResult =
    -- | The code terminated okay
    RunOk String
    -- | The code threw an exception
  | RunProgException String
    -- | GHC itself threw an exception when we tried to run the code
  | RunGhcException String
    -- | The session was restarted
  | RunForceCancelled
  deriving (Show, Eq)

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
  | RunLineBuffering  { runBufferTimeout :: Maybe Int }
  | RunBlockBuffering { runBufferBlockSize :: Maybe Int
                      , runBufferTimeout   :: Maybe Int
                      }
  deriving Show

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

$(deriveJSON id ''RunResult)
$(deriveJSON id ''RunBufferMode)
$(deriveJSON id ''Import)

-- | Set static flags at server startup and return dynamic flags.
submitStaticOpts :: [String] -> IO DynamicOpts
submitStaticOpts opts = do
  (dynFlags, _) <- parseStaticFlags (map noLoc opts)
  return $ DynamicOpts dynFlags

-- | Create dynamic flags from their command-line names.
optsToDynFlags :: [String] -> DynamicOpts
optsToDynFlags = DynamicOpts . map noLoc

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
             -> Bool                -- ^ whether to generate code
             -> Int                 -- ^ verbosity level
             -> IORef [SourceError] -- ^ the IORef where GHC stores errors
             -> (String -> IO ())   -- ^ handler for each SevOutput message
             -> (String -> IO ())   -- ^ handler for remaining non-error msgs
             -> Ghc ( [SourceError]
                    , [ModuleName]
                    , ([(ModuleName, ([Import], [IdInfo]))], IM.IntMap IdInfo)
                    )
compileInGhc configSourcesDir (DynamicOpts dynOpts)
             generateCode verbosity
             errsRef handlerOutput handlerRemaining = do
    -- Reset errors storage.
    liftIO $ writeIORef errsRef []
    -- Determine files to process.
    targets <- liftIO $ find always
                             ((`elem` hsExtentions) `liftM` extension)
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
#if __GLASGOW_HASKELL__ < 707
  -- A workaround for http://hackage.haskell.org/trac/ghc/ticket/1381.
                   `dopt_unset` Opt_GhciSandbox
#endif
    handleErrors flags $ do
      defaultCleanupHandler flags $ do
        -- Set up the GHC flags.
#if __GLASGOW_HASKELL__ < 706 || defined(GHC_761)
        invalidateModSummaryCache
#endif
        setSessionDynFlags flags
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
    session <- getSession
    graph   <- getModuleGraph
    errs   <- liftIO $ readIORef errsRef
    loaded <- filterM isLoaded (map ms_mod_name graph)
    importsAuto <- liftIO $ extractImportsAuto flags session graph
    return ( reverse errs
           , map moduleNameString loaded
           , importsAuto
           )
  where
    sourceErrorHandler :: DynFlags -> HscTypes.SourceError -> Ghc ()
    sourceErrorHandler _flags e = liftIO $ do
      debug dVerbosity $ "handleSourceError: " ++ show e
      errs <- readIORef errsRef
      writeIORef errsRef (fromHscSourceError e : errs)

    -- A workaround for http://hackage.haskell.org/trac/ghc/ticket/7430.
    -- Some errors are reported as exceptions instead.
    ghcExceptionHandler :: DynFlags -> GhcException -> Ghc ()
    ghcExceptionHandler _flags e = liftIO $ do
      let eText   = show e  -- no SrcSpan as a field in GhcException
          exError =
            SourceError KindError (TextSpan "<from GhcException>") eText
            -- though it may be embedded in string
      debug dVerbosity $ "handleOtherErrors: " ++ eText
      errs <- readIORef errsRef
      writeIORef errsRef (exError : errs)

    handleErrors :: DynFlags -> Ghc () -> Ghc ()
    handleErrors flags = ghandle (ghcExceptionHandler flags)
                       . handleSourceError (sourceErrorHandler flags)

extractImportsAuto :: DynFlags -> HscEnv -> ModuleGraph
                   -> IO ( [(ModuleName, ([Import], [IdInfo]))]
                         , IM.IntMap IdInfo )
extractImportsAuto dflags session graph = do
  idInfoCacheRef <- newIORef IM.empty
  assocs <- mapM (goMod idInfoCacheRef) graph
  cache <- readIORef idInfoCacheRef
  return (assocs, cache)
  where
    goMod :: IORef (IntMap IdInfo) -> ModSummary
          -> IO (ModuleName, ([Import], [IdInfo]))
    goMod idInfoCacheRef summary = do
      envs <- autoEnvs summary
      idIs <- mapM (eltsToAutocompleteMap idInfoCacheRef) envs
      return ( moduleNameString (ms_mod_name summary)
             , ( map goImp (ms_srcimps summary)
                 ++ map goImp (ms_textual_imps summary)
               , idIs
               )
             )

    goImp :: Located (ImportDecl RdrName) -> Import
    goImp (L _ decl) = Import {
        importModule    = moduleNameString (unLoc (ideclName decl))
      , importPackage   = unpackFS <$> ideclPkgQual decl
      , importQualified = ideclQualified decl
      , importImplicit  = ideclImplicit decl
      , importAs        = moduleNameString <$> ideclAs decl
      , importHiding    = second (map unLIE) <$> ideclHiding decl
      }

    -- TODO: This is lossy. We might want a more accurate data type.
    unLIE :: LIE RdrName -> String
    unLIE (L _ name) = GHC.showSDoc (GHC.ppr name)

    eltsToAutocompleteMap :: IORef (IntMap IdInfo) -> GlobalRdrElt -> IO IdInfo
    eltsToAutocompleteMap idInfoCacheRef elt = do
      cache <- readIORef idInfoCacheRef
      let name = gre_name elt
      case IM.lookup (Unique.getKey $ Unique.getUnique name) cache of
        Nothing -> do
          let idInfo = fromJust $ idInfoForName dflags name False (Just elt)
              ncache =
                IM.insert (Unique.getKey $ Unique.getUnique name) idInfo cache
          writeIORef idInfoCacheRef ncache
          return idInfo
        Just idInfo -> return idInfo

    autoEnvs :: ModSummary -> IO [GlobalRdrElt]
    autoEnvs ModSummary{ ms_mod
                               , ms_hsc_src
                               , ms_srcimps
                               , ms_textual_imps
                               } = do
      let go :: RnM (a, GlobalRdrEnv, b, c) -> IO [GlobalRdrElt]
          go op = do
            ((_warns, errs), res) <- initTc session ms_hsc_src False ms_mod op
            case res of
              Nothing -> do
                -- TODO: deal with import errors
#if DEBUG
                appendFile "/tmp/ghc.importerrors" $ show
                                                   . map GHC.showSDoc
                                                   $ ErrUtils.pprErrMsgBag errs
#endif
                return []
              Just (_, elts, _, _) ->
                return . concat $ occEnvElts elts
      env1 <- go $ rnImports ms_srcimps
      env2 <- go $ rnImports ms_textual_imps
      return $ env1 ++ env2

-- | Run a snippet.
runInGhc :: (ModuleName, String)  -- ^ module and function to execute
         -> RunBufferMode         -- ^ Buffer mode for stdout
         -> RunBufferMode         -- ^ Buffer mode for stderr
         -> Ghc RunResult
runInGhc (m, fun) outBMode errBMode = do
  flags <- getSessionDynFlags
  -- Half of a workaround for http://hackage.haskell.org/trac/ghc/ticket/7456.
  -- Set GHC verbosity to avoid stray GHC messages, e.g., from the linker.
  setSessionDynFlags (flags { verbosity = 0 })
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
      runRes <- runStmt expr RunToCompletion
      case runRes of
        GHC.RunOk [name] ->
          -- TODO: ignore @name@; this was only useful for debug
          return $ RunOk $ showSDocDebug flags (GHC.ppr name)
        GHC.RunOk _ ->
          error "checkModule: unexpected names in RunOk"
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

collectSrcError :: IORef [SourceError]
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

collectSrcError' :: IORef [SourceError]
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
  = let msgstr = showSDocForUser flags (qualName style,qualModule style) msg
        sp = extractSourceSpan srcspan
     in modifyIORef errsRef (SourceError errKind sp msgstr :)

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
fromHscSourceError :: HscTypes.SourceError -> SourceError
fromHscSourceError e = case bagToList (HscTypes.srcErrorMessages e) of
  [errMsg] -> case ErrUtils.errMsgSpans errMsg of
    [real@RealSrcSpan{}] ->
      SourceError
        KindError (extractSourceSpan real) (show e)
    _ -> SourceError KindError (TextSpan "<no location info>") (show e)
  _ -> SourceError
         KindError (TextSpan "<no location info>") (show e)

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
