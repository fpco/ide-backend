{-# LANGUAGE CPP #-}
-- Copyright   : (c) JP Moresmau 2011,
--                   Well-Typed 2012
-- (JP Moresmau's buildwrapper package used as template for GHC API use)
--
-- | Implementation details of the calls to GHC that compute information
-- based on source files and provides progress information.
-- Only this file should import the GHC-internals modules.
module GhcRun
  ( Ghc
  , liftIO
  , ghandle
  , ghandleJust
  , GhcException
  , DynamicOpts
  , submitStaticOpts
  , optsToDynFlags
  , runFromGhc
  , compileInGhc
  , runInGhc
  , checkModuleInProcess
  ) where

import qualified Config as GHC
import DynFlags (dopt_unset)
import Exception (ghandle)
import FastString ( unpackFS )
import qualified GHC
import GHC hiding (flags, ModuleName, RunResult(..))
import GhcMonad (liftIO, modifySession)
import HscTypes (HscEnv(hsc_mod_graph))
import qualified HscTypes
import Outputable ( PprStyle, qualName, qualModule )
import qualified Outputable as GHC
#if __GLASGOW_HASKELL__ >= 706
import ErrUtils   ( MsgDoc )
#else
import ErrUtils   ( Message )
#endif

import Control.Applicative
import qualified Control.Exception as Ex
import Control.Monad (filterM, liftM)
import Data.IORef
import Data.List ((\\))
import System.FilePath.Find (find, always, extension)
import System.Process
#if __GLASGOW_HASKELL__ >= 706
import Data.Time
#else
import System.Time
#endif

import Common
import ModuleName (ModuleName, LoadedModules)
import qualified ModuleName as MN

import Bag (bagToList)
import qualified ErrUtils
import qualified SrcLoc

newtype DynamicOpts = DynamicOpts [Located String]

-- | Set static flags at server startup and return dynamic flags.
submitStaticOpts :: [String] -> IO DynamicOpts
submitStaticOpts opts = do
  (dynFlags, _) <- parseStaticFlags (map noLoc opts)
  return $ DynamicOpts dynFlags

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

#if __GLASGOW_HASKELL__ < 707
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
             -> Ghc ([SourceError], LoadedModules)
compileInGhc configSourcesDir (DynamicOpts dynOpts)
             generateCode verbosity
             errsRef handlerOutput handlerRemaining = do
    -- Reset errors storage.
    liftIO $ writeIORef errsRef []
    -- Determine files to process.
    targets <- liftIO $ find always
                             ((`elem` hsExtentions) `liftM` extension)
                             configSourcesDir
    handleErrors $ do
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
                   `dopt_unset` Opt_GhciSandbox
      defaultCleanupHandler flags $ do
        -- Set up the GHC flags.
#if __GLASGOW_HASKELL__ < 707
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
        _loadRes <- load LoadAllTargets
        -- Recover all saved errors.
        prepareResult
  where
    sourceErrorHandler :: HscTypes.SourceError -> Ghc ([SourceError], LoadedModules)
    sourceErrorHandler e = do
      liftIO $ debug dVerbosity $ "handleSourceError: " ++ show e
      (errs, context) <- prepareResult
      return (errs ++ [fromHscSourceError e], context)

    -- Some errors are reported as exceptions instead
    ghcExceptionHandler :: GhcException -> Ghc ([SourceError], LoadedModules)
    ghcExceptionHandler e = do
      let eText   = "Bar: " ++ show e
          exError = OtherError eText
      liftIO $ debug dVerbosity $ "handleOtherErrors: " ++ eText
      -- In case of an exception, don't lose saved errors.
      (errs, context) <- prepareResult
      return (errs ++ [exError], context)

    handleErrors :: Ghc ([SourceError], LoadedModules)
                 -> Ghc ([SourceError], LoadedModules)
    handleErrors = ghandle ghcExceptionHandler
                 . handleSourceError sourceErrorHandler

    prepareResult :: Ghc ([SourceError], LoadedModules)
    prepareResult = do
      errs <- liftIO $ readIORef errsRef
      graph <- getModuleGraph
      let moduleNames = map ms_mod_name graph
      loadedNames <- filterM isLoaded moduleNames
      let loadedModules = map (MN.fromString . moduleNameString) loadedNames
      return (reverse errs, loadedModules)

runInGhc :: (ModuleName, String)  -- ^ module and function to run, if any
         -> Ghc RunResult
runInGhc (m, fun) = do
  flags <- getSessionDynFlags
  -- TODO: not sure if this cleanup handler is needed:
  defaultCleanupHandler flags . handleErrors $ do
-- TODO: these debug statements break tests currently:
--    _debugPpContext flags "context before setContext"
    setContext $ [ IIDecl $ simpleImportDecl $ mkModuleName (MN.toString m)
                 , IIDecl $ simpleImportDecl $ mkModuleName "System.IO"
                 ]
--    _debugPpContext flags "context after setContext"
    handleErrors $ do
      runRes <- runStmt expr RunToCompletion
      case runRes of
        GHC.RunOk [name] ->
          -- TODO: is it really useful to report these names as strings?
          return $ RunOk $ showSDocDebug flags (GHC.ppr name)
        GHC.RunOk _ ->
          error "checkModule: unexpected names in RunOk"
        GHC.RunException ex ->
          return . RunProgException $ showExWithClass ex
        GHC.RunBreak{} ->
          error "checkModule: RunBreak"
  where
    expr :: String
    expr = "do"
        ++ " System.IO.hSetBuffering System.IO.stdout System.IO.NoBuffering"
        ++ "; System.IO.hSetBuffering System.IO.stderr System.IO.NoBuffering"
        ++ "; " ++ MN.toString m ++ "." ++ fun
        ++ "; System.IO.hFlush System.IO.stdout"
        ++ "; System.IO.hFlush System.IO.stderr"

    handleError :: Show a => a -> Ghc RunResult
    handleError = return . RunGhcException . show

    -- "such-and-such not in scope" is reported as a source error
    -- not sure when GhcExceptions are thrown (if at all)
    handleErrors :: Ghc RunResult -> Ghc RunResult
    handleErrors = handleSourceError handleError
                 . ghandle (handleError :: GhcException -> Ghc RunResult)

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
  , Just (file, st, end) <- extractErrSpan srcspan
  = let msgstr = showSDocForUser flags (qualName style,qualModule style) msg
     in modifyIORef errsRef (SrcError errKind file st end msgstr :)

collectSrcError' errsRef _ _ flags SevError _srcspan style msg
  = let msgstr = showSDocForUser flags (qualName style,qualModule style) msg
     in modifyIORef errsRef (OtherError msgstr :)

collectSrcError' _errsRef handlerOutput _ flags SevOutput _srcspan style msg
  = let msgstr = showSDocForUser flags (qualName style,qualModule style) msg
     in handlerOutput msgstr

collectSrcError' _errsRef _ handlerRemaining flags _severity _srcspan style msg
  = let msgstr = showSDocForUser flags (qualName style,qualModule style) msg
     in handlerRemaining msgstr

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

extractErrSpan :: SrcSpan -> Maybe (FilePath, (Int, Int), (Int, Int))
extractErrSpan (RealSrcSpan srcspan) =
  Just (unpackFS (srcSpanFile srcspan)
       ,(srcSpanStartLine srcspan, srcSpanStartCol srcspan)
       ,(srcSpanEndLine   srcspan, srcSpanEndCol   srcspan))
extractErrSpan _ = Nothing

_debugPpContext :: DynFlags -> String -> Ghc ()
_debugPpContext flags msg = do
  context <- getContext
  liftIO $ debug dVerbosity
    $ msg ++ ": " ++ showSDocDebug flags (GHC.ppr context)

-- Kept for in-process tests.
checkModuleInProcess :: FilePath     -- ^ target directory
                     -> DynamicOpts  -- ^ dynamic flags for this run of runGhc
                     -> Bool         -- ^ whether to generate code
                     -> Maybe (String, String)
                                     -- ^ module and function to run, if any
                     -> Int          -- ^ verbosity level
                     -> (String -> IO ())
                                     -- ^ handler for each SevOutput message
                     -> (String -> IO ())
                                     -- ^ handler for remaining non-error msgs
                     -> IO (Either [SourceError] RunResult)
                                     -- ^ errors, warnings and results, if any
checkModuleInProcess configSourcesDir dynOpts ideGenerateCode funToRun
                     verbosity handlerOutput handlerRemaining = do
  errsRef <- newIORef []
  let collectedErrors = reverse <$> readIORef errsRef
      handleOtherErrors =
        Ex.handle $ \e -> do
          debug dVerbosity $ "handleOtherErrors: " ++ showExWithClass e
          let exError = OtherError (show (e :: Ex.SomeException))
          -- In case of an exception, don't lose saved errors.
          errs <- collectedErrors
          return $ Left (errs ++ [exError])
  -- Catch all errors.
  handleOtherErrors $ do
    libdir <- getGhcLibdir
    -- Call the GHC API.
    runGhc (Just libdir) $ do
        (errs, _) <- compileInGhc configSourcesDir dynOpts
                                  ideGenerateCode verbosity
                                  errsRef handlerOutput handlerRemaining
        case funToRun of
          Just (m, fun) -> Right <$> runInGhc (MN.fromString m, fun)
          Nothing -> return (Left errs)

-- | Version of handleJust for use in the GHC monad
ghandleJust :: Ex.Exception e => (e -> Maybe b) -> (b -> Ghc a) -> Ghc a -> Ghc a
ghandleJust p handler a = ghandle handler' a
  where
    handler' e = case p e of
                   Nothing -> liftIO $ Ex.throwIO e
                   Just b  -> handler b

-- | Convert GHC's SourceError type into our
fromHscSourceError :: HscTypes.SourceError -> SourceError
fromHscSourceError e = case bagToList (HscTypes.srcErrorMessages e) of
  [errMsg] -> case ErrUtils.errMsgSpans errMsg of
    [RealSrcSpan sp] ->
      SrcError KindError
               (unpackFS (SrcLoc.srcSpanFile sp))
               (srcSpanStartLine sp, srcSpanStartCol sp)
               (srcSpanEndLine sp, srcSpanEndCol sp)
               (show e)
    _ -> OtherError (show e)
  _ -> OtherError (show e)
