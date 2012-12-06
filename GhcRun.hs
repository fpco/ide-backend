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
  , DynamicOpts
  , submitStaticOpts
  , optsToDynFlags
  , runFromGhc
  , compileInGhc
  , runInGhc
  , checkModuleInProcess
  ) where

import GHC hiding (flags, ModuleName, RunResult)
import qualified Config as GHC
import GhcMonad (liftIO)
#if __GLASGOW_HASKELL__ >= 706
import ErrUtils   ( MsgDoc )
#else
import ErrUtils   ( Message )
#endif
import Outputable ( PprStyle, qualName, qualModule )
import qualified Outputable as GHC
import FastString ( unpackFS )

import System.Process
import Data.IORef
import Control.Applicative
import qualified Control.Exception as Ex
import System.Directory
import System.FilePath ((</>), takeExtension)
import Data.List ((\\))

import Common

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

compileInGhc :: FilePath            -- ^ target directory
             -> DynamicOpts         -- ^ dynamic flags for this run of runGhc
             -> Bool                -- ^ whether to generate code
             -> Int                 -- ^ verbosity level
             -> IORef [SourceError] -- ^ the IORef where GHC stores errors
             -> (String -> IO ())   -- ^ handler for each SevOutput message
             -> (String -> IO ())   -- ^ handler for remaining non-error msgs
             -> Ghc [SourceError]
compileInGhc configSourcesDir (DynamicOpts dynOpts)
             generateCode verbosity
             errsRef handlerOutput handlerRemaining = do
    -- Reset errors storage.
    liftIO $ writeIORef errsRef []
    -- Determine files to process.
    cnts <- liftIO $ getDirectoryContents configSourcesDir
    let targets = map (configSourcesDir </>)
                  $ filter ((`elem` hsExtentions) . takeExtension) cnts
    handleSourceError (\ e -> do
                          errs <- liftIO $ reverse <$> readIORef errsRef
                          return (errs ++ [OtherError (show e)])) $ do
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
      defaultCleanupHandler flags $ do
        -- Set up the GHC flags.
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
        debugPpContext flags "context after LoadAllTargets"
        -- TODO: record the boolean 'succeeded loadRes' below:
        -- Recover all saved errors.
        liftIO $ reverse <$> readIORef errsRef

runInGhc :: (String, String)    -- ^ module and function to run, if any
         -> IORef [SourceError] -- ^ the IORef where GHC stores errors
         -> Ghc RunOutcome
runInGhc (m, fun) errsRef = do
    -- Reset errors storage. TODO: optionally we refuse resetting the storage
    -- and keep here errors from the last typechecking, as an explanation
    -- of run failures, if any.
    liftIO $ writeIORef errsRef []
    handleSourceError (\ e -> do
                          errs <- liftIO $ reverse <$> readIORef errsRef
                          return (errs ++ [OtherError (show e)], Nothing)) $ do
      flags <- getSessionDynFlags
      -- TODO: not sure if this cleanup handler is needed:
      defaultCleanupHandler flags $ do
        debugPpContext flags "context before setContext"
        -- Run code.
        setContext $ [ IIDecl $ simpleImportDecl $ mkModuleName m
                     , IIDecl $ simpleImportDecl $ mkModuleName "System.IO"
                     ]
        debugPpContext flags "context after setContext"
        let expr = "do " ++ fun
                   ++ "; System.IO.hFlush System.IO.stdout"
                   ++ "; System.IO.hFlush System.IO.stderr"
        runRes <- runStmt expr RunToCompletion
        let resOrEx = case runRes of
              RunOk [name] ->
                let ident = showSDocDebug flags (GHC.ppr name)
                in Just $ Left ident
              RunOk _ -> error "checkModule: unexpected names in RunOk"
              RunException ex ->
                let exDesc = showExWithClass ex
                in Just $ Right exDesc
              RunBreak{} -> error "checkModule: RunBreak"
        -- Recover all saved errors.
        errs <- liftIO $ reverse <$> readIORef errsRef
        return (errs, resOrEx)

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

debugPpContext :: DynFlags -> String -> Ghc ()
debugPpContext flags msg = do
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
                     -> IO RunOutcome
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
          return $ (errs ++ [exError], Nothing)
  -- Catch all errors.
  handleOtherErrors $ do
    libdir <- getGhcLibdir
    -- Call the GHC API.
    runGhc (Just libdir) $ do
        errs <- compileInGhc configSourcesDir dynOpts
                             ideGenerateCode verbosity
                             errsRef handlerOutput handlerRemaining
        case funToRun of
          Just mfun -> runInGhc mfun errsRef
          Nothing -> return (errs, Nothing)
