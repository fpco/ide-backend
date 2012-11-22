{-# LANGUAGE CPP #-}
-- Copyright   : (c) JP Moresmau 2011,
--                   Well-Typed 2012
-- (JP Moresmau's buildwrapper package used as template for GHC API use)
--
-- | Implementation details of the calls to GHC that compute information
-- based on source files and provides progress information.
-- Only this file should import the GHC-internals modules.
module GhcRun
  ( DynamicOpts
  , submitStaticOpts
  , optsToDynFlags
  , checkModule
  ) where

import GHC hiding (flags, ModuleName)
import qualified Config as GHC
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
import Control.Monad (void)

import Common

newtype DynamicOpts = DynamicOpts [Located String]

-- | Set static flags at server startup and return dynamic flags.
submitStaticOpts :: [String] -> IO DynamicOpts
submitStaticOpts opts = do
  (dynFlags, _) <- parseStaticFlags (map noLoc opts)
  return $ DynamicOpts dynFlags

optsToDynFlags :: [String] -> DynamicOpts
optsToDynFlags = DynamicOpts . map noLoc

checkModule :: [FilePath]        -- ^ target files
            -> DynamicOpts       -- ^ dynamic flags for this run of runGhc
            -> Maybe String      -- ^ name of the function to run, if any
            -> Int               -- ^ verbosity level
            -> (String -> IO ()) -- ^ handler for each SevOutput message
            -> (String -> IO ()) -- ^ handler for remaining non-error messages
            -> IO [SourceError]  -- ^ any errors and warnings
checkModule targets (DynamicOpts dynOpts) funToRun verbosity
            handlerOutput handlerRemaining = do
  errsRef <- newIORef []
  let collectedErrors = reverse <$> readIORef errsRef
      handleOtherErrors =
        Ex.handle $ \e -> do
          let exError = OtherError (show (e :: Ex.SomeException))
          (++ [exError]) <$> collectedErrors
      (hscTarget, ghcLink, runOrNot) = case funToRun of
        Nothing  -> (HscNothing,     NoLink,
                     return ())
        Just fun -> (HscInterpreted, LinkInMemory,
                     void $ runStmt fun RunToCompletion)
  handleOtherErrors $ do

    libdir <- getGhcLibdir

    runGhc (Just libdir) $
      handleSourceError printException $ do

      flags0 <- getSessionDynFlags
      (flags, _, _) <- parseDynamicFlags flags0 dynOpts

      defaultCleanupHandler flags $ do
        setSessionDynFlags flags {
                             hscTarget,
                             ghcLink,
                             ghcMode    = CompManager,
#if __GLASGOW_HASKELL__ >= 706
                             log_action = collectSrcError errsRef handlerOutput handlerRemaining,
#else
                             log_action = collectSrcError errsRef handlerOutput handlerRemaining flags,
#endif
                             verbosity
                           }
        let addSingle filename = do
              addTarget Target
                { targetId           = TargetFile filename Nothing
                , targetAllowObjCode = True
                , targetContents     = Nothing
                }
        mapM_ addSingle targets
        load LoadAllTargets
        runOrNot

    collectedErrors

getGhcLibdir :: IO FilePath
getGhcLibdir = do
  let ghcbinary = "ghc-" ++ GHC.cProjectVersion
  out <- readProcess ghcbinary ["--print-libdir"] ""
  case lines out of
    [libdir] -> return libdir
    _        -> fail "cannot parse output of ghc --print-libdir"

-- Put into the log_action field for extra debugging. Also, set verbosity to 3.
_collectSrcError_debug :: IORef [SourceError]
                       -> (String -> IO ())
                       -> (String -> IO ())
                       -> DynFlags
                       -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()
_collectSrcError_debug errsRef handlerOutput handlerRemaining flags severity srcspan style msg = do
  let showSeverity SevOutput  = "SevOutput"
#if __GLASGOW_HASKELL__ >= 706
      showSeverity SevDump    = "SevDump"
#endif
      showSeverity SevInfo    = "SevInfo"
      showSeverity SevWarning = "SevWarning"
      showSeverity SevError   = "SevError"
      showSeverity SevFatal   = "SevFatal"
  appendFile "log_debug"
    $  "Severity: "   ++ showSeverity severity
    ++ "  SrcSpan: "  ++ show srcspan
--    ++ "  PprStyle: " ++ show style
    ++ "  MsgDoc: "   ++ showSDocForUser flags (qualName style,qualModule style) msg
    ++ "\n"
  collectSrcError
    errsRef handlerOutput handlerRemaining flags severity srcspan style msg

collectSrcError :: IORef [SourceError]
                -> (String -> IO ())
                -> (String -> IO ())
                -> DynFlags
                -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()
collectSrcError errsRef _ _ flags severity srcspan style msg
  | Just errKind <- case severity of
                      SevWarning -> Just KindWarning
                      SevError   -> Just KindError
                      SevFatal   -> Just KindError
                      _          -> Nothing
  , Just (file, st, end) <- extractErrSpan srcspan
  = let msgstr = showSDocForUser flags (qualName style,qualModule style) msg
     in modifyIORef errsRef (SrcError errKind file st end msgstr:)

collectSrcError errsRef _ _ flags SevError _srcspan style msg
  = let msgstr = showSDocForUser flags (qualName style,qualModule style) msg
     in modifyIORef errsRef (OtherError msgstr:)

collectSrcError _errsRef handlerOutput _ flags SevOutput _srcspan style msg
  = let msgstr = showSDocForUser flags (qualName style,qualModule style) msg
     in handlerOutput msgstr

collectSrcError _errsRef _ handlerRemaining flags _severity _srcspan style msg
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

extractErrSpan :: SrcSpan -> Maybe (FilePath, (Int, Int), (Int, Int))
extractErrSpan (RealSrcSpan srcspan) =
  Just (unpackFS (srcSpanFile srcspan)
       ,(srcSpanStartLine srcspan, srcSpanStartCol srcspan)
       ,(srcSpanEndLine   srcspan, srcSpanEndCol   srcspan))
extractErrSpan _ = Nothing
