{-# LANGUAGE CPP #-}
-- Copyright   : (c) JP Moresmau 2011,
--                   Well-Typed 2012
-- (JP Moresmau's buildwrapper package used as template for GHC API use)
--
-- | Implementation details of the calls to GHC that compute information
-- based on source files and provides progress information.
-- Only this file should import the GHC-internals modules.
module GhcRun
  ( LeftoverOpts
  , submitOpts
  , checkModule
  ) where

import GHC hiding (flags, ModuleName)
import qualified Config as GHC
#if __GLASGOW_HASKELL__ >= 706
import ErrUtils   ( MsgDoc )
#else
import ErrUtils   ( Message )
#endif
import Outputable ( PprStyle, showSDocForUser, qualName, qualModule )
import FastString ( unpackFS )
import StringBuffer ( stringToStringBuffer )

import System.Process
#if __GLASGOW_HASKELL__ >= 706
import Data.Time
#else
import System.Time
#endif
import Data.IORef
import Control.Applicative
import qualified Control.Exception as Ex

import Common

newtype LeftoverOpts = LeftoverOpts [Located String]

submitOpts :: [String] -> IO LeftoverOpts
submitOpts opts = do
  (ghcState, _) <- parseStaticFlags (map noLoc opts)
  return $ LeftoverOpts ghcState

checkModule :: [FilePath]        -- ^ target files
            -> Maybe String      -- ^ optional content of the file
            -> LeftoverOpts      -- ^ leftover ghc static options
            -> IO ()             -- ^ handler for each "compiling M" message
            -> IO [SourceError]  -- ^ any errors and warnings
checkModule targets mfilecontent (LeftoverOpts leftoverOpts) handler =
  handleOtherErrors $ do

    libdir <- getGhcLibdir

    errsRef <- newIORef []

    _mcontent <- case mfilecontent of
                  Nothing          -> return Nothing
                  Just filecontent -> do
#if __GLASGOW_HASKELL__ >= 704
                    let strbuf = stringToStringBuffer filecontent
#else
                    strbuf <- stringToStringBuffer filecontent
#endif
#if __GLASGOW_HASKELL__ >= 706
                    strtime <- getCurrentTime
#else
                    strtime <- getClockTime
#endif
                    return (Just (strbuf, strtime))

    runGhc (Just libdir) $
#if __GLASGOW_HASKELL__ >= 706
      handleSourceError printException $ do
#else
      handleSourceError printExceptionAndWarnings $ do
#endif

      flags0 <- getSessionDynFlags
      (flags, _, _) <- parseDynamicFlags flags0 leftoverOpts

      defaultCleanupHandler flags $ do
        setSessionDynFlags flags {
                             hscTarget  = HscNothing,
                             ghcLink    = NoLink,
                             ghcMode    = CompManager,
                             log_action = collectSrcError errsRef handler,
                             -- print "compiling M ... done." for each module
                             verbosity  = 1
                           }
        let addSingle filename = do
              addTarget Target
                { targetId           = TargetFile filename Nothing
                , targetAllowObjCode = True
                , targetContents     = Nothing
                }
        mapM_ addSingle targets
        load LoadAllTargets
        return ()

    reverse <$> readIORef errsRef
  where
    handleOtherErrors =
      Ex.handle $ \e -> return [OtherError (show (e :: Ex.SomeException))]

getGhcLibdir :: IO FilePath
getGhcLibdir = do
  let ghcbinary = "ghc-" ++ GHC.cProjectVersion
  out <- readProcess ghcbinary ["--print-libdir"] ""
  case lines out of
    [libdir] -> return libdir
    _        -> fail "cannot parse output of ghc --print-libdir"

-- Put into the log_action field for extra debugging. Also, set verbosity to 3.
_collectSrcError_debug :: IORef [SourceError] -> IO ()
                       -> DynFlags
                       -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()
_collectSrcError_debug errsRef handler flags severity srcspan style msg = do
  let showSeverity SevOutput  = "SevOutput"
      showSeverity SevDump    = "SevDump"
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
  collectSrcError errsRef handler flags severity srcspan style msg

#if __GLASGOW_HASKELL__ >= 706
collectSrcError :: IORef [SourceError] -> IO ()
                -> DynFlags
                -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()
collectSrcError errsRef _handler flags severity srcspan style msg
  | Just errKind <- case severity of
                      SevWarning -> Just KindWarning
                      SevError   -> Just KindError
                      SevFatal   -> Just KindError
                      _          -> Nothing
  , Just (file, st, end) <- extractErrSpan srcspan
  = let msgstr = showSDocForUser flags (qualName style,qualModule style) msg
     in modifyIORef errsRef (SrcError errKind file st end msgstr:)

collectSrcError errsRef _handler flags SevError _srcspan style msg
  = let msgstr = showSDocForUser flags (qualName style,qualModule style) msg
     in modifyIORef errsRef (OtherError msgstr:)

collectSrcError _errsRef handler _flags SevOutput _srcspan _style _msg =
  -- TODO: verify that it's the "compiling M" message
  handler

collectSrcError _ _ _ _ _ _ _ = return ()
#else
collectSrcError :: IORef [SourceError] -> IO ()
                -> Severity -> SrcSpan -> PprStyle -> Message -> IO ()
collectSrcError errsRef _handler severity srcspan style msg
  | Just errKind <- case severity of
                      SevWarning -> Just KindWarning
                      SevError   -> Just KindError
                      SevFatal   -> Just KindError
                      _          -> Nothing
  , Just (file, st, end) <- extractErrSpan srcspan
  = let msgstr = showSDocForUser (qualName style,qualModule style) msg
     in modifyIORef errsRef (SrcError errKind file st end msgstr:)

collectSrcError errsRef _handler SevError _srcspan style msg
  = let msgstr = showSDocForUser (qualName style,qualModule style) msg
     in modifyIORef errsRef (OtherError msgstr:)

collectSrcError _errsRef handler SevOutput _srcspan _style _msg =
  -- TODO: verify that it's the "compiling M" message
  handler

collectSrcError _ _ _ _ _ _ = return ()
#endif

extractErrSpan :: SrcSpan -> Maybe (FilePath, (Int, Int), (Int, Int))
#if __GLASGOW_HASKELL__ >= 704
extractErrSpan (RealSrcSpan srcspan) =
#else
extractErrSpan srcspan | isGoodSrcSpan srcspan =
#endif
  Just (unpackFS (srcSpanFile srcspan)
       ,(srcSpanStartLine srcspan, srcSpanStartCol srcspan)
       ,(srcSpanEndLine   srcspan, srcSpanEndCol   srcspan))
extractErrSpan _ = Nothing
