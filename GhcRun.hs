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
import Outputable ( PprStyle, qualName, qualModule )
import qualified Outputable as GHC 
import FastString ( unpackFS )

import System.Process
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
            -> LeftoverOpts      -- ^ leftover ghc static options
            -> IO ()             -- ^ handler for each "compiling M" message
            -> IO [SourceError]  -- ^ any errors and warnings
checkModule targets (LeftoverOpts leftoverOpts) handler = do
  errsRef <- newIORef []
  let collectedErrors = reverse <$> readIORef errsRef
  let handleOtherErrors =
        Ex.handle $ \e -> do
          let exError = OtherError (show (e :: Ex.SomeException))
          (++ [exError]) <$> collectedErrors
  handleOtherErrors $ do

    libdir <- getGhcLibdir

    runGhc (Just libdir) $
      handleSourceError printException $ do

      flags0 <- getSessionDynFlags
      (flags, _, _) <- parseDynamicFlags flags0 leftoverOpts

      defaultCleanupHandler flags $ do
        setSessionDynFlags flags {
                             hscTarget  = HscNothing,
                             ghcLink    = NoLink,
                             ghcMode    = CompManager,
#if __GLASGOW_HASKELL__ >= 706
                             log_action = collectSrcError errsRef handler,
#else
                             log_action = collectSrcError errsRef handler flags,
#endif
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

    collectedErrors

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
  collectSrcError errsRef handler flags severity srcspan style msg

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
