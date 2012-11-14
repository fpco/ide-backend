{-# LANGUAGE CPP #-}
-- Copyright   : (c) JP Moresmau 2011,
--                   Well-Typed 2012
-- (JP Moresmau's buildwrapper package used as template for GHC API use)
module GhcRun
  ( GhcState
  , optsToGhcState
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
import Control.Exception

import qualified Common as Common

type GhcState = [Located String]

optsToGhcState :: [String] -> IO GhcState
optsToGhcState opts = do
  (ghcState, _) <- parseStaticFlags (map noLoc opts)
  return ghcState

checkModule :: [FilePath]        -- ^ target files
            -> Maybe String      -- ^ optional content of the file
            -> [Located String]  -- ^ leftover ghc static options
            -> IO [Common.SourceError]  -- ^ any errors and warnings
checkModule targets mfilecontent leftoverOpts = handleOtherErrors $ do

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
                             log_action = collectSrcError errsRef
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
      handle $ \e -> return [Common.OtherError (show (e :: SomeException))]

getGhcLibdir :: IO FilePath
getGhcLibdir = do
  let ghcbinary = "ghc-" ++ GHC.cProjectVersion
  out <- readProcess ghcbinary ["--print-libdir"] ""
  case lines out of
    [libdir] -> return libdir
    _        -> fail "cannot parse output of ghc --print-libdir"

#if __GLASGOW_HASKELL__ >= 706
collectSrcError :: IORef [Common.SourceError]
                -> DynFlags
                -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()
collectSrcError errsRef flags severity srcspan style msg
  | Just errKind <- case severity of
                      SevWarning -> Just Common.Warning
                      SevError   -> Just Common.Error
                      SevFatal   -> Just Common.Error
                      _          -> Nothing
  , Just (file, st, end) <- extractErrSpan srcspan
  = let msgstr = showSDocForUser flags (qualName style,qualModule style) msg
     in modifyIORef errsRef (Common.SrcError errKind file st end msgstr:)

collectSrcError errsRef flags SevError _srcspan style msg
  = let msgstr = showSDocForUser flags (qualName style,qualModule style) msg
     in modifyIORef errsRef (Common.OtherError msgstr:)

collectSrcError _ _ _ _ _ _ = return ()
#else
collectSrcError :: IORef [Common.SourceError]
                -> Severity -> SrcSpan -> PprStyle -> Message -> IO ()
collectSrcError errsRef severity srcspan style msg
  | Just errKind <- case severity of
                      SevWarning -> Just Common.Warning
                      SevError   -> Just Common.Error
                      SevFatal   -> Just Common.Error
                      _          -> Nothing
  , Just (file, st, end) <- extractErrSpan srcspan
  = let msgstr = showSDocForUser (qualName style,qualModule style) msg
     in modifyIORef errsRef (Common.SrcError errKind file st end msgstr:)

collectSrcError errsRef SevError _srcspan style msg
  = let msgstr = showSDocForUser (qualName style,qualModule style) msg
     in modifyIORef errsRef (Common.OtherError msgstr:)

collectSrcError _ _ _ _ _ = return ()
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
