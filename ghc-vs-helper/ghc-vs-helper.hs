-- Copyright   : (c) JP Moresmau 2011,
--                   Well-Typed 2012

-- (JP Moresmau's buildwrapper package used as template for GHC API use)

{-# LANGUAGE CPP #-}

module Main where

import GHC hiding (flags)
import qualified Config as GHC
import ErrUtils   ( Message )
import Outputable ( PprStyle, showSDocForUser, qualName, qualModule )
import FastString ( unpackFS )
import StringBuffer ( stringToStringBuffer )

import Text.JSON as JSON

import System.Environment
import System.Process
import System.Time
import Data.IORef
import Control.Applicative
import Control.Exception

main :: IO ()
main = do
  args <- getArgs
  (target, mcontent) <-
    case args of
      [target] -> return (target, Nothing)
      [target, "--input"] -> do
        content <- getContents
        return (target, Just content)
      _ -> fail "usage: file.hs [--input]"

  errs <- checkModule target mcontent []
  putStrLn (formatErrorMessagesJSON errs)

checkModule :: FilePath          -- ^ target file
            -> Maybe String      -- ^ optional content of the file
            -> [String]          -- ^ any ghc options
            -> IO [ErrorMessage] -- ^ any errors and warnings
checkModule filename mfilecontent opts = handleOtherErrors $ do
  
    libdir <- getGhcLibdir
    
    errsRef <- newIORef []
    
    mcontent <- case mfilecontent of
                  Nothing          -> return Nothing
                  Just filecontent -> do
#if __GLASGOW_HASKELL__ >= 704
                    let strbuf = stringToStringBuffer filecontent
#else
                    strbuf <- stringToStringBuffer filecontent
#endif
                    strtime <- getClockTime
                    return (Just (strbuf, strtime))

    (leftoverOpts, _) <- parseStaticFlags (map noLoc opts)
    runGhc (Just libdir) $
      handleSourceError printExceptionAndWarnings $ do

      flags0 <- getSessionDynFlags
      (flags, _, _) <- parseDynamicFlags flags0 leftoverOpts

      defaultCleanupHandler flags $ do
        setSessionDynFlags flags {
                             hscTarget  = HscNothing,
                             ghcLink    = NoLink,
                             ghcMode    = CompManager,
                             log_action = collectSrcError errsRef
                           }
        addTarget Target {
                    targetId           = TargetFile filename Nothing,
                    targetAllowObjCode = True,
                    targetContents     = mcontent
                  }
        load LoadAllTargets
        return ()

    reverse <$> readIORef errsRef
  where
    handleOtherErrors =
      handle $ \e -> return [OtherError (show (e :: SomeException))]

getGhcLibdir :: IO FilePath
getGhcLibdir = do
  let ghcbinary = "ghc-" ++ GHC.cProjectVersion
  out <- readProcess ghcbinary ["--print-libdir"] ""
  case lines out of
    [libdir] -> return libdir
    _        -> fail "cannot parse output of ghc --print-libdir"


data ErrorMessage = SrcError   ErrorKind FilePath (Int, Int) (Int, Int) String
                  | OtherError String
  deriving Show
data ErrorKind    = Error | Warning
  deriving Show

collectSrcError :: IORef [ErrorMessage]
                -> Severity -> SrcSpan -> PprStyle -> Message -> IO ()
collectSrcError errsRef severity srcspan style msg
  | Just errKind <- case severity of
                      SevWarning -> Just Main.Warning
                      SevError   -> Just Main.Error
                      SevFatal   -> Just Main.Error
                      _          -> Nothing
  , Just (file, st, end) <- extractErrSpan srcspan
  = let msgstr = showSDocForUser (qualName style,qualModule style) msg
     in modifyIORef errsRef (SrcError errKind file st end msgstr:)

collectSrcError errsRef SevError _srcspan style msg
  = let msgstr = showSDocForUser (qualName style,qualModule style) msg
     in modifyIORef errsRef (OtherError msgstr:)

collectSrcError _ _ _ _ _ = return ()

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

formatErrorMessagesJSON :: [ErrorMessage] -> String
formatErrorMessagesJSON = JSON.encode . map errorMessageToJSON

errorMessageToJSON :: ErrorMessage -> JSValue
errorMessageToJSON (SrcError errKind file (stline, stcol)
                                          (endline, endcol) msgstr) =
  JSObject $
    toJSObject
      [ ("kind",      showJSON (toJSString (show errKind)))
      , ("file",      showJSON (toJSString file))
      , ("startline", showJSON stline)
      , ("startcol",  showJSON stcol)
      , ("endline",   showJSON endline)
      , ("endcol",    showJSON endcol)
      , ("message",   showJSON (toJSString msgstr))
      ]
errorMessageToJSON (OtherError msgstr) =
  JSObject $
    toJSObject
      [ ("kind",      showJSON (toJSString "message"))
      , ("message",   showJSON (toJSString msgstr))
      ]
