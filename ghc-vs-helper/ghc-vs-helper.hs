-- Copyright   : (c) JP Moresmau 2011,
--                   Well-Typed 2012

-- (JP Moresmau's buildwrapper package used as template for GHC API use)

{-# LANGUAGE CPP #-}

module Main where

import GHC hiding (flags)
import qualified Config as GHC
#if __GLASGOW_HASKELL__ >= 706
import ErrUtils   ( MsgDoc )
#else
import ErrUtils   ( Message )
#endif
import Outputable ( PprStyle, showSDocForUser, qualName, qualModule )
import FastString ( unpackFS )
import StringBuffer ( stringToStringBuffer )

import Text.JSON as JSON

import System.Environment
import System.Process
#if __GLASGOW_HASKELL__ >= 706
import Data.Time
#else
import System.Time
#endif
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import Data.IORef
import Control.Applicative
import Control.Exception
import System.Random (randomIO)
import System.FilePath (combine)

data SessionConfig = SessionConfig
  { configSourcesDir :: FilePath
  }

type State = Map ModuleName (FilePath, Maybe String)

type Handle = IORef State

data IdeSession = IdeSession SessionConfig Handle

data ModuleChange = PutModule ModuleName String

main :: IO ()
main = do
  args <- getArgs
  let configSourcesDir = case args of
        [dir] -> dir
        [] -> "."
        _ -> fail "usage: ghc-vs-helper [source-dir]"
  let sessionConfig = SessionConfig{..}
  -- Two sample scenarios:
  b <- randomIO
  if b
    then do
      session <- initSession sessionConfig
                             [(mkModuleName "Main", "ghc-vs-helper.hs")]
      updateModules session [PutModule (mkModuleName "Main") "wrong"]
      updateModules session [PutModule (mkModuleName "Main") "correct"]
      updateModules session [ PutModule (mkModuleName "Main") "wrong"
                            , PutModule (mkModuleName "Main") "correct" ]
      shutdown session
    else do
      session <- initSession sessionConfig
                             [(mkModuleName "Main", "ghc-vs-helper.hs")]
      shutdown session

initSession :: SessionConfig -> [(ModuleName, FilePath)] -> IO IdeSession
initSession sessionConfig ms = do
  h <- newIORef $ Map.fromList $ map (\ (m, p) -> (m, (p, Nothing))) ms
  return $ IdeSession sessionConfig h

shutdown :: IdeSession -> IO ()
shutdown (IdeSession (SessionConfig{configSourcesDir}) h) = do
  let checkSingle (_m, (p, mcontent)) = do
        let target = combine configSourcesDir p
        errs <- checkModule target mcontent []
        return $ formatErrorMessagesJSON errs
  state <- readIORef h
  allErrs <- mapM checkSingle (Map.toList state)
  putStrLn $ List.intercalate "\n\n" allErrs

updateModules :: IdeSession -> [ModuleChange] -> IO ()
updateModules _ [] =
  return ()
updateModules session@(IdeSession _ h) (PutModule m s : rest) = do
  state <- readIORef h
  writeIORef h $ Map.adjust (\ (p, _) -> (p, Just s)) m state
  updateModules session rest

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
#if __GLASGOW_HASKELL__ >= 706
                    strtime <- getCurrentTime
#else
                    strtime <- getClockTime
#endif
                    return (Just (strbuf, strtime))

    (leftoverOpts, _) <- parseStaticFlags (map noLoc opts)
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

#if __GLASGOW_HASKELL__ >= 706
collectSrcError :: IORef [ErrorMessage]
                -> DynFlags
                -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()
collectSrcError errsRef flags severity srcspan style msg
  | Just errKind <- case severity of
                      SevWarning -> Just Main.Warning
                      SevError   -> Just Main.Error
                      SevFatal   -> Just Main.Error
                      _          -> Nothing
  , Just (file, st, end) <- extractErrSpan srcspan
  = let msgstr = showSDocForUser flags (qualName style,qualModule style) msg
     in modifyIORef errsRef (SrcError errKind file st end msgstr:)

collectSrcError errsRef flags SevError _srcspan style msg
  = let msgstr = showSDocForUser flags (qualName style,qualModule style) msg
     in modifyIORef errsRef (OtherError msgstr:)

collectSrcError _ _ _ _ _ _ = return ()
#else
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
