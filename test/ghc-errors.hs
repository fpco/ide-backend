module Main (main) where

import System.Unix.Directory (withTemporaryDirectory)
import System.FilePath ((</>))

-- getExecutablePath is in base only for >= 4.6
import qualified Control.Exception as Ex
import Data.IORef
import Control.Applicative

import GHC hiding (flags, ModuleName, RunResult)
import qualified Config as GHC
import GhcMonad (liftIO)
import ErrUtils   ( Message )
import Outputable ( PprStyle, qualName, qualModule )
import qualified Outputable as GHC
import FastString ( unpackFS )

import System.Process
import System.Directory
import System.FilePath (takeExtension)
import Data.List ((\\))

import RpcServer
import Common

--------------------------------------------------------------------------------


newtype DynamicOpts = DynamicOpts [Located String]

-- | Set static flags at server startup and return dynamic flags.
submitStaticOpts :: [String] -> IO DynamicOpts
submitStaticOpts opts = do
  (dynFlags, _) <- parseStaticFlags (map noLoc opts)
  return $ DynamicOpts dynFlags

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
                           log_action = collectSrcError errsRef handlerOutput handlerRemaining flags
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

collectSrcError :: IORef [SourceError]
                -> (String -> IO ())
                -> (String -> IO ())
                -> DynFlags
                -> Severity -> SrcSpan -> PprStyle -> MsgDoc -> IO ()
collectSrcError errsRef handlerOutput handlerRemaining flags
                severity srcspan style msg = do
  -- Prepare debug prints.
  let showSeverity SevOutput  = "SevOutput"
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

type MsgDoc = Message

showSDocForUser :: DynFlags -> PrintUnqualified -> MsgDoc -> String
showSDocForUser _flags uqual msg = GHC.showSDocForUser       uqual msg

showSDocDebug :: DynFlags -> MsgDoc -> String
showSDocDebug _flags msg = GHC.showSDocDebug        msg

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


--------------------------------------------------------------------------------


type GhcRequest = ()
data GhcResponse = RespWorking PCounter | RespDone RunOutcome
  deriving Show

-- Keeps the dynamic portion of the options specified at server startup
-- (they are among the options listed in SessionConfig).
-- They are only fed to GHC if no options are set via a session update command.
data GhcInitData = GhcInitData { dOpts :: DynamicOpts
                               , errsRef :: IORef [SourceError]
                               }

-- * Server-side operations

-- TODO: Do we want to return partial error information while it's
-- generated by runGHC, e.g., warnings? We could either try to run checkModule
-- file by file (do depanalSource and then DFS over the resulting graph,
-- doing \ m -> load (LoadUpTo m)) or rewrite collectSrcError to place
-- warnings in an mvar instead of IORef and read from it into Progress,
-- as soon as they appear.
-- | This function runs in end endless loop, most of which takes place
-- inside the @Ghc@ monad, making incremental compilation possible.
ghcServerEngine :: FilePath -> RpcServerActions GhcRequest GhcResponse GhcResponse -> IO ()
ghcServerEngine configSourcesDir RpcServerActions{..} = do
  -- Submit static opts and get back leftover dynamic opts.
  dOpts <- submitStaticOpts []
  -- Init error collection and define the exception handler.
  errsRef <- newIORef []
  let handleOtherErrors =
        Ex.handle $ \e -> do
          debug dVerbosity $ "handleOtherErrors: " ++ showExWithClass e
          let exError = OtherError (show (e :: Ex.SomeException))
          -- In case of an exception, don't lose saved errors.
          errs <- reverse <$> readIORef errsRef
          -- Don't disrupt the communication.
          putResponse $ RespDone (errs ++ [exError], Nothing)
          -- Restart the Ghc session.
          startGhcSession
      startGhcSession =
        handleOtherErrors $ runFromGhc $ dispatcher GhcInitData{..}

  startGhcSession

 where
  dispatcher :: GhcInitData -> Ghc ()
  dispatcher ghcInitData = do
    req <- liftIO $ getRequest
    resp <- ghcServerHandler configSourcesDir ghcInitData req
    liftIO $ putResponse resp
    dispatcher ghcInitData

ghcServerHandler :: FilePath -> GhcInitData -> GhcRequest -> Ghc GhcResponse
ghcServerHandler configSourcesDir GhcInitData{dOpts, errsRef} () = do
  -- Setup progress counter. It goes from [1/n] onwards.
  counterIORef <- liftIO $ newIORef (1 :: Int)
  let -- Let GHC API print "compiling M ... done." for each module.
      verbosity = 1
      -- TODO: verify that _ is the "compiling M" message
      handlerOutput msg = do
        oldCounter <- readIORef counterIORef
        putStrLn $ "~~~~~~ " ++ show oldCounter ++ ": " ++ msg
        modifyIORef counterIORef (+1)
      handlerRemaining _ = return ()  -- TODO: put into logs somewhere?
  errs <- compileInGhc configSourcesDir dOpts
                       False verbosity
                       errsRef handlerOutput handlerRemaining
  liftIO $ debug dVerbosity "returned from compileInGhc"
  return (RespDone (errs, Nothing))

--------------------------------------------------------------------------------

check :: FilePath -> IO ()
check configSourcesDir = do
    -- Init session.
    ideGhcServer <- forkRpcServer (ghcServerEngine configSourcesDir)

    -- Test the computations.
    putStrLn "----- 1 ------"
    copyFile "test/AerrorB/B.hs" (configSourcesDir </> "B.hs")
    copyFile "test/AerrorB/A.hs" (configSourcesDir </> "A.hs")
    _ <- rpc ideGhcServer ()

    putStrLn "----- 2 ------"
    _ <- rpc ideGhcServer ()

    putStrLn "----- 3 ------"

main :: IO ()
main = withTemporaryDirectory "ide-backend-test" check

