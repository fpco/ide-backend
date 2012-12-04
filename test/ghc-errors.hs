module Main (main) where

import Control.Concurrent (forkIO, newChan, readChan, writeChan)

import GHC (
    Ghc
  , runGhc
  , handleSourceError
  , getSessionDynFlags
  , Severity(SevOutput)
  , SrcSpan
  , Severity
  , LoadHowMuch(LoadAllTargets)
  , load
  , removeTarget
  , TargetId(TargetFile)
  , Target(Target)
  , targetId
  , addTarget
  , setSessionDynFlags
  , verbosity
  , log_action
  , targetContents
  , targetAllowObjCode
  , getTargets
  , defaultCleanupHandler
  , GhcMode(CompManager)
  , GhcLink(NoLink)
  , hscTarget
  , HscTarget(HscNothing)
  , ghcLink
  , ghcMode
  )
import qualified Config as GHC
import GhcMonad (liftIO)
import ErrUtils   ( Message )
import Outputable ( PprStyle, qualName, qualModule )
import qualified Outputable as GHC

import System.Process (readProcess)
import Data.List ((\\))


--------------------------------------------------------------------------------

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

compileInGhc :: [FilePath]          -- ^ Targets
             -> (String -> IO ())   -- ^ handler for each SevOutput message
             -> Ghc ()
compileInGhc targets handlerOutput =
    -- Determine files to process.
    handleSourceError (\_ -> error "uh oh") $ do
      -- Compute new GHC flags.
      flags0 <- getSessionDynFlags
      let flags = flags0 {
                           hscTarget  = HscNothing,
                           ghcLink    = NoLink,
                           ghcMode    = CompManager,
                           verbosity  = 1,
                           log_action = collectSrcError handlerOutput
                         }
      defaultCleanupHandler flags $ do
        -- Set up the GHC flags.
        setSessionDynFlags flags
        -- Set up targets.
        oldTargets <- getTargets
        let targetIdFromFile file = TargetFile file Nothing
            addSingle filename =
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
        mapM_ (removeTarget . targetIdFromFile) $ oldFiles \\ targets
        -- Load modules to typecheck and perhaps generate code, too.
        _loadRes <- load LoadAllTargets
        return ()

collectSrcError :: (String -> IO ())
                -> Severity -> SrcSpan -> PprStyle -> Message -> IO ()
collectSrcError handlerOutput SevOutput _srcspan style msg
  = handlerOutput $ GHC.showSDocForUser (qualName style,qualModule style) msg
collectSrcError _ _ _ _ _
  = return ()

forkGhc :: IO ([FilePath] -> IO ())
forkGhc = do
  req  <- newChan
  resp <- newChan

  let dispatcher n = do
        targets <- liftIO $ readChan req
        response <- compileInGhc targets $ \msg -> print (n, msg)
        liftIO $ writeChan resp response
        dispatcher (n + 1)

  forkIO . runFromGhc $ dispatcher (0 :: Int)
  return (\targets -> writeChan req targets >> readChan resp)

main :: IO ()
main =  do
  -- Init session.
  callGhc <- forkGhc

  -- Test the computations.
  putStrLn "----- 1 ------"
  callGhc ["test/AerrorB/A.hs", "test/AerrorB/B.hs"]

  putStrLn "----- 2 ------"
  callGhc ["test/AerrorB/A.hs", "test/AerrorB/B.hs"]

  putStrLn "----- 3 ------"
