module Main (main) where

import System.Process (readProcess)
import Data.List ((\\))
import Control.Monad (void)

import GHC
import qualified Config as GHC
import qualified Outputable as GHC
import GhcMonad (liftIO)
import ErrUtils (Message)
import Outputable (PprStyle, qualName, qualModule)

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
compileInGhc targets handlerOutput = do
    -- Set flags
    flags0 <- getSessionDynFlags
    let flags = flags0 {verbosity = 1, log_action = collectSrcError handlerOutput}
    setSessionDynFlags flags
    -- Set up targets.
    oldTargets <- getTargets
    let oldFiles = map fileFromTarget oldTargets
    mapM_ addSingle (targets \\ oldFiles)
    mapM_ (removeTarget . targetIdFromFile) $ oldFiles \\ targets
    -- Load modules to typecheck
    void $ load LoadAllTargets
  where
    targetIdFromFile file = TargetFile file Nothing

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

collectSrcError :: (String -> IO ())
                -> Severity -> SrcSpan -> PprStyle -> Message -> IO ()
collectSrcError handlerOutput SevOutput _srcspan style msg
  = handlerOutput $ GHC.showSDocForUser (qualName style,qualModule style) msg
collectSrcError _ _ _ _ _
  = return ()

main :: IO ()
main = runFromGhc $ do
  liftIO $ putStrLn "----- 0 ------"
  compileInGhc ["A.hs", "B.hs"] $ \msg -> print (0 :: Int, msg)

  liftIO $ putStrLn "----- 1 ------"
  compileInGhc ["A.hs", "B.hs"] $ \msg -> print (1 :: Int, msg)

  liftIO $ putStrLn "----- 2 ------"
  compileInGhc ["C.hs"] $ \msg -> print (2 :: Int, msg)
