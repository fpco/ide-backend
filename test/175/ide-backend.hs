{-# LANGUAGE CPP #-}
module Main (main) where

import Control.Concurrent

import GHC hiding (flags, ModuleName)
import qualified GHC as GHCGHC
import qualified Config as GHC

import System.Process
import GhcMonad (liftIO)

import Numeric.LinearAlgebra

main2 = let s = show $ randomVector 77777 Uniform 10 in print ("asasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasdfasfdfasdf1" ++ (take 1600 (drop 1 s)))

-- A sample program using the library. Naively, separately, type-checks
-- all programs in the given directory and prints out the list of errors
-- in JSON format.

main :: IO ()
main = do
--  main2
  mv <- newEmptyMVar
  let actionMv = do
--        a <- main2
        a <- checkModule
        putMVar mv a
  forkOS actionMv
--  forkIO actionMv
--  actionMv
  takeMVar mv
  return ()


checkModule :: IO ()
checkModule = do -- handleOtherErrors $ do

    libdir <- getGhcLibdir

    runGhc (Just libdir) $ do
#if __GLASGOW_HASKELL__ >= 706
      handleSourceError printException $ do
#else
 --     handleSourceError printExceptionAndWarnings $ do
#endif

      flags <- getSessionDynFlags
--      (flags, _, _) <- parseDynamicFlags flags0 leftoverOpts

      defaultCleanupHandler flags $ do
        setSessionDynFlags flags {
                             hscTarget  = HscInterpreted,
                             ghcLink    = LinkInMemory,
                             verbosity = 1,
                             ghcMode    = CompManager
--                             ghcMode    = OneShot, --CompManager,
--                             log_action = collectSrcError errsRef
                           }
        addTarget Target {
                    targetId           = TargetFile "/mikolaj/r/ide-backend/test/175/test7.hs" Nothing,
                    targetAllowObjCode = True, -- False doesn't matter
                    targetContents     = Nothing
                  }
        load LoadAllTargets
        setContext $ [IIDecl $ simpleImportDecl $ mkModuleName "Main"]
        GHCGHC.runStmt "main" GHCGHC.RunToCompletion
        liftIO (print "OK")

    return ()

getGhcLibdir :: IO FilePath
getGhcLibdir = do
  let ghcbinary = "ghc-" ++ GHC.cProjectVersion
  out <- readProcess ghcbinary ["--print-libdir"] ""
  case lines out of
    [libdir] -> return libdir
    _        -> fail "cannot parse output of ghc --print-libdir"
