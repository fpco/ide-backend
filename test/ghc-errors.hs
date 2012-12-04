module Main (main) where

import System.Unix.Directory (withTemporaryDirectory)

--------------------------------------------------------------------------------

import System.Directory
import System.FilePath ((</>))

import GhcServer

--------------------------------------------------------------------------------

check :: FilePath -> IO ()
check configSourcesDir = do
    -- Init session.
    ideGhcServer <- forkGhcServer ["-no-user-package-conf"]

    -- Test the computations.
    putStrLn "----- 1 ------"
    copyFile "test/AerrorB/B.hs" (configSourcesDir </> "B.hs")
    copyFile "test/AerrorB/A.hs" (configSourcesDir </> "A.hs")
    _ <- rpcGhcServer ideGhcServer $ ReqCompile configSourcesDir

    putStrLn "----- 2 ------"
    _ <- rpcGhcServer ideGhcServer $ ReqCompile configSourcesDir

    putStrLn "----- 3 ------"

main :: IO ()
main = withTemporaryDirectory "ide-backend-test" check

