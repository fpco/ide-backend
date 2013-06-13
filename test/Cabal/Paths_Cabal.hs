module Paths_Cabal (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [1,17,0], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/mikolaj/.cabal/bin"
libdir     = "/mikolaj/.cabal/lib/x86_64-linux-ghc-7.4.2/Cabal-1.17.0"
datadir    = "/mikolaj/.cabal/share/x86_64-linux-ghc-7.4.2/Cabal-1.17.0"
libexecdir = "/mikolaj/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "Cabal_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Cabal_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Cabal_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Cabal_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
