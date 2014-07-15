module Paths_Cabal (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [1,18,1,3], versionTags = []}
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/Users/dev/.cabal/bin"
libdir     = "/Users/dev/.cabal/lib/x86_64-osx-ghc-7.8.2/Cabal-1.18.1.3"
datadir    = "/Users/dev/.cabal/share/x86_64-osx-ghc-7.8.2/Cabal-1.18.1.3"
libexecdir = "/Users/dev/.cabal/libexec"
sysconfdir = "/Users/dev/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Cabal_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Cabal_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "Cabal_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Cabal_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Cabal_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
