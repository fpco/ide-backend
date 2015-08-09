{-# LANGUAGE CPP #-}
module IdeSession.Util.PortableFiles (
    setFileTimes
  , getFileStatus
  , modificationTime
) where


import System.PosixCompat.Types
import System.PosixCompat.Files hiding (setFileTimes)

#ifdef VERSION_unix
import qualified System.Posix.Files as Posix
#else
import Foreign
import Data.Time.Clock.POSIX
import System.FilePath.Posix
import qualified System.Win32 as Win32
import Control.Exception (bracket)
#endif

setFileTimes :: FilePath -> EpochTime -> EpochTime -> IO ()

#ifdef VERSION_unix

setFileTimes = Posix.setFileTimes

#else

-- Unlike `utime`, which doesn't work for directories on Windows, this works for both for files and directories
-- Mostly copied from @System.Directory@ 1.2.3.0
setFileTimes path atime mtime =
  bracket (openFileHandle path' Win32.gENERIC_WRITE)
          Win32.closeHandle $ \ handle ->
    with (posixToWindowsTime atime) $ \ atime' ->
      with (posixToWindowsTime mtime) $ \ mtime' ->
        Win32.failIf_ not "setFileTimes" $
          Win32.c_SetFileTime handle nullPtr atime' mtime'
  where
    path'  = normalise path             -- handle empty paths
    windowsPosixEpochDifference :: Num a => a
    windowsPosixEpochDifference = 116444736000000000
    posixToWindowsTime :: EpochTime -> Win32.FILETIME
    posixToWindowsTime t = Win32.FILETIME $
      truncate ((realToFrac t :: POSIXTime) * 10000000 + windowsPosixEpochDifference)
    openFileHandle :: String -> Win32.AccessMode -> IO Win32.HANDLE
    openFileHandle p mode = Win32.createFile p mode share Nothing
                                             Win32.oPEN_EXISTING flags Nothing
      where share =  Win32.fILE_SHARE_DELETE
                 .|. Win32.fILE_SHARE_READ
                 .|. Win32.fILE_SHARE_WRITE
            flags =  Win32.fILE_ATTRIBUTE_NORMAL
                 .|. Win32.fILE_FLAG_BACKUP_SEMANTICS -- required for directories

#endif
