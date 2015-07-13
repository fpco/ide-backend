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
import Foreign.C
import System.Posix.Internals
#endif

setFileTimes :: FilePath -> EpochTime -> EpochTime -> IO ()

#ifdef VERSION_unix

setFileTimes = Posix.setFileTimes

#else

-- Mostly copied from System.Posix.Files
setFileTimes name atime mtime =
  withCString name $ \s ->
   allocaBytes 16 $ \p -> do
     (\hsc_ptr -> pokeByteOff hsc_ptr 0)  p atime
     (\hsc_ptr -> pokeByteOff hsc_ptr 8) p mtime
     throwErrnoPathIfMinus1_ "setFileTimes" name (c__utime s p)

foreign import ccall unsafe "utime.h _utime"
   c__utime :: CString -> Ptr CUtimbuf -> IO CInt

#endif
