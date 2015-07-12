{-# LANGUAGE CPP, RecordWildCards #-}
module IdeSession.Util.PortableIO where

import System.IO (Handle, IOMode(..))

#ifdef VERSION_unix
import System.Posix.Types (Fd)
import qualified System.Posix.IO as Posix
import System.Posix.Files
#else
import Foreign.C.Error (throwErrnoIfMinus1_, throwErrnoIfMinus1)
import Foreign.C.Types (CInt(..), CUInt(..))
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Storable (peek, peekElemOff)
import GHC.IO.FD (mkFD)
import GHC.IO.Device (IODeviceType(Stream))
import GHC.IO.Handle.FD (mkHandleFromFD)
import System.IO (openFile)

{- For the reimplementation of handleToFd from System.Posix.IO -}
import System.IO.Error
import GHC.IO.Handle.Internals
import GHC.IO.Handle.Types hiding (Handle)
import qualified GHC.IO.FD as FD
import GHC.IO.Exception
import Data.Typeable (cast)
#endif

#ifdef VERSION_unix
type FileDescriptor = Fd
#else
type FileDescriptor = CInt
#endif

createPipe :: IO (FileDescriptor, FileDescriptor)
fdToHandle :: FileDescriptor -> IOMode -> IO Handle
handleToFd :: Handle -> IO FileDescriptor
closeFd :: FileDescriptor -> IO ()

dup :: FileDescriptor -> IO FileDescriptor
dupTo :: FileDescriptor -> FileDescriptor -> IO FileDescriptor

-- Opens, and possibly creates, the given file for writing
openWritableFile :: FilePath -> IO FileDescriptor

stdInput, stdOutput, stdError :: FileDescriptor

#ifdef VERSION_unix

createPipe = Posix.createPipe
fdToHandle fd _ = Posix.fdToHandle fd
handleToFd = Posix.handleToFd
closeFd = Posix.closeFd

dup = Posix.dup
dupTo = Posix.dupTo

openWritableFile fp = Posix.openFd fp Posix.WriteOnly (Just mode) Posix.defaultFileFlags
  where
    mode = unionFileModes ownerReadMode ownerWriteMode

stdInput = Posix.stdInput
stdOutput = Posix.stdOutput
stdError = Posix.stdError

#else

createPipe = allocaArray 2 $ \pfds -> do
    throwErrnoIfMinus1_ "_pipe" $ c__pipe pfds 2 ({- _O_BINARY -} 32768)
    readfd <- peek pfds
    writefd <- peekElemOff pfds 1
    return (readfd, writefd)

fdToHandle fd mode = do
    (fd', deviceType) <- mkFD fd mode (Just (Stream, 0, 0)) False False
    mkHandleFromFD fd' deviceType "" mode False Nothing

closeFd = throwErrnoIfMinus1_ "_close" . c__close

dup = throwErrnoIfMinus1 "_dup" . c__dup

dupTo fd1 fd2 = throwErrnoIfMinus1 "_dup2" $ c__dup2 fd1 fd2

-- Previously this was a ccall to '_open' in 'io.h'
-- TODO could this implementation work for the Unix version?
openWritableFile fp = openFile fp WriteMode >>= handleToFd

stdInput = 0
stdOutput = 1
stdError = 2

{- Copied from System.Posix.IO -}
handleToFd h@(FileHandle _ m) =
  withHandle' "handleToFd" h m $ handleToFd' h
handleToFd h@(DuplexHandle _ r w) = do
  _ <- withHandle' "handleToFd" h r $ handleToFd' h
  withHandle' "handleToFd" h w $ handleToFd' h
  -- for a DuplexHandle, make sure we mark both sides as closed,
  -- otherwise a finalizer will come along later and close the other
  -- side. (#3914)

handleToFd' :: Handle -> Handle__ -> IO (Handle__, FileDescriptor)
handleToFd' h h_@Handle__{haType=_,..} =
  case cast haDevice of
    Nothing -> ioError (ioeSetErrorString (mkIOError IllegalOperation
                                           "handleToFd" (Just h) Nothing)
                        "handle is not a file descriptor")
    Just fd -> do
     -- converting a Handle into an Fd effectively means
     -- letting go of the Handle; it is put into a closed
     -- state as a result.
     flushWriteBuffer h_
     FD.release fd
     return (Handle__{haType=ClosedHandle,..}, FD.fdFD fd)

foreign import ccall "io.h _pipe" c__pipe ::
    Ptr CInt -> CUInt -> CInt -> IO CInt

foreign import ccall "io.h _close" c__close ::
    CInt -> IO CInt

foreign import ccall "io.h _dup" c__dup ::
    CInt -> IO CInt

foreign import ccall "io.h _dup2" c__dup2 ::
    CInt -> CInt -> IO CInt

#endif
