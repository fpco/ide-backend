{-# LANGUAGE CPP #-}
module IdeSession.RPC.PortablePipes (
    createPipe
  , fdToHandle
  , closeFd
  , FileDescriptor
) where

import System.IO (Handle, IOMode)

#ifdef VERSION_unix
import System.Posix.Types (Fd)
import qualified System.Posix.IO as Posix
#else
import Foreign.C.Error (throwErrnoIfMinus1_)
import Foreign.C.Types (CInt(..), CUInt(..))
import Foreign.Ptr (Ptr)
import Foreign.Marshal.Array (allocaArray)
import Foreign.Storable (peek, peekElemOff)
import GHC.IO.FD (mkFD)
import GHC.IO.Device (IODeviceType(Stream))
import GHC.IO.Handle.FD (mkHandleFromFD)
#endif

#ifdef VERSION_unix
type FileDescriptor = Fd
#else
type FileDescriptor = CInt
#endif

createPipe :: IO (FileDescriptor, FileDescriptor)
fdToHandle :: FileDescriptor -> IOMode -> IO Handle
closeFd :: FileDescriptor -> IO ()

#if mingw32_HOST_OS
createPipe = do
    allocaArray 2 $ \ pfds -> do
        throwErrnoIfMinus1_ "_pipe" $ c__pipe pfds 2 ({- _O_BINARY -} 32768)
        readfd <- peek pfds
        writefd <- peekElemOff pfds 1
        return (readfd, writefd)

fdToHandle fd mode = do
    (fd', deviceType) <- mkFD fd mode (Just (Stream, 0, 0)) False False
    mkHandleFromFD fd' deviceType "" mode False Nothing

closeFd = throwErrnoIfMinus1_ "_close" . c__close

foreign import ccall "io.h _pipe" c__pipe ::
    Ptr CInt -> CUInt -> CInt -> IO CInt

foreign import ccall "io.h _close" c__close ::
    CInt -> IO CInt
#else
createPipe = Posix.createPipe
fdToHandle fd _ = Posix.fdToHandle fd
closeFd = Posix.closeFd
#endif
