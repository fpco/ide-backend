{-

Modified by Michael Sloan, FP Complete, in 2015

Copyright 2012-2013 Google Inc. All Rights Reserved.
Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at
    http://www.apache.org/licenses/LICENSE-2.0
Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- | Modified by Michael Sloan, FP Complete, in 2015
--
-- Based on https://github.com/mzero/plush/blob/ffa6c84f7bcb7bd57079e9a519e2bdc1ae07b4be/src/System/Posix/Missing.hsc
module Posix (
    dupFd, dupFdCloseOnExec,

    loginTerminal,
    setControllingTerminal,

    getArg0,
    executeFile0,
    -- * From Plush.Run.Posix.IO
    write,
    readChunk
    ) where

import           Control.Applicative ((<$>))
import           Control.Arrow (second)
import           Control.Exception (throwIO)
import           Control.Monad (when, void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Internal as BS (createAndTrim)
import           Data.ByteString.Unsafe (unsafeUseAsCStringLen)
import           Foreign (withArray0, withMany)
import Foreign.C (CInt(..), CString, CULong(..),
    throwErrnoIfMinus1, throwErrnoIfMinus1_, throwErrnoPathIfMinus1_)
import           Foreign.Ptr (Ptr, nullPtr, castPtr, plusPtr)
import           Foreign.Ptr (nullPtr, Ptr)
import           Foreign.Safe (alloca, peek, peekElemOff)
import           GHC.Foreign (peekCString)
import           GHC.IO.Device (SeekMode(SeekFromEnd, AbsoluteSeek))
import           GHC.IO.Encoding (getFileSystemEncoding)
import           GHC.IO.Exception (IOErrorType(UnsupportedOperation))
import           System.IO.Error (ioeGetErrorType, catchIOError)
import           System.Posix.IO (fdReadBuf, fdSeek, fdWriteBuf, setFdOption, FdOption(CloseOnExec))
import           System.Posix.Internals (c_fcntl_write, FD, withFilePath)
import           System.Posix.Process.Internals (pPrPr_disableITimers)
import           System.Posix.Types (Fd(Fd))

#include "HsUnix.h"

-- | This is like 'dup', but takes an extra argument. The new file descriptor
-- will be the lowest free file descriptor not less than the second argument.
-- The 'CloseOnExec' flag (FD_CLOEXEC) will be cleared on the new descriptor.
dupFd :: Fd -> Fd -> IO Fd
dupFd = dupFdViaFcntl (#const F_DUPFD)

-- | Same as 'dupFd', but the 'CloseOnExec' flag (FD_CLOEXEC) will be set.
dupFdCloseOnExec :: Fd -> Fd -> IO Fd
-- dupFdCloseOnExec = dupFdViaFcntl (#const F_DUPFD_CLOEXEC)
    -- While this is POSIX, is is relatively new, and many systems do not
    -- support this operation. The following code simulates it.
dupFdCloseOnExec src base = do
    dest <- dupFd src base
    setFdOption dest CloseOnExec True
    return dest

dupFdViaFcntl :: CInt -> Fd -> Fd -> IO Fd
dupFdViaFcntl op (Fd srcFd) (Fd minFd) = Fd `fmap`
  throwErrnoIfMinus1 "dupFdViaFcntl"
                      (c_fcntl_write srcFd op (fromIntegral minFd))

-- | Prepares for login on the tty, which should be a real tty or the slave
-- from a call to `openPseudoTerminal`. A new session will be started, the tty
-- will become the controlling terminal, and the tty is dup'd down to become
-- the standard in, out and err FDs.
loginTerminal :: Fd -> IO ()

#ifdef HAVE_OPENPTY
    -- In theory this should be testing HAVE_LOGIN_TTY, but that doesn't exist.
    -- The calls login_tty, openpty, and forkpty are part of the same BSD module
    -- and so are all generally present together or not.
loginTerminal (Fd ttyFd) =
    throwErrnoIfMinus1_ "loginTerminal" (c_login_tty ttyFd)

foreign import ccall unsafe "login_tty"
  c_login_tty :: CInt -> IO CInt

#else
loginTerminal tty = do
    _ <- createSession
    setControllingTerminal tty
    _ <- dupTo tty stdInput
    _ <- dupTo tty stdOutput
    _ <- dupTo tty stdError
    when (tty > stdError) $ closeFd tty
#endif

-- | Set the controlling terminal of the process. The tty should be a real tty
-- or the slave from a call to 'openPseudoTerminal'.
setControllingTerminal :: Fd -> IO ()

#ifdef TIOCSCTTY
setControllingTerminal (Fd ttyfd) =
    throwErrnoIfMinus1_ "setControllingTerminal"
        (ioctl ttyfd (#const TIOCSCTTY) nullPtr)

foreign import ccall ioctl :: FD -> CULong -> Ptr a -> IO CInt
#else
-- POSIX doesn't spec how to set the controlling terminal! The ioctl TIOSCTTY
-- method above is generally available, but some systems don't have it. The
-- second most common way to set the controlling terminal is to open it when no
-- other terminal is opened in the process. That is what this alternative
-- implementation does. Be forewarned that they may be systems that use yet some
-- third unknown method.
setControllingTerminal tty = do
    ttyName <- getTerminalName tty
    when (tty /= stdInput) $ closeFd stdInput
    when (tty /= stdOutput) $ closeFd stdOutput
    when (tty /= stdError) $ closeFd stdError
    openFd ttyName ReadWrite Nothing defaultFileFlags >>= closeFd
#endif

-- | Return @argv[0]@ as passed to the program on invocation. Unlike
-- 'getProgName', this is the raw value passed to the process. It may, or may
-- not, have anything to do with the name of the running executable!
getArg0 :: IO String
getArg0 =
    alloca $ \ p_argc ->
    alloca $ \ p_argv -> do
        getProgArgv p_argc p_argv
        argv <- peek p_argv
        enc <- getFileSystemEncoding
        peekElemOff argv 0 >>= peekCString enc

foreign import ccall unsafe "getProgArgv"
  getProgArgv :: Ptr CInt -> Ptr (Ptr CString) -> IO ()


-- | Like 'executeFile', but allow for expressly setting @argv[0]@
-- For simplicity, this version has no path search option, and always takes an
-- environment.  If incorporating into System.Posix.Process, the signature
-- should better match 'executeFile'.
executeFile0 :: FilePath -> String -> [String] -> [(String, String)] -> IO a
executeFile0 path cmd args env = do
  withFilePath path $ \s ->
    withMany withFilePath (cmd:args) $ \cstrs ->
      withArray0 nullPtr cstrs $ \arg_arr ->
    let env' = map (\ (name, val) -> name ++ ('=' : val)) env in
    withMany withFilePath env' $ \cenv ->
      withArray0 nullPtr cenv $ \env_arr -> do
    pPrPr_disableITimers
    throwErrnoPathIfMinus1_ "executeFile" path
        (c_execve s arg_arr env_arr)
    return undefined -- never reached

foreign import ccall unsafe "execve"
  c_execve :: CString -> Ptr CString -> Ptr CString -> IO CInt

-- Copied from Plush.Run.Posix
-- https://github.com/mzero/plush/blob/ffa6c84f7bcb7bd57079e9a519e2bdc1ae07b4be/src/Plush/Run/Posix.hs

-- | Based on code for readLoop, but instead expects a blocking Fd.
readChunk :: Fd -> IO BS.ByteString
readChunk fd =
    BS.createAndTrim bufSize $ \buf ->
        fmap fromIntegral (fdReadBuf fd buf (fromIntegral bufSize))
  where
    bufSize = 4096

-- | 'write' for 'IO': Seek to the end, and write.
--
-- Modified to take a strict bytestring
write :: Fd -> BS.ByteString -> IO ()
write fd bs = unsafeUseAsCStringLen bs writeBuf
  where
    writeBuf (p, n) | n > 0 = do
        ignoreUnsupportedOperation $ fdSeek fd SeekFromEnd 0
        m <- fromIntegral `fmap` fdWriteBuf fd (castPtr p) (fromIntegral n)
        when (0 <= m && m <= n) $ writeBuf (p `plusPtr` m, n - m)
    writeBuf _ = return ()

ignoreUnsupportedOperation :: IO a -> IO ()
ignoreUnsupportedOperation act = void act `catchIOError` \ex ->
    if ioeGetErrorType ex == UnsupportedOperation
        then return ()
        else throwIO ex
