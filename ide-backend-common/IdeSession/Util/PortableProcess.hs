{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module IdeSession.Util.PortableProcess where

#ifdef VERSION_unix
import System.Posix.Signals
import System.Posix.Types
#else
import System.Process
import System.Process.Internals
import System.Win32
#endif

#ifdef VERSION_unix
type Pid = ProcessID
#else
type Pid = ProcessId
#endif

-- Attempts to kill the process, on Unix using sigKILL
sigKillProcess :: Pid -> IO ()

-- Attempts to terminate the process, on Unix using sigTERM
sigTermProcess :: Pid -> IO ()

-- Raises a sigKILL
raiseSigKill :: IO ()

#ifdef VERSION_unix
sigKillProcess = signalProcess sigKILL
sigTermProcess = signalProcess sigTERM
raiseSigKill = raiseSignal sigKILL
#else
-- On Windows, we just terminate, no special support for sigKILL
sigKillProcess = sigTermProcess
sigTermProcess pid = do
  ptr <- openProcess pROCESS_TERMINATE False pid
  ph <- mkProcessHandle ptr
  terminateProcess ph

foreign import ccall unsafe "winbase.h GetCurrentProcessId"
    c_GetCurrentProcessId :: IO DWORD

getCurrentPid :: IO Pid
getCurrentPid = c_GetCurrentProcessId

raiseSigKill = do
  pid <- getCurrentPid
  sigKillProcess pid
#endif
