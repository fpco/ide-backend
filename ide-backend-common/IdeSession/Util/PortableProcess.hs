{-# LANGUAGE CPP #-}
module IdeSession.Util.PortableProcess where

import System.Process (ProcessHandle)

#ifdef VERSION_unix
import System.Posix.Signals
import System.Posix.Types
import System.Process.Internals (withProcessHandle, ProcessHandle__(..))
#else
import System.Process
import System.Process.Internals hiding (ProcessHandle)
import System.Win32 hiding (ProcessHandle)
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

-- If available sends a sigKill to the given process handle
killProcessHandle :: ProcessHandle -> IO ()

#ifdef VERSION_unix
sigKillProcess = signalProcess sigKILL
sigTermProcess = signalProcess sigTERM
raiseSigKill = raiseSignal sigKILL

killProcessHandle ph = withProcessHandle ph $ \p_ ->
    case p_ of
      ClosedHandle _ ->
        leaveHandleAsIs p_
      OpenHandle pID -> do
        signalProcess sigKILL pID
        leaveHandleAsIs p_
  where
      leaveHandleAsIs _p =
#if MIN_VERSION_process(1,2,0)
        return ()
#else
        return (_p, ())
#endif

#else
-- On Windows, we just terminate, no special support for sigKILL
sigKillProcess = sigTermProcess
sigTermProcess pid = do
  ptr <- openProcess pROCESS_TERMINATE False pid
  ph <- mkProcessHandle ptr
  terminateProcess ph

-- On Windows, no special support for sigKill
killProcessHandle = terminateProcess

foreign import ccall unsafe "winbase.h GetCurrentProcessId"
    c_GetCurrentProcessId :: IO DWORD

getCurrentPid :: IO Pid
getCurrentPid = c_GetCurrentProcessId

raiseSigKill = do
  pid <- getCurrentPid
  sigKillProcess pid
#endif
