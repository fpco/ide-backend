-- | Working with IDE sessions
module TestSuite.Session (
    updateSessionD
  ) where

import Data.IORef
import Test.HUnit

import IdeSession

updateSessionD :: IdeSession -> IdeSessionUpdate () -> Int -> IO ()
updateSessionD session update numProgressUpdates = do
  progressRef <- newIORef []

  -- We just collect the progress messages first, and verify them afterwards
  updateSession session update $ \p -> do
    progressUpdates <- readIORef progressRef
    writeIORef progressRef $ progressUpdates ++ [p]

  -- These progress messages are often something like
  --
  -- [18 of 27] Compiling IdeSession.Types.Private ( IdeSession/Types/Private.hs, dist/build/IdeSession/Types/Private.o )
  -- [19 of 27] Compiling IdeSession.GHC.API ( IdeSession/GHC/API.hs, dist/build/IdeSession/GHC/API.o )
  -- [20 of 27] Compiling IdeSession.GHC.Client ( IdeSession/GHC/Client.hs, dist/build/IdeSession/GHC/Client.p_o )
  -- [21 of 27] Compiling IdeSession.Types.Translation ( IdeSession/Types/Translation.hs, dist/build/IdeSession/Types/Translation.p_o )
  -- [23 of 27] Compiling IdeSession.State ( IdeSession/State.hs, dist/build/IdeSession/State.p_o )
  --
  -- So these numbers don't need to start at 1, may be discontiguous, out of
  -- order, and may not end with [X of X]. The only thing we can check here is
  -- that we get at most the number of progress messages we expect.
  progressUpdates <- readIORef progressRef
  assertBool ("We expected " ++ show numProgressUpdates ++ " progress messages, but got " ++ show progressUpdates)
             (length progressUpdates <= numProgressUpdates)

