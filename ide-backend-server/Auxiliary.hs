{-# LANGUAGE CPP #-}

module Auxiliary where

import Network
import Control.Concurrent (forkIO, ThreadId)
import System.Environment (withArgs)
import GhcMonad(Ghc(..))


import IdeSession.RPC.API
import IdeSession.RPC.Server
import IdeSession.Util
import IdeSession.Util.PortableProcess


#ifdef VERSION_unix
import System.Posix.Process (forkProcess)
#else
forkProcess :: IO () -> IO Pid
forkProcess = error "unsupported on non-Unix"
#endif


-- | Generalization of captureOutput
captureGhcOutput :: Ghc a -> Ghc (String, a)
captureGhcOutput = unsafeLiftIO captureOutput

-- | Lift operations on `IO` to the `Ghc` monad. This is unsafe as it makes
-- operations possible in the `Ghc` monad that weren't possible before
-- (for instance, @unsafeLiftIO forkIO@ is probably a bad idea!).
unsafeLiftIO :: (IO a -> IO b) -> Ghc a -> Ghc b
unsafeLiftIO f (Ghc ghc) = Ghc $ \session -> f (ghc session)

-- | Generalization of 'unsafeLiftIO'
_unsafeLiftIO1 :: ((c -> IO a) -> IO b) -> (c -> Ghc a) -> Ghc b
_unsafeLiftIO1 f g = Ghc $ \session ->
  f $ \c -> case g c of Ghc ghc -> ghc session

-- | Generalization of 'unsafeLiftIO'
--
-- TODO: Is there a more obvious way to define this progression?
unsafeLiftIO2 :: ((c -> d -> IO a) -> IO b) -> (c -> d -> Ghc a) -> Ghc b
unsafeLiftIO2 f g = Ghc $ \session ->
  f $ \c d -> case g c d of Ghc ghc -> ghc session

-- | Lift `withArgs` to the `Ghc` monad. Relies on `unsafeLiftIO`.
ghcWithArgs :: [String] -> Ghc a -> Ghc a
ghcWithArgs = unsafeLiftIO . withArgs

-- | Fork within the `Ghc` monad. Use with caution.
_forkGhc :: Ghc () -> Ghc ThreadId
_forkGhc = unsafeLiftIO forkIO

-- | forkProcess within the `Ghc` monad. Use with extreme caution.
forkGhcProcess :: Ghc () -> Ghc Pid
forkGhcProcess = unsafeLiftIO forkProcess

-- | Lifted version of concurrentConversation
ghcConcurrentConversation :: (FilePath -> RpcConversation -> Ghc ())
                          -> Socket
                          -> Socket
                          -> FilePath
                          -> Ghc ()
ghcConcurrentConversation f requestR responseW errorLog =
  unsafeLiftIO2 (concurrentConversation requestR responseW errorLog) f
