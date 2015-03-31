-- IORefs that always evaluate their contents to WHNF
module IdeSession.Strict.IORef (
    StrictIORef -- Abstract
  , newIORef
  , readIORef
  , writeIORef
  , modifyIORef
  ) where

import Control.Applicative ((<$>))
import Control.Exception (evaluate)
import Data.IORef (IORef)
import qualified Data.IORef as IORef

newtype StrictIORef a = StrictIORef (IORef a)

newIORef :: a -> IO (StrictIORef a)
newIORef x = StrictIORef <$> (evaluate x >>= IORef.newIORef)

readIORef :: StrictIORef a -> IO a
readIORef (StrictIORef v) = IORef.readIORef v

writeIORef :: StrictIORef a -> a -> IO ()
writeIORef (StrictIORef v) x = evaluate x >>= IORef.writeIORef v

-- base 4.6 exports modifyIORef' but 4.5 does not.
modifyIORef :: StrictIORef a -> (a -> a) -> IO ()
modifyIORef v f = readIORef v >>= writeIORef v . f
