module IdPropCaching (
    MonadIdPropCaching(..)
  , modifyIdPropCache
  , extendIdPropCache
  ) where

import System.IO.Unsafe (unsafePerformIO)

import IdeSession.Types.Private (IdPropPtr(..), IdProp)
import IdeSession.Strict.IORef
import IdeSession.Strict.Container
import qualified IdeSession.Strict.IntMap as IntMap

class Monad m => MonadIdPropCaching m where
  getIdPropCache :: m (Strict IntMap IdProp)
  putIdPropCache :: Strict IntMap IdProp -> m ()

idPropCacheRef :: StrictIORef (Strict IntMap IdProp)
{-# NOINLINE idPropCacheRef #-}
idPropCacheRef = unsafePerformIO $ newIORef IntMap.empty

instance MonadIdPropCaching IO where
  getIdPropCache = readIORef  idPropCacheRef
  putIdPropCache = writeIORef idPropCacheRef

modifyIdPropCache :: MonadIdPropCaching m => IdPropPtr -> (IdProp -> IdProp) -> m ()
modifyIdPropCache ptr f = do
  cache <- getIdPropCache
  let uniq = idPropPtr ptr
  putIdPropCache $ IntMap.adjust f uniq cache

extendIdPropCache :: MonadIdPropCaching m => IdPropPtr -> IdProp -> m ()
extendIdPropCache ptr prop = do
  cache <- getIdPropCache
  -- Don't overwrite existing entries, because we might lose type information
  -- that we gleaned earlier
  let uniq = idPropPtr ptr
  putIdPropCache $ IntMap.insertWith (\_new old -> old) uniq prop cache
