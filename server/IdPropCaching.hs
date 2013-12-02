module IdPropCaching (
    MonadIdPropCaching(..)
  , modifyIdPropCache
  , extendIdPropCache
  , recordIdPropType
  ) where

import Control.Applicative ((<|>))
import System.IO.Unsafe (unsafePerformIO)

import IdeSession.Types.Private (IdPropPtr(..), IdProp(..))
import IdeSession.Types.Public (Type)
import IdeSession.Strict.IORef
import IdeSession.Strict.Container
import qualified IdeSession.Strict.IntMap as IntMap
import qualified IdeSession.Strict.Maybe  as Maybe

import GHC (Ghc)
import MonadUtils

class Monad m => MonadIdPropCaching m where
  getIdPropCache :: m (Strict IntMap IdProp)
  putIdPropCache :: Strict IntMap IdProp -> m ()

idPropCacheRef :: StrictIORef (Strict IntMap IdProp)
{-# NOINLINE idPropCacheRef #-}
idPropCacheRef = unsafePerformIO $ newIORef IntMap.empty

instance MonadIdPropCaching IO where
  getIdPropCache = readIORef  idPropCacheRef
  putIdPropCache = writeIORef idPropCacheRef

instance MonadIdPropCaching Ghc where
  getIdPropCache = liftIO $ getIdPropCache
  putIdPropCache = liftIO . putIdPropCache

modifyIdPropCache :: MonadIdPropCaching m => IdPropPtr -> (IdProp -> IdProp) -> m ()
modifyIdPropCache ptr f = do
  cache <- getIdPropCache
  putIdPropCache $ IntMap.adjust f (idPropPtr ptr) cache

extendIdPropCache :: MonadIdPropCaching m => IdPropPtr -> IdProp -> m ()
extendIdPropCache ptr prop = do
    cache <- getIdPropCache
    putIdPropCache $ IntMap.insertWith update (idPropPtr ptr) prop cache
  where
    -- We need to make sure not to lose information that we learned earlier
    update :: IdProp -> IdProp -> IdProp
    update new old
      = new { idType       = idType       new <|> idType       old
            , idHomeModule = idHomeModule new <|> idHomeModule old
            }

recordIdPropType :: MonadIdPropCaching m => IdPropPtr -> Type -> m ()
recordIdPropType ptr tp =
  modifyIdPropCache ptr $ \idInfo -> idInfo {
      idType = Maybe.just tp
    }
