module FilePathCaching (
    MonadFilePathCaching(..)
  , mkFilePathPtr
  ) where

import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import System.IO.Unsafe (unsafePerformIO)

import IdeSession.Types.Private (FilePathPtr(..))
import IdeSession.Strict.IORef

class Monad m => MonadFilePathCaching m where
  getFilePathCache :: m (HashMap FilePath Int, Int)
  putFilePathCache :: (HashMap FilePath Int, Int) -> m ()

filePathCacheRef :: StrictIORef (HashMap FilePath Int, Int)
{-# NOINLINE filePathCacheRef #-}
filePathCacheRef = unsafePerformIO $ newIORef (HashMap.empty, 0)

instance MonadFilePathCaching IO where
  getFilePathCache = readIORef  filePathCacheRef
  putFilePathCache = writeIORef filePathCacheRef

mkFilePathPtr :: MonadFilePathCaching m => FilePath -> m FilePathPtr
mkFilePathPtr path = do
  (hash, next) <- getFilePathCache
  case HashMap.lookup path hash of
    Nothing -> do
      let next' = next + 1
      putFilePathCache (HashMap.insert path next' hash, next')
      return $ FilePathPtr next'
    Just key ->
       return $ FilePathPtr key
