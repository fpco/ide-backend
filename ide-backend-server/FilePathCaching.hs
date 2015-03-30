module FilePathCaching (
    MonadFilePathCaching(..)
  , mkFilePathPtr
  , extractSourceSpan
  ) where

import Control.Monad (liftM)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import System.IO.Unsafe (unsafePerformIO)
import Data.Text (Text)
import qualified Data.Text as Text

import GHC
import FastString
import MonadUtils

import IdeSession.Types.Private
import IdeSession.Strict.IORef
import IdeSession.Strict.Pair

class Monad m => MonadFilePathCaching m where
  getFilePathCache :: m (StrictPair (HashMap FilePath Int) Int)
  putFilePathCache :: StrictPair (HashMap FilePath Int) Int -> m ()

filePathCacheRef :: StrictIORef (StrictPair (HashMap FilePath Int) Int)
{-# NOINLINE filePathCacheRef #-}
filePathCacheRef = unsafePerformIO $ newIORef $ fromLazyPair (HashMap.empty, 0)

instance MonadFilePathCaching IO where
  getFilePathCache = readIORef  filePathCacheRef
  putFilePathCache = writeIORef filePathCacheRef

instance MonadFilePathCaching Ghc where
  getFilePathCache = liftIO $ getFilePathCache
  putFilePathCache = liftIO . putFilePathCache

mkFilePathPtr :: MonadFilePathCaching m => FilePath -> m FilePathPtr
mkFilePathPtr path = do
  (hash, next) <- toLazyPair `liftM` getFilePathCache
  case HashMap.lookup path hash of
    Nothing -> do
      let next' = next + 1
      putFilePathCache $ fromLazyPair (HashMap.insert path next' hash, next')
      return $ FilePathPtr next'
    Just key ->
       return $ FilePathPtr key

extractSourceSpan :: MonadFilePathCaching m => SrcSpan -> m EitherSpan
extractSourceSpan (RealSrcSpan srcspan) = do
  key <- mkFilePathPtr $ unpackFS (srcSpanFile srcspan)
  return . ProperSpan $ SourceSpan
    key
    (srcSpanStartLine srcspan) (srcSpanStartCol srcspan)
    (srcSpanEndLine srcspan)   (srcSpanEndCol   srcspan)
extractSourceSpan (UnhelpfulSpan s) =
  return . TextSpan $ fsToText s

fsToText :: FastString -> Text
fsToText = Text.pack . unpackFS
