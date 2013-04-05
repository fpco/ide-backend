-- | Wrapper around Data.IntMap that guarantees elements are evaluated when
-- the Map is. containers-0.5 provides this out of the box, but alas ghc 7.4
-- is built against containers-0.4.
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module IdeSession.Strict.IntMap (
    StrictIntMap
  , fromList
  , toList
  , (!)
  , empty
  , adjust
  , insertWith
  , map
  ) where

import Prelude hiding (map)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Aeson.TH (deriveJSON)

newtype StrictIntMap v = StrictIntMap { toIntMap :: IntMap v }

$(deriveJSON id ''StrictIntMap)

fromIntMap :: IntMap v -> StrictIntMap v
fromIntMap m = StrictIntMap $ forceValues m `seq` m
  where
    forceValues :: IntMap v -> ()
    forceValues = IntMap.foldr' seq ()

(!) :: StrictIntMap v -> Int -> v
(!) = (IntMap.!) . toIntMap

fromList :: [(Int, v)] -> StrictIntMap v
fromList = fromIntMap . IntMap.fromList

toList :: StrictIntMap v -> [(Int, v)]
toList = IntMap.toList . toIntMap

empty :: StrictIntMap v
empty = StrictIntMap $ IntMap.empty

-- We use alter because it gives us something to anchor a seq to
adjust :: forall v. (v -> v) -> Int -> StrictIntMap v -> StrictIntMap v
adjust f i = StrictIntMap . IntMap.alter aux i . toIntMap
  where
    aux :: Maybe v -> Maybe v
    aux Nothing  = Nothing
    aux (Just v) = let v' = f v in v' `seq` Just v'

insertWith :: (v -> v -> v) -> Int -> v -> StrictIntMap v -> StrictIntMap v
insertWith f i v = StrictIntMap . IntMap.insertWith' f i v . toIntMap

map :: (a -> b) -> StrictIntMap a -> StrictIntMap b
map f = fromIntMap . IntMap.map f . toIntMap
