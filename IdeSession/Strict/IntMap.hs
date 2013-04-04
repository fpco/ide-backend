-- | Wrapper around Data.IntMap that guarantees elements are evaluated when
-- the Map is. containers-0.5 provides this out of the box, but alas ghc 7.4
-- is built against containers-0.4.
module IdeSession.Strict.IntMap (
    StrictIntMap
  , fromList
  , toList
  , (!)
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap

newtype StrictIntMap v = StrictIntMap { toIntMap :: IntMap v }

fromIntMap :: IntMap v -> StrictIntMap v
fromIntMap m = StrictIntMap $ forceValues m `seq` m
  where
    forceValues :: IntMap v -> ()
    forceValues = IntMap.foldr' seq ()

-- (!) :: StrictIntMap v -> Int -> v
(!) :: StrictIntMap v -> Int -> v
(!) = (IntMap.!) . toIntMap

fromList :: [(Int, v)] -> StrictIntMap v
fromList = fromIntMap . IntMap.fromList

toList :: StrictIntMap v -> [(Int, v)]
toList = IntMap.toList . toIntMap
