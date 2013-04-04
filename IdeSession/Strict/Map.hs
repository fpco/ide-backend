-- | Wrapper around Data.Map that guarantees elements are evaluated when
-- the Map is. containers-0.5 provides this out of the box, but alas ghc 7.4
-- is built against containers-0.4.
module IdeSession.Strict.Map (
    StrictMap -- Abstract
  , toList
  , map
  , mapKeys
  ) where

import Prelude hiding (map)
import Data.Map (Map)
import qualified Data.Map as Map

newtype StrictMap k v = StrictMap { toMap :: Map k v }

fromMap :: Map k v -> StrictMap k v
fromMap m = StrictMap $ forceValues m `seq` m
  where
    forceValues :: Map k v -> ()
    forceValues = Map.foldr' seq ()

toList :: StrictMap k v -> [(k, v)]
toList = Map.toList . toMap

map :: (a -> b) -> StrictMap k a  -> StrictMap k b
map f = fromMap . Map.map f . toMap

mapKeys :: Ord k' => (k -> k') -> StrictMap k v -> StrictMap k' v
-- Maps are already strict in keys
mapKeys f = StrictMap . Map.mapKeys f . toMap
