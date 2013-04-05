{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
-- | Wrapper around Data.Map that guarantees elements are evaluated when
-- the Map is. containers-0.5 provides this out of the box, but alas ghc 7.4
-- is built against containers-0.4.
module IdeSession.Strict.Map (
    StrictMap -- Abstract
  , toMap
  , toList
  , fromList
  , map
  , mapKeys
  , empty
  , insert
  , union
  , filterWithKey
  , lookup
  , keysSet
  , (\\)
  , alter
  , member
  , (!)
  , keys
  ) where

import Prelude hiding (map, lookup)
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map as Map
import Data.Aeson (FromJSON(..), ToJSON(..))

newtype StrictMap k v = StrictMap { toMap :: Map k v }

-- For some reason Aeson doesn't have a built-in instance for map,
-- so we cannot use deriveJSON
instance (Ord k, FromJSON k, FromJSON v) => FromJSON (StrictMap k v) where
  parseJSON = fmap fromList . parseJSON

instance (ToJSON k, ToJSON v) => ToJSON (StrictMap k v) where
  toJSON = toJSON . toList

fromMap :: Map k v -> StrictMap k v
fromMap m = StrictMap $ forceValues m `seq` m
  where
    forceValues :: Map k v -> ()
    forceValues = Map.foldr' seq ()

toList :: StrictMap k v -> [(k, v)]
toList = Map.toList . toMap

fromList :: Ord k => [(k, v)] -> StrictMap k v
fromList = fromMap . Map.fromList

map :: (a -> b) -> StrictMap k a  -> StrictMap k b
map f = fromMap . Map.map f . toMap

mapKeys :: Ord k' => (k -> k') -> StrictMap k v -> StrictMap k' v
-- Maps are already strict in keys
mapKeys f = StrictMap . Map.mapKeys f . toMap

empty :: StrictMap k v
empty = StrictMap Map.empty

insert :: Ord k => k -> v -> StrictMap k v -> StrictMap k v
insert k v = StrictMap . Map.insertWith' const k v . toMap

union :: Ord k => StrictMap k v -> StrictMap k v -> StrictMap k v
union a b = StrictMap $ Map.union (toMap a) (toMap b)

filterWithKey :: Ord k => (k -> v -> Bool) -> StrictMap k v -> StrictMap k v
filterWithKey p = StrictMap . Map.filterWithKey p . toMap

keysSet :: StrictMap k v -> Set k
keysSet = Map.keysSet . toMap

lookup :: Ord k => k -> StrictMap k v -> Maybe v
lookup k = Map.lookup k . toMap

(\\) :: Ord k => StrictMap k a -> StrictMap k b -> StrictMap k a
(\\) a b = StrictMap $ (Map.\\) (toMap a) (toMap b)

alter :: forall k a. Ord k
      => (Maybe a -> Maybe a) -> k -> StrictMap k a -> StrictMap k a
alter f k = StrictMap . Map.alter aux k . toMap
  where
    aux :: Maybe a -> Maybe a
    aux ma = case f ma of
               Nothing -> Nothing
               Just a  -> a `seq` Just a

member :: Ord k => k -> StrictMap k v -> Bool
member k = Map.member k . toMap

(!) :: Ord k => StrictMap k v -> k -> v
(!) = (Map.!) . toMap

keys :: StrictMap k a -> [k]
keys = Map.keys . toMap
