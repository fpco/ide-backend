{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
-- | Wrapper around Data.Map that guarantees elements are evaluated when
-- the Map is. containers-0.5 provides this out of the box, but alas ghc 7.4
-- is built against containers-0.4.
module IdeSession.Strict.Map (
    toList
  , fromList
  , map
  , mapWithKey
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
  , delete
  ) where

import Prelude hiding (map, lookup)
import Data.Set (Set)
import qualified Data.Map as Map

import IdeSession.Strict.Container

toList :: Strict (Map k) v -> [(k, v)]
toList = Map.toList . toLazyMap

fromList :: Ord k => [(k, v)] -> Strict (Map k) v
fromList = force . Map.fromList

map :: (a -> b) -> Strict (Map k) a  -> Strict (Map k) b
map f = force . Map.map f . toLazyMap

mapWithKey :: (k -> a -> b) -> Strict (Map k) a  -> Strict (Map k) b
mapWithKey f = force . Map.mapWithKey f . toLazyMap

mapKeys :: Ord k' => (k -> k') -> Strict (Map k) v -> Strict (Map k') v
-- Maps are already strict in keys
mapKeys f = StrictMap . Map.mapKeys f . toLazyMap

empty :: Strict (Map k) v
empty = StrictMap Map.empty

insert :: Ord k => k -> v -> Strict (Map k) v -> Strict (Map k) v
insert k v = StrictMap . Map.insertWith' const k v . toLazyMap

union :: Ord k => Strict (Map k) v -> Strict (Map k) v -> Strict (Map k) v
union a b = StrictMap $ Map.union (toLazyMap a) (toLazyMap b)

filterWithKey :: Ord k => (k -> v -> Bool) -> Strict (Map k) v -> Strict (Map k) v
filterWithKey p = StrictMap . Map.filterWithKey p . toLazyMap

keysSet :: Strict (Map k) v -> Set k
keysSet = Map.keysSet . toLazyMap

lookup :: Ord k => k -> Strict (Map k) v -> Maybe v
lookup k = Map.lookup k . toLazyMap

(\\) :: Ord k => Strict (Map k) a -> Strict (Map k) b -> Strict (Map k) a
(\\) a b = StrictMap $ (Map.\\) (toLazyMap a) (toLazyMap b)

alter :: forall k a. Ord k
      => (Maybe a -> Maybe a) -> k -> Strict (Map k) a -> Strict (Map k) a
alter f k = StrictMap . Map.alter aux k . toLazyMap
  where
    aux :: Maybe a -> Maybe a
    aux ma = case f ma of
               Nothing -> Nothing
               Just a  -> a `seq` Just a

member :: Ord k => k -> Strict (Map k) v -> Bool
member k = Map.member k . toLazyMap

(!) :: Ord k => Strict (Map k) v -> k -> v
(!) = (Map.!) . toLazyMap

keys :: Strict (Map k) a -> [k]
keys = Map.keys . toLazyMap

delete :: Ord k => k -> Strict (Map k) a -> Strict (Map k) a
delete k = StrictMap . Map.delete k . toLazyMap
