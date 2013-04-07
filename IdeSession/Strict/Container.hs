{-# LANGUAGE TypeFamilies, FlexibleInstances, StandaloneDeriving #-}
module IdeSession.Strict.Container
  ( StrictContainer(..)
  , Strict(..)
    -- * For convenience, we export the names of the lazy types too
  , Maybe
  , Map
  , IntMap
  , Trie
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Trie (Trie)
import Data.Foldable as Foldable

class StrictContainer t where
  data Strict (t :: * -> *) :: * -> *
  force   :: t a -> Strict t a
  project :: Strict t a -> t a

{------------------------------------------------------------------------------
  IntMap
------------------------------------------------------------------------------}

instance StrictContainer IntMap where
  newtype Strict IntMap v = StrictIntMap { toLazyIntMap :: IntMap v }

  force m = IntMap.foldr' seq () m `seq` StrictIntMap m
  project = toLazyIntMap

instance FromJSON v => FromJSON (Strict IntMap v) where
  parseJSON = fmap (force . IntMap.fromList) . parseJSON

instance ToJSON v => ToJSON (Strict IntMap v) where
  toJSON = toJSON . IntMap.toList . toLazyIntMap

{------------------------------------------------------------------------------
  Lists
------------------------------------------------------------------------------}

instance StrictContainer [] where
  newtype Strict [] a = StrictList { toLazyList :: [a] }
    deriving Eq

  force m = List.foldl' (flip seq) () m `seq` StrictList m
  project = toLazyList

instance FromJSON a => FromJSON (Strict [] a) where
  parseJSON = fmap force . parseJSON

instance ToJSON a => ToJSON (Strict [] a) where
  toJSON = toJSON . toLazyList

{------------------------------------------------------------------------------
  Map
------------------------------------------------------------------------------}

instance StrictContainer (Map k) where
  newtype Strict (Map k) v = StrictMap { toLazyMap :: Map k v }

  force m = Map.foldr' seq () m `seq` StrictMap m
  project = toLazyMap

instance (Ord k, FromJSON k, FromJSON v) => FromJSON (Strict (Map k) v) where
  parseJSON = fmap (force . Map.fromList) . parseJSON

instance (ToJSON k, ToJSON v) => ToJSON (Strict (Map k) v) where
  toJSON = toJSON . Map.toList . toLazyMap

{------------------------------------------------------------------------------
  Maybe
------------------------------------------------------------------------------}

instance StrictContainer Maybe where
  newtype Strict Maybe a = StrictMaybe { toLazyMaybe :: Maybe a }

  force Nothing  = StrictMaybe Nothing
  force (Just x) = x `seq` StrictMaybe $ Just x
  project = toLazyMaybe

instance FromJSON a => FromJSON (Strict Maybe a) where
  parseJSON = fmap force . parseJSON

instance ToJSON a => ToJSON (Strict Maybe a) where
  toJSON = toJSON . toLazyMaybe

deriving instance Eq a => Eq (Strict Maybe a)

instance Functor (Strict Maybe) where
  fmap f = force . fmap f . toLazyMaybe

{------------------------------------------------------------------------------
  Trie
------------------------------------------------------------------------------}

instance StrictContainer Trie where
  newtype Strict Trie a = StrictTrie { toLazyTrie :: Trie a }

  force m = Foldable.foldr seq () m `seq` StrictTrie m
  project = toLazyTrie
