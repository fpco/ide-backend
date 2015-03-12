module IdeSession.Strict.Trie (
    empty
  , submap
  , elems
  , fromListWith
  , toList
  ) where

import Data.ByteString (ByteString)
import qualified Data.Trie as Trie
import qualified Data.Trie.Convenience as Trie
import IdeSession.Strict.Container

empty :: Strict Trie a
empty = StrictTrie $ Trie.empty

submap :: ByteString -> Strict Trie a -> Strict Trie a
submap bs = StrictTrie . Trie.submap bs . toLazyTrie

elems :: Strict Trie a -> [a]
elems = Trie.elems . toLazyTrie

fromListWith :: (a -> a -> a) -> [(ByteString, a)] -> Strict Trie a
fromListWith f = force . Trie.fromListWith f

toList :: Strict Trie a -> [(ByteString, a)]
toList = Trie.toList . toLazyTrie
