-- | (Orphan) PrettyVal instances for various standard datatypes
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module IdeSession.Util.PrettyVal (
    -- * Re-exports
    PrettyVal(..)
  ) where

import Text.Show.Pretty
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Trie (Trie)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.IntMap           as IntMap
import qualified Data.Map              as Map
import qualified Data.Trie             as Trie
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text             as Text

#if !MIN_VERSION_pretty_show(1,6,9)
instance PrettyVal a => PrettyVal (Maybe a) where
  prettyVal Nothing  = Con "Nothing" []
  prettyVal (Just x) = Con "Just"    [prettyVal x]
#endif

-- TODO: This has encoding issues
instance PrettyVal ByteString where
  prettyVal = prettyVal . BSC.unpack

instance PrettyVal a => PrettyVal (IntMap a) where
  prettyVal m = Con "fromList" [prettyVal . IntMap.toList $ m]

instance (PrettyVal k, PrettyVal a) => PrettyVal (Map k a) where
  prettyVal m = Con "fromList" [prettyVal . Map.toList $ m]

instance PrettyVal a => PrettyVal (Trie a) where
  prettyVal m = Con "fromList" [prettyVal . Trie.toList $ m]

#if !MIN_VERSION_pretty_show(1,6,9)
instance PrettyVal Bool where
  prettyVal True  = Con "True"  []
  prettyVal False = Con "False" []
#endif

instance PrettyVal Text where
  prettyVal = prettyVal . Text.unpack
