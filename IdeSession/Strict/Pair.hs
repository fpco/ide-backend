-- | Strict pairs
--
-- Unfortunately, this doesn't fit into the Strict.Container hierarchy
-- (different kind)
module IdeSession.Strict.Pair (
    StrictPair
  , toLazyPair
  , fromLazyPair
  ) where

newtype StrictPair a b = StrictPair { toLazyPair :: (a, b) }

fromLazyPair :: (a, b) -> StrictPair a b
fromLazyPair (a, b) = a `seq` b `seq` StrictPair (a, b)
