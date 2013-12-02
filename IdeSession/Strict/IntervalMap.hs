module IdeSession.Strict.IntervalMap (
    StrictIntervalMap
  , dominators
  , fromList
  , toList
  , empty
  , insert
    -- * Re-exports
  , Interval(..)
  ) where

import Data.IntervalMap.FingerTree (Interval(..), IntervalMap)
import qualified Data.IntervalMap.FingerTree as IntervalMap
import Text.Show.Pretty

{-
  We maintain an interval spanning the entire map, in order to support a toList
  operation.
-}

data StrictIntervalMap v a = StrictIntervalMap {
    toLazyIntervalMap :: !(IntervalMap v a)
  , maxInterval       :: !(Maybe (Interval v))
  }

instance (Ord v, Show v, Show a) => Show (StrictIntervalMap v a) where
  show m = "fromList " ++ show (toList m)

instance (Ord v, PrettyVal v, PrettyVal a) => PrettyVal (StrictIntervalMap v a) where
  prettyVal m = Con "fromList" [prettyVal . map flattenIntervals . toList $ m]
    where
      flattenIntervals :: (Interval v, a) -> ((v, v), a)
      flattenIntervals (Interval lo hi, a) = ((lo, hi), a)

unionInterval :: Ord v => Interval v -> Maybe (Interval v) -> Maybe (Interval v)
unionInterval i@(Interval low high) Nothing =
  low `seq` high `seq` Just i
unionInterval (Interval low1 high1) (Just (Interval low2 high2)) =
  let low  = min low1  low2
      high = max high1 high2
  in low `seq` high `seq` Just (Interval low high)

dominators :: Ord v => Interval v -> StrictIntervalMap v a -> [(Interval v, a)]
dominators i = IntervalMap.dominators i . toLazyIntervalMap

empty :: Ord v => StrictIntervalMap v a
empty = StrictIntervalMap IntervalMap.empty Nothing

insert :: Ord v => Interval v -> a -> StrictIntervalMap v a -> StrictIntervalMap v a
insert i a m =
  a `seq` StrictIntervalMap {
      toLazyIntervalMap = IntervalMap.insert i a $ toLazyIntervalMap m
    , maxInterval       = unionInterval i        $ maxInterval m
    }

fromList :: Ord v => [(Interval v, a)] -> StrictIntervalMap v a
fromList = foldr (\(i, a) m -> insert i a m) empty

toList :: Ord v => StrictIntervalMap v a -> [(Interval v, a)]
toList m = case maxInterval m of
             Nothing -> []
             Just i  -> IntervalMap.intersections i (toLazyIntervalMap m)
