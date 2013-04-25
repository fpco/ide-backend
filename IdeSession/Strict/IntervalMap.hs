module IdeSession.Strict.IntervalMap (
    StrictIntervalMap
  , immediateDominator
  , fromList
  , empty
  , insert
    -- * Re-exports
  , Interval(..)
  ) where

import Data.Maybe (listToMaybe)
import Data.IntervalMap.FingerTree (Interval, IntervalMap)
import qualified Data.IntervalMap.FingerTree as IntervalMap

newtype StrictIntervalMap v a = StrictIntervalMap {
    toLazyIntervalMap :: IntervalMap v a
  }

immediateDominator :: Ord v => Interval v -> StrictIntervalMap v a -> Maybe (Interval v, a)
immediateDominator i = listToMaybe . IntervalMap.dominators i . toLazyIntervalMap

empty :: Ord v => StrictIntervalMap v a
empty = StrictIntervalMap IntervalMap.empty

insert :: Ord v => Interval v -> a -> StrictIntervalMap v a -> StrictIntervalMap v a
insert i a m = a `seq` (StrictIntervalMap . IntervalMap.insert i a $ toLazyIntervalMap m)

fromList :: Ord v => [(Interval v, a)] -> StrictIntervalMap v a
fromList = foldr (\(i, a) m -> insert i a m) empty
