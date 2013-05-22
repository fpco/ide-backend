{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- | Strict lists
module IdeSession.Strict.List (
    nil
  , cons
  , singleton
  , map
  , all
  , any
  , reverse
  , (++)
  , elem
  , (\\)
  ) where

import Prelude hiding (map, all, any, reverse, (++), elem)
import qualified Data.List as List

import IdeSession.Strict.Container

nil :: Strict [] a
nil = StrictList []

cons :: a -> Strict [] a -> Strict [] a
cons x xs = x `seq` StrictList (x : toLazyList xs)

singleton :: a -> Strict [] a
singleton x = x `seq` StrictList [x]

map :: (a -> b) -> Strict [] a -> Strict [] b
map f = force . List.map f . toLazyList

all :: (a -> Bool) -> Strict [] a -> Bool
all p = List.all p . toLazyList

any :: (a -> Bool) -> Strict [] a -> Bool
any p = List.any p . toLazyList

elem :: Eq a => a -> Strict [] a -> Bool
elem x = List.elem x . toLazyList

reverse :: Strict [] a -> Strict [] a
reverse = StrictList . List.reverse . toLazyList

(++) :: Strict [] a -> Strict [] a -> Strict [] a
xs ++ ys = StrictList $ toLazyList xs List.++ toLazyList ys

(\\) :: Eq a => Strict [] a -> Strict [] a -> Strict [] a
xs \\ ys = StrictList $ toLazyList xs List.\\ toLazyList ys
