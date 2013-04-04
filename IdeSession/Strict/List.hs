{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- | Strict lists
module IdeSession.Strict.List (
  -- * Strict lists
    StrictList -- Abstract
  , singleton
  , map
  , concat
  , fromList
  , toList
  ) where

import Prelude hiding (map, concat, concatMap)
import Control.Applicative
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)

data StrictList a = Nil | Cons !a !(StrictList a)
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Applicative StrictList where
  pure = singleton
  Nil         <*> _           = Nil
  _           <*> Nil         = Nil
  (Cons f fs) <*> (Cons x xs) = Cons (f x) (fs <*> xs)

map :: (a -> b) -> StrictList a -> StrictList b
map _ Nil         = Nil
map f (Cons x xs) = Cons (f x) (map f xs)

concat :: StrictList (StrictList a) -> StrictList a
concat Nil                    = Nil
concat (Cons Nil         xss) = concat xss
concat (Cons (Cons x xs) xss) = Cons x (concat (Cons xs xss))

concatMap :: (a -> StrictList b) -> StrictList a -> StrictList b
concatMap f = concat . map f

instance Monad StrictList where
  return = singleton
  (>>=)  = flip concatMap

singleton :: a -> StrictList a
singleton x = Cons x Nil

fromList :: [a] -> StrictList a
fromList []     = Nil
fromList (x:xs) = Cons x (fromList xs)

toList :: StrictList a -> [a]
toList Nil         = []
toList (Cons x xs) = x : toList xs
