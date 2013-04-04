module IdeSession.Util (
    applyMapDiff
  , showExWithClass
  , accessorName
  , lookup'
  ) where

import Data.Typeable (typeOf)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Control.Exception as Ex
import Data.Accessor (Accessor, accessor)

applyMapDiff :: Ord k => Map k (Maybe v) -> Map k v -> Map k v
applyMapDiff diff m =
  let f m2 (k, v) = Map.alter (const v) k m2
  in List.foldl' f m $ Map.toList diff

-- | Show an exception together with its most precise type tag.
showExWithClass :: Ex.SomeException -> String
showExWithClass (Ex.SomeException ex) = show (typeOf ex) ++ ": " ++ show ex

-- | Translate record field '_name' to the accessor 'name'
accessorName :: String -> Maybe String
accessorName ('_' : str) = Just str
accessorName _           = Nothing

-- | Prelude.lookup as an accessor
lookup' :: Eq a => a -> Accessor [(a, b)] (Maybe b)
lookup' key =
    accessor (lookup key) $ \mVal list ->
      case mVal of
        Nothing  -> delete key list
        Just val -> override key val list
  where
    override :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
    override a b [] = [(a, b)]
    override a b ((a', b') : xs)
      | a == a'   = (a, b) : xs
      | otherwise = (a', b') : override a b xs

    delete :: Eq a => a -> [(a, b)] -> [(a, b)]
    delete _ [] = []
    delete a ((a', b') : xs)
      | a == a'   = xs
      | otherwise = (a', b') : delete a xs
