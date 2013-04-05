{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}
-- | Version of maybe that is strict in its argument
module IdeSession.Strict.Maybe (
    StrictMaybe(..) -- Abstract
  , toMaybe
  , fromMaybe
  ) where

import Prelude hiding (Nothing, Just)
import qualified Prelude
import Data.Aeson.TH (deriveJSON)

data StrictMaybe a = Nothing | Just !a
  deriving (Eq, Functor)

$(deriveJSON id ''StrictMaybe)

toMaybe :: StrictMaybe a -> Maybe a
toMaybe Nothing  = Prelude.Nothing
toMaybe (Just x) = Prelude.Just x

fromMaybe :: Maybe a -> StrictMaybe a
fromMaybe Prelude.Nothing  = Nothing
fromMaybe (Prelude.Just x) = Just x
