{-# LANGUAGE TemplateHaskell, DeriveFunctor #-}
-- | Version of maybe that is strict in its argument
module IdeSession.Strict.Maybe (
    nothing
  , just
  , maybe
  ) where

import Prelude hiding (maybe)
import IdeSession.Strict.Container
import qualified Data.Maybe as Maybe

nothing :: Strict Maybe a
nothing = force $ Nothing

just :: a -> Strict Maybe a
just = force . Just

maybe :: b -> (a -> b) -> Strict Maybe a -> b
maybe x f =  Maybe.maybe x f . toLazyMaybe
