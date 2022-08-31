module Paraglider.Operator.FromAff where

import Prelude

import Data.Bifunctor (lmap)
import Data.Either (Either)
import Effect.Aff (Aff, Error, launchAff_, try)
import Effect.Class (liftEffect)
import FRP.Event (Event, makeEvent)

-- if the cancelers cancels, theoretically we don't need to unsubscribe
-- as nothing will be firing
fromAff :: Aff ~> Event
fromAff a = makeEvent \k -> launchAff_ (a >>= liftEffect <<< k) *> pure (pure unit)

fromAffSafe :: ∀ a e. (Error -> e) -> Aff a -> Event (Either e a)
fromAffSafe f a = makeEvent \k -> launchAff_ (a' >>= liftEffect <<< k) *> pure (pure unit)
  where
  a' :: Aff (Either e a)
  a' = lmap f <$> try a