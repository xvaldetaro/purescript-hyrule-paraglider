module Paraglider.Operator.FromCallable where

import Prelude

import Data.Array (length, snoc)
import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Int (round)
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), Milliseconds(..), error, joinFiber, killFiber, launchAff, launchAff_, makeAff, try)
import Effect.Class (liftEffect)
import Effect.Ref as Effect.Ref
import Effect.Timer (clearTimeout, setTimeout)
import FRP.Event (AnEvent, Event, makeEvent, subscribe)
import FRP.Event.Class (fold)

fromCallable :: ∀ a. Effect a -> Event a
fromCallable e = makeEvent \k -> (e >>= k) *> pure (pure unit)