module Paraglider.Operator.MakeEventAff where

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

-- / A version of makeEvent that operates on Aff instead of Effect
makeEventAff :: ∀ a. ((a -> Effect Unit) -> Aff (Effect Unit)) -> Event a
makeEventAff cb = makeEvent \k -> do
  fiber <- launchAff $ cb k
  pure $ launchAff_ do
    killFiber (error "Event Unsubscribed") fiber
    eiCleanup <- try $ joinFiber fiber
    case eiCleanup of
      -- We killed the Fiber before it emitted its result
      Left _ -> pure unit
      -- The Fiber completed and we joined with its result
      Right cleanup -> liftEffect cleanup
