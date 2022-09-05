module Paraglider.Operator.MakeEventAff where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, error, joinFiber, killFiber, launchAff, launchAff_, try)
import Effect.Class (liftEffect)
import FRP.Event (Event, makeEvent)

-- | A version of makeLemmingEvent that operates on Aff instead of Effect
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
