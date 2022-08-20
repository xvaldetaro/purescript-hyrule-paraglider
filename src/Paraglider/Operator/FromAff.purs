module Paraglider.Operator.FromAff where

import Prelude

import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (Event, makeEvent)

-- if the cancelers cancels, theoretically we don't need to unsubscribe
-- as nothing will be firing
fromAff :: Aff ~> Event
fromAff a = makeEvent \k -> launchAff_ (a >>= liftEffect <<< k) *> pure (pure unit)