module Paraglider.Operator.FromEffect where

import Prelude

import Effect (Effect)
import FRP.Event (Event, makeEvent)

fromEffect :: Effect ~> Event
fromEffect e = makeEvent \k -> (e >>= k) *> pure (pure unit)