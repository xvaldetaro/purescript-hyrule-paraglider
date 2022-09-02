module Paraglider.Operator.Lift where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST.Global (Global)
import Effect (Effect)
import FRP.Event (Event, makeEvent, makeLemmingEvent)

-- | Lifts a monad into event, resulting in a single emission.
-- | Different that `pure` in that it also triggers the monadic side effect
-- | in addition to emitting the value.
liftPure :: ∀ a. ST Global a -> Event a
liftPure monad = makeLemmingEvent \_ k -> do
  emission <- monad
  k emission
  pure $ pure unit

lift :: ∀ a. Effect a -> Event a
lift monad = makeEvent \k -> do
  emission <- monad
  k emission
  pure $ pure unit