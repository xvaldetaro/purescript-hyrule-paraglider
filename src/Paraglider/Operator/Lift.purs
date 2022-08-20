module Paraglider.Operator.Lift where

import Prelude

import FRP.Event (AnEvent, makeEvent)

-- | Lifts a monad into event, resulting in a single emission.
-- | Different that `pure` in that it also triggers the monadic side effect
-- | in addition to emitting the value.
lift :: ∀ a m. Monad m => m a -> AnEvent m a
lift monad = makeEvent \k -> do
  emission <- monad
  k emission
  pure $ pure unit