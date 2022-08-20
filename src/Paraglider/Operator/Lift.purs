module Paraglider.Operator.Lift where

import Prelude

import FRP.Event (AnEvent, makeEvent)

lift :: ∀ a m. Monad m => m a -> AnEvent m a
lift monad = makeEvent \k -> do
  emission <- monad
  k emission
  pure $ pure unit