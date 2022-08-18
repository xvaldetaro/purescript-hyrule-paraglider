module Paraglider.Operator.FromEffect where

import Prelude

import FRP.Event (AnEvent, makeEvent)

fromEffect :: ∀ a m. Monad m => m a -> AnEvent m a
fromEffect effect = makeEvent \k -> do
  emission <- effect
  k emission
  pure $ pure unit