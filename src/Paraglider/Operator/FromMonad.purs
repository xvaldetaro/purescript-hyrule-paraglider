module Paraglider.Operator.FromMOnad where

import Prelude

import FRP.Event (AnEvent, makeEvent)

fromMonad :: ∀ a m. Monad m => m a -> AnEvent m a
fromMonad monad = makeEvent \k -> do
  emission <- monad
  k emission
  pure $ pure unit