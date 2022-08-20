module Paraglider.Operator.MapEffectful where

import Prelude

import Control.Monad.ST.Class (class MonadST)
import FRP.Event (AnEvent, makeEvent, subscribe)

-- | Calls `work` on every emission. `work` is a callback that receives the emitted a as argument. `work`'s return is an Effectful computation
mapEffectful :: ∀ s m a b. MonadST s m => (a -> m b) -> AnEvent m a -> AnEvent m b
mapEffectful  work upstream = makeEvent \downstreamPush -> do
  subscribe upstream \a -> do
    b <- work a
    downstreamPush b
