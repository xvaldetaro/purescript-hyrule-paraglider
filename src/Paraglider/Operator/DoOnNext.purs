module Paraglider.Operator.DoOnNext where

import Prelude

import FRP.Event (AnEvent, makeEvent, subscribe)

-- | Calls `work` on every emission. `work` is a callback that receives the emitted a as argument. `work`'s return is an Effectful computation
doOnNext :: ∀ m a. Bind m => (a -> m Unit) -> AnEvent m a -> AnEvent m a
doOnNext work upstream = makeEvent \downstreamPush -> do
  subscribe upstream \a -> do
    work a
    downstreamPush a
