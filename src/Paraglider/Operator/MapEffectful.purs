module Paraglider.Operator.MapEffectful where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST.Global (Global)
import Effect (Effect)
import FRP.Event (Event, makeEvent, makeLemmingEvent, subscribe)

-- | Calls `work` on every emission. `work` is a callback that receives the emitted a as argument. `work`'s return is an Effectful computation
mapEffectful :: ∀ a b. (a -> Effect b) -> Event a -> Event b
mapEffectful  work upstream = makeEvent \downstreamPush -> do
  subscribe upstream \a -> do
    b <- work a
    downstreamPush b

mapSTful :: ∀ a b. (a -> ST Global b) -> Event a -> Event b
mapSTful  work upstream = makeLemmingEvent \sub downstreamPush -> do
  sub upstream \a -> do
    b <- work a
    downstreamPush b