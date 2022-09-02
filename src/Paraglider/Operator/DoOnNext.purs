module Paraglider.Operator.DoOnNext where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST.Global (Global)
import Effect (Effect)
import FRP.Event (Event, makeEvent, makeLemmingEvent, subscribe)

-- | Calls `work` on every emission. `work` is a callback that receives the emitted a as argument. `work`'s return is an Effectful computation
doOnNext :: ∀ a. (a -> Effect Unit) -> Event a -> Event a
doOnNext work upstream = makeEvent \downstreamPush -> do
  subscribe upstream \a -> do
    work a
    downstreamPush a

doOnNextPure :: ∀ a. (a -> ST Global Unit) -> Event a -> Event a
doOnNextPure work upstream = makeLemmingEvent \sub downstreamPush -> do
  sub upstream \a -> do
    work a
    downstreamPush a
