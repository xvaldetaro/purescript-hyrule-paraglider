module Paraglider.Operator.DoOnSubscribe where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST.Global (Global)
import Effect (Effect)
import FRP.Event (Event, makeEvent, makeLemmingEvent, subscribe)

-- | Calls `work` upon Subscription. `work` is a callback that receives the "push" function of the
-- | Event as argument. `work`'s return is an Effectful computation
doOnSubscribe :: ∀ a. ((a -> Effect Unit) -> Effect Unit) -> Event a -> Event a
doOnSubscribe work upstream = makeEvent \downstreamPush -> do
  work downstreamPush
  subscribe upstream downstreamPush

doOnSubscribePure :: ∀ a. ((a -> ST Global Unit) -> ST Global Unit) -> Event a -> Event a
doOnSubscribePure work upstream = makeLemmingEvent \sub downstreamPush -> do
  work downstreamPush
  sub upstream downstreamPush