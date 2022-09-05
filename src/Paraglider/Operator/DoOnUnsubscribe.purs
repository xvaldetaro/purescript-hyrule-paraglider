module Paraglider.Operator.DoOnUnsubscribe where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST.Global (Global)
import Effect (Effect)
import FRP.Event (Event, makeEvent, makeLemmingEvent, subscribe)

-- | Adds `work`` to be done upon Unsubscription
doOnUnsubscribe :: Effect Unit -> Event ~> Event
doOnUnsubscribe work upstream = makeEvent \downstreamPush -> do
  upstreamDisposable <- subscribe upstream downstreamPush
  pure $ upstreamDisposable *> work

doOnUnsubscribePure :: ST Global Unit -> Event ~> Event
doOnUnsubscribePure work upstream = makeLemmingEvent \sub downstreamPush -> do
  upstreamDisposable <- sub upstream downstreamPush
  pure $ upstreamDisposable *> work