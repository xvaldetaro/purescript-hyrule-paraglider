module Paraglider.Operator.ToClosure where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST.Global (Global)
import Effect (Effect)
import FRP.Event (Event, makeEvent, makeLemmingEvent)

toClosure :: forall r a. Effect a -> (a -> r) -> Event r
toClosure upstreamEffect closure = makeEvent \downstreamPush -> do
  upstream <- upstreamEffect
  downstreamPush (closure upstream)
  pure (pure unit)

toSTClosure :: forall r a. ST Global a -> (a -> r) -> Event r
toSTClosure upstreamEffect closure = makeLemmingEvent \_ downstreamPush -> do
  upstream <- upstreamEffect
  downstreamPush (closure upstream)
  pure (pure unit)