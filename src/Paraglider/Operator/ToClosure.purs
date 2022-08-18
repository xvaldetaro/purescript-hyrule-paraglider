module Paraglider.Operator.ToClosure where

import Prelude

import Control.Monad.ST.Class (class MonadST)
import FRP.Event (AnEvent, makeEvent)

toClosure :: forall m s r a. MonadST s m => m (AnEvent m a) -> (AnEvent m a -> r) -> AnEvent m r
toClosure upstreamEffect closure = makeEvent \downstreamPush -> do
  upstream <- upstreamEffect
  downstreamPush (closure upstream)
  pure (pure unit)
