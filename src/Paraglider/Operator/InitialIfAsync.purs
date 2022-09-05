module Paraglider.Operator.InitialIfAsync where

import Prelude

import Control.Monad.ST.Class (liftST)
import Control.Monad.ST.Internal as Ref
import FRP.Event (Event, makeLemmingEvent)

-- | If the event emits synchronously upon subscription then does nothing.
-- | Otherwise adds `initial` as a synchronous first emission to the event
initialIfAsync :: ∀ a. a -> Event a -> Event a
initialIfAsync initial ev = makeLemmingEvent \subscribe k -> do
  didEmitRef <- liftST $ Ref.new false
  sub <- subscribe ev \v -> do
    k v
    void $ liftST $ Ref.write true didEmitRef
  didEmit <- liftST $ Ref.read didEmitRef
  unless didEmit $ k initial
  pure sub