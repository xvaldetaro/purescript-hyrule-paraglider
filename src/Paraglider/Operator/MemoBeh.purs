module Paraglider.Operator.MemoBeh (memoBeh, memoBeh') where

import Prelude

import Control.Monad.ST.Internal as STRef
import Data.Maybe (Maybe(..), maybe)
import FRP.Event (Event, createPure, makeLemmingEvent)

-- | A memoized event that acts like a behavior, emitting immediately
-- | the most recent value to all subscribers.
memoBeh :: forall a r. Event a -> a -> (Event a -> r) -> Event r
memoBeh e a f = memoBehInternal e (Just a) f

-- | A memoized event that acts like a behavior, emitting immediately
-- | the most recent value to all subscribers.
memoBeh' :: forall a r. Event a -> (Event a -> r) -> Event r
memoBeh' e f = memoBehInternal e Nothing f

-- | A memoized event that acts like a behavior, emitting immediately
-- | the most recent value to all subscribers.
memoBehInternal :: forall a r. Event a -> Maybe a -> (Event a -> r) -> Event r
memoBehInternal e mbA f = makeLemmingEvent \subscribeO k -> do
  { push, event } <- createPure
  current <- STRef.new mbA
  let
    writeVal v = STRef.write (Just v) current
    event' = makeLemmingEvent \subscribeI k' -> do
      STRef.read current >>= maybe (pure unit) k'
      subscribeI event k'
  k (f event')
  subscribeO e (\v -> writeVal v *> push v)