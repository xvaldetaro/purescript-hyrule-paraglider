module Paraglider.Operator.MemoBeh (memoBeh, memoBeh') where

import Prelude

import Control.Monad.ST.Class (class MonadST, liftST)
import Control.Monad.ST.Internal as STRef
import Data.Maybe (Maybe(..), maybe)
import FRP.Event (AnEvent, create, makeEvent, subscribe)

-- | A memoized event that acts like a behavior, emitting immediately
-- | the most recent value to all subscribers.
memoBeh :: forall m a t r. Applicative m => MonadST t m => AnEvent m a -> a -> (AnEvent m a -> r) -> AnEvent m r
memoBeh e a f = memoBehInternal e (Just a) f

-- | A memoized event that acts like a behavior, emitting immediately
-- | the most recent value to all subscribers.
memoBeh' :: forall m a t r. Applicative m => MonadST t m => AnEvent m a -> (AnEvent m a -> r) -> AnEvent m r
memoBeh' e f = memoBehInternal e Nothing f

-- | A memoized event that acts like a behavior, emitting immediately
-- | the most recent value to all subscribers.
memoBehInternal :: forall m a t r. Applicative m => MonadST t m => AnEvent m a -> Maybe a -> (AnEvent m a -> r) -> AnEvent m r
memoBehInternal e mbA f = makeEvent \k -> do
  { push, event } <- create
  current <- liftST (STRef.new mbA)
  let
    writeVal v = liftST (STRef.write (Just v) current) :: m (Maybe a)
    event' = makeEvent \k' -> do
      liftST (STRef.read current) >>= maybe (pure unit) k'
      subscribe event k'
  k (f event')
  subscribe e (\v -> writeVal v *> push v)