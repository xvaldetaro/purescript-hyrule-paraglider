module Paraglider.Operator.FlatMap where

import Prelude

import FRP.Event (Event, makeLemmingEvent)
import Paraglider.Util.DisposingRef as DisposingRef

-- | Flatten a nested `Event`, reporting values only from the all inner Events.
flatMap :: ∀ a b. (a -> Event b) -> Event a -> Event b
flatMap f e = makeLemmingEvent \subscribe k -> do
  disposingRef <- DisposingRef.create
  upstreamDisposable <- subscribe e \a -> do
    innerDisposable <- subscribe (f a) k
    DisposingRef.addSub disposingRef innerDisposable
  pure $ upstreamDisposable *> DisposingRef.dispose disposingRef