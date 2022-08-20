module Paraglider.Operator.FlatMap where

import Prelude

import Control.Monad.ST.Class (class MonadST)
import FRP.Event (AnEvent, makeEvent, subscribe)
import Paraglider.Util.DisposingRef as DisposingRef

-- | Flatten a nested `Event`, reporting values only from the all inner Events.
flatMap :: ∀ s m a b. MonadST s m => (a -> AnEvent m b) -> AnEvent m a -> AnEvent m b
flatMap f e = makeEvent \k -> do
  disposingRef <- DisposingRef.create
  upstreamDisposable <- subscribe e \a -> do
    innerDisposable <- subscribe (f a) k
    DisposingRef.addSub disposingRef innerDisposable
  pure $ upstreamDisposable *> DisposingRef.dispose disposingRef