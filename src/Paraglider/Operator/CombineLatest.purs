module Paraglider.Operator.CombineLatest where

import Prelude

import Control.Monad.ST.Class (class MonadST)
import FRP.Event (AnEvent)

-- | When an item is emitted by either one of the upstream Events will call `f` to combine the items
-- | and emit that to downstream
combineLatest
  :: ∀ a b c m s. MonadST s m => (a -> b -> c) -> AnEvent m a -> AnEvent m b -> AnEvent m c
combineLatest f e1 e2 = f <$> e1 <*> e2

combineLatest3
  :: ∀ a b c d m s
   . MonadST s m
  => (a -> b -> c -> d)
  -> AnEvent m a
  -> AnEvent m b
  -> AnEvent m c
  -> AnEvent m d
combineLatest3 f e1 e2 e3 = f <$> e1 <*> e2 <*> e3