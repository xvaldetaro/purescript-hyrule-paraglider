module Paraglider.Operator.Combine where

import Prelude

import Control.Monad.ST.Class (class MonadST)
import Data.Array (foldl)
import Data.Traversable (sequence)
import FRP.Event (AnEvent)

combineFold' :: ∀ b m s . MonadST s m => Monoid b => Array (AnEvent m b) -> AnEvent m b
combineFold' = combineFold append mempty

-- | Folds over the **last** emissions from upstream events, emits the folded result to downstream.
-- |
-- | Notice that this is different from `folded`. In `folded` we keep folding over every upstream
-- | emission since the start. Effectively making all upstream emissions (including previous emissions)
-- | partof the fold. Whereas in `combineFold` we fold over the **last** round of emissions from
-- | upstream only.
combineFold :: ∀ a b m s . MonadST s m => (a -> b -> b) -> b -> Array (AnEvent m a) -> AnEvent m b
combineFold f initial xs = foldl (flip f) initial <$> sequence xs

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