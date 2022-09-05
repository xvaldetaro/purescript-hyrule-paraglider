module Paraglider.Operator.Combine where

import Prelude

import Data.Foldable (foldl)
import Data.Traversable (sequence)
import FRP.Event (Event)

combineFold' :: ∀ b. Monoid b => Array (Event b) -> Event b
combineFold' = combineFold append mempty

-- | Folds over the **last** emissions from upstream events, emits the folded result to downstream.
-- |
-- | Notice that this is different from `folded`. In `folded` we keep folding over every upstream
-- | emission since the start. Effectively making all upstream emissions (including previous emissions)
-- | partof the fold. Whereas in `combineFold` we fold over the **last** round of emissions from
-- | upstream only.
combineFold :: ∀ a b. (a -> b -> b) -> b -> Array (Event a) -> Event b
combineFold f initial xs = foldl (flip f) initial <$> sequence xs

-- | When an item is emitted by either one of the upstream Events will call `f` to combine the items
-- | and emit that to downstream
combineLatest :: ∀ a b c. (a -> b -> c) -> Event a -> Event b -> Event c
combineLatest f e1 e2 = f <$> e1 <*> e2

combineLatest3 :: ∀ a b c d. (a -> b -> c -> d) -> Event a -> Event b -> Event c -> Event d
combineLatest3 f e1 e2 e3 = f <$> e1 <*> e2 <*> e3