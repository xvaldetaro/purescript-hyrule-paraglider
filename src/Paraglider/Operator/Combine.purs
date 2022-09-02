module Paraglider.Operator.Combine where

import Prelude

import Control.Monad.ST.Class (class MonadST)
import Data.Array (length, mapWithIndex)
import Data.Filterable (filterMap)
import Data.Foldable (sequence_)
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import FRP.Event (Event, createPure, fold, makeLemmingEvent)

combineFold' :: ∀ b. Monoid b => Array (Event b) -> Event b
combineFold' = combineFold append mempty

-- | Folds over the **last** emissions from upstream events, emits the folded result to downstream.
-- |
-- | Notice that this is different from `folded`. In `folded` we keep folding over every upstream
-- | emission since the start. Effectively making all upstream emissions (including previous emissions)
-- | partof the fold. Whereas in `combineFold` we fold over the **last** round of emissions from
-- | upstream only.
combineFold :: ∀ a b. (a -> b -> b) -> b -> Array (Event a) -> Event b
combineFold f initial xs = makeLemmingEvent \subscribe downstreamPush -> do
  { push, event } <- createPure
  let
    len = length xs

    aggregateEmissionsInMap :: { i :: Int, v :: a } -> Map Int a -> Map Int a
    aggregateEmissionsInMap { i, v } ivMap = Map.insert i v ivMap

    filterPartiallyEmitted :: Map Int a -> Maybe (List a)
    filterPartiallyEmitted ivMap =
      let
        values = Map.values ivMap
      in
        if List.length values == len then Just values else Nothing

    foldEmissions :: List a -> b
    foldEmissions = List.foldl (\acc v -> f v acc) initial

    aggrEv :: Event (Map Int a)
    aggrEv = fold aggregateEmissionsInMap event Map.empty

    foldedEvent :: Event b
    foldedEvent = aggrEv
      # filterMap filterPartiallyEmitted
      # map foldEmissions

  unsubDs <- subscribe foldedEvent downstreamPush
  unsubAll <- sequence $ mapWithIndex (\i ev -> subscribe ev (\v -> push { i, v })) xs
  pure $ unsubDs *> sequence_ unsubAll

-- | When an item is emitted by either one of the upstream Events will call `f` to combine the items
-- | and emit that to downstream
combineLatest
  :: ∀ a b c. (a -> b -> c) -> Event a -> Event b -> Event c
combineLatest f e1 e2 = f <$> e1 <*> e2

combineLatest3
  :: ∀ a b c d m s
   . MonadST s m
  => (a -> b -> c -> d)
  -> Event a
  -> Event b
  -> Event c
  -> Event d
combineLatest3 f e1 e2 e3 = f <$> e1 <*> e2 <*> e3