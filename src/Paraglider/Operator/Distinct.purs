module Paraglider.Operator.DistinctUntilChanged where

import Prelude

import Data.Filterable (filterMap)
import Data.Maybe (Maybe(..), maybe)
import FRP.Event (Event, withLast)

-- | Filters consecutive repeated emissions
distinctUntilChanged :: ∀ a. Eq a => Event a -> Event a
distinctUntilChanged = distinctUntilChangedBy identity

-- | Filters consecutive repeated emissions. Accepts `f` to get an Eq field (like an id) from a
distinctUntilChangedBy :: ∀ a b. Eq b => (a -> b) -> Event a -> Event a
distinctUntilChangedBy f = filterMap go <<< withLast
  where
    go {now, last} = if (Just $ f now) == (f <$> last) then Nothing else Just now

-- | Filters consecutive repeated emissions. Accepts `f` as a comparator function between last and
-- | current emissions. NOTE THAT when f returns `true` it means that elements are equal therefore
-- | it will NOT EMIT DOWNSTREAM
distinctUntilChangedF :: ∀ a. (a -> a -> Boolean) -> Event a -> Event a
distinctUntilChangedF f = filterMap go <<< withLast
  where
    go {now, last} = maybe (Just now) (\l -> if (f l now) then Nothing else Just now) last