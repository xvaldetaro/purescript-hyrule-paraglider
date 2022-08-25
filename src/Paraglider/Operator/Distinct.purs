module Paraglider.Operator.DistinctUntilChanged where

import Prelude

import Control.Monad.ST.Class (class MonadST)
import Data.Filterable (filterMap)
import Data.Maybe (Maybe(..), maybe)
import FRP.Event (AnEvent, withLast)

-- | Filters consecutive repeated emissions
distinctUntilChanged :: ∀ a m s. Eq a => MonadST s m => AnEvent m a -> AnEvent m a
distinctUntilChanged = distinctUntilChangedBy identity

-- | Filters consecutive repeated emissions. Accepts `f` to get an Eq field (like an id) from a
distinctUntilChangedBy :: ∀ a b m s. Eq b => MonadST s m => (a -> b) -> AnEvent m a -> AnEvent m a
distinctUntilChangedBy f = filterMap go <<< withLast
  where
    go {now, last} = if (Just $ f now) == (f <$> last) then Nothing else Just now

-- | Filters consecutive repeated emissions. Accepts `f` as a comparator function between last and
-- | current emissions. NOTE THAT when f returns `true` it means that elements are equal therefore
-- | it will NOT EMIT DOWNSTREAM
distinctUntilChangedF :: ∀ a m s. MonadST s m => (a -> a -> Boolean) -> AnEvent m a -> AnEvent m a
distinctUntilChangedF f = filterMap go <<< withLast
  where
    go {now, last} = maybe (Just now) (\l -> if (f l now) then Nothing else Just now) last