module Paraglider.Operator.DistinctUntilChanged where

import Prelude

import Control.Monad.ST.Class (class MonadST)
import Data.Filterable (filterMap)
import Data.Maybe (Maybe(..))
import FRP.Event (AnEvent, withLast)

distinctUntilChanged :: ∀ a m s. Eq a => MonadST s m => AnEvent m a -> AnEvent m a
distinctUntilChanged = filterMap go <<< withLast
  where
    go {now, last} = if (Just now) == last then Nothing else Just now
