module Paraglider.Operator.BlockingGetN where

import Prelude

import Data.Array (length, snoc)
import Data.Filterable (filter)
import Effect.Aff (Aff)
import FRP.Event (Event)
import FRP.Event.Class (fold)
import Paraglider.Operator.ToAff (toAff)

-- | Will collect `i` emissions. Then will push that to the returned Aff and unsubscribe from upstream
blockingGetN :: ∀ a. Int -> Event a -> Aff (Array a)
blockingGetN i e = toAff $ filter (length >>> eq i) aggregatedEvent
  where
  aggregatedEvent = fold (flip snoc) e []