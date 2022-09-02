module Paraglider.Operator.SwitchMap where

import Prelude

import FRP.Event (Event, keepLatest)

-- | Flatten a nested `Event`, reporting values only from the most recent
-- | inner `Event`.
switchMap :: ∀ a b. (a -> Event b) -> Event a -> Event b
switchMap f e = keepLatest (f <$> e)
