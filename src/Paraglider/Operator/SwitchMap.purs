module Paraglider.Operator.SwitchMap where

import Prelude

import Control.Monad.ST.Class (class MonadST)
import FRP.Event (AnEvent, keepLatest)

-- | Flatten a nested `Event`, reporting values only from the most recent
-- | inner `Event`.
switchMap :: ∀ a b m s. MonadST s m => (a -> AnEvent m b) -> AnEvent m a -> AnEvent m b
switchMap f e = keepLatest (f <$> e)
