module Paraglider.Operator.Timeout where

import Prelude

import Data.DateTime.Instant (unInstant)
import Data.Int (floor)
import Data.Time.Duration (Milliseconds(..))
import Effect.Now (now)
import Effect.Timer (clearTimeout, setTimeout)
import FRP.Event (Event, makeEvent)

timeout :: Int -> Event Unit
timeout durationMillis = makeEvent \k -> do
  timeoutId <- setTimeout durationMillis (k unit)
  pure $ clearTimeout timeoutId

timeoutAt :: Milliseconds -> Event Unit
timeoutAt (Milliseconds timestamp) = makeEvent \k -> do
  (Milliseconds time) <- unInstant <$> now
  timeoutId <- setTimeout (floor $ timestamp - time) (k unit)
  pure $ clearTimeout timeoutId