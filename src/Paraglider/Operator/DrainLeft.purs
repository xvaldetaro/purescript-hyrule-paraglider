module Paraglider.Operator.DrainLeft where

import Prelude

import Data.Either (Either(..), hush)
import Effect (Effect)
import FRP.Event (Event, filterMap, makeEvent, subscribe)

drainLeft :: ∀ e a r. (e -> Effect Unit) -> Event (Either e a) -> (Event a -> r) -> Event r
drainLeft sink upstr cont = makeEvent \k -> do
  k (cont $ filterMap hush upstr)
  subscribe upstr case _ of
    Left e -> sink e
    Right _ -> pure unit
