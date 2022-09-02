module Paraglider.Operator.ToConnectable where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST.Global (Global)
import Effect (Effect)
import FRP.Event (Event, create, createPure, subscribe, subscribePure)
import Paraglider.Operator.ConnectableEvent (ConnectableEvent)

toConnectable :: ∀ a. Event a -> Effect (ConnectableEvent Effect a)
toConnectable upstream = do
  { event, push } <- create
  let connect = subscribe upstream push
  pure { event, connect }

toConnectablePure :: ∀ a. Event a -> ST Global (ConnectableEvent (ST Global) a)
toConnectablePure upstream = do
  { event, push } <- createPure
  let connect = subscribePure upstream push
  pure { event, connect }