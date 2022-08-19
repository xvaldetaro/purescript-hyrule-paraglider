module Paraglider.Operator.ToConnectable where

import Prelude

import Control.Monad.ST.Class (class MonadST)
import FRP.Event (AnEvent, create, subscribe)
import Paraglider.Operator.ConnectableEvent (ConnectableEvent)

toConnectable :: ∀ m s a. MonadST s m => AnEvent m a -> m (ConnectableEvent m a)
toConnectable upstream = do
  { event, push } <- create
  let connect = subscribe upstream push
  pure { event, connect }