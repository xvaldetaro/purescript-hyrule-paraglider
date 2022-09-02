module Paraglider.Operator.DrainLeft where

import Prelude

import Control.Monad.ST.Class (class MonadST)
import Data.Either (Either(..), hush)
import FRP.Event (AnEvent, filterMap, makeEvent, subscribe)

drainLeft :: ∀ s m e a r
  . MonadST s m
  => (e -> m Unit)
  -> AnEvent m (Either e a)
  -> (AnEvent m a -> r)
  -> AnEvent m r
drainLeft sink upstr cont = makeEvent \k -> do
  k (cont $ filterMap hush upstr)
  subscribe upstr case _ of
    Left e -> sink e
    Right _ -> pure unit
