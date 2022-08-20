module Paraglider.Operator.WithUnsubscribe where

import Prelude

import Control.Monad.ST.Class (class MonadST, liftST)
import Control.Monad.ST.Internal as Ref
import FRP.Event (AnEvent, makeEvent, subscribe)

-- | Takes an event and enriches it with its own unsubscribe function.
-- | This is useful, for example, if a downstream event or subscription needs to
-- | trigger an unsubscription.
withUnsubscribe :: forall s m a. MonadST s m =>  AnEvent m a -> AnEvent m {unsubscribe :: m Unit, value :: a}
withUnsubscribe e = makeEvent \ff -> do
  let f unsubscribe value = ff { unsubscribe, value }
  active <- liftST $ Ref.new true
  ro <- liftST $ Ref.new (pure unit)
  let
    cancel = do
      _ <- liftST $ Ref.write false active
      join (liftST $ Ref.read ro)
    f' = f cancel
    callback a = do
      whenM (liftST $ Ref.read active) (f' a)
  o <- subscribe e callback
  (liftST $ Ref.read active) >>= case _ of
    false -> o $> pure unit
    true -> liftST $ Ref.write o ro $> o

