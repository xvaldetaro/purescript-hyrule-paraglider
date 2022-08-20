module Paraglider.Operator.RefCount where

import Prelude

import Control.Monad.ST.Class (class MonadST)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import FRP.Event (AnEvent)
import Paraglider.Operator.ConnectableEvent (ConnectableEvent)
import Paraglider.Operator.DoOnSubscribe (doOnSubscribe)
import Paraglider.Operator.DoOnUnsubscribe (doOnUnsubscribe)
import Paraglider.Util.STRefWrapper as RefW

-- | Creates 1 upstream subscription for N downstream subscribers. The upstream subscription is
-- | started when the first downstream subscription happens. The upstream subscription is terminated
-- | when the last downstream subscription is terminated.
refCount :: ∀ a m s. MonadST s m => ConnectableEvent m a -> m (AnEvent m a)
refCount { connect, event } = do
  subCountRef <- RefW.new 0
  upstreamSubRef <- RefW.new Nothing
  let
    onDownstreamSub :: (a -> m Unit) -> m Unit
    onDownstreamSub _ = do
      subCount <- RefW.modify (_ + 1) subCountRef
      when (subCount == 1) do
        upstreamSub <- connect
        RefW.write (Just upstreamSub) upstreamSubRef

    onDownstreamUnsub :: m Unit
    onDownstreamUnsub = do
      subCount <- RefW.modify (_ - 1) subCountRef
      when (subCount == 0) do
        mbUpstreamSub <- RefW.read upstreamSubRef
        for_ mbUpstreamSub identity
        RefW.write Nothing upstreamSubRef

  pure $ doOnSubscribe onDownstreamSub $ doOnUnsubscribe onDownstreamUnsub event