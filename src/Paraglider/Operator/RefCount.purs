module Paraglider.Operator.RefCount where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST.Global (Global)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import FRP.Event (Event)
import Paraglider.Operator.ConnectableEvent (ConnectableEvent)
import Paraglider.Operator.DoOnSubscribe (doOnSubscribe, doOnSubscribePure)
import Paraglider.Operator.DoOnUnsubscribe (doOnUnsubscribe, doOnUnsubscribePure)
import Paraglider.Util.STRefWrapper as RefW

-- | Creates 1 upstream subscription for N downstream subscribers. The upstream subscription is
-- | started when the first downstream subscription happens. The upstream subscription is terminated
-- | when the last downstream subscription is terminated.
refCount :: ∀ a. ConnectableEvent Effect a -> Effect (Event a)
refCount { connect, event } = do
  subCountRef <- RefW.new 0
  upstreamSubRef <- RefW.new Nothing
  let
    onDownstreamSub :: (a -> Effect Unit) -> Effect Unit
    onDownstreamSub _ = do
      subCount <- RefW.modify (_ + 1) subCountRef
      when (subCount == 1) do
        upstreamSub <- connect
        RefW.write (Just upstreamSub) upstreamSubRef

    onDownstreamUnsub :: Effect Unit
    onDownstreamUnsub = do
      subCount <- RefW.modify (_ - 1) subCountRef
      when (subCount == 0) do
        mbUpstreamSub <- RefW.read upstreamSubRef
        for_ mbUpstreamSub identity
        RefW.write Nothing upstreamSubRef

  pure $ doOnSubscribe onDownstreamSub $ doOnUnsubscribe onDownstreamUnsub event

refCountPure :: ∀ a. ConnectableEvent (ST Global) a -> ST Global (Event a)
refCountPure { connect, event } = do
  subCountRef <- RefW.new 0
  upstreamSubRef <- RefW.new Nothing
  let
    onDownstreamSub :: (a -> ST Global Unit) -> ST Global Unit
    onDownstreamSub _ = do
      subCount <- RefW.modify (_ + 1) subCountRef
      when (subCount == 1) do
        upstreamSub <- connect
        RefW.write (Just upstreamSub) upstreamSubRef

    onDownstreamUnsub :: ST Global Unit
    onDownstreamUnsub = do
      subCount <- RefW.modify (_ - 1) subCountRef
      when (subCount == 0) do
        mbUpstreamSub <- RefW.read upstreamSubRef
        for_ mbUpstreamSub identity
        RefW.write Nothing upstreamSubRef

  pure $ doOnSubscribePure onDownstreamSub $ doOnUnsubscribePure onDownstreamUnsub event