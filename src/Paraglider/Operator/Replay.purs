module Paraglider.Operator.Replay where

import Prelude

import Control.Monad.ST (ST)
import Control.Monad.ST.Global (Global)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import FRP.Event (Event, subscribe, subscribePure)
import Paraglider.Operator.ConnectableEvent (ConnectableEvent)
import Paraglider.Operator.DoOnSubscribe (doOnSubscribe)
import Paraglider.Operator.RefCount (refCount, refCountPure)
import Paraglider.Operator.ToConnectable (toConnectable, toConnectablePure)
import Paraglider.Util.STRefWrapper as RefW

-- | Once upstream is connected it will cache the last emitted value from upstream. Any downstream
-- | subscribers will get an immediate emission of the last cached value upon subscription (if any)
replay :: ∀ a. Event a -> Effect (ConnectableEvent Effect a)
replay upstream = do
  lastEmissionRef <- RefW.new Nothing
  { connect, event } <- toConnectable upstream
  let
    connectWithWriting :: Effect (Effect Unit)
    connectWithWriting = do
      let onUnsub = RefW.write Nothing lastEmissionRef
      writeRefSub <- subscribe event \emission -> RefW.write (Just emission) lastEmissionRef
      c1 <- connect
      pure $ c1 *> writeRefSub *> onUnsub

    withReplay :: Event a
    withReplay = flip doOnSubscribe event \push -> do
      mbLastEmission <- RefW.read lastEmissionRef
      for_ mbLastEmission push

  pure { connect: connectWithWriting, event: withReplay }

-- | Just composing replay and refCount
replayRefCount :: forall a. Event a -> Effect (Event a)
replayRefCount = replay >=> refCount

replayPure :: ∀ a. Event a -> ST Global (ConnectableEvent (ST Global) a)
replayPure upstream = do
  lastEmissionRef <- RefW.new Nothing
  { connect, event } <- toConnectablePure upstream
  let
    connectWithWriting :: ST Global (ST Global Unit)
    connectWithWriting = do
      let onUnsub = RefW.write Nothing lastEmissionRef
      writeRefSub <- subscribePure event \emission -> RefW.write (Just emission) lastEmissionRef
      c1 <- connect
      pure $ c1 *> writeRefSub *> onUnsub

    withReplay :: Event a
    withReplay = flip doOnSubscribe event \push -> do
      mbLastEmission <- RefW.read lastEmissionRef
      for_ mbLastEmission push

  pure { connect: connectWithWriting, event: withReplay }

-- | Just composing replay and refCount
replayRefCountPure :: forall a. Event a -> ST Global (Event a)
replayRefCountPure = replayPure >=> refCountPure