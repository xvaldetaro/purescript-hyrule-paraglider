module Paraglider.Rx where

import Prelude

import Control.Monad.ST.Class (class MonadST)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import FRP.Event (AnEvent, create, makeEvent, subscribe)
import FRP.Event.Class (biSampleOn)
import Paraglider.DisposingRef as DisposingRef
import Paraglider.STRefWrapper as RefW

-- / A "hot on demand" Event. Connects/Disconnects downstream subscriptions to upstream based on
-- / `connect` field being executed. (`connect` returns the unsubscription Effecvgt)
type ConnectableEvent m a = { connect :: m (m Unit), event :: AnEvent m a }

-- / Adds `work`` to be done upon Unsubscription
doOnUnsubscribe :: ∀ m a. Applicative m => Bind m => m Unit -> AnEvent m a -> AnEvent m a
doOnUnsubscribe work upstream = makeEvent \downstreamPush -> do
  upstreamDisposable <- subscribe upstream downstreamPush
  pure $ upstreamDisposable *> work

-- / Calls `work` upon Subscription. `work` is a callback that receives the "push" function of the
-- / Event as argument. `work`'s return is an Effectful computation
doOnSubscribe :: ∀ m a. Apply m => Bind m => ((a -> m Unit) -> m Unit) -> AnEvent m a -> AnEvent m a
doOnSubscribe work upstream = makeEvent \downstreamPush -> do
  work downstreamPush
  subscribe upstream downstreamPush

toConnectable :: ∀ m s a. MonadST s m => AnEvent m a -> m (ConnectableEvent m a)
toConnectable upstream = do
  { event, push } <- create
  let connect = subscribe upstream push
  pure { event, connect }

-- / Just composing replay and refCount
replayRefCount :: forall a s m. Bind m => MonadST s m => AnEvent m a -> m (AnEvent m a)
replayRefCount = replay >=> refCount

-- / Creates 1 upstream subscription for N downstream subscribers. The upstream subscription is
-- / started when the first downstream subscription happens. The upstream subscription is terminated
-- / when the last downstream subscription is terminated.
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

-- / Once upstream is connected it will cache the last emitted value from upstream. Any downstream
-- / subscribers will get an immediate emission of the last cached value upon subscription (if any)
replay :: ∀ a m s. MonadST s m => AnEvent m a -> m (ConnectableEvent m a)
replay upstream = do
  lastEmissionRef <- RefW.new Nothing
  { connect, event } <- toConnectable upstream
  let
    connectWithWriting :: m (m Unit)
    connectWithWriting = do
      let onUnsub = RefW.write Nothing lastEmissionRef
      writeRefSub <- subscribe event \emission -> RefW.write (Just emission) lastEmissionRef
      c1 <- connect
      pure $ c1 *> writeRefSub *> onUnsub

    withReplay :: AnEvent m a
    withReplay = flip doOnSubscribe event \push -> do
      mbLastEmission <- RefW.read lastEmissionRef
      for_ mbLastEmission push

  pure { connect: connectWithWriting, event: withReplay }

-- / Terminates upstream subscription after `n` emissions
take :: ∀ a m s. MonadST s m => Applicative m => Int -> AnEvent m a -> AnEvent m a
take n e = makeEvent \k -> do
  subRef <- DisposingRef.create
  countRef <- RefW.new n
  sub <- subscribe e \a -> do
    count <- RefW.read countRef
    when (count == 1) $ DisposingRef.dispose subRef
    when (count > 0) do
      RefW.write (count - 1) countRef
      k a
  DisposingRef.addSub subRef sub
  pure $ DisposingRef.dispose subRef

-- / When an item is emitted by either one of the upstream Events will call `f` to combine the items
-- / and emit that to downstream
combineLatest
  :: ∀ a b c m s. MonadST s m => (a -> b -> c) -> AnEvent m a -> AnEvent m b -> AnEvent m c
combineLatest f e1 e2 = biSampleOn e2 $ f <$> e1

combineLatest3
  :: ∀ a b c d m s
   . MonadST s m
  => (a -> b -> c -> d)
  -> AnEvent m a
  -> AnEvent m b
  -> AnEvent m c
  -> AnEvent m d
combineLatest3 f e1 e2 e3 = biSampleOn e3 $ biSampleOn e2 (f <$> e1)