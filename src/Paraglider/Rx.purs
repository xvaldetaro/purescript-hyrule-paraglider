module Paraglider.Rx where

import Prelude

import Control.Monad.ST.Class (class MonadST)
import Data.Array (fromFoldable, length, mapWithIndex, replicate)
import Data.Array as Array
import Data.Filterable (filterMap)
import Data.Foldable (class Foldable, for_, sequence_, traverse_)
import Data.Foldable as Foldable
import Data.List (List)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Debug (spy)
import FRP.Event (AnEvent, create, fold, keepLatest, makeEvent, mapAccum, subscribe, withLast)
import Paraglider.DisposingRef as DisposingRef
import Paraglider.STRefWrapper as RefW

-- / A "hot on demand" Event. Connects/Disconnects downstream subscriptions to upstream based on
-- / `connect` field being executed. (`connect` returns the unsubscription Effecvgt)
type ConnectableEvent m a = { connect :: m (m Unit), event :: AnEvent m a }

combineFold' :: ∀ b m s . MonadST s m => Monoid b => Array (AnEvent m b) -> AnEvent m b
combineFold' = combineFold append mempty

-- / Folds over the **last** emissions from upstream events, emits the folded result to downstream.
-- /
-- / Notice that this is different from `folded`. In `folded` we keep folding over every upstream
-- / emission since the start. Effectively making all upstream emissions (including previous emissions)
-- / partof the fold. Whereas in `combineFold` we fold over the **last** round of emissions from
-- / upstream only.
combineFold :: ∀ a b m s . MonadST s m => (a -> b -> b) -> b -> Array (AnEvent m a) -> AnEvent m b
combineFold f initial xs = makeEvent \downstreamPush -> do
  {push, event} <- create
  let
      len = length xs
      aggregateEmissionsInMap :: {i :: Int, v :: a} -> Map Int a -> Map Int a
      aggregateEmissionsInMap {i, v} ivMap = Map.insert i v ivMap

      filterPartiallyEmitted :: Map Int a -> Maybe (List a)
      filterPartiallyEmitted ivMap = let values = Map.values ivMap in
        if List.length values == len then Just values else Nothing

      foldEmissions :: List a -> b
      foldEmissions = List.foldl (\acc v -> f v acc) initial

      aggrEv :: AnEvent m (Map Int a)
      aggrEv = fold aggregateEmissionsInMap event Map.empty

      foldedEvent :: AnEvent m b
      foldedEvent = aggrEv
          # filterMap filterPartiallyEmitted
            # map foldEmissions

  unsubDs <- subscribe foldedEvent downstreamPush
  unsubAll <- sequence $ mapWithIndex (\i ev -> subscribe ev (\v -> push {i, v})) xs
  pure $ unsubDs *> sequence_ unsubAll

-- / When an item is emitted by either one of the upstream Events will call `f` to combine the items
-- / and emit that to downstream
combineLatest
  :: ∀ a b c m s. MonadST s m => (a -> b -> c) -> AnEvent m a -> AnEvent m b -> AnEvent m c
combineLatest f e1 e2 = f <$> e1 <*> e2

combineLatest3
  :: ∀ a b c d m s
   . MonadST s m
  => (a -> b -> c -> d)
  -> AnEvent m a
  -> AnEvent m b
  -> AnEvent m c
  -> AnEvent m d
combineLatest3 f e1 e2 e3 = f <$> e1 <*> e2 <*> e3

distinctUntilChanged :: ∀ a m s. Eq a => MonadST s m => AnEvent m a -> AnEvent m a
distinctUntilChanged = filterMap go <<< withLast
  where
    go {now, last} = if (Just now) == last then Nothing else Just now

-- / Calls `work` on every emission. `work` is a callback that receives the emitted a as argument. `work`'s return is an Effectful computation
doOnNext :: ∀ m a. Bind m => (a -> m Unit) -> AnEvent m a -> AnEvent m a
doOnNext work upstream = makeEvent \downstreamPush -> do
  subscribe upstream \a -> do
    work a
    downstreamPush a

-- / Calls `work` upon Subscription. `work` is a callback that receives the "push" function of the
-- / Event as argument. `work`'s return is an Effectful computation
doOnSubscribe :: ∀ m a. Apply m => Bind m => ((a -> m Unit) -> m Unit) -> AnEvent m a -> AnEvent m a
doOnSubscribe work upstream = makeEvent \downstreamPush -> do
  work downstreamPush
  subscribe upstream downstreamPush

-- / Adds `work`` to be done upon Unsubscription
doOnUnsubscribe :: ∀ m a. Applicative m => Bind m => m Unit -> AnEvent m a -> AnEvent m a
doOnUnsubscribe work upstream = makeEvent \downstreamPush -> do
  upstreamDisposable <- subscribe upstream downstreamPush
  pure $ upstreamDisposable *> work

-- | Flatten a nested `Event`, reporting values only from the all inner Events.
flatMap :: ∀ s m a b. MonadST s m => (a -> AnEvent m b) -> AnEvent m a -> AnEvent m b
flatMap f e = makeEvent \k -> do
  disposingRef <- DisposingRef.create
  upstreamDisposable <- subscribe e \a -> do
    innerDisposable <- subscribe (f a) k
    DisposingRef.addSub disposingRef innerDisposable
  pure $ upstreamDisposable *> DisposingRef.dispose disposingRef

-- / Calls `work` on every emission. `work` is a callback that receives the emitted a as argument. `work`'s return is an Effectful computation
mapEffectful :: ∀ s m a b. MonadST s m => (a -> m b) -> AnEvent m a -> AnEvent m b
mapEffectful  work upstream = makeEvent \downstreamPush -> do
  subscribe upstream \a -> do
    b <- work a
    downstreamPush b

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

-- / Just composing replay and refCount
replayRefCount :: forall a s m. Bind m => MonadST s m => AnEvent m a -> m (AnEvent m a)
replayRefCount = replay >=> refCount

-- / Terminates upstream subscription once the predicate check fails
skipWhile :: ∀ a m s. MonadST s m => Applicative m => (a -> Boolean) -> AnEvent m a -> AnEvent m a
skipWhile f e = filterMap filterF accumEv
  where
  filterF :: Tuple Boolean a -> Maybe a
  filterF (Tuple isOpen a) = if isOpen then Just a else Nothing

  accumEv :: AnEvent m (Tuple Boolean a)
  accumEv = mapAccum accumF e false

  accumF :: a -> Boolean -> Tuple Boolean (Tuple Boolean a)
  accumF a isOpen = let isOpen' = isOpen || (not $ f a) in Tuple isOpen' (Tuple isOpen' a)

-- | Flatten a nested `Event`, reporting values only from the most recent
-- | inner `Event`.
switchMap :: ∀ a b m s. MonadST s m => (a -> AnEvent m b) -> AnEvent m a -> AnEvent m b
switchMap f e = keepLatest (f <$> e)

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

-- / Terminates upstream subscription once the predicate check fails
takeWhile :: ∀ a b m s. MonadST s m => Applicative m => (a -> Maybe b) -> AnEvent m a -> AnEvent m b
takeWhile f e = makeEvent \k -> do
  subRef <- DisposingRef.create
  sub <- subscribe e \a -> do
    case f a of
      Nothing -> DisposingRef.dispose subRef
      Just b -> k b
  DisposingRef.addSub subRef sub
  pure $ DisposingRef.dispose subRef

-- / Terminates upstream subscription once the predicate check fails
takeWhile' :: ∀ a m s. MonadST s m => Applicative m => (a -> Boolean) -> AnEvent m a -> AnEvent m a
takeWhile' f e = takeWhile (\x -> if f x then Just x else Nothing) e

toConnectable :: ∀ m s a. MonadST s m => AnEvent m a -> m (ConnectableEvent m a)
toConnectable upstream = do
  { event, push } <- create
  let connect = subscribe upstream push
  pure { event, connect }

toClosure :: forall m s r a. MonadST s m => m (AnEvent m a) -> (AnEvent m a -> r) -> AnEvent m r
toClosure upstreamEffect closure = makeEvent \downstreamPush -> do
  upstream <- upstreamEffect
  downstreamPush (closure upstream)
  pure (pure unit)
