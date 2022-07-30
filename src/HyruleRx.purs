module HyruleRx where

import Prelude

import Control.Monad.ST.Class (class MonadST, liftST)
import Control.Monad.ST.Internal as Ref
import Data.Array (length, snoc)
import Data.DateTime.Instant (Instant)
import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Foldable (for_, oneOfMap)
import Data.Int (floor, round)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), Milliseconds(..), error, joinFiber, killFiber, launchAff, launchAff_, makeAff, try)
import Effect.Class (liftEffect)
import Effect.Ref as Effect.Ref
import Effect.Timer (clearTimeout, setTimeout)
import FRP.Event (AnEvent, Event, bang, create, makeEvent, subscribe)
import FRP.Event as Event
import FRP.Event.Class (biSampleOn, fold)
import FRP.Event.Time as Event.Time
import Halogen.Subscription (Emitter, makeEmitter, unsubscribe)
import Halogen.Subscription as Subscription
import SubRef (SubRef, addSub, dispose)
import SubRef as SubRef

doOnUnsubscribe :: ∀ m a. Applicative m => Bind m => m Unit -> AnEvent m a -> AnEvent m a
doOnUnsubscribe work upstream = makeEvent \downstreamPush -> do
  upstreamDisposable <- subscribe upstream downstreamPush
  pure $ upstreamDisposable *> work

doOnSubscribe :: ∀ m a. Apply m => Bind m => m Unit -> AnEvent m a -> AnEvent m a
doOnSubscribe work upstream = makeEvent \downstreamPush -> do
  work
  subscribe upstream downstreamPush

-- data EventLifecycle a
--   = Emission a
--   | OnSubscribe
--   | OnUnsubscribe

-- withLifecycle :: ∀ a m. AnEvent m a -> AnEvent m (EventLifecycle a)
-- withLifecycle e = bus \push event ->
--   doOnSubscribe onSubscribe


-- type ConnectableEvent m a = { onConnect :: AnEvent m Unit, connect :: m (m Unit), event :: AnEvent m a }

-- connectable :: ∀ m1 m2 s a . MonadST s m1 => MonadST s m2 => AnEvent m2 a -> m1 (ConnectableEvent m2 a)
-- connectable upstream =
--   { push, event } <- create
--   onConnectEv <- create
--   let downstream = makeEvent \k -> subscribe event k
--   let connect = subscribe upstream push
--   pure { onConnect: onConnectEv, connect, event: downstream }

replayRefCount :: ∀ a m s. MonadST s m => AnEvent m a -> m (AnEvent m a)
replayRefCount upstream = do
  lastEmissionRef <- liftST $ Ref.new Nothing
  let onUpstreamUnsubscribed = void $ liftST $ Ref.write Nothing lastEmissionRef
  reffed <- doOnUnsubscribe onUpstreamUnsubscribed <$> refCount upstream
  pure $ makeEvent \k -> do
    mbLast <- liftST $ Ref.read lastEmissionRef
    for_ mbLast k
    subscribe reffed \upstreamEmission -> do
      void $ liftST $ Ref.write (Just upstreamEmission) lastEmissionRef
      k upstreamEmission

refCount :: ∀ m s a . MonadST s m => AnEvent m a -> m (AnEvent m a)
refCount e = do
  upstreamSubRef <- liftST $ Ref.new Nothing
  refCountRef <- liftST $ Ref.new 0
  { push, event } <- create
  pure $ makeEvent \k -> do
    refCountBegin <- liftST $ Ref.modify (_ + 1) refCountRef
    when (refCountBegin == 1) do
      upstreamSub <- subscribe e push
      void $ liftST $ Ref.write (Just upstreamSub) upstreamSubRef

    downstreamSub <- subscribe event k

    pure $ downstreamSub *> do
      refCountEnd <- liftST $ Ref.modify (_ - 1) refCountRef
      when (refCountEnd == 0) do
        mbUpstreamSubRef <- liftST $ Ref.read upstreamSubRef
        for_ mbUpstreamSubRef identity
        void $ liftST $ Ref.write Nothing upstreamSubRef

fromHalo :: Emitter ~> Event
fromHalo emitter = makeEvent \k -> do
  sub <- Subscription.subscribe emitter k
  pure $ unsubscribe sub

toHalo :: Event ~> Emitter
toHalo event = makeEmitter \k -> do
  sub <- subscribe event k
  pure $ sub

-- if the cancelers cancels, theoretically we don't need to unsubscribe
-- as nothing will be firing
fromAff :: Aff ~> Event
fromAff a = makeEvent \k -> launchAff_ (a >>= liftEffect <<< k) *> pure (pure unit)

fromEffect :: ∀ a. Effect a -> Event a
fromEffect effect = makeEvent \k -> do
  emission <- effect
  k emission
  pure $ pure unit

eventToAff :: Event ~> Aff
eventToAff e = makeAff \k -> do
  u <- Effect.Ref.new (pure unit)
  unsub <- subscribe e \v -> do
    k (Right v)
    join (Effect.Ref.read u)
    Effect.Ref.write (pure unit) u
  Effect.Ref.write unsub u
  pure (Canceler \_ -> liftEffect (join (Effect.Ref.read u)))

-- / A version of makeEvent that operates on Aff instead of Effect
makeEventAff :: ∀ a . ((a -> Effect Unit) -> Aff (Effect Unit)) -> Event a
makeEventAff cb = makeEvent \k -> do
  fiber <- launchAff $ cb k
  pure $ launchAff_ do
    killFiber (error "Event Unsubscribed") fiber
    eiCleanup <- try $ joinFiber fiber
    case eiCleanup of
      -- We killed the Fiber before it emitted its result
      Left _ -> pure unit
      -- The Fiber completed and we joined with its result
      Right cleanup -> liftEffect cleanup

collectEventToAff :: forall a. Milliseconds -> Event a -> Aff (Array a)
collectEventToAff (Milliseconds ms) e = makeAff \k -> do
  c <- Effect.Ref.new []
  tid <- setTimeout (round ms) do
    Effect.Ref.read c >>= k <<< Right
  unsub <- subscribe e \v -> do
    Effect.Ref.modify_ (_ <> [ v ]) c
  pure (Canceler \_ -> liftEffect (clearTimeout tid) *> liftEffect unsub)

-- take :: ∀ a m s. MonadST s m => Int -> AnEvent m a -> AnEvent m a
-- take n e = makeEvent \k -> do
--   subRef :: ?x <- liftST $ Ref.new (pure unit)
--   countRef <- liftST $ Ref.new n
--   sub <- subscribe e \a -> do
--     count <- liftST $ Ref.read countRef
--     when (count == 1) do
--       work <- liftST $ Ref.read subRef
--       work
--     when (count > 0) do
--       void $ liftST $ Ref.write (count - 1) countRef
--       k a
--   void $ liftST $ Ref.write sub subRef
--   pure $ sub

take :: ∀ a m s. MonadST s m => Applicative m => Int -> AnEvent m a -> AnEvent m a
take n e = makeEvent \k -> do
  (subRef :: SubRef s m) <- liftST $ SubRef.create
  countRef <- liftST $ Ref.new n
  sub <- subscribe e \a -> do
    count <- liftST $ Ref.read countRef
    when (count == 1) do
      disposeEffect <- liftST $ SubRef.dispose subRef
      disposeEffect
    when (count > 0) do
      void $ liftST $ Ref.write (count - 1) countRef
      k a
  liftST $ SubRef.addSub subRef sub
  pure do
    disposal <- liftST $ SubRef.dispose subRef
    disposal

interval :: Milliseconds -> Event Instant
interval (Milliseconds ms) = Event.Time.interval $ floor ms

delay :: ∀ a. Milliseconds -> Event a -> Event a
delay (Milliseconds ms) = Event.delay (floor ms)

blockingGetN :: ∀ a. Int -> Event a -> Aff (Array a)
blockingGetN i e = eventToAff $ filter (length >>> eq i) aggregatedEvent
  where
  aggregatedEvent = fold (flip snoc) e []

fromArray :: Array ~> Event
fromArray = oneOfMap bang

just :: ∀ a. a -> Event a
just = bang

fromCallable :: ∀ a. Effect a -> Event a
fromCallable e = makeEvent \k -> (e >>= k) *> pure (pure unit)

combineLatest :: ∀ a b c. (a -> b -> c) -> Event a -> Event b -> Event c
combineLatest f e1 e2 = biSampleOn e2 $ f <$> e1

combineLatest3 :: ∀ a b c d. (a -> b -> c -> d) -> Event a -> Event b -> Event c -> Event d
combineLatest3 f e1 e2 e3 = biSampleOn e3 $ biSampleOn e2 (f <$> e1)