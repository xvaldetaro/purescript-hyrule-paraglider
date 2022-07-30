module HyruleRx where

import Prelude

import Control.Monad.ST.Class (class MonadST)
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
import Effect.Class.Console (log)
import Effect.Ref as Ref
import Effect.Timer (clearTimeout, setTimeout)
import FRP.Event (AnEvent, Event, bang, create, makeEvent, memoize, subscribe)
import FRP.Event as Event
import FRP.Event.Class (biSampleOn, fold)
import FRP.Event.Time as Event.Time
import Halogen.Subscription (Emitter, makeEmitter, unsubscribe)
import Halogen.Subscription as Subscription
import SubRef (addSub, dispose)
import SubRef as SubRef

replayRefCount :: ∀ a. Event a -> Effect (Event a)
replayRefCount e = do
  lastRef <- Ref.new Nothing
  upstreamSubRef <- Ref.new Nothing
  refCountRef <- Ref.new 0
  { push, event } <- create

  pure $ makeEvent \k -> do
    refCount <- Ref.modify (_ + 1) refCountRef
    when (refCount == 1) do
      upstreamSub <- subscribe e \upstreamEmission -> do
        Ref.write (Just upstreamEmission) lastRef
        push upstreamEmission
      Ref.write (Just upstreamSub) upstreamSubRef

    mbLast <- Ref.read lastRef
    for_ mbLast k
    downstreamSub <- subscribe event k

    pure $ downstreamSub *> do
      refCount' <- Ref.modify (_ - 1) refCountRef
      when (refCount' == 0) do
        mbUpstreamSubRef <- Ref.read upstreamSubRef
        for_ mbUpstreamSubRef identity
        Ref.write Nothing upstreamSubRef
        Ref.write Nothing lastRef



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

eventToAff :: Event ~> Aff
eventToAff e = makeAff \k -> do
  u <- Ref.new (pure unit)
  unsub <- subscribe e \v -> do
    k (Right v)
    join (Ref.read u)
    Ref.write (pure unit) u
  Ref.write unsub u
  pure (Canceler \_ -> liftEffect (join (Ref.read u)))

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
  c <- Ref.new []
  tid <- setTimeout (round ms) do
    Ref.read c >>= k <<< Right
  unsub <- subscribe e \v -> do
    Ref.modify_ (_ <> [ v ]) c
  pure (Canceler \_ -> liftEffect (clearTimeout tid) *> liftEffect unsub)

take :: ∀ a. Int -> Event a -> Event a
take n e = makeEvent \k -> do
  subRef <- SubRef.create
  countRef <- Ref.new n
  sub <- subscribe e \a -> do
    count <- Ref.read countRef
    when (count == 1) $ dispose subRef
    when (count > 0) do
      Ref.write (count - 1) countRef
      k a
  addSub subRef sub
  pure $ dispose subRef

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