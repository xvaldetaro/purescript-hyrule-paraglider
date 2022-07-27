module HaloSubRx where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array (cons, length, snoc)
import Data.Either (Either(..))
import Data.Foldable (sequence_, traverse_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), Milliseconds(..), delay, joinFiber, killFiber, launchAff, launchAff_, makeAff, try)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Effect.Ref as Ref
import Halogen.Subscription (Emitter, makeEmitter, subscribe, unsubscribe)

-- / Adds a delay to every emission.
delay :: ∀ a. Milliseconds -> Emitter a -> Emitter a
delay ms e = makeEmitter \k -> do
  isUnsubscribedRef <- Ref.new false
  unsub <- subscribe e \x -> launchAff_ do
    Aff.delay ms
    isUnsubscribed <- liftEffect $ Ref.read isUnsubscribedRef
    unless isUnsubscribed $ liftEffect $ k x
  pure $ unsubscribe unsub *> Ref.write true isUnsubscribedRef

blockingGetN :: ∀ a. Int -> Emitter a -> Aff (Array a)
blockingGetN count e = makeAff \callback -> do
  accRef <- Ref.new []
  unsub <- subscribe (take count e) \x -> do
    acc <- Ref.modify (flip snoc x) accRef
    when (length acc == count) $ callback $ Right acc
  pure $ Canceler \_ -> liftEffect $ unsubscribe unsub

fromArray :: Array ~> Emitter
fromArray x = makeEmitter \k -> do
  sequence_ $ k <$> x
  pure $ pure unit

just :: ∀ a. a -> Emitter a
just x = makeEmitter \k -> do
  k x
  pure $ pure unit

fromCallable :: ∀ a. Effect a -> Emitter a
fromCallable effect = makeEmitter \k -> do
  a <- effect
  k a
  pure $ pure unit

-- / A version of makeEmitter that operates on Aff instead of Effect
makeEmitterAff :: ∀ a . ((a -> Effect Unit) -> Aff (Effect Unit)) -> Emitter a
makeEmitterAff cb = makeEmitter \k -> do
  fiber <- launchAff $ cb k
  pure $ launchAff_ do
    killFiber (error "Event Unsubscribed") fiber
    eiCleanup <- try $ joinFiber fiber
    case eiCleanup of
      -- We killed the Fiber before it emitted its result
      Left _ -> pure unit
      -- The Fiber completed and we joined with its result
      Right cleanup -> liftEffect cleanup

-- / Pipes the returned value from the Aff into the returned Emitter
fromAff :: Aff ~> Emitter
fromAff a = makeEmitter \k -> do
  fib <- launchAff (a >>= liftEffect <<< k)
  pure (launchAff_ (killFiber (error "Event unsubscribed") fib))

-- / Emits Unit at ticks. `initial` determines what delay to wait until the first emission.
interval :: Milliseconds -> Milliseconds -> Emitter Unit
interval initial tick = makeEmitterAff \k -> do
  Aff.delay initial
  void $ forever do
    liftEffect $ k unit
    Aff.delay tick
  pure $ pure unit

take :: ∀ a. Int -> Emitter a -> Emitter a
take n e = makeEmitter \k -> do
  countRef <- Ref.new n
  unsubRef <- Ref.new Nothing
  unsub <- subscribe e \a -> do
    count <- Ref.read countRef
    if count > 0 then do
      Ref.write (count - 1) countRef
      k a
      when (count == 1) do
        mbUnsub <- Ref.read unsubRef
        traverse_ unsubscribe mbUnsub
    else log "take operator had emission with count < 1"

  -- In case the original Emitter emitted at Subscription time
  count <- Ref.read countRef
  if count < 1 then unsubscribe unsub
  else Ref.write (Just unsub) unsubRef
  pure $ unsubscribe unsub

combineLatest' :: ∀ a b. Emitter a -> Emitter b -> Emitter (Tuple a b)
combineLatest' e1 e2 = combineLatest Tuple e1 e2

combineLatest :: ∀ a b c. (a -> b -> c) -> Emitter a -> Emitter b -> Emitter c
combineLatest f e1 e2 = f <$> e1 <*> e2

combineLatest3 :: ∀ a b c d. (a -> b -> c -> d) -> Emitter a -> Emitter b -> Emitter c -> Emitter d
combineLatest3 f e1 e2 e3 = f <$> e1 <*> e2 <*> e3