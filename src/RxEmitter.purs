module RxEmitter where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array (cons, length, snoc)
import Data.Either (Either(..))
import Data.Foldable (sequence_, traverse_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), Milliseconds(..), delay, joinFiber, killFiber, launchAff, launchAff_, makeAff, try)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Effect.Ref as Ref
import Halogen.Subscription (Emitter, makeEmitter, subscribe, unsubscribe)

-- / Adds a delay to every emission.
--/ https://raw.githubusercontent.com/wiki/ReactiveX/RxJava/images/rx-operators
rxDelay :: ∀ a. Milliseconds -> Emitter a -> Emitter a
rxDelay ms e = makeEmitter \k -> do
  isUnsubscribedRef <- Ref.new false
  unsub <- subscribe e \x -> launchAff_ do
    delay ms
    isUnsubscribed <- liftEffect $ Ref.read isUnsubscribedRef
    unless isUnsubscribed $ liftEffect $ k x
  pure $ unsubscribe unsub *> Ref.write true isUnsubscribedRef

rxBlockingGetN :: ∀ a. Int -> Emitter a -> Aff (Array a)
rxBlockingGetN count e = makeAff \callback -> do
  accRef <- Ref.new []
  unsub <- subscribe (rxTake count e) \x -> do
    acc <- Ref.modify (flip snoc x) accRef
    when (length acc == count) $ callback $ Right acc
  pure $ Canceler \_ -> liftEffect $ unsubscribe unsub

rxFromArray :: Array ~> Emitter
rxFromArray x = makeEmitter \k -> do
  sequence_ $ k <$> x
  pure $ pure unit

rxJust :: ∀ a. a -> Emitter a
rxJust x = makeEmitter \k -> do
  k x
  pure $ pure unit

rxFromCallable :: ∀ a. Effect a -> Emitter a
rxFromCallable effect = makeEmitter \k -> do
  a <- effect
  k a
  pure $ pure unit

-- / A version of makeEmitter that operates on Aff instead of Effect
rxMakeEmitterAff :: ∀ a . ((a -> Effect Unit) -> Aff (Effect Unit)) -> Emitter a
rxMakeEmitterAff cb = makeEmitter \k -> do
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
rxFromAff :: Aff ~> Emitter
rxFromAff a = makeEmitter \k -> do
  fib <- launchAff (a >>= liftEffect <<< k)
  pure (launchAff_ (killFiber (error "Event unsubscribed") fib))

-- / Emits Unit at ticks. `initial` determines what delay to wait until the first emission.
rxInterval :: Milliseconds -> Milliseconds -> Emitter Unit
rxInterval initial tick = rxMakeEmitterAff \k -> do
  delay initial
  void $ forever do
    liftEffect $ k unit
    delay tick
  pure $ pure unit

rxTake :: ∀ a. Int -> Emitter a -> Emitter a
rxTake n e = makeEmitter \k -> do
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

rxCombineLatest' :: ∀ a b. Emitter a -> Emitter b -> Emitter (Tuple a b)
rxCombineLatest' e1 e2 = rxCombineLatest Tuple e1 e2

rxCombineLatest :: ∀ a b c. (a -> b -> c) -> Emitter a -> Emitter b -> Emitter c
rxCombineLatest f e1 e2 = f <$> e1 <*> e2

rxCombineLatest3 :: ∀ a b c d. (a -> b -> c -> d) -> Emitter a -> Emitter b -> Emitter c -> Emitter d
rxCombineLatest3 f e1 e2 e3 = f <$> e1 <*> e2 <*> e3