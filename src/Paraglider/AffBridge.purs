module Paraglider.AffBridge where

import Prelude

import Data.Array (length, snoc)
import Data.Either (Either(..))
import Data.Filterable (filter)
import Data.Int (round)
import Effect (Effect)
import Effect.Aff (Aff, Canceler(..), Milliseconds(..), error, joinFiber, killFiber, launchAff, launchAff_, makeAff, try)
import Effect.Class (liftEffect)
import Effect.Ref as Effect.Ref
import Effect.Timer (clearTimeout, setTimeout)
import FRP.Event (Event, makeEvent, subscribe)
import FRP.Event.Class (fold)

-- / Will collect `i` emissions. Then will push that to the returned Aff and unsubscribe from upstream
blockingGetN :: ∀ a. Int -> Event a -> Aff (Array a)
blockingGetN i e = eventToAff $ filter (length >>> eq i) aggregatedEvent
  where
  aggregatedEvent = fold (flip snoc) e []

fromCallable :: ∀ a. Effect a -> Event a
fromCallable e = makeEvent \k -> (e >>= k) *> pure (pure unit)

-- if the cancelers cancels, theoretically we don't need to unsubscribe
-- as nothing will be firing
fromAff :: Aff ~> Event
fromAff a = makeEvent \k -> launchAff_ (a >>= liftEffect <<< k) *> pure (pure unit)

fromEffect :: ∀ a. Effect a -> Event a
fromEffect effect = makeEvent \k -> do
  emission <- effect
  k emission
  pure $ pure unit

-- / Subscribes to upstream and waits for an Emission. Once Emitted unsubscribes from upstream and
-- / Pushes the emission into the returned Aff
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
makeEventAff :: ∀ a. ((a -> Effect Unit) -> Aff (Effect Unit)) -> Event a
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

-- / Subscribes to upstream for `ms` and aggregate emissions. Once `ms` is passed will unsubscribe
-- / to upstream and Push the aggregated result into the returned Aff
collectEventToAff :: forall a. Milliseconds -> Event a -> Aff (Array a)
collectEventToAff (Milliseconds ms) e = makeAff \k -> do
  c <- Effect.Ref.new []
  tid <- setTimeout (round ms) do
    Effect.Ref.read c >>= k <<< Right
  unsub <- subscribe e \v -> do
    Effect.Ref.modify_ (_ <> [ v ]) c
  pure (Canceler \_ -> liftEffect (clearTimeout tid) *> liftEffect unsub)
