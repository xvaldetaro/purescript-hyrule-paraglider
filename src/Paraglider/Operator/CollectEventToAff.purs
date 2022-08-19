module Paraglider.Operator.CollectEventToAff where

import Prelude

import Data.Either (Either(..))
import Data.Int (round)
import Effect.Aff (Aff, Canceler(..), Milliseconds(..), makeAff)
import Effect.Class (liftEffect)
import Effect.Ref as Effect.Ref
import Effect.Timer (clearTimeout, setTimeout)
import FRP.Event (Event, subscribe)

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
