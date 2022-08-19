module Paraglider.Operator.ToAff where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff, Canceler(..), makeAff)
import Effect.Class (liftEffect)
import Effect.Ref as Effect.Ref
import FRP.Event (Event, subscribe)

-- / Subscribes to upstream and waits for an Emission. Once Emitted unsubscribes from upstream and
-- / Pushes the emission into the returned Aff
toAff :: Event ~> Aff
toAff e = makeAff \k -> do
  u <- Effect.Ref.new (pure unit)
  unsub <- subscribe e \v -> do
    k (Right v)
    join (Effect.Ref.read u)
    Effect.Ref.write (pure unit) u
  Effect.Ref.write unsub u
  pure (Canceler \_ -> liftEffect (join (Effect.Ref.read u)))
