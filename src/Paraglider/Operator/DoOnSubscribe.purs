module Paraglider.Operator.DoOnSubscribe where

import Prelude

import FRP.Event (AnEvent, makeEvent, subscribe)

-- / Calls `work` upon Subscription. `work` is a callback that receives the "push" function of the
-- / Event as argument. `work`'s return is an Effectful computation
doOnSubscribe :: ∀ m a. Apply m => Bind m => ((a -> m Unit) -> m Unit) -> AnEvent m a -> AnEvent m a
doOnSubscribe work upstream = makeEvent \downstreamPush -> do
  work downstreamPush
  subscribe upstream downstreamPush