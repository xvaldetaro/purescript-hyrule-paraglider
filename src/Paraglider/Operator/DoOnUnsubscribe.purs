module Paraglider.Operator.DoOnUnsubscribe where

import Prelude

import FRP.Event (AnEvent, makeEvent, subscribe)

-- / Adds `work`` to be done upon Unsubscription
doOnUnsubscribe :: ∀ m a. Applicative m => Bind m => m Unit -> AnEvent m a -> AnEvent m a
doOnUnsubscribe work upstream = makeEvent \downstreamPush -> do
  upstreamDisposable <- subscribe upstream downstreamPush
  pure $ upstreamDisposable *> work