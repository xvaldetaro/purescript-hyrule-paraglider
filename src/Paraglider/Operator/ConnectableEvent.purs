module Paraglider.Operator.ConnectableEvent where

import Prelude

import FRP.Event (AnEvent)

-- | A "hot on demand" Event. Connects/Disconnects downstream subscriptions to upstream based on
-- | `connect` field being executed. (`connect` returns the unsubscription Effecvgt)
type ConnectableEvent m a = { connect :: m (m Unit), event :: AnEvent m a }
