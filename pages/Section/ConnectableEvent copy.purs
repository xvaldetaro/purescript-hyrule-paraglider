module Section.ConnectableEvent where

import Prelude

import Bolson.Core (envy)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Deku.Core (class Korok, Domable, bussedUncurried)
import Deku.DOM as D
import Deku.Do (useState)
import Deku.Do as Doku
import FRP.Event (AnEvent)
import Paraglider.Operator.ToClosure (toClosure)
import Paraglider.Operator.ToConnectable (toConnectable)
import Util.InputRow as InputRow
import Util.Util (flexCol, subtext)

docs :: String
docs = """
A "hot on demand" Event. Connects/Disconnects downstream subscriptions to upstream based on
`connect` field being executed. (`connect` returns the unsubscription Effect)
"""

nut :: ∀ s m lock payload. Korok s m => Domable m lock payload
nut = Doku.do
  upstreamPush /\ upstreamEv <- bussedUncurried
  connectionPush /\ connectionEv <- useState Nothing
  {connect, event: upstreamConnEv} <- envy <<< (toClosure $ toConnectable $ (upstreamEv :: AnEvent m String))

  -- let
  --     onConnectClicked Nothing = do
  --       connection <- connect
  --       connectionPush (Just connection)
  --     onConnectClicked (Just conn) = do
  --       conn
  --       connectionPush Nothing

  D.div (flexCol)
    [ subtext docs
    , InputRow.text "Upstream" "Emit" upstreamPush
    -- , D.div_
    --   [ D.button (click $ pure connectionPush) [text_ "toConnectable"]
    --   ]
    ]