module Section.FlatMap where

import Prelude

import Data.Array (snoc)
import Data.Int (floor)
import Data.Profunctor (lcmap)
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.Core (class Korok, Domable, bussedUncurried)
import Deku.DOM as D
import Deku.Do as Doku
import FRP.Event (fold, fromEvent)
import FRP.Event.Time (interval)
import Paraglider.Operator.FlatMap (flatMap)
import Util.InputRow as InputRow
import Util.Util (flexCol, subtext)

docs :: String
docs = """
Flatten a nested `Event`, reporting values only from the all inner Events.
"""

nut :: ∀ s m lock payload. Korok s m => Domable m lock payload
nut = Doku.do
  p /\ e <- bussedUncurried

  let
      incrementingInterval start = fold (\_ x -> x + 1) (fromEvent $ interval 1000) start
      downstreamEv = flatMap incrementingInterval e
      printEv = show <$> fold (\x acc -> snoc acc x) downstreamEv []

  D.div (flexCol)
    [ subtext docs
    , InputRow.number "Upstream emit a number:" "Emit" (lcmap floor p)
    , text_ "incrementingInterval emits incrementing numbers starting from Upstream's emission"
    , D.div_
      [ text_ "Downstream emissions of (flatMap incrementingInterval upstream): "
      , D.div_ [text $ printEv]
      ]
    ]