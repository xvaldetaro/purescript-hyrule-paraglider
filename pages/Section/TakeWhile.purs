module Section.TakeWhile where

import Prelude

import Control.Alt ((<|>))
import Data.Int (floor)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor (lcmap)
import Data.Tuple.Nested ((/\))
import Deku.Control (text)
import Deku.Core (class Korok, Domable, bussedUncurried)
import Deku.DOM as D
import Deku.Do (useState)
import Deku.Do as Doku
import Paraglider.Operator.SwitchMap (switchMap)
import Paraglider.Operator.Take (takeWhile')
import Util.InputRow as InputRow
import Util.Util (flexCol, printFold, subtext)

docs :: String
docs = """Terminates upstream subscription once the predicate check fails"""

nut :: ∀ s m lock payload. Korok s m => Domable m lock payload
nut = Doku.do
  p /\ e <- bussedUncurried
  takeThresholdPush /\ takeThresholdEv <- useState Nothing

  let
      downstreamEv = takeThresholdEv # switchMap \s -> case s of
        Nothing -> pure "[]"
        Just x -> pure "[]" <|> (printFold $ takeWhile' (_ < x) e)


  D.div (flexCol)
    [ subtext docs
    , InputRow.number "Upstream emit a number:" "Emit" (lcmap floor p)
    , InputRow.number "Take while upstream emission is less (<) than:" "Set" (lcmap (Just <<< floor) takeThresholdPush)
    , D.div_
      [ text ((\s -> "Downstream emissions of (takeWhile " <> (fromMaybe "" $ show <$> s) <> "): ") <$> takeThresholdEv)
      , D.div_ [text $ downstreamEv]
      ]
    ]