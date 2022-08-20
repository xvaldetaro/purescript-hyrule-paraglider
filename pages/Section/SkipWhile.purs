module Section.SkipWhile where

import Prelude

import Data.Int (floor)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor (lcmap)
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.Core (class Korok, Domable, bussedUncurried)
import Deku.DOM as D
import Deku.Do (useState)
import Deku.Do as Doku
import Paraglider.Operator.SkipWhile (skipWhile)
import Paraglider.Operator.SwitchMap (switchMap)
import Util.InputRow as InputRow
import Util.Util (flexCol, printFold, subtext)

docs :: String
docs = """Filters until the first predicate check succeeds. Then it emits freely."""

nut :: ∀ s m lock payload. Korok s m => Domable m lock payload
nut = Doku.do
  p /\ e <- bussedUncurried
  skipThresholdPush /\ skipThresholdEv <- useState Nothing

  let
      downstreamEv = skipThresholdEv # switchMap \s -> case s of
        Nothing -> pure "[]"
        Just x -> printFold $ skipWhile (_ < x) e

  D.div (flexCol)
    [ subtext docs
    , InputRow.number "Upstream emit a number:" "Emit" (lcmap floor p)
    , InputRow.number "Skip while upstream emission is less (<) than:" "Set" (lcmap (Just <<< floor) skipThresholdPush)
    , D.div_
      [ text ((\s -> "Downstream emissions of (skipWhile " <> (fromMaybe "" $ show <$> s) <> "): ") <$> skipThresholdEv)
      , D.div_ [text $ downstreamEv]
      ]
    ]