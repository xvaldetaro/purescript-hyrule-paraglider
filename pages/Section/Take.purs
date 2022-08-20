module Section.Take where

import Prelude

import Control.Alt ((<|>))
import Data.Int (floor)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Profunctor (lcmap)
import Data.Tuple.Nested ((/\))
import Deku.Control (text)
import Deku.Core (class Korok, Domable)
import Deku.DOM as D
import Deku.Do (useState, useState')
import Deku.Do as Doku
import Paraglider.Operator.SwitchMap (switchMap)
import Paraglider.Operator.Take (take)
import Util.InputRow as InputRow
import Util.Util (flexCol, printFold, subtext)

docs :: String
docs = """Terminates upstream subscription after `n` emissions"""

nut :: ∀ s m lock payload. Korok s m => Domable m lock payload
nut = Doku.do
  p /\ e <- useState'
  takeNPush /\ takeNEv <- useState Nothing

  let
      downstreamEv = takeNEv # switchMap \s -> case s of
        Nothing -> pure "[]"
        Just x -> pure "[]" <|> (printFold $ take x e)

  D.div (flexCol)
    [ subtext docs
    , InputRow.number "Upstream emit a number:" "Emit" (lcmap floor p)
    , InputRow.number "How many emissions to take before terminating?" "Set" (lcmap (Just <<< floor) takeNPush)
    , D.div_
      [ text ((\s -> "Downstream emissions of (take " <> (fromMaybe "" $ show <$> s) <> "): ") <$> takeNEv)
      , D.div_ [text $ downstreamEv]
      ]
    ]