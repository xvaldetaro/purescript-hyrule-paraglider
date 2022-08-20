module Section.BlockingGetN where

import Prelude

import Control.Alt ((<|>))
import Data.Array (cons)
import Data.Int (floor)
import Data.String (joinWith)
import Data.Tuple.Nested ((/\))
import Deku.Control (text)
import Deku.Core (class Korok, Domable, bussedUncurried)
import Deku.DOM as D
import Deku.Do as Doku
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (count, fold, toEvent)
import Paraglider.Operator.BlockingGetN (blockingGetN)
import Paraglider.Operator.SwitchMap (switchMap)
import Util.InputRow as InputRow
import Util.Util (flexCol, subtext)

docs :: String
docs = """
Will collect `i` emissions. Then will push that to the returned Aff and unsubscribe from upstream
"""

nut :: ∀ s m lock payload. Korok s m => Domable m lock payload
nut = Doku.do
  nEmissionsPush /\ nEmissionsEv <- bussedUncurried
  actualPush /\ actualEv <- bussedUncurried
  affUnblockedPush /\ affUnblockedEv' <- bussedUncurried

  let affUnblockedEv = switchMap (\_ -> affUnblockedEv' <|> pure []) nEmissionsEv
  let allEmissions = switchMap (\_ -> fold cons actualEv [] <|> pure []) nEmissionsEv
  let countEmissions = count actualEv <|> pure 0
  let diffRemainingEmissions nEmissions = countEmissions <#> \c -> max 0 (floor nEmissions - c)
  let remainingEmissionsEv = switchMap diffRemainingEmissions nEmissionsEv

  let
      onEmissionsClick :: Number -> Effect Unit
      onEmissionsClick requiredNum = do
        nEmissionsPush requiredNum
        launchAff_ do
          emissions <- blockingGetN (floor requiredNum) (toEvent actualEv)
          liftEffect $ affUnblockedPush emissions

  let
      affReturnedText [] = ""
      affReturnedText emissionArr = "Aff returned: " <> show emissionArr

  D.div (flexCol)
    [ subtext docs
    , InputRow.number "How many emissions should we block until?" "Set" onEmissionsClick
    , InputRow.text "Click to emit numbers" "Emit" actualPush
    , D.div_
      [text
        $ remainingEmissionsEv
          <#> (\remainingN -> "Aff waiting for " <> (show remainingN) <> " more emissions")]
    , D.div_
      [text $ (\arr -> "All Emissions: [" <> joinWith "," arr <> "]") <$> allEmissions]
    , D.div_ [text $ affUnblockedEv <#> affReturnedText ]
    ]