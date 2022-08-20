module Section.CollectEventToAff where

import Prelude

import Control.Alt ((<|>))
import Data.Array (cons)
import Data.DateTime.Instant (Instant, diff)
import Data.String (joinWith)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple.Nested ((/\))
import Deku.Control (text)
import Deku.Core (class Korok, Domable, bussedUncurried)
import Deku.DOM as D
import Deku.Do as Doku
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Now as Now
import FRP.Event (fold, fromEvent, toEvent)
import FRP.Event.Time (interval)
import Paraglider.Operator.CollectEventToAff (collectEventToAff)
import Paraglider.Operator.SwitchMap (switchMap)
import Util.InputRow as InputRow
import Util.Util (flexCol, subtext)

docs :: String
docs = """
Subscribes to upstream for `ms` and aggregate emissions. Once `ms` is passed will unsubscribe to
upstream and Push the aggregated result into the returned Aff"
"""

nut :: ∀ s m lock payload. Korok s m => Domable m lock payload
nut = Doku.do
  nTimePush /\ nTimeEv <- bussedUncurried
  actualPush /\ actualEv <- bussedUncurried
  affUnblockedPush /\ affUnblockedEv' <- bussedUncurried

  let affUnblockedEv = switchMap (\_ -> affUnblockedEv' <|> pure []) nTimeEv
  let allEmissions = switchMap (\_ -> fold cons actualEv [] <|> pure []) nTimeEv
  let diffRemainingTime (span /\ startedAt) = fromEvent $ interval 100 <#> remainingTime span startedAt
  let remainingEmissionsEv = switchMap diffRemainingTime nTimeEv

  let
      onTimeClick :: Number -> Effect Unit
      onTimeClick requiredSpan' = do
        let requiredSpan = Milliseconds $ requiredSpan'
        nowInstant <- Now.now
        nTimePush $ (requiredSpan /\ nowInstant)
        launchAff_ do
          emissions <- collectEventToAff requiredSpan (toEvent actualEv)
          liftEffect $ affUnblockedPush emissions

  let
      affReturnedText [] = ""
      affReturnedText emissionArr = "Aff returned: " <> show emissionArr

  D.div (flexCol)
    [ subtext docs
    , InputRow.number "For how many milliseconds should we collect emissions?" "Set" onTimeClick
    , InputRow.text "Click to emit numbers" "Emit" actualPush
    , D.div_
      [text
        $ remainingEmissionsEv
          <#> (\remainingN -> "Aff collecting for " <> (show remainingN) <> "ms more")]
    , D.div_
      [text $ (\arr -> "All Emissions: [" <> joinWith "," arr <> "]") <$> allEmissions]
    , D.div_ [text $ affUnblockedEv <#> affReturnedText ]
    ]

remainingTime :: Milliseconds -> Instant -> Instant -> String
remainingTime (Milliseconds span) startedAt now =
  let (Milliseconds instantDiff) = diff now startedAt in
  show $ max 0.0 (span - instantDiff)