module Section.Combine where

import Prelude

import Control.Alt ((<|>))
import Data.Array (reverse)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple.Nested ((/\))
import Deku.Control (text, text_)
import Deku.Core (class Korok, Domable)
import Deku.DOM as D
import Deku.Do as Doku
import FRP.Event (AnEvent, filterMap, fold)
import Paraglider.Operator.Combine (combineFold)
import Paraglider.Operator.SwitchMap (switchMap)
import Util.Util (flexCol, styled, subtext)
import Util.VarUpstream (SourceId)
import Util.VarUpstream as VarUpstream

docs :: String
docs = """
Folds over the **last** emissions from upstream events, emits the folded result to downstream.
Notice that this is different from `folded`. In `folded` we keep folding over every upstream
emission since the start. Effectively making all upstream emissions (including previous emissions)
partof the fold. Whereas in `combineFold` we fold over the **last** round of emissions from
upstream only.
"""

nut :: ∀ s m lock payload. Korok s m => Domable m lock payload
nut = Doku.do
  {sourcesEv, busEv} /\ varUpstreamUi <- VarUpstream.crackedNut

  let
      pendingSourcesEv :: AnEvent m (Set SourceId)
      pendingSourcesEv = sourcesEv # switchMap \allSources ->
        let reducePending (sourceId /\ _) pending = Set.delete sourceId pending in
        fold reducePending busEv allSources <|> pure allSources

      pendingSourcesPrintEv = (pendingSourcesEv <#> show <<< Array.fromFoldable) <|> pure ""

      filterEmissionsFromSource sourceId =
        filterMap
          (\(id /\ payload) -> if id == sourceId then Just payload else Nothing)
          busEv

      combinedFoldEv = sourcesEv # switchMap \allSources ->
        let emissionsPerSourceArr =
              filterEmissionsFromSource <$> (reverse $ Array.fromFoldable allSources) in
        combineFold (\a b -> a <> "_" <> b) "" emissionsPerSourceArr

  D.div (flexCol)
    [ subtext docs
    , D.div (styled "border: 1px solid blue; display: flex; flex-direction: column")
      [ D.h4_ [text_ "Add Upstream sources to Combine:"]
      , varUpstreamUi
      ]
    , D.div_ [text_ "Waiting on emissions from Ids: ", text pendingSourcesPrintEv ]
    , D.div_
      [ text_ "Result of `(combineFold (\\a b -> a <> \"_\" <> b) \"\" event)` is: "
      , text $ show <$> combinedFoldEv
      ]
    ]