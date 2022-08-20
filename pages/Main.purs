module Main where

import Prelude

import Deku.Control (text_)
import Deku.Core (class Korok, Domable)
import Deku.DOM as D
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Section.BlockingGetN as BlockingGetN
import Section.CollectEventToAff as CollectEventToAff
import Section.Combine as Combine
import Section.FlatMap as FlatMap
import Section.SkipWhile as SkipWhile
import Section.SwitchMap as SwitchMap
import Section.Take as Take
import Section.TakeWhile as TakeWhile
import Util.Util (flexCol, styled)

nut :: ∀ s m lock payload. Korok s m => Domable m lock payload
nut = Doku.do
  D.div (flexCol)
    [ section "BlockingGetN", BlockingGetN.nut
    , section "CollectEventToAff", CollectEventToAff.nut
    , section "Combine", Combine.nut
    , section "FlatMap", FlatMap.nut
    , section "SkipWhile", SkipWhile.nut
    , section "SwitchMap", SwitchMap.nut
    , section "Take", Take.nut
    , section "TakeWhile", TakeWhile.nut
    ]
  where
  section header = D.div (styled "margin-top: 10px; border-top: 1px solid gray") [D.h2_ [text_ header]]

main :: Effect Unit
main = runInBody (nut)
