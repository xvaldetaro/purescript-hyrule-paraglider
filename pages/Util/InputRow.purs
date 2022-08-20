module Util.InputRow where

import Prelude

import Deku.Control (text_)
import Deku.Core (class Korok, Domable, bussed)
import Deku.DOM as D
import Deku.Listeners (click, numeric, textInput)
import Effect (Effect)
import Util.Util (styled)

text :: forall s m l p. Korok s m => String -> String -> (String -> Effect Unit) -> Domable m l p
text label buttonText onClick = bussed \pushText textEv ->
  D.div (styled "display: flex")
    [ D.label_ [text_ label]
    , D.input (textInput $ pure pushText) []
    , D.button (click $ textEv <#> onClick) [text_ buttonText]
    ]

number :: forall s m l p. Korok s m => String -> String -> (Number -> Effect Unit) -> Domable m l p
number label buttonText onClick = bussed \pushNumber numberEv ->
  D.div (styled "display: flex")
    [ D.label_ [text_ label]
    , D.input (numeric $ pure pushNumber) []
    , D.button (click $ numberEv <#> onClick) [text_ buttonText]
    ]