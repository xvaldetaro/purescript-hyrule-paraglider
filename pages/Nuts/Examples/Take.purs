module Nuts.Examples.Take where

import Prelude

import Deku.Control (text_)
import Platform.Misc (Nut_)

nut :: ∀ s m l p. Nut_ s m l p
nut = text_ "Sidebar"