module Util.Util where

import Prelude

import Bolson.Core (Entity)
import Data.Array (snoc)
import Deku.Attribute (class Attr, Attribute, attr)
import Deku.Control (text_)
import Deku.Core (class Korok, Node)
import Deku.DOM (Style)
import Deku.DOM as D
import FRP.Event (AnEvent, fold)

styled :: ∀ s m e. Attr e Style String => Korok s m => String -> AnEvent m (Attribute e)
styled s = pure $ attr D.Style s

flexRow :: ∀ s m e. Attr e Style String => Korok s m => AnEvent m (Attribute e)
flexRow = styled "display: flex"

flexCol :: ∀ s m e. Attr e Style String => Korok s m => AnEvent m (Attribute e)
flexCol = styled "display: flex; flex-direction: column"

subtext :: ∀ s31 m32 lock33 payload34. Korok s31 m32 => String -> Entity Int (Node m32 lock33 payload34) m32 lock33
subtext s = D.div (styled "color: gray; margin-bottom: 10px;") [ text_ s ]

printFold :: ∀ s m a. Korok s m => Show a => AnEvent m a -> AnEvent m String
printFold e = show <$> fold (\x acc -> snoc acc x) e []