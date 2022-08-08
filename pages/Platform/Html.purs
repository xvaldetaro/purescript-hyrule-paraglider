module Platform.Html where

import Prelude

import Control.Monad.ST.Class (class MonadST)
import Data.Foldable (for_, oneOfMap)
import Data.String (joinWith)
import Deku.Attribute (class Attr, Attribute, attr, cb, (:=))
import Deku.DOM (Class)
import Deku.DOM as D
import Effect (Effect)
import FRP.Event (AnEvent, bang)
import Paraglider.Rx (combineFold')
import Web.Event.Event (target)
import Web.HTML.HTMLInputElement (fromEventTarget, value)
import Web.UIEvent.KeyboardEvent as KeyboardEvent

-- / Creates a Class event from a String
bangCss :: ∀ e m. Applicative m => Attr e Class String => String -> AnEvent m (Attribute e)
bangCss s = bang (D.Class := s)

-- / Creates a Class event from a concatenation of an Array String
bangCss' :: ∀ e m. Applicative m => Attr e Class String => Array String -> AnEvent m (Attribute e)
bangCss' xs = bang (D.Class := (joinWith " " xs))

-- / combines the emissions from multiple String events, concatenate them all and emit a Class event
-- / This is useful if you have both permanent and dynamic CSS classes in an element so you can
-- / create 2 separate events.
combineCss
  :: ∀ e s m
   . MonadST s m
  => Attr e Class String
  => Array (AnEvent m String)
  -> AnEvent m (Attribute e)
combineCss stringEvents = attr D.Class <$> combineFold' stringEvents

-- Used to prefix a CSS string with "Css" so that Tailwind's VSCode extension can detect it and
-- launch intellisense
css :: String -> String
css s = " " <> s <> " "

-- / Creates an OnClick event that will run the provided effect once Click happens
bangClick :: ∀ e m. Applicative m => Effect Unit -> AnEvent m (Attribute e)
bangClick effect = bang $ D.OnClick := cb (const effect)

-- / Creates an OnInput event that will run the provided callback when Input is changed
bangInput :: ∀ e m. Applicative m => (String -> Effect Unit) -> AnEvent m (Attribute e)
bangInput userCb = bang $ D.OnInput := cb \webEvent ->
  let
    mbTargetInputElement = target webEvent >>= fromEventTarget
  in
    for_ mbTargetInputElement (value >=> userCb)

-- / Same as `bangInput`, but also accepts a callback for when user releases "Enter".
bangInputWithEnter :: ∀ e m. Applicative m => (String -> Effect Unit) -> AnEvent m (Attribute e)
bangInputWithEnter onInputText = bang $
  D.OnInput := cb \webEvent ->
      let
        mbTargetInputElement = target webEvent >>= fromEventTarget
      in
        for_ mbTargetInputElement (value >=> onInputText)

bangOnEnterUp :: ∀ e m. Applicative m => Effect Unit -> AnEvent m (Attribute e)
bangOnEnterUp onEnterUp = bang $ D.OnKeyup := cb \webEvent ->
      for_ (KeyboardEvent.fromEvent webEvent) \kbEvent -> do
        when (KeyboardEvent.code kbEvent == "Enter") onEnterUp
