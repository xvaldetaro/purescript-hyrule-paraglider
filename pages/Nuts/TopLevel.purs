module Nuts.TopLevel where

import Prelude

import App.Navigation (routeChangeEvent)
import App.Route (Route)
import App.Route as Route
import Data.Tuple (curry)
import Data.Tuple.Nested (type (/\), (/\))
import Deku.Control (switcher, text, text_)
import Deku.Core (Nut, vbussed)
import Deku.DOM as D
import FRP.Event (AnEvent)
import FRP.Event.VBus (V)
import Nuts.Examples.Combine as Combine
import Nuts.Examples.Take as Take
import Nuts.Sidebar as Sidebar
import Paraglider.Rx (replayRefCount)
import Platform.Html (bangCss)
import Platform.Misc (Nut_, PlainOl, initializeEvents, usingEffect)
import Platform.QualifiedDo as QDO
import Type.Proxy (Proxy(..))

type Events :: forall k. (Type -> k) -> Row k
type Events t =
  ( route :: t Route
  )


nut :: ∀ s m l p. Nut_ s m l p
nut = vbussed (Proxy :: _ (V (Events PlainOl))) \push (eventRaw :: { | Events (AnEvent m)}) ->
  usingEffect (replayRefCount routeChangeEvent) \currentRouteEv ->
    D.div (bangCss "flex flex-col w-screen h-screen")
    [ D.div
      (bangCss "bg-slate-900 text-slate-50 h-14 px-8 text-xl flex flex-col justify-center")
      [text_ "Paraglider Examples"]
    , D.div (bangCss "flex flex-row h-full")
        [ Sidebar.nut currentRouteEv
        , D.div (bangCss "m-5") [switcher fromRouteToNut currentRouteEv]
        ]
    ]
  where
  fromRouteToNut = case _ of
    Route.Landing -> D.div_ [text_ "Select the operator on the side menu to see how it works"]
    Route.Combine -> Combine.nut
    Route.Take -> Take.nut



