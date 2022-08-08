module Nuts.Sidebar where

import Prelude

import App.Route (Route, routeCodec)
import App.Route as Route
import Control.Alt ((<|>))
import Deku.Attribute ((:=))
import Deku.Control (text_)
import Deku.DOM as D
import FRP.Event (AnEvent, bang)
import Platform.Html (bangCss, combineCss, css)
import Platform.Misc (Nut_)
import Routing.Duplex (print)

nut :: ∀ s m l p. AnEvent m Route -> Nut_ s m l p
nut currentRouteEv =
  D.div (bangCss "flex flex-col h-full text-slate-700 px-8 pt-6 border-r")
    [ menuItem Route.Combine "Combine"
    , menuItem Route.Take "Take"
    ]
  where
  menuItem route text = D.a
    ( (bang $ D.Href := safeHref route)
        <|> combineCss
          [ bang $ css "hover:text-teal-500 align-middle items-center mb-1"
          , currentRouteEv <#> (\r -> if r == route then css "text-teal-500" else "")
          ]
      )
      [ text_ text ]


-- | We must provide a `String` to the "href" attribute, but we represent routes with the much
-- | better `Route` type. This utility is a drop-in replacement for `href` that uses `Route`.
safeHref :: Route -> String
safeHref = append "#" <<< print routeCodec