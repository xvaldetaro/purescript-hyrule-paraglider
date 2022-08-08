module Main where

import Prelude

import App.Navigation (redirectToIfInialRouteIsInvalid)
import App.Route as Route
import Deku.Toplevel (runInBody)
import Effect (Effect)
import Nuts.TopLevel as TopLevel

main :: Effect Unit
main = do
  redirectToIfInialRouteIsInvalid Route.Landing
  runInBody TopLevel.nut