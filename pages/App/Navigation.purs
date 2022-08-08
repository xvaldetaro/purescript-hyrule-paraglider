module App.Navigation where

import Prelude

import App.Route (Route, routeCodec)
import App.Route as Route
import Control.Monad.ST.Class (class MonadST)
import Data.Either (hush)
import Data.Maybe (Maybe(..), isNothing)
import Data.Monoid.Always (class Always)
import Data.Monoid.Endo (Endo)
import Effect (Effect)
import Effect.Class.Console (log)
import FRP.Event (AnEvent, Event, fromEvent, makeEvent)
import Routing.Duplex (parse, print)
import Routing.Duplex as RD
import Routing.Hash (getHash, matchesWith, setHash)

navigate :: Route -> Effect Unit
navigate = setHash <<< print Route.routeCodec

redirectToIfInialRouteIsInvalid :: Route -> Effect Unit
redirectToIfInialRouteIsInvalid route = do
  initialRoute <- hush <<< (RD.parse routeCodec) <$> getHash
  when (isNothing initialRoute) do
    log "Got invalid Route. Navigating to Landing."
    navigate route

routeChangeEvent :: ∀ s m. Always (m Unit) (Effect Unit)
  => Always (Endo Function (Effect (Effect Unit))) (Endo Function (m (m Unit)))
  => Monad m
  => MonadST s m
  => AnEvent m Route
routeChangeEvent = fromEvent $ makeEvent \k -> do
  matchesWith (parse routeCodec) \old new ->
    when (old /= Just new) do
      log $ "Changing route from " <> show old <> " to " <> show new
      k new