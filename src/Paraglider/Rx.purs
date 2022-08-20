module Paraglider.Rx where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Exception (Error)
import FRP.Event (Event, makeEvent, subscribe)

data Observable :: forall k. k -> Type
data Observable a

foreign import observableToEventImpl :: forall a. (Error -> Effect Unit) -> (a -> Effect Unit) -> Observable a -> Effect (Effect Unit)

observableToEvent :: forall a. Observable a -> Event (Either Error a)
observableToEvent o = makeEvent \k ->
  observableToEventImpl (Left >>> k) (Right >>> k) o

foreign import eventToObservableImpl :: forall a. ((Error -> Effect Unit) -> (a -> Effect Unit) -> Effect (Effect Unit)) -> Observable a

eventToObservable :: forall a. Event (Either Error a) -> Observable a
eventToObservable e = eventToObservableImpl \errF resF -> subscribe e case _ of
  Left err -> errF err
  Right res -> resF res