module Test.Rx where

import Prelude

import Data.Array (cons)
import Data.Either (Either(..))
import Data.Traversable (sequence)
import Effect.Class (liftEffect)
import Effect.Ref (modify_, new, read)
import FRP.Event (create, makeLemmingEvent, subscribe)
import Paraglider.Rx (eventToObservable, observableToEvent)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

testRx :: Spec Unit
testRx = describe "Rx" do
  it "Correctly turns an observable into an event" $ liftEffect do
    r <- new []
    { push, event } <- create
    let
      withUnsub = makeLemmingEvent \k -> do
        o <- subscribe event k
        pure (modify_ (cons $ Right 42) r *> o)
    u <- subscribe (observableToEvent (eventToObservable withUnsub)) \k -> modify_ (cons k) r
    push $ Right 1
    push $ Right 2
    push $ Right 3
    u
    r' <- read r
    case sequence r' of
      Right a -> a `shouldEqual` [ 42, 3, 2, 1 ]
      Left e -> fail $ "Error" <> show e

