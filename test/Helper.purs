module Test.Helper where

import Prelude

import Data.Array (snoc)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import FRP.Event (Event, subscribe)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

assertRef :: ∀ a. Show a => Ref a -> (a -> Boolean) -> Effect Unit
assertRef ref pred = do
  value <- Ref.read ref
  shouldSatisfy value pred

printRef :: ∀ a. Show a => Ref a -> Effect Unit
printRef ref = do
  v <- Ref.read ref
  log $ show v

assertRef' :: ∀ a. Eq a => Show a => Ref a -> a -> Effect Unit
assertRef' ref value = do
  expected <- Ref.read ref
  shouldEqual value expected

testSubscribe :: ∀ a. Event a
  -> Effect { subscription :: Effect Unit, capturesRef :: Ref (Array a) }
testSubscribe e = do
  capturesRef <- Ref.new []
  subscription <- subscribe e \x -> Ref.modify_ (flip snoc x) capturesRef
  pure { subscription, capturesRef }