module Test.Helper where

import Prelude

import Data.Array (snoc)
import Effect (Effect)
import Effect.Class.Console (log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import FRP.Event (Event, subscribe)
import Test.Assert (assertEqual', assertTrue')

assertRef :: ∀ a. String -> Ref a -> (a -> Boolean) -> Effect Unit
assertRef callsite ref pred = do
  value <- Ref.read ref
  assertTrue' callsite $ pred value

printRef :: ∀ a. Show a => Ref a -> Effect Unit
printRef ref = do
  v <- Ref.read ref
  log $ show v

assertRef' :: ∀ a. Eq a => Show a => String -> Ref a -> a -> Effect Unit
assertRef' callsite ref value = do
  value' <- Ref.read ref
  assertEqual' callsite {actual: value', expected: value}

testSubscribe :: ∀ a. Event a
  -> Effect { subscription :: Effect Unit, capturesRef :: Ref (Array a) }
testSubscribe e = do
  capturesRef <- Ref.new []
  subscription <- subscribe e \x -> Ref.modify_ (flip snoc x) capturesRef
  pure { subscription, capturesRef }