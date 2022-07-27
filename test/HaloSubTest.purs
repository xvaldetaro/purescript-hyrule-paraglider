module Test.HaloSubTest where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array (snoc)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, Milliseconds(..), launchAff_, try)
import Effect.Aff (Milliseconds(Milliseconds), launchAff_)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throwException)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import HaloSubRx (blockingGetN, combineLatest, combineLatest', combineLatest3, delay, fromAff, fromArray, fromCallable, interval, just, makeEmitterAff, take)
import Halogen.Subscription (Emitter, Subscription, create, makeEmitter, notify, subscribe, unsubscribe)
import Test.Assert (assert', assertEqual', assertTrue, assertTrue')

main :: Effect Unit
main = do
  testCombineLatest
  testBangs
  testInterval
  testTake
  launchAff_ do
    testFromAff
    testBlocking

testBlocking :: Aff Unit
testBlocking = do
  let original = [1, 2, 3]
  arr <- blockingGetN 3 $ fromArray original
  liftEffect $ assert' "Blocking get" (arr == original)

testFromAff :: Aff Unit
testFromAff = do
  let myAff = Aff.delay (Milliseconds 5.0) *> pure 3
  {capturesRef} <- liftEffect $ testSubscribe (fromAff myAff)
  liftEffect $ assertRef' "fromAff before capture" capturesRef []
  Aff.delay $ Milliseconds 6.0
  liftEffect $ assertRef' "from aff after capture" capturesRef [3]
  liftEffect $ Ref.write [] capturesRef
  {subscription, capturesRef: ref2} <- liftEffect $ testSubscribe (fromAff myAff)
  liftEffect $ unsubscribe subscription
  Aff.delay $ Milliseconds 6.0
  liftEffect $ assertRef' "from aff shouldn't capture when unsubscribed" capturesRef []

testBangs :: Effect Unit
testBangs = do
  let fromCallableEmitter = fromCallable callable
  t1 <- testSubscribe (just 3)
  t2 <- testSubscribe fromCallableEmitter
  assertRef' "just value" t1.capturesRef [3]
  assertRef' "fromCallable value" t2.capturesRef [100]
  log ""
  where
  callable = pure 100

testInterval :: Effect Unit
testInterval = do
  t <- testSubscribe (interval (Milliseconds 20.0) (Milliseconds 20.0))
  assertRef' "testInterval before interval" t.capturesRef []
  launchAff_ do
    Aff.delay $ Milliseconds 65.0
    liftEffect $ assertRef' "testInterval after interval" t.capturesRef [unit, unit, unit]
    liftEffect $ unsubscribe t.subscription
    Aff.delay $ Milliseconds 6.0
    liftEffect $ assertRef' "testInterval after subscription" t.capturesRef [unit, unit, unit]

testTake :: Effect Unit
testTake = do
  {listener, emitter} <- create
  t <- testSubscribe (take 3 emitter)
  notify listener 1
  notify listener 1
  notify listener 1
  unsubscribe t.subscription
  notify listener 1
  notify listener 1
  assertRef' "testTake" t.capturesRef [1,1,1]

testCombineLatest :: Effect Unit
testCombineLatest = do
  {listener, emitter} <- create
  {listener: listener2, emitter: emitter2} <- create

  let comb = combineLatest (+) emitter emitter2

  { subscription, capturesRef } <- testSubscribe comb
  notify listener 1
  notify listener2 2
  notify listener 3
  notify listener 4
  notify listener2 5
  -- arr <- blockingGetN 9 comb
  -- log $ "array: " <> show arr
  -- unsub <- liftEffect $ subscribe comb $ \s -> log $ show s

  assertRef' "Combine latest array" capturesRef [3, 5, 6, 9]
  Ref.write [] capturesRef
  liftEffect $ unsubscribe subscription
  notify listener 4
  notify listener2 5
  assertRef' "Combine latest stopped emitting" capturesRef []

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
  assertEqual' callsite {actual: value, expected: value'}

testSubscribe :: ∀ a. Emitter a
  -> Effect { subscription :: Subscription, capturesRef :: Ref (Array a) }
testSubscribe e = do
  capturesRef <- Ref.new []
  subscription <- subscribe e \x -> Ref.modify_ (flip snoc x) capturesRef
  pure { subscription, capturesRef }
