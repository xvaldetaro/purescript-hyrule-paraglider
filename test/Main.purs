module Test.Main where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array (snoc)
import Data.Array.NonEmpty (fromArray)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, Milliseconds(..), delay, launchAff_, try)
import Effect.Aff (Milliseconds(Milliseconds), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throwException)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Halogen.Subscription (Emitter, Subscription, create, makeEmitter, notify, subscribe, unsubscribe)
import RxEmitter (rxBlockingGetN, rxCombineLatest, rxCombineLatest', rxCombineLatest3, rxDelay, rxFromAff, rxFromArray, rxFromCallable, rxInterval, rxJust, rxMakeEmitterAff, rxTake)
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
  arr <- rxBlockingGetN 3 $ rxFromArray original
  liftEffect $ assert' "Blocking get" (arr == original)

testFromAff :: Aff Unit
testFromAff = do
  let myAff = delay (Milliseconds 5.0) *> pure 3
  {capturesRef} <- liftEffect $ rxTestSubscribe (rxFromAff myAff)
  liftEffect $ assertRef' "fromAff before capture" capturesRef []
  delay $ Milliseconds 6.0
  liftEffect $ assertRef' "from aff after capture" capturesRef [3]
  liftEffect $ Ref.write [] capturesRef
  {subscription, capturesRef: ref2} <- liftEffect $ rxTestSubscribe (rxFromAff myAff)
  liftEffect $ unsubscribe subscription
  delay $ Milliseconds 6.0
  liftEffect $ assertRef' "from aff shouldn't capture when unsubscribed" capturesRef []

testBangs :: Effect Unit
testBangs = do
  let fromCallableEmitter = rxFromCallable callable
  t1 <- rxTestSubscribe (rxJust 3)
  t2 <- rxTestSubscribe fromCallableEmitter
  assertRef' "rxJust value" t1.capturesRef [3]
  assertRef' "rxFromCallable value" t2.capturesRef [100]
  log ""
  where
  callable = pure 100

testInterval :: Effect Unit
testInterval = do
  t <- rxTestSubscribe (rxInterval (Milliseconds 20.0) (Milliseconds 20.0))
  assertRef' "testInterval before interval" t.capturesRef []
  launchAff_ do
    delay $ Milliseconds 65.0
    liftEffect $ assertRef' "testInterval after interval" t.capturesRef [unit, unit, unit]
    liftEffect $ unsubscribe t.subscription
    delay $ Milliseconds 6.0
    liftEffect $ assertRef' "testInterval after subscription" t.capturesRef [unit, unit, unit]

testTake :: Effect Unit
testTake = do
  {listener, emitter} <- create
  t <- rxTestSubscribe (rxTake 3 emitter)
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

  let comb = rxCombineLatest (+) emitter emitter2

  { subscription, capturesRef } <- rxTestSubscribe comb
  notify listener 1
  notify listener2 2
  notify listener 3
  notify listener 4
  notify listener2 5
  -- arr <- rxBlockingGetN 9 comb
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

rxTestSubscribe :: ∀ a. Emitter a
  -> Effect { subscription :: Subscription, capturesRef :: Ref (Array a) }
rxTestSubscribe e = do
  capturesRef <- Ref.new []
  subscription <- subscribe e \x -> Ref.modify_ (flip snoc x) capturesRef
  pure { subscription, capturesRef }
