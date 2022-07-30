module Test.Main where

import Prelude

import Data.Array (length)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), launchAff_)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import FRP.Event (bus, create, subscribe)
import HyruleRx (blockingGetN, combineLatest, fromAff, fromArray, fromCallable, interval, just, replayRefCount, take)
import Test.Assert (assertEqual')
import Test.Helper (assertRef, assertRef', testSubscribe)
import Test.SubRefTest as SubRefTest

main :: Effect Unit
main = do
  testRefCount
  SubRefTest.test
  -- testCombineLatest
  -- testBangs
  -- testInterval
  testTake
  -- launchAff_ do
  --   testFromAff
  --   testBlocking

testRefCount :: Effect Unit
testRefCount = do
  {event, push} <- create
  refEvent <- replayRefCount event
  t <- testSubscribe refEvent
  push 1
  push 2
  t2 <- testSubscribe refEvent
  assertRef' "testRefCount 1" t.capturesRef [1,2]
  assertRef' "testRefCount 2" t2.capturesRef [2]
  push 3
  assertRef' "testRefCount 3" t.capturesRef [1,2,3]
  assertRef' "testRefCount 4" t2.capturesRef [2,3]
  t.subscription
  push 4
  assertRef' "testRefCount 5" t.capturesRef [1,2,3]
  assertRef' "testRefCount 6" t2.capturesRef [2,3,4]
  t2.subscription
  push 5
  assertRef' "testRefCount 7" t.capturesRef [1,2,3]
  assertRef' "testRefCount 8" t2.capturesRef [2,3,4]
  t3 <- testSubscribe refEvent
  t4 <- testSubscribe refEvent
  assertRef' "testRefCount 7" t3.capturesRef []
  assertRef' "testRefCount 8" t4.capturesRef []
  push 1
  assertRef' "testRefCount 7" t3.capturesRef [1]
  assertRef' "testRefCount 8" t4.capturesRef [1]
  -- t.subscription
  -- assertRef' "testTake" t.capturesRef [1,1,1]


testBlocking :: Aff Unit
testBlocking = do
  let original = [1, 2, 3]
  arr <- blockingGetN 3 $ fromArray original
  liftEffect $ assertEqual' "Blocking get" {expected: original, actual: arr}

testFromAff :: Aff Unit
testFromAff = do
  let myAff = Aff.delay (Milliseconds 5.0) *> pure 3
  {capturesRef} <- liftEffect $ testSubscribe (fromAff myAff)
  liftEffect $ assertRef' "fromAff before capture" capturesRef []
  Aff.delay $ Milliseconds 6.0
  liftEffect $ assertRef' "from aff after capture" capturesRef [3]
  liftEffect $ Ref.write [] capturesRef
  {subscription} <- liftEffect $ testSubscribe (fromAff myAff)
  liftEffect $ subscription
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
  t <- testSubscribe $ interval (Milliseconds 20.0)
  assertRef' "testInterval before interval" t.capturesRef []
  launchAff_ do
    Aff.delay $ Milliseconds 65.0
    liftEffect $ assertRef "testInterval after interval" t.capturesRef (length >>> eq 3)
    liftEffect $ t.subscription
    Aff.delay $ Milliseconds 6.0
    liftEffect $ assertRef "testInterval after subscription" t.capturesRef (length >>> eq 3)

testTake :: Effect Unit
testTake = do
  {event, push} <- create
  t <- testSubscribe (take 3 event)
  push 1
  push 1
  push 1
  t.subscription
  push 1
  push 1
  assertRef' "testTake" t.capturesRef [1,1,1]

testCombineLatest :: Effect Unit
testCombineLatest = do
  {event, push} <- create
  {event: event2, push: push2} <- create

  let comb = combineLatest (+) event event2

  { subscription, capturesRef } <- testSubscribe comb
  push 1
  push2 2
  push 3
  push 4
  push2 5
  -- arr <- blockingGetN 9 comb
  -- log $ "array: " <> show arr
  -- unsub <- liftEffect $ subscribe comb $ \s -> log $ show s

  assertRef' "Combine latest array" capturesRef [3, 5, 6, 9]
  Ref.write [] capturesRef
  liftEffect subscription
  push 4
  push2 5
  assertRef' "Combine latest stopped emitting" capturesRef []

