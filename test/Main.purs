module Test.Main where

import Prelude

import Data.Array (cons, snoc)
import Data.Foldable (oneOfMap)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), launchAff_)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import FRP.Event (bang, create)
import Paraglider.AffBridge (blockingGetN, fromAff, fromCallable)
import Paraglider.Rx (combineFold, combineFold', combineLatest, replayRefCount, skipWhile, take, takeWhile)
import Test.Assert (assertEqual')
import Test.DisposingRefTest as DisposingRefTest
import Test.Helper (assertRef', testSubscribe)

main :: Effect Unit
main = do
  testTakeWhile
  testSkipWhile
  testCombineFold
  testRefCount
  DisposingRefTest.test
  testCombineLatest
  testBangs
  testTake
  launchAff_ do
    testFromAff
    testBlocking

testTakeWhile :: Effect Unit
testTakeWhile = do
  {event, push} <- create
  t <- testSubscribe $ takeWhile (\x -> if x < 2 then Just (x + 100) else Nothing) event
  push 0
  push 1
  push 4
  push 1
  assertRef' "testTakeWhile 1" t.capturesRef [100,101]

testSkipWhile :: Effect Unit
testSkipWhile = do
  {event, push} <- create
  t <- testSubscribe $ skipWhile (_ < 2) event
  push 0
  push 1
  push 4
  push 1
  assertRef' "testSkipWhile 1" t.capturesRef [4,1]

testCombineFold :: Effect Unit
testCombineFold = do
  {event, push} <- create
  t <- testSubscribe $ combineFold (flip snoc) [] [bang 1, bang 2,  event, bang 10]
  push 3
  push 4
  assertRef' "testCombineFold 1" t.capturesRef [[1,2,3,10], [1,2,4,10]]

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
  assertRef' "testRefCount 9" t3.capturesRef []
  assertRef' "testRefCount 10" t4.capturesRef []
  push 1
  assertRef' "testRefCount 11" t3.capturesRef [1]
  assertRef' "testRefCount 12" t4.capturesRef [1]
  -- t.subscription
  -- assertRef' "testTake" t.capturesRef [1,1,1]


testBlocking :: Aff Unit
testBlocking = do
  let original = [1, 2, 3]
  arr <- blockingGetN 3 $ oneOfMap bang original
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
  t1 <- testSubscribe (bang 3)
  t2 <- testSubscribe fromCallableEmitter
  assertRef' "just value" t1.capturesRef [3]
  assertRef' "fromCallable value" t2.capturesRef [100]
  log ""
  where
  callable = pure 100

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

