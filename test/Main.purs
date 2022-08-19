module Test.Main where

import Prelude

import Control.Alt ((<|>))
import Data.Array (snoc)
import Data.Foldable (oneOfMap)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), launchAff_)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import FRP.Event (create)
import Paraglider.Operator.BlockingGetN (blockingGetN)
import Paraglider.Operator.Combine (combineFold, combineLatest)
import Paraglider.Operator.FlatMap (flatMap)
import Paraglider.Operator.FromAff (fromAff)
import Paraglider.Operator.FromCallable (fromCallable)
import Paraglider.Operator.Replay (replayRefCount)
import Paraglider.Operator.SkipWhile (skipWhile)
import Paraglider.Operator.Take (take, takeWhile)
import Test.Assert (assertEqual')
import Test.DisposingRefTest as DisposingRefTest
import Test.Helper (assertRef', testSubscribe)
import Test.Spec (describe, it)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

  -- testFlatMap
  -- testTakeWhile
  -- testSkipWhile
  -- testCombineFold
  -- testRefCount
  -- DisposingRefTest.test
  -- testCombineLatest
  -- testBangs
  -- testTake
  -- launchAff_ do
  --   testFromAff
  --   testBlocking

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Flat Map" do
    it "should emit from all upstream sources" $ liftEffect do
      {event, push} <- create
      t <- testSubscribe $ flatMap identity event
      push (pure 1 <|> pure 2)
      {event: innerEv, push: innerPush} <- create
      push (innerEv)
      innerPush 3
      push (pure 4 <|> pure 5)
      innerPush 6
      innerPush 7
      push (pure 8)
      innerPush 9
      assertRef' t.capturesRef [1,2,3,4,5,6,7,8,9]

  describe "takeWhile" do
    it "should stop upstream subscription after n" $ liftEffect do
      {event, push} <- create
      t <- testSubscribe $ takeWhile (\x -> if x < 2 then Just (x + 100) else Nothing) event
      push 0
      push 1
      push 4
      push 1
      assertRef' t.capturesRef [100,101]

  describe "skipWhile" do
    it "should filter until first pred check succeeds" $ liftEffect do
      {event, push} <- create
      t <- testSubscribe $ skipWhile (_ < 2) event
      push 0
      push 1
      push 4
      push 1
      assertRef' t.capturesRef [4,1]

  describe "combineFold" do
    it "should fold only after all upstream emitted" $ liftEffect do
      {event, push} <- create
      t <- testSubscribe $ combineFold (flip snoc) [] [pure 1, pure 2, event, pure 10]
      push 3
      push 4
      t.subscription
      push 5
      assertRef' t.capturesRef [[1,2,3,10], [1,2,4,10]]

  describe "refCount" do
    it "should keep a single sub to upstream while there is a ds sub" $ liftEffect do
      {event, push} <- create
      refEvent <- replayRefCount event
      t <- testSubscribe refEvent
      push 1
      push 2
      t2 <- testSubscribe refEvent
      assertRef' t.capturesRef [1,2]
      assertRef' t2.capturesRef [2]
      push 3
      assertRef' t.capturesRef [1,2,3]
      assertRef' t2.capturesRef [2,3]
      t.subscription
      push 4
      assertRef' t.capturesRef [1,2,3]
      assertRef' t2.capturesRef [2,3,4]
      t2.subscription
      push 5
      assertRef' t.capturesRef [1,2,3]
      assertRef' t2.capturesRef [2,3,4]
      t3 <- testSubscribe refEvent
      t4 <- testSubscribe refEvent
      assertRef' t3.capturesRef []
      assertRef' t4.capturesRef []
      push 1
      assertRef' t3.capturesRef [1]
      assertRef' t4.capturesRef [1]

  describe "blockingGet" do
    it "should emit in aff" do
      let original = [1, 2, 3]
      arr <- blockingGetN 3 $ oneOfMap pure original
      liftEffect $ assertEqual' "Blocking get" {expected: original, actual: arr}

  describe "fromAff" do
    it "pipe the Aff return to the Event" do
      let myAff = Aff.delay (Milliseconds 5.0) *> pure 3
      {capturesRef} <- liftEffect $ testSubscribe (fromAff myAff)
      liftEffect $ assertRef' capturesRef []
      Aff.delay $ Milliseconds 6.0
      liftEffect $ assertRef' capturesRef [3]
      liftEffect $ Ref.write [] capturesRef
      {subscription} <- liftEffect $ testSubscribe (fromAff myAff)
      liftEffect $ subscription
      Aff.delay $ Milliseconds 6.0
      liftEffect $ assertRef' capturesRef []

  describe "fromCallable" do
    it "should pipe the return from Effect into the Event" $ liftEffect do
      let fromCallableEmitter = fromCallable (pure 100)
      t1 <- testSubscribe (pure 3)
      t2 <- testSubscribe fromCallableEmitter
      assertRef' t1.capturesRef [3]
      assertRef' t2.capturesRef [100]
      log ""

  describe "take" do
    it "should stop upstream subscription after n emissions" $ liftEffect do
      {event, push} <- create
      t <- testSubscribe (take 3 event)
      push 1
      push 1
      push 1
      t.subscription
      push 1
      push 1
      assertRef' t.capturesRef [1,1,1]

  describe "combineLatest" do
    it "should combine upstream emissions with provided combiner f" $ liftEffect do
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

      assertRef' capturesRef [3, 5, 6, 9]
      Ref.write [] capturesRef
      liftEffect subscription
      push 4
      push2 5
      assertRef' capturesRef []

  describe "DisposingRef" do
    it "After disposable new subscriptions added should be dispose immediately " do
      DisposingRefTest.test

