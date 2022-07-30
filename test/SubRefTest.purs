module Test.SubRefTest where

import Prelude

import Control.Monad.ST.Class (liftST)
import Data.Array (snoc)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), joinFiber, launchAff, launchAff_)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import SubRef (addSub, create, dispose, dispose')
import Test.Helper (assertRef')

test :: Effect Unit
test = do
  resultRef <- Ref.new []
  let work x = Ref.modify_ (flip snoc x) resultRef
  subRef <- create
  addSub subRef $ work 1
  fiber <- launchAff do
    Aff.delay $ Milliseconds 10.0
    liftEffect $ liftST $ addSub subRef $ work 3
  addSub subRef $ work 2
  dispose' subRef
  assertRef' "subRef did sync works" resultRef [1,2]
  launchAff_ do
    joinFiber fiber
    liftEffect $ assertRef' "subRef did async work as well" resultRef [1,2,3]
    disposal <- liftST $ dispose subRef
    liftEffect $ disposal
    liftEffect $ assertRef' "subRef double dispose is no-op" resultRef [1,2,3]
