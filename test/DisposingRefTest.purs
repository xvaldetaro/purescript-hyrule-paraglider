module Test.DisposingRefTest where

import Prelude

import Data.Array (snoc)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), joinFiber, launchAff, launchAff_)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Paraglider.DisposingRef as DRef
import Test.Helper (assertRef')

test :: Effect Unit
test = do
  resultRef <- Ref.new []
  let work x = Ref.modify_ (flip snoc x) resultRef
  subRef <- DRef.create
  DRef.addSub subRef $ work 1
  fiber <- launchAff do
    Aff.delay $ Milliseconds 10.0
    liftEffect $ DRef.addSub subRef $ work 3
  DRef.addSub subRef $ work 2
  DRef.dispose subRef
  assertRef' "subRef did sync works" resultRef [1,2]
  launchAff_ do
    joinFiber fiber
    liftEffect $ assertRef' "subRef did async work as well" resultRef [1,2,3]
    liftEffect $ DRef.dispose subRef
    liftEffect $ assertRef' "subRef double dispose is no-op" resultRef [1,2,3]
