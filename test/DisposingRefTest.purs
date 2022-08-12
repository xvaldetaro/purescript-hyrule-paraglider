module Test.DisposingRefTest where

import Prelude

import Data.Array (snoc)
import Effect.Aff (Aff, Milliseconds(..), forkAff, joinFiber)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Paraglider.DisposingRef as DRef
import Test.Helper (assertRef')

test :: Aff Unit
test = do
    resultRef <- liftEffect $ Ref.new []
    let work x = liftEffect $ Ref.modify_ (flip snoc x) resultRef
    subRef <- liftEffect $ DRef.create
    liftEffect $ DRef.addSub subRef $ work 1

    fiber <- forkAff do
      Aff.delay $ Milliseconds 10.0
      liftEffect $ DRef.addSub subRef $ work 3

    liftEffect $ DRef.addSub subRef $ work 2
    liftEffect $ DRef.dispose subRef
    liftEffect $ assertRef' resultRef [1,2]

    joinFiber fiber
    liftEffect $ assertRef' resultRef [1,2,3]
    liftEffect $ DRef.dispose subRef
    liftEffect $ assertRef' resultRef [1,2,3]
