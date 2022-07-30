module Paraglider.DisposingRef (create, addSub, dispose, DisposingRef) where

import Prelude

import Control.Monad.ST.Class (class MonadST, liftST)
import Control.Monad.ST.Internal (STRef)
import Paraglider.STRefWrapper as RefW

-- / DisposingRef is like a Ref that holds a Subscription, but if you call dispose and then afterward try
-- / to add the Subscription it disposes of the Subscription immediately.
newtype DisposingRef s subM = DisposingRef (STRef s {isDisposed :: Boolean, work :: subM Unit})

create :: ∀ s subM. MonadST s subM => Applicative subM => subM (DisposingRef s subM)
create = do
  let val = {isDisposed: false, work: pure unit}
  ref <- RefW.new val
  pure $ DisposingRef ref

addSub :: ∀ s subM. MonadST s subM => DisposingRef s subM -> subM Unit -> subM Unit
addSub (DisposingRef ref) sub = do
  {isDisposed, work} <- RefW.read ref
  if isDisposed then sub
  else do
    void $ liftST $ RefW.write {isDisposed, work: work *> sub} ref
    pure unit

dispose :: ∀ s subM. MonadST s subM => Applicative subM => DisposingRef s subM -> subM Unit
dispose (DisposingRef ref) = do
  {isDisposed, work} <- RefW.read ref
  unless isDisposed do
    RefW.write {isDisposed: true, work: pure unit} ref
  work
