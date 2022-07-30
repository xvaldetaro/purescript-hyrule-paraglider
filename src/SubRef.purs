module SubRef (create, addSub, dispose, SubRef) where

import Prelude

import Control.Monad.ST.Class (class MonadST, liftST)
import Control.Monad.ST.Internal (STRef)
import EzRef as EzRef

-- / SubRef is like a Ref that holds a Subscription, but if you call dispose and then afterward try
-- / to add the Subscription it disposes of the Subscription immediately.
newtype SubRef s subM = SubRef (STRef s {isDisposed :: Boolean, work :: subM Unit})

create :: ∀ s subM. MonadST s subM => Applicative subM => subM (SubRef s subM)
create = do
  let val = {isDisposed: false, work: pure unit}
  ref <- EzRef.new val
  pure $ SubRef ref

addSub :: ∀ s subM. MonadST s subM => SubRef s subM -> subM Unit -> subM Unit
addSub (SubRef ref) sub = do
  {isDisposed, work} <- EzRef.read ref
  if isDisposed then sub
  else do
    void $ liftST $ EzRef.write {isDisposed, work: work *> sub} ref
    pure unit

dispose :: ∀ s subM. MonadST s subM => Applicative subM => SubRef s subM -> subM Unit
dispose (SubRef ref) = do
  {isDisposed, work} <- EzRef.read ref
  unless isDisposed do
    EzRef.write {isDisposed: true, work: pure unit} ref
  work
