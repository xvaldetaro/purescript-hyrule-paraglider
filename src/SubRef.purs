module SubRef (create, addSub, dispose, dispose', SubRef) where

import Prelude

import Control.Monad.ST.Class (class MonadST, liftST)
import Control.Monad.ST.Internal (ST, STRef)
import Control.Monad.ST.Internal as Ref
import Effect (Effect)

newtype SubRef s subM = SubRef (STRef s {isDisposed :: Boolean, work :: subM Unit})

create :: ∀ s subM. Applicative subM => ST s (SubRef s subM)
create = do
  let val = {isDisposed: false, work: pure unit}
  ref <- Ref.new val
  pure $ SubRef ref

addSub :: ∀ s subM. Apply subM => SubRef s subM -> subM Unit -> ST s Unit
addSub (SubRef ref) sub = do
  {isDisposed, work} <-  Ref.read ref
  if isDisposed then pure unit
  else void $ Ref.write {isDisposed, work: work *> sub} ref

dispose :: ∀ s subM. Applicative subM => SubRef s subM -> ST s (subM Unit)
dispose (SubRef ref) = do
  {isDisposed, work} <- Ref.read ref
  unless isDisposed do
    void $ Ref.write {isDisposed: true, work: pure unit} ref
  pure work

dispose' :: ∀ s subM. MonadST s subM => Applicative subM => SubRef s subM -> subM Unit
dispose' (SubRef ref) = do
  {isDisposed, work} <- liftST $ Ref.read ref
  unless isDisposed do
    void $ liftST $ Ref.write {isDisposed: true, work: pure unit} ref
  work