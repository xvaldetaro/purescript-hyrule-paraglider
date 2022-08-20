module Paraglider.Operator.Take where

import Prelude

import Control.Monad.ST.Class (class MonadST)
import Data.Maybe (Maybe(..))
import FRP.Event (AnEvent, makeEvent, subscribe)
import Paraglider.Util.DisposingRef as DisposingRef
import Paraglider.Util.STRefWrapper as RefW

-- | Terminates upstream subscription after `n` emissions
take :: ∀ a m s. MonadST s m => Applicative m => Int -> AnEvent m a -> AnEvent m a
take n e = makeEvent \k -> do
  subRef <- DisposingRef.create
  countRef <- RefW.new n
  sub <- subscribe e \a -> do
    count <- RefW.read countRef
    when (count == 1) $ DisposingRef.dispose subRef
    when (count > 0) do
      RefW.write (count - 1) countRef
      k a
  DisposingRef.addSub subRef sub
  pure $ DisposingRef.dispose subRef

-- | Terminates upstream subscription once the predicate check fails
takeWhile :: ∀ a b m s. MonadST s m => Applicative m => (a -> Maybe b) -> AnEvent m a -> AnEvent m b
takeWhile f e = makeEvent \k -> do
  subRef <- DisposingRef.create
  sub <- subscribe e \a -> do
    case f a of
      Nothing -> DisposingRef.dispose subRef
      Just b -> k b
  DisposingRef.addSub subRef sub
  pure $ DisposingRef.dispose subRef

-- | Terminates upstream subscription once the predicate check fails
takeWhile' :: ∀ a m s. MonadST s m => Applicative m => (a -> Boolean) -> AnEvent m a -> AnEvent m a
takeWhile' f e = takeWhile (\x -> if f x then Just x else Nothing) e