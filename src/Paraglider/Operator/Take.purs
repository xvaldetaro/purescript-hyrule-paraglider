module Paraglider.Operator.Take where

import Prelude

import Data.Maybe (Maybe(..))
import FRP.Event (Event, makeLemmingEvent)
import Paraglider.Util.DisposingRef as DisposingRef
import Paraglider.Util.STRefWrapper as RefW

-- | Terminates upstream subscription after `n` emissions
take :: ∀ a. Int -> Event a -> Event a
take n e = makeLemmingEvent \subscribe k -> do
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
takeWhile :: ∀ a b. (a -> Maybe b) -> Event a -> Event b
takeWhile f e = makeLemmingEvent \subscribe k -> do
  subRef <- DisposingRef.create
  sub <- subscribe e \a -> do
    case f a of
      Nothing -> DisposingRef.dispose subRef
      Just b -> k b
  DisposingRef.addSub subRef sub
  pure $ DisposingRef.dispose subRef

-- | Terminates upstream subscription once the predicate check fails
takeWhile' :: ∀ a. (a -> Boolean) -> Event a -> Event a
takeWhile' f e = takeWhile (\x -> if f x then Just x else Nothing) e