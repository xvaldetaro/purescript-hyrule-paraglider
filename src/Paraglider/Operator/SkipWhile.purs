module Paraglider.Operator.SkipWhile where

import Prelude

import Control.Monad.ST.Class (class MonadST)
import Data.Filterable (filterMap)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import FRP.Event (AnEvent, mapAccum)

-- / Filters until the first predicate check succeeds. Then it emits freely.
skipWhile :: ∀ a m s. MonadST s m => Applicative m => (a -> Boolean) -> AnEvent m a -> AnEvent m a
skipWhile f e = filterMap filterF accumEv
  where
  filterF :: Tuple Boolean a -> Maybe a
  filterF (Tuple isOpen a) = if isOpen then Just a else Nothing

  accumEv :: AnEvent m (Tuple Boolean a)
  accumEv = mapAccum accumF e false

  accumF :: a -> Boolean -> Tuple Boolean (Tuple Boolean a)
  accumF a isOpen = let isOpen' = isOpen || (not $ f a) in Tuple isOpen' (Tuple isOpen' a)