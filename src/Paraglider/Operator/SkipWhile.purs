module Paraglider.Operator.SkipWhile where

import Prelude

import Data.Filterable (filterMap)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import FRP.Event (Event, mapAccum)

-- | Filters until the first predicate check succeeds. Then it emits freely.
skipWhile :: ∀ a.  (a -> Boolean) -> Event a -> Event a
skipWhile f e = filterMap filterF accumEv
  where
  filterF :: Tuple Boolean a -> Maybe a
  filterF (Tuple isOpen a) = if isOpen then Just a else Nothing

  accumEv :: Event (Tuple Boolean a)
  accumEv = mapAccum accumF e false

  accumF :: a -> Boolean -> Tuple Boolean (Tuple Boolean a)
  accumF a isOpen = let isOpen' = isOpen || (not $ f a) in Tuple isOpen' (Tuple isOpen' a)