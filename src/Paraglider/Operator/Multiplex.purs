module Paraglider.Operator.Multiplex
  ( multiplex
  , demultiplex
  , Multiplex
  , class UberPartition
  , uberPartition
  ) where

import Prelude

import Data.Compactable (compact)
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import FRP.Event (Event, mapAccum)
import Heterogeneous.Folding (class FoldingWithIndex, class HFoldlWithIndex, hfoldlWithIndex)
import Prim.Row (class Cons, class Lacks)
import Prim.RowList (class RowToList)
import Prim.RowList as RN
import Record (insert, get)
import Record as Record
import Type.Proxy (Proxy(..))

multiplex :: ∀ recordOfEvents eventOfRecord
  . HFoldlWithIndex Multiplex (Event (Record ())) recordOfEvents eventOfRecord
  => recordOfEvents
  -> eventOfRecord
multiplex e = hfoldlWithIndex Multiplex (pure {} :: Event {}) e

demultiplex :: forall r i o. RowToList i r => UberPartition r i o => Event { | i } -> { | o }
demultiplex = uberPartition (Proxy :: _ r)

data Multiplex = Multiplex

instance
  ( IsSymbol sym
  , Lacks sym r0
  , Cons sym a r0 r1
  ) =>
  FoldingWithIndex Multiplex (Proxy sym) (Event { | r0 }) (Event a) (Event { | r1 }) where
  foldingWithIndex Multiplex prop r0 a = Record.insert prop <$> a <*> r0

dedup :: forall a. Eq a => Event a -> Event a
dedup e = compact $
  mapAccum (\a b -> let ja = Just a in Tuple ja (if b == ja then Nothing else Just a)) e Nothing

class UberPartition :: forall k. k -> Row Type -> Row Type -> Constraint
class UberPartition r i o | r -> o where
  uberPartition :: Proxy r -> Event { | i } -> { | o }

instance UberPartition RN.Nil i () where
  uberPartition _ _ = {}

instance
  ( IsSymbol key
  , Lacks key o'
  , Cons key (Event value) o' o
  , Cons key value i' i
  , Eq value
  , UberPartition rest i o'
  ) =>
  UberPartition (RN.Cons key value rest) i o where
  uberPartition _ i = insert px (dedup (map (get px) i)) (uberPartition (Proxy :: _ rest) i)
    where
    px = Proxy :: _ key

