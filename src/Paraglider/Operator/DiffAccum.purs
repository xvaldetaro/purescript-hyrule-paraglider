module Paraglider.Operator.DiffAccum where

import Prelude

import Control.Monad.ST.Class (class MonadST)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import FRP.Event (AnEvent, mapAccum)

diffAccum :: ∀ s m a id
  . Ord id
  => MonadST s m
  => (a -> id)
  -> AnEvent m (Array a)
  -> AnEvent m { added :: Map id a, removed :: Map id a, all :: Map id a }
diffAccum getId upst = mapAccum go upst Map.empty
  where
  go incomingArr accDict =
    let
        incomingDict = Map.fromFoldable $ (\item -> Tuple (getId item) item) <$> incomingArr
        newItemsDict = Map.difference incomingDict accDict
        removedItemsDict = Map.difference accDict incomingDict
    in Tuple
      incomingDict
      { added: newItemsDict
      , removed: removedItemsDict
      , all: incomingDict
      }