module Paraglider.Operator.DiffAccum where

import Prelude

import Control.Monad.ST.Class (class MonadST)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple (Tuple(..))
import FRP.Event (Event, mapAccum)

diffAccum :: ∀ a id
  . Ord id
  => (a -> id)
  -> Event (Array a)
  -> Event { added :: Map id a, removed :: Map id a, all :: Map id a }
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