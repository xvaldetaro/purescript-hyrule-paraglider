module Paraglider.Operator.Replay where

import Prelude

import Control.Monad.ST.Class (class MonadST)
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import FRP.Event (AnEvent, subscribe)
import Paraglider.Operator.ConnectableEvent (ConnectableEvent)
import Paraglider.Operator.DoOnSubscribe (doOnSubscribe)
import Paraglider.Operator.RefCount (refCount)
import Paraglider.Operator.ToConnectable (toConnectable)
import Paraglider.Util.STRefWrapper as RefW

-- / Once upstream is connected it will cache the last emitted value from upstream. Any downstream
-- / subscribers will get an immediate emission of the last cached value upon subscription (if any)
replay :: ∀ a m s. MonadST s m => AnEvent m a -> m (ConnectableEvent m a)
replay upstream = do
  lastEmissionRef <- RefW.new Nothing
  { connect, event } <- toConnectable upstream
  let
    connectWithWriting :: m (m Unit)
    connectWithWriting = do
      let onUnsub = RefW.write Nothing lastEmissionRef
      writeRefSub <- subscribe event \emission -> RefW.write (Just emission) lastEmissionRef
      c1 <- connect
      pure $ c1 *> writeRefSub *> onUnsub

    withReplay :: AnEvent m a
    withReplay = flip doOnSubscribe event \push -> do
      mbLastEmission <- RefW.read lastEmissionRef
      for_ mbLastEmission push

  pure { connect: connectWithWriting, event: withReplay }

-- / Just composing replay and refCount
replayRefCount :: forall a s m. Bind m => MonadST s m => AnEvent m a -> m (AnEvent m a)
replayRefCount = replay >=> refCount