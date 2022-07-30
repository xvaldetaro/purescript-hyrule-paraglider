module EzRef where

import Prelude

import Control.Monad.ST.Class (class MonadST, liftST)
import Control.Monad.ST.Internal (STRef)
import Control.Monad.ST.Internal as Ref

read :: ∀ a m s. MonadST s m => STRef s a -> m a
read = liftST <<< Ref.read

write :: ∀ a m s. MonadST s m => a -> STRef s a -> m Unit
write a = void <<< liftST <<< Ref.write a

new :: ∀ a m s. MonadST s m => a -> m (STRef s a)
new = liftST <<< Ref.new

modify :: ∀ a m s. MonadST s m => (a -> a) -> STRef s a -> m a
modify a = liftST <<< Ref.modify a

modify_ :: ∀ a m s. MonadST s m => (a -> a) -> STRef s a -> m Unit
modify_ a = void <<< liftST <<< Ref.modify a