module Platform.Misc where

import Prelude

import Bolson.Core as Bolson
import Control.Alt ((<|>))
import Data.Monoid.Always (class Always)
import Data.Symbol (class IsSymbol)
import Deku.Core (class Korok, Domable, envy)
import Effect (Effect)
import Effect.Class.Console (log)
import FRP.Event (AnEvent, bang, fromEvent, toEvent)
import Paraglider.AffBridge (fromEffect)
import Paraglider.Rx (doOnSubscribe, doOnUnsubscribe, replayRefCount)
import Prim.Row as R
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))

type Nut_ s m l p = Korok s m => Domable m l p

class InitializeEvents :: forall k. k -> Row Type -> Row Type -> Constraint
class InitializeEvents needleRL needle haystack where
  initializeEvents' :: Proxy needleRL -> { | needle } -> { | haystack } -> { | haystack }

instance InitializeEvents RL.Nil r1 r2 where
  initializeEvents' _ _ = identity

instance
  ( Applicative m
  , IsSymbol key
  , R.Cons key value needle' needle
  , R.Cons key (AnEvent m value) haystack' haystack
  , InitializeEvents rest needle haystack
  ) =>
  InitializeEvents (RL.Cons key value rest) needle haystack where
  initializeEvents' _ needle haystack =
    let
      key = Proxy :: _ key
    in
      initializeEvents' (Proxy :: _ rest) needle
        (Record.modify key (bang (Record.get key needle) <|> _) haystack)

initializeEvents
  :: forall needleRL needle haystack
   . RL.RowToList needle needleRL
  => InitializeEvents needleRL needle haystack
  => { | needle }
  -> { | haystack }
  -> { | haystack }
initializeEvents = initializeEvents' (Proxy :: _ needleRL)

type PlainOl :: forall k. k -> k
type PlainOl t = t

wrapLogs
  :: forall s m a
   . Korok s m
  => Always (m Unit) (Effect Unit)
  => String
  -> String
  -> AnEvent m a
  -> AnEvent m a
wrapLogs onSub onUnsub e =
  fromEvent $ doOnSubscribe (\_ -> log onSub) $ doOnUnsubscribe (log onUnsub) $ toEvent e

shareEvent
  :: forall s m lock logic obj a
   . Korok s m
  => Always (m Unit) (Effect Unit)
  => AnEvent m a
  -> (AnEvent m a -> Bolson.Entity logic obj m lock)
  -> Bolson.Entity logic obj m lock
shareEvent e f = envy $ fromEffect (f <$> replayRefCount e)

usingEffect
  :: forall s m lock logic obj a
   . Korok s m
  => Always (m Unit) (Effect Unit)
  => m a
  -> (a -> Bolson.Entity logic obj m lock)
  -> Bolson.Entity logic obj m lock
usingEffect effect f = envy $ fromEffect (f <$> effect)