module SubRef (create, addSub, dispose, SubRef, Sub) where

import Prelude

import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref

type Sub = Effect Unit

newtype SubRef = SubRef (Ref {isDisposed :: Boolean, work :: Effect Unit})

create :: Effect SubRef
create = do
  let val = {isDisposed: false, work: pure unit}
  ref <- Ref.new val
  pure $ SubRef ref

addSub :: SubRef -> Sub -> Effect Unit
addSub (SubRef ref) sub = do
  {isDisposed, work} <- Ref.read ref
  if isDisposed then sub
  else Ref.write {isDisposed, work: work *> sub} ref

dispose :: SubRef -> Effect Unit
dispose (SubRef ref) = do
  {isDisposed, work} <- Ref.read ref
  unless isDisposed do
    Ref.write {isDisposed: true, work: pure unit} ref
    work