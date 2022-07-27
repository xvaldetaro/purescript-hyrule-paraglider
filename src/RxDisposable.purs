module RxDisposable where

import Prelude

import Effect (Effect)

type Disposable = Effect Unit
newtype CompositeDisposable = CompositeDisposable { disposables :: Array Disposable }