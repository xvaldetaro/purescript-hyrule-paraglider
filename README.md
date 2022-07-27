# Rx Operators for halogen-subscription

* A collection of functions that mimick Rx Operators and can be used to manipulate `Emitter` from `halogen-subscription`.
* Few methods to help interop `Emitter` and `Aff`.

### Rx Operators:
```
rxDelay :: ∀ a. Milliseconds -> Emitter a -> Emitter a
rxBlockingGetN :: ∀ a. Int -> Emitter a -> Aff (Array a)
rxFromArray :: Array ~> Emitter
rxJust :: ∀ a. a -> Emitter a
rxFromCallable :: ∀ a. Effect a -> Emitter a
rxInterval :: Milliseconds -> Milliseconds -> Emitter Unit
rxTake :: ∀ a. Int -> Emitter a -> Emitter a
rxCombineLatest :: ∀ a b c. (a -> b -> c) -> Emitter a -> Emitter b -> Emitter c
rxCombineLatest3 :: ∀ a b c d. (a -> b -> c -> d) -> Emitter a -> Emitter b -> Emitter c -> Emitter d
```

### Aff Helper methods
```
-- / A version of makeEmitter that operates on Aff instead of Effect
rxMakeEmitterAff :: ∀ a . ((a -> Effect Unit) -> Aff (Effect Unit)) -> Emitter a

-- / Pipes the returned value from the Aff into the returned Emitter
rxFromAff :: Aff ~> Emitter
```

### Examples
Look at the `test/` folder for examples.