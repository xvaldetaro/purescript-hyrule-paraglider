# Paraglider makes it easier to explore Hyrule

Provides:
* Collection of Rx-inpired Operators for [hyrule](https://github.com/mikesol/purescript-hyrule)'s FRP.Event and [halogen-subscriptions](https://github.com/purescript-halogen/purescript-halogen-subscriptions)'s Emitter
* Few methods to help interop `Event` and `Aff`.

Refer to Rx's documentation for the similar methods to understand the functionality (specially marble diagrams) 

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
Look at the [Event test](https://github.com/xvaldetaro/purescript-hyrule-paraglider/blob/main/test/Main.purs) for `FRP.Event` examples.
Or Look at [Emitter test](https://github.com/xvaldetaro/purescript-hyrule-paraglider/blob/main/test/HaloSubTest.purs) for `Emitter` examples.
