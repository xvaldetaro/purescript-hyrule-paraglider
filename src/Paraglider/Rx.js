import { Observable } from 'rxjs';
// observableToEventImpl :: forall a. (Error -> Effect Unit) -> (a -> Effect Unit) -> Observable a -> Effect (Effect Unit)
export const observableToEventImpl = (err) => (res) => (obs) => () => {
    const subscription = obs.subscribe({
        next(x) { res(x)(); },
        error(e) { err(e)(); },
    });
    return () => {
        subscription.unsubscribe();
    };
}

// foreign import eventToObservableImpl :: forall a. ((Error -> Effect Unit) -> (a -> Effect Unit) -> Effect (Effect Unit)) -> Observable a
export const eventToObservableImpl = (f) => new Observable(subscriber => {
    const unsub = f(err => () => subscriber.error(err))(res => () => subscriber.next(res))();
    return function unsubscribe() {
        unsub();
    };
});