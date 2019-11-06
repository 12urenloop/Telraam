# BEACONS

Yes I really need a README to explain my shitty code.

The main class in this package is the BeaconAggregator, which is basically an `Event<B>` generator, B being the thing built from the data, here `BeaconMessages`.

`BeaconAggregator` extends `TCPFactory` that spawns `Beacon<B>`'s that actually do the generating. On TCPFactories you can subscribe handlers for basic events, like Data (with `onData`), errors (with `onError`), connects (with `onConnect`) and disconnects (with `onDisconnect`).

So beacons generate events, which is a nice way to hide IO exceptions etc that is wrapped in `Event<B>`. These get handled by `Event.EventHandlers` like the `TCPFactory`.

TODO: Function scoping, eg the functions `exit` `error` and `data` should not be public, but interfaces bla bla bla.
