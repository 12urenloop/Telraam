# BEACONS

Yes I really need a README to explain my shitty code.
So, generics are a thing in Java. But they suck.

The main class in this package is the BeaconAggregator, which is basically an `Event<B>` generator, B being the thing built from the data.

BeaconAggregator extends TCPFactory that spawns all the connecting Beacon<B> that actually do the generating. On TCPFactories you can subscribe handlers for basic events, like Errors (with `onError`), data (with `onData`) and disconnects (with `onDisconnect`).

So beacons generate events, which is a nice way to hide IO exceptions etc that is wrapped in Event<B>. These get handled by `Event.EventHandlers` like the TCPFactory.

TODO: Function scoping, eg the functions `exit` `error` and `data` should not be public, but interfaces bla bla bla.

TODO: How does one create B's from the plain sockets in Beacon. Remove B generic, I sure hope not?
