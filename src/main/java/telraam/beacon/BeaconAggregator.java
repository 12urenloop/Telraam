package telraam.beacon;

import java.io.IOException;

public class BeaconAggregator extends TCPFactory<BeaconMessage> implements Callback<Void, Event<BeaconMessage>> {

    public BeaconAggregator(int port) throws IOException {
        // Does not work, java can't handle cool code
        // super((s) -> new Beacon(s, this), port);
        super(port);
        super.creator = (s) -> {
            new Beacon(s, this);
            return null;
        };
    }

    public Void handle(Event<BeaconMessage> event) {
        // this is the handler for event.
        // Sending the data to the correct handlers set by TCPFactory
        event.handle(this);
        return null;
    }
}
