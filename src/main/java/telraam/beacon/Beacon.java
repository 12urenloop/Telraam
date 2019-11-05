package telraam.beacon;

import java.net.Socket;

public class Beacon extends EventGenerator<BeaconMessage> {
    public Beacon(Callback<Void, Event<BeaconMessage>> h) {
        super(h);
    }

    public Beacon(Socket socket, Callback<Void, Event<BeaconMessage>> h) {
        this(h);

        // socket.getInputStream().readNBytes(10);

        // TODO: Setup socket, generating B things somehow
    }
}
