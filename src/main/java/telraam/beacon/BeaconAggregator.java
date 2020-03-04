package telraam.beacon;

import java.io.IOException;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.ArrayList;
import java.util.List;

/**
 * BeaconAggregator is the main class to aggregate BeaconMessages. Register
 * listeners to data, errors, connects and disconnects.
 * <p>
 * If port is negative no tcp socket server is created (should only be used in
 * tests).
 *
 * @author Arthur Vercruysse
 */
public class BeaconAggregator extends TCPFactory<BeaconMessage>
        implements Callback<Void, Event<BeaconMessage>>, Event.EventHandler<BeaconMessage> {
    private static Logger logger = Logger.getLogger(BeaconAggregator.class.getName());

    protected List<Callback<Void, BeaconMessage>> handlers = new ArrayList<>();
    protected List<Callback<Void, Exception>> errorHandlers = new ArrayList<>();
    protected List<Callback<Void, Void>> exitHandlers = new ArrayList<>();
    protected List<Callback<Void, Void>> connectHandlers = new ArrayList<>();

    // Create net Beacon Aggregtor, listening on this port.
    public BeaconAggregator(int port) throws IOException {
        super(port);
        initializeCreator();
    }

    // Create net Beacon Aggregtor, listening on a random port.
    public BeaconAggregator() throws IOException {
        super();
        initializeCreator();
    }

    // Set the correct handler for connecting sockets
    // Here creating Beacons.
    private void initializeCreator() {
        super.creator = (s) -> {
            try {
                new Beacon(s, this);
            } catch (IOException e) {
                logger.log(Level.WARNING, "Failed to spawn beacon", e);
            }
            return null;
        };
    }

    // This function is the core of all the event handling.
    // event.handle(this), the event handles itself, by calling the correct
    // function on the event handler (here BeaconAggregator).
    public Void handle(Event<BeaconMessage> event) {
        event.handle(this);
        return null;
    }

    public BeaconAggregator onError(Callback<Void, Exception> handler) {
        this.errorHandlers.add(handler);
        return this;
    }

    public BeaconAggregator onData(Callback<Void, BeaconMessage> handler) {
        this.handlers.add(handler);
        return this;
    }

    public BeaconAggregator onDisconnect(Callback<Void, Void> handler) {
        this.exitHandlers.add(handler);
        return this;
    }

    public BeaconAggregator onConnect(Callback<Void, Void> handler) {
        this.connectHandlers.add(handler);
        return this;
    }

    @Override
    public void exit() {
        this.exitHandlers.forEach((eh) -> eh.handle(null));
    }

    @Override
    public void connect() {
        this.connectHandlers.forEach((th) -> th.handle(null));
    }

    @Override
    public void error(Exception e) {
        this.errorHandlers.forEach((eh) -> eh.handle(e));
    }

    @Override
    public void data(BeaconMessage t) {
        this.handlers.forEach((th) -> th.handle(t));
    }
}
