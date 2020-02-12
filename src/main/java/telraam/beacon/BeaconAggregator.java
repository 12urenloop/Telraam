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
 * If port is negative no server is started (should only be used in tests).
 *
 * @author Arthur Vercruysse
 */
public class BeaconAggregator extends TCPFactory<BeaconMessage>
        implements Callback<Void, Event<BeaconMessage>>,
        Event.EventHandler<BeaconMessage> {
    private static Logger logger =
            Logger.getLogger(BeaconAggregator.class.getName());

    protected List<Callback<Void, BeaconMessage>> handlers = new ArrayList<>();
    protected List<Callback<Void, Exception>> errorHandlers = new ArrayList<>();
    protected List<Callback<Void, Void>> exitHandlers = new ArrayList<>();
    protected List<Callback<Void, Void>> connectHandlers = new ArrayList<>();

    public BeaconAggregator(int port) throws IOException {
        // Does not work, java can't handle cool code
        // super((s) -> new Beacon(s, this), port);
        super(port);
        initializeCreator();
    }

    public BeaconAggregator() throws IOException {
        super();
        initializeCreator();
    }

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

    public Void handle(Event<BeaconMessage> event) {
        // this is the handler for event.
        // Sending the data to the correct handlers set by TCPFactory
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

    public void exit() {
        this.exitHandlers.forEach((eh) -> eh.handle(null));
    }

    public void connect() {
        this.connectHandlers.forEach((th) -> th.handle(null));
    }

    public void error(Exception e) {
        this.errorHandlers.forEach((eh) -> eh.handle(e));
    }

    public void data(BeaconMessage t) {
        this.handlers.forEach((th) -> th.handle(t));
    }
}
