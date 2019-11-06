package telraam;

import telraam.beacon.BeaconAggregator;
import telraam.database.Database;

import java.io.IOException;
import java.sql.Connection;
import java.util.logging.Level;
import java.util.logging.Logger;

public class App {
    private static Logger logger = Logger.getLogger(App.class.getName());

    public static void main(String[] args) throws IOException {
        logger.log(Level.INFO, "Main method");
        // Connection conn = Database.getInstance().getDataAccessContext().getConnection();
        BeaconAggregator ba = new BeaconAggregator(5678);
        ba.onData((b) -> {
            logger.log(Level.INFO, new String(b.data));
            return null;
        });
        ba.onError((b) -> {
            logger.log(Level.WARNING, b.getMessage());
            return null;
        });
        ba.onDisconnect((_e) -> {
            logger.log(Level.INFO, "Beacon disconnected");
            return null;
        });

        ba.onConnect((_e) -> {
            logger.log(Level.INFO, "Beacon connected");
            return null;
        });

        ba.run();
    }

    /**
     * Temporary test method
     */
    public String greeting() {
        return "test";
    }
}
