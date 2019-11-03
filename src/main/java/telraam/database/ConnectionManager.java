package telraam.database;

import telraam.Config;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Singleton for managing the database connection.
 */
public class ConnectionManager {
    private static final Logger logger =
            Logger.getLogger(ConnectionManager.class.getName());
    private static ConnectionManager myInstance;
    private Connection connection;

    private ConnectionManager() {
        try (Connection conn = DriverManager
                .getConnection(Config.getInstance().getDbUrl())) {
            this.connection = conn;
        } catch (SQLException sqle) {
            logger.log(Level.SEVERE, sqle.getMessage());
        }
    }

    /**
     * Initialize the singleton instance if it doesn't exist, and return it
     *
     * @return the instance
     */
    public static ConnectionManager getInstance() {
        if (myInstance == null) {
            myInstance = new ConnectionManager();
        }
        return myInstance;
    }

    /**
     * Get the database connection.
     *
     * @return the connection.
     */
    public Connection getConnection() {
        return connection;
    }


}
