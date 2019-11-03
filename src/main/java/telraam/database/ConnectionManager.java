package telraam.database;

import telraam.Config;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.logging.Level;
import java.util.logging.Logger;

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

    public static ConnectionManager getInstance() {
        if (myInstance == null) {
            myInstance = new ConnectionManager();
        }
        return myInstance;
    }

    public Connection getConnection() {
        return connection;
    }


}
