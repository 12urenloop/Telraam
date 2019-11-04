package telraam.database.jdbc;

import telraam.Config;
import telraam.database.DataAccessContext;
import telraam.database.DataAccessProvider;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.logging.Logger;

/**
 * Singleton for managing the database connection.
 */
public class JDBCDataAccessProvider implements DataAccessProvider {
    private static final Logger logger =
            Logger.getLogger(JDBCDataAccessProvider.class.getName());

    public JDBCDataAccessProvider() {
        initDriver();
    }

    /**
     * Get the database connection.
     *
     * @return the connection.
     */
    private Connection getConnection() throws SQLException {
        return DriverManager
                .getConnection(Config.getInstance().getDbUrl());
    }

    private void initDriver() {
        try {
            Class.forName(Config.getInstance().getDbDriver());
        } catch (ClassNotFoundException ex) {
            throw new RuntimeException("Could not find database driver", ex);
        }
    }

    @Override
    public DataAccessContext getDataAccessContext() {
        try {
            return new JDBCDataAccessContext(getConnection());
        } catch (SQLException ex) {
            // TODO decide if we want to log here or throw the exception further up, returning null normally means there is no connection
            // throw new DataAccessException("Could not create data access context", ex);
            logger.severe(String.format("Could not create a data access context %nReason: %s", ex.getMessage()));
            return null;
        }
    }

}
