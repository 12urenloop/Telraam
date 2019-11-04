package telraam.database.daos.jdbc;

import telraam.database.DataAccessContext;
import telraam.database.DataAccessException;
import telraam.database.daos.BatonDAO;

import java.sql.Connection;
import java.sql.SQLException;

/**
 * This is the factory for the model DAO's for this specific implementation.
 */
public class JDBCDataAccessContext implements DataAccessContext {

    private Connection connection;

    JDBCDataAccessContext(Connection connection) {
        this.connection = connection;
    }


    @Override
    public BatonDAO getBatonDAO() {
        return new JDBCBatonDAO(connection);
    }

    @Override
    public Connection getConnection() {
        return connection;
    }

    @Override
    public void close() throws DataAccessException {
        try {
            connection.close();
        } catch (SQLException ex) {
            throw new DataAccessException("Could not close context", ex);
        }
    }
}
