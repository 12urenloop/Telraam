package telraam.database;

import telraam.database.daos.BatonDAO;

import java.sql.Connection;

public interface DataAccessContext extends AutoCloseable {

    /*
        Has methods to return all data access objects necessary to make and change wanted and used objects
     */

    BatonDAO getBatonDAO();

    Connection getConnection();


    @Override
    void close() throws DataAccessException;
}
