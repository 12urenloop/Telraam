package telraam.database;

import telraam.database.daos.jdbc.JDBCDataAccessProvider;

public class Database {

    private static Database myInstance;

    // This defines what implementation we are using. Here this will be JDBC.
    private DataAccessProvider dap = new JDBCDataAccessProvider();

    /**
     * Initialize the singleton instance if it doesn't exist, and return it
     *
     * @return the instance
     */
    public static Database getInstance() {
        if (myInstance == null) {
            myInstance = new Database();
        }
        return myInstance;
    }

    /**
     * Creates a database connection and returns an
     * @return access object for our DAO's with an open connection
     */
    public DataAccessContext getDataAccessContext(){
        return dap.getDataAccessContext();
    }
}
