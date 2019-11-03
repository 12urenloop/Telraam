package telraam.database;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.util.logging.Level;
import java.util.logging.Logger;

public class DBTest {
    private static final Logger logger =
            Logger.getLogger(DBTest.class.getName());

    public static void connect() {
        String url = "jdbc:sqlite::memory:";
        try (Connection conn = DriverManager.getConnection(url)) {
            logger.log(Level.INFO, "connection established");

        } catch (SQLException sqle) {
            logger.log(Level.SEVERE, sqle.getMessage());

        }

    }

}
