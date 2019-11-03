package telraam.database;

import org.junit.jupiter.api.Test;

import java.sql.Connection;

import static org.junit.jupiter.api.Assertions.*;

class ConnectionManagerTest {

    @Test
    void getInstance() {
        ConnectionManager testInstance = ConnectionManager.getInstance();
        assertNotNull(testInstance);
    }

    @Test
    void getConnection() {
        Connection testConnection = ConnectionManager.getInstance().getConnection();
        assertNotNull(testConnection);
    }
}