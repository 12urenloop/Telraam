package telraam.database;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertNotNull;

class ConnectionManagerTest {

    @Test
    void getInstance() {
        Database testInstance = Database.getInstance();
        assertNotNull(testInstance);
    }

    @Test
    void getConnection() {
        DataAccessContext testContext = Database.getInstance().getDataAccessContext();
        assertNotNull(testContext);
    }
}