package telraam;

import org.junit.jupiter.api.Test;
import telraam.database.ConnectionManager;

import static org.junit.jupiter.api.Assertions.*;

class ConfigTest {

    @Test
    void testInstanceCanBeAcquired() {
        Config testInstance = Config.getInstance();
        assertNotNull(testInstance);

    }

    @Test
    void testConfigKeyIsTesting() {
        Config testInstance = Config.getInstance();
        assertEquals(testInstance.getCurrentEnvironment(), "TESTING");
    }
}