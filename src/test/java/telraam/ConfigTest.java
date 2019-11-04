package telraam;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

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