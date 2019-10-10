package telraam;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;


public class AppTest {
    @Test
    public void testAppHasGreeting() {
        App testInstance = new App();
        assertNotNull(testInstance.greeting());

    }
}
