package telraam;

import java.util.logging.Level;
import java.util.logging.Logger;

public class App {
    private static Logger logger = Logger.getLogger(App.class.getName());

    public static void main(String[] args) {
        logger.log(Level.INFO, "Main method");
    }

    /**
     * Temporary test method
     */
    public String greeting() {

        return "test";
    }
}
