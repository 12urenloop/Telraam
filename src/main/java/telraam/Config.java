package telraam;

import java.io.IOException;
import java.io.InputStream;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Singleton for loading config settings. Requires a CONFIG_KEY environment
 * variable to exist.
 */
public class Config {
    private static final Logger logger =
            Logger.getLogger(Config.class.getName());
    private static Config myInstance;
    private String dbUrl;
    private Properties properties;
    private String propertyFile;
    private Map<String, String> propFileMap;
    private String configKey;

    private Config() {
        this.propFileMap =
                Map.of("TESTING", "testConfig.properties", "DEVELOPMENT",
                        "devConfig.properties", "PRODUCTION",
                        "prodConfig.properties");
        String envKey = System.getenv("CONFIG_KEY");
        if (envKey == null ||
                !this.propFileMap.containsKey(envKey.toUpperCase())) {
            logger.severe(
                    "Environment variable CONFIG_KEY must be set to one of TESTING, DEVELOPMENT or PRODUCTION");
            throw new RuntimeException(
                    "Could not initialize: CONFIG_KEY missing");
        }

        this.configKey = envKey.toUpperCase();
        this.propertyFile = this.propFileMap.get(this.configKey);
        // Since there is a method call in the info() call, sonar wants us
        // to make sure the info level is actually active, since the
        // method within info() will be called regardless of whether
        // the logging is active or not, creating unnecessary overhead
        if (logger.isLoggable(Level.INFO)) {
            logger.info(String.format("Running in %s mode", this.configKey));
        }

        this.properties = new Properties();
        try (InputStream inputStream = this.getClass()
                .getResourceAsStream(propertyFile)) {

            this.properties.load(inputStream);
            this.dbUrl = this.properties.getProperty("DB_URL");

        } catch (IOException e) {

            String errorMsg =
                    String.format("Could not load property file: %s%n%s",
                            propertyFile, e.getMessage());
            logger.severe(errorMsg);
            throw new RuntimeException(
                    "Initialization failed: unable to load property file");
        }
    }

    /**
     * Initialize the singleton instance if it doesn't exist, and return it
     *
     * @return the instance
     */
    public static Config getInstance() {
        if (myInstance == null) {
            myInstance = new Config();
        }
        return myInstance;
    }

    /**
     * Get the database URL corresponding to the current environment
     *
     * @return the url
     */
    public String getDbUrl() {
        return dbUrl;
    }

    /**
     * Get the currently active environment
     *
     * @return DEVELOPMENT, PRODUCTION or TESTING
     */
    public String getCurrentEnvironment() {
        return this.configKey;
    }
}
