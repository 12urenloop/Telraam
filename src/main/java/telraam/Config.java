package telraam;

import java.io.IOException;
import java.io.InputStream;
import java.lang.module.Configuration;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Config {
    private static final Logger logger =
            Logger.getLogger(Config.class.getName());
    private static Config myInstance;
    private String dbUrl;
    private Properties properties;
    private String propertyFile;
    private Map<String, String> propFileMap;

    private Config() {
        this.propFileMap =
                Map.of("TESTING", "testConfig.properties", "DEVELOPMENT",
                        "devConfig.properties", "PRODUCTION",
                        "prodConfig.properties");
        String envKey = System.getenv("CONFIG_KEY");
        if (envKey == null ||
                !this.propFileMap.containsKey(envKey.toUpperCase())) {
            logger.log(Level.SEVERE,
                    "Environment variable CONFIG_KEY must be set to one of TESTING, DEVELOPMENT or PRODUCTION");
            throw new RuntimeException(
                    "Could not initialize: CONFIG_KEY missing");
        }

        this.propertyFile = this.propFileMap.get(envKey.toUpperCase());
        logger.log(Level.INFO,
                String.format("Running in %s mode", envKey.toUpperCase()));

        this.properties = new Properties();
        try (InputStream inputStream = this.getClass()
                .getResourceAsStream(propertyFile)) {

            this.properties.load(inputStream);
            this.dbUrl = this.properties.getProperty("DB_URL");

        } catch (IOException e) {

            String errorMsg =
                    String.format("Could not load property file: %s%n%s",
                            propertyFile, e.getMessage());
            logger.log(Level.SEVERE, errorMsg);
            throw new RuntimeException(
                    "Initialization failed: unable to load property file");
        }
    }

    public static Config getInstance() {
        if (myInstance == null) {
            myInstance = new Config();
        }
        return myInstance;
    }

    public String getDbUrl() {
        return dbUrl;
    }
}
