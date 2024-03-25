package telraam;

import io.dropwizard.core.setup.Environment;
import io.dropwizard.db.DataSourceFactory;
import io.dropwizard.db.ManagedDataSource;
import io.dropwizard.testing.ResourceHelpers;
import io.dropwizard.testing.junit5.DropwizardAppExtension;
import io.dropwizard.testing.junit5.DropwizardExtensionsSupport;
import org.flywaydb.core.Flyway;
import org.jdbi.v3.core.Jdbi;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;

@ExtendWith(DropwizardExtensionsSupport.class)
public abstract class DatabaseTest {
    private static final String CONFIG_PATH =
            ResourceHelpers.resourceFilePath("telraam/testConfig.yml");
    protected static final DropwizardAppExtension<AppConfiguration>
            APP_EXTENSION =
            new DropwizardAppExtension<>(App.class,
                    CONFIG_PATH);
    protected static Jdbi jdbi;
    private static Flyway flyway;

    @BeforeAll
    public static void initialize() {
        Environment environment = APP_EXTENSION.getEnvironment();
        DataSourceFactory dataSourceFactory =
                APP_EXTENSION.getConfiguration().getDataSourceFactory();
        ManagedDataSource ds = dataSourceFactory
                .build(environment.metrics(),
                        environment.getName());
        flyway = Flyway.configure()
                .dataSource(ds)
                .schemas()
                .cleanDisabled(false)
                .load();
        jdbi = ((App) APP_EXTENSION.getApplication()).getDatabase();
    }

    @BeforeEach
    public void setUp() throws Exception {
        flyway.clean();
        flyway.migrate();
    }

}
