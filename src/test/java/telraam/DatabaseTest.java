package telraam;

import com.codahale.metrics.MetricRegistry;
import com.codahale.metrics.jdbi3.strategies.TimedAnnotationNameStrategy;
import com.fasterxml.jackson.databind.ObjectMapper;
import io.dropwizard.db.DataSourceFactory;
import io.dropwizard.db.ManagedDataSource;
import io.dropwizard.jdbi3.JdbiFactory;
import io.dropwizard.jersey.validation.Validators;
import io.dropwizard.logging.BootstrapLogging;
import io.dropwizard.setup.Environment;
import org.eclipse.jetty.util.component.LifeCycle;
import org.flywaydb.core.Flyway;
import org.jdbi.v3.core.Jdbi;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.BeforeEach;

public abstract class DatabaseTest {
    static {
        BootstrapLogging.bootstrap();
    }

    private MetricRegistry metricRegistry = new MetricRegistry();
    private Environment environment;
    protected Jdbi jdbi;

    @BeforeEach
    public void setUp() throws Exception {
        environment = new Environment("test", new ObjectMapper(),
                Validators.newValidator(), metricRegistry,
                ClassLoader.getSystemClassLoader());
        DataSourceFactory dataSourceFactory = new DataSourceFactory();
        dataSourceFactory.setUrl("jdbc:h2:mem:jdbi3-test");
        dataSourceFactory.setUser("sa");
        dataSourceFactory.setDriverClass("org.h2.Driver");
        ManagedDataSource ds = dataSourceFactory
                .build(environment.metrics(), environment.getName());

        Flyway flyway = Flyway.configure().dataSource(ds).schemas().load();

        flyway.migrate();
        jdbi = new JdbiFactory(new TimedAnnotationNameStrategy())
                .build(environment, dataSourceFactory, ds, "h2");
        for (LifeCycle lc : environment.lifecycle().getManagedObjects()) {
            lc.start();
        }
    }

    @AfterEach
    public void tearDown() throws Exception {
        for (LifeCycle lc : environment.lifecycle().getManagedObjects()) {
            lc.stop();
        }
    }
}
