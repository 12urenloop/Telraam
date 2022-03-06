package telraam;

import io.dropwizard.Application;
import io.dropwizard.jdbi3.JdbiFactory;
import io.dropwizard.jdbi3.bundles.JdbiExceptionsBundle;
import io.dropwizard.jersey.setup.JerseyEnvironment;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;
import io.federecio.dropwizard.swagger.SwaggerBundle;
import io.federecio.dropwizard.swagger.SwaggerBundleConfiguration;
import org.eclipse.jetty.servlets.CrossOriginFilter;
import org.jdbi.v3.core.Jdbi;
import telraam.api.*;
import telraam.database.daos.*;
import telraam.healthchecks.TemplateHealthCheck;
import telraam.station.Fetcher;

import javax.servlet.DispatcherType;
import javax.servlet.FilterRegistration;
import java.io.IOException;
import java.net.URI;
import java.util.EnumSet;
import java.util.logging.Logger;

public class App extends Application<AppConfiguration> {
    private static Logger logger = Logger.getLogger(App.class.getName());
    private AppConfiguration config;
    private Environment environment;
    private Jdbi database;

    public static void main(String[] args) throws Exception {
        new App().run(args);
    }

    @Override
    public String getName() {
        return "hello-world";
    }

    @Override
    public void initialize(Bootstrap<AppConfiguration> bootstrap) {
        // nothing to do yet
        bootstrap.addBundle(new JdbiExceptionsBundle());

        bootstrap.addBundle(new SwaggerBundle<>() {
            @Override
            protected SwaggerBundleConfiguration getSwaggerBundleConfiguration(AppConfiguration configuration) {
                return configuration.swaggerBundleConfiguration;
            }
        });
    }

    @Override
    public void run(AppConfiguration configuration, Environment environment)
            throws IOException {
        this.config = configuration;
        this.environment = environment;
        // Add database
        final JdbiFactory factory = new JdbiFactory();
        database = factory.build(environment, configuration.getDataSourceFactory(),
                "postgresql");

        // Add api resources
        JerseyEnvironment jersey = environment.jersey();
        jersey.register(new BatonResource(database.onDemand(BatonDAO.class)));
        jersey.register(new BeaconResource(database.onDemand(BeaconDAO.class)));
        jersey.register(
                new DetectionResource(database.onDemand(DetectionDAO.class)));
        jersey.register(new LapResource(database.onDemand(LapDAO.class)));
        jersey.register(new TeamResource(database.onDemand(TeamDAO.class)));
        jersey.register(new LapSourceResource(database.onDemand(LapSourceDAO.class)));
        environment.healthChecks().register("template",
                new TemplateHealthCheck(configuration.getTemplate()));

        // Enable CORS
        final FilterRegistration.Dynamic cors = environment.servlets().addFilter("CORS", CrossOriginFilter.class);

        // Configure CORS parameters
        cors.setInitParameter("allowedOrigins", "*");
        cors.setInitParameter("allowedHeaders", "X-Requested-With,Content-Type,Accept,Origin");
        cors.setInitParameter("allowedMethods", "OPTIONS,GET,PUT,POST,DELETE,HEAD");

        // Add URL mapping
        cors.addMappingForUrlPatterns(EnumSet.allOf(DispatcherType.class), true, "/*");

        Fetcher fetcher = new Fetcher();
        fetcher.addStation("http://localhost:8001/detection/");
        fetcher.addStation("http://localhost:8002/detection/");
        fetcher.addStation("http://localhost:8003/detection/");
        fetcher.addStation("http://localhost:8004/detection/");

        fetcher.addDetectionHanlder(x -> System.out.println(x.getStationId() + " " + x.getId()));

        Thread thread = new Thread(fetcher.start());
        thread.start();
    }

    public AppConfiguration getConfig() {
        return config;
    }

    public Environment getEnvironment() {
        return environment;
    }

    public Jdbi getDatabase() {
        return database;
    }
}
