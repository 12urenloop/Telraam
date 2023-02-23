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
import telraam.database.models.Station;
import telraam.healthchecks.TemplateHealthCheck;
import telraam.logic.Lapper;
import telraam.logic.monitoring.MonitoringLapper;
import telraam.logic.monitoring.MonitoringResource;
import telraam.logic.external.ExternalLapper;
import telraam.logic.robustLapper.RobustLapper;
import telraam.logic.viterbi.ViterbiLapper;
import telraam.station.Fetcher;
import telraam.util.AcceptedLapsUtil;

import javax.servlet.DispatcherType;
import javax.servlet.FilterRegistration;
import java.io.IOException;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Logger;

public class App extends Application<AppConfiguration> {
    private static Logger logger = Logger.getLogger(App.class.getName());
    private AppConfiguration config;
    private Environment environment;
    private Jdbi database;
    private boolean testing;

    public static void main(String[] args) throws Exception {
        App app = new App();
        app.setTesting(false);
        app.run(args);
    }

    public App() {
        testing = true;
    }

    public void setTesting(boolean testing) {
        this.testing = testing;
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
    public void run(AppConfiguration configuration, Environment environment) throws IOException {
        this.config = configuration;
        this.environment = environment;
        // Add database
        final JdbiFactory factory = new JdbiFactory();
        this.database = factory.build(environment, configuration.getDataSourceFactory(), "postgresql");

        // Initialize AcceptedLapUtil
        AcceptedLapsUtil.createInstance(this.database);

        // Add api resources
        JerseyEnvironment jersey = environment.jersey();
        jersey.register(new BatonResource(database.onDemand(BatonDAO.class)));
        jersey.register(new StationResource(database.onDemand(StationDAO.class)));
        jersey.register(new DetectionResource(database.onDemand(DetectionDAO.class)));
        jersey.register(new LapResource(database.onDemand(LapDAO.class)));
        jersey.register(new TeamResource(database.onDemand(TeamDAO.class), database.onDemand(BatonSwitchoverDAO.class)));
        jersey.register(new LapSourceResource(database.onDemand(LapSourceDAO.class)));
        jersey.register(new BatonSwitchoverResource(database.onDemand(BatonSwitchoverDAO.class)));
        jersey.register(new LapSourceSwitchoverResource(database.onDemand(LapSourceSwitchoverDAO.class)));
        jersey.register(new AcceptedLapsResource());
        jersey.register(new TimeResource());
        jersey.register(new LapCountResource(database.onDemand(TeamDAO.class)));
        environment.healthChecks().register("template", new TemplateHealthCheck(configuration.getTemplate()));


        // Enable CORS
        final FilterRegistration.Dynamic cors = environment.servlets().addFilter("CORS", CrossOriginFilter.class);

        // Configure CORS parameters
        cors.setInitParameter("allowedOrigins", "*");
        cors.setInitParameter("allowedHeaders", "X-Requested-With,Content-Type,Accept,Origin");
        cors.setInitParameter("allowedMethods", "OPTIONS,GET,PUT,POST,DELETE,HEAD");

        // Add URL mapping
        cors.addMappingForUrlPatterns(EnumSet.allOf(DispatcherType.class), true, "/*");

        if (! testing) {
            // Set up lapper algorithms
            Set<Lapper> lappers = new HashSet<>();

            // Old viterbi lapper is disabled
            //lappers.add(new ViterbiLapper(this.database));

            lappers.add(new ExternalLapper(this.database));
            lappers.add(new RobustLapper(this.database));
            lappers.add(new MonitoringLapper(this.database));

            // Enable lapper APIs
            for (Lapper lapper : lappers) {
                lapper.registerAPI(jersey);
            }

            // Start fetch thread for each station
            StationDAO stationDAO = this.database.onDemand(StationDAO.class);
            for (Station station : stationDAO.getAll()) {
                new Thread(() -> new Fetcher(this.database, station, lappers).fetch()).start();
            }
        }

        logger.info("Up and running!");
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
