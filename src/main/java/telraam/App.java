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
import telraam.database.models.Baton;
import telraam.database.models.Station;
import telraam.database.models.Detection;
import telraam.healthchecks.TemplateHealthCheck;
import telraam.station.Fetcher;
import telraam.logic.Lapper;
import telraam.logic.viterbi.ViterbiLapper;

import javax.servlet.DispatcherType;
import javax.servlet.FilterRegistration;
import java.io.IOException;
import java.sql.Timestamp;
import java.util.EnumSet;
import java.util.HashSet;
import java.util.Optional;
import java.util.Set;
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
        this.database =
                factory.build(environment, configuration.getDataSourceFactory(),
                        "postgresql");

        // Add api resources
        JerseyEnvironment jersey = environment.jersey();
        jersey.register(new BatonResource(database.onDemand(BatonDAO.class)));
        jersey.register(new StationResource(database.onDemand(StationDAO.class)));
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
        
        // Set up lapper algorithms
        Set<Lapper> lappers = new HashSet<>();

        lappers.add(new ViterbiLapper(this.database));

        // Enable lapper APIs
        for (Lapper lapper : lappers) {
            lapper.registerAPI(jersey);
        }
        
        Fetcher fetcher = new Fetcher();

        StationDAO stationDAO = this.database.onDemand(StationDAO.class);
        DetectionDAO detectionDAO = this.database.onDemand(DetectionDAO.class);
        stationDAO.getAll().forEach(station -> fetcher.addStation(station.getUrl() + "/detections/", station.getId()));

        fetcher.addDetectionHandler(x -> {
            BatonDAO batonDAO = this.database.onDemand(BatonDAO.class);
            Optional<Baton> baton = batonDAO.getByMAC(x.getMac());
            Optional<Station> station = stationDAO.getById(x.getStationId());

            if (baton.isEmpty() || station.isEmpty()) {
                return;
            }

            Detection detection = new Detection(
                baton.get().getId(),
                station.get().getId(),
                new Timestamp(x.getDetectionTimestamp())
            );

            detectionDAO.insert(detection);

            for (Lapper lapper : lappers) {
                lapper.handle(detection);
            }
        });

        Thread thread = new Thread(fetcher.start());
        thread.start();
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
