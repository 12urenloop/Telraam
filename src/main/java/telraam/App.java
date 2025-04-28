package telraam;

import io.dropwizard.core.Application;
import io.dropwizard.core.setup.Bootstrap;
import io.dropwizard.core.setup.Environment;
import io.dropwizard.jdbi3.JdbiFactory;
import io.dropwizard.jdbi3.bundles.JdbiExceptionsBundle;
import io.dropwizard.jersey.setup.JerseyEnvironment;
import io.federecio.dropwizard.swagger.SwaggerBundle;
import io.federecio.dropwizard.swagger.SwaggerBundleConfiguration;
import jakarta.servlet.DispatcherType;
import jakarta.servlet.FilterRegistration;
import lombok.Getter;
import lombok.Setter;
import org.eclipse.jetty.servlets.CrossOriginFilter;
import org.eclipse.jetty.websocket.server.config.JettyWebSocketServletContainerInitializer;
import org.jdbi.v3.core.Jdbi;
import telraam.api.*;
import telraam.database.daos.*;
import telraam.database.models.Station;
import telraam.healthchecks.TemplateHealthCheck;
import telraam.logic.lapper.Lapper;
import telraam.logic.lapper.external.ExternalLapper;
import telraam.logic.lapper.robust.RobustLapper;
import telraam.logic.lapper.slapper.Slapper;
import telraam.logic.positioner.Positioner;
import telraam.logic.positioner.Stationary.Stationary;
import telraam.logic.positioner.nostradamus.v2.Nostradamus;
import telraam.logic.positioner.nostradamus.v1.NostradamusV1;
import telraam.station.FetcherFactory;
import telraam.util.AcceptedLapsUtil;
import telraam.websocket.WebSocketConnection;

import java.util.EnumSet;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

public class App extends Application<AppConfiguration> {
    private static final Logger logger = Logger.getLogger(App.class.getName());

    @Getter
    private AppConfiguration config;

    @Getter
    private Environment environment;

    @Getter
    private Jdbi database;

    @Setter
    private boolean testing;

    public static void main(String[] args) throws Exception {
        App app = new App();
        app.setTesting(false);
        app.run(args);
    }

    public App() {
        testing = true;
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
    public void run(AppConfiguration configuration, Environment environment) {
        this.config = configuration;
        this.environment = environment;
        // Add database
        final JdbiFactory factory = new JdbiFactory();
        this.database = factory.build(environment, configuration.getDataSourceFactory(), "postgresql");

        // Initialize AcceptedLapUtil
        AcceptedLapsUtil.createInstance(this.database);

        // Register websocket endpoint
        JettyWebSocketServletContainerInitializer.configure(
                environment.getApplicationContext(),
                (servletContext, wsContainer) -> {
                    wsContainer.setMaxTextMessageSize(65535);
                    wsContainer.addMapping("/ws", (req, res) -> new WebSocketConnection());
                }
        );

        // Add api resources
        JerseyEnvironment jersey = environment.jersey();
        jersey.register(new BatonResource(database.onDemand(BatonDAO.class)));
        jersey.register(new StationResource(database.onDemand(StationDAO.class)));
        jersey.register(new DetectionResource(database.onDemand(DetectionDAO.class)));
        jersey.register(new LapResource(database.onDemand(LapDAO.class)));
        jersey.register(new TeamResource(database.onDemand(TeamDAO.class), database.onDemand(BatonSwitchoverDAO.class)));
        jersey.register(new LapSourceResource(database.onDemand(LapSourceDAO.class)));
        jersey.register(new PositionSourceResource(database.onDemand(PositionSourceDAO.class)));
        jersey.register(new BatonSwitchoverResource(database.onDemand(BatonSwitchoverDAO.class)));
        jersey.register(new LapSourceSwitchoverResource(database.onDemand(LapSourceSwitchoverDAO.class)));
        jersey.register(new AcceptedLapsResource());
        jersey.register(new TimeResource());
        jersey.register(new LapCountResource(database.onDemand(TeamDAO.class), database.onDemand(LapDAO.class)));
        jersey.register(new MonitoringResource(database));
        environment.healthChecks().register("template", new TemplateHealthCheck(configuration.getTemplate()));

        // Enable CORS
        final FilterRegistration.Dynamic cors = environment.servlets().addFilter("CORS", CrossOriginFilter.class);

        // Configure CORS parameters
        cors.setInitParameter("allowedOrigins", "*");
        cors.setInitParameter("allowedHeaders", "X-Requested-With,Content-Type,Accept,Origin");
        cors.setInitParameter("allowedMethods", "OPTIONS,GET,PUT,POST,DELETE,HEAD");

        // Add URL mapping
        cors.addMappingForUrlPatterns(EnumSet.allOf(DispatcherType.class), true, "/*");

        if (!testing) {
            // Set up lapper algorithms
            Set<Lapper> lappers = new HashSet<>();

            lappers.add(new ExternalLapper(this.database));
            lappers.add(new RobustLapper(this.database));
            lappers.add(new Slapper(this.database));

            // Enable lapper APIs
            for (Lapper lapper : lappers) {
                lapper.registerAPI(jersey);
            }

            // Set up positioners
            Set<Positioner> positioners = new HashSet<>();

            positioners.add(new Stationary(this.database));
            positioners.add(new NostradamusV1(this.database));
            positioners.add(new Nostradamus(configuration, this.database));

            // Start fetch thread for each station
            FetcherFactory fetcherFactory = new FetcherFactory(this.database, lappers, positioners);
            StationDAO stationDAO = this.database.onDemand(StationDAO.class);
            for (Station station : stationDAO.getAll()) {
                new Thread(() -> {
                    var fetcher = fetcherFactory.create(station);
                    while (true) {
                        System.out.println("Starting fetcher");
                        fetcher.fetch();
                        try {
                            Thread.sleep(1000);
                        } catch (InterruptedException e) {
                            throw new RuntimeException(e);
                        }
                    }
                }).start();
            }
        }

        logger.info("Up and running!");
    }
}
