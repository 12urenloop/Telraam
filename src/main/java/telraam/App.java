package telraam;

import io.dropwizard.Application;
import io.dropwizard.auth.AuthDynamicFeature;
import io.dropwizard.auth.AuthValueFactoryProvider;
import io.dropwizard.auth.basic.BasicCredentialAuthFilter;
import io.dropwizard.jdbi3.JdbiFactory;
import io.dropwizard.jdbi3.bundles.JdbiExceptionsBundle;
import io.dropwizard.jersey.setup.JerseyEnvironment;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;
import io.federecio.dropwizard.swagger.SwaggerBundle;
import io.federecio.dropwizard.swagger.SwaggerBundleConfiguration;
import org.eclipse.jetty.servlets.CrossOriginFilter;
import org.glassfish.jersey.server.filter.RolesAllowedDynamicFeature;
import org.jdbi.v3.core.Jdbi;
import telraam.api.*;
import telraam.beacon.BeaconAggregator;
import telraam.database.daos.*;
import telraam.database.models.User;
import telraam.healthchecks.TemplateHealthCheck;

import javax.servlet.DispatcherType;
import javax.servlet.FilterRegistration;
import java.io.IOException;
import java.util.EnumSet;
import java.util.Map;
import java.util.Optional;
import java.util.logging.Logger;
import java.util.stream.Collectors;


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
        database =
                factory.build(environment, configuration.getDataSourceFactory(),
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
        jersey.register(new HelloworldResource("Hello %s", "Anonymous"));
        environment.healthChecks().register("template",
                new TemplateHealthCheck(configuration.getTemplate()));

        // Enable CORS
        final FilterRegistration.Dynamic cors =
                environment.servlets().addFilter("CORS", CrossOriginFilter.class);

        // Configure CORS parameters
        cors.setInitParameter("allowedOrigins", "*");
        cors.setInitParameter("allowedHeaders", "X-Requested-With,Content-Type,Accept,Origin");
        cors.setInitParameter("allowedMethods", "OPTIONS,GET,PUT,POST,DELETE,HEAD");

        // Add URL mapping
        cors.addMappingForUrlPatterns(EnumSet.allOf(DispatcherType.class), true, "/*");

        // Add basic authentication
        Map<String, String> validCredentials = config.getApplicationCredentials().stream()
                .collect(Collectors.toMap(
                        AppConfiguration.ApplicationCredentialFactory::getUsername,
                        AppConfiguration.ApplicationCredentialFactory::getPassword));
        environment.jersey().register(new AuthDynamicFeature(
                new BasicCredentialAuthFilter.Builder<User>()
                        .setAuthenticator(credentials -> {
                            // If the password is 'secret' then create a user with the specified username
                            if (validCredentials.containsKey(credentials.getUsername()) &&
                                    validCredentials.get(credentials.getUsername()).equals(credentials.getPassword())
                            ) {
                                return Optional.of(new User(credentials.getUsername()));
                            }
                            return Optional.empty();
                        })
                        .setAuthorizer((principal, role) -> {
                            // All users that can specify a valid key are atm authorized.
                            // In the feature this could check user and/or role parameters
                            return true;
                        })
                        .setRealm("SUPER SECRET STUFF")
                        .buildAuthFilter()));
        environment.jersey().register(RolesAllowedDynamicFeature.class);
        //If you want to use @Auth to inject a custom Principal type into your resource
        environment.jersey().register(new AuthValueFactoryProvider.Binder<>(User.class));

        // Add beacon aggregator / listener
        BeaconAggregator ba;
        if (configuration.getBeaconPort() < 0) {
            ba = new BeaconAggregator();
        } else {
            ba = new BeaconAggregator(configuration.getBeaconPort());
        }
        ba.onError(e -> {
            logger.warning(e.getMessage());
            return null;
        });
        ba.onData(e -> {
            logger.info(e.toString());
            return null;
        });
        ba.onConnect(_e -> {
            logger.info("Connect");
            return null;
        });
        ba.onDisconnect(_e -> {
            logger.info("Disconnected");
            return null;
        });
        Thread beaconMessages = new Thread(ba);
        beaconMessages.start();
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
