package telraam;

import io.dropwizard.Application;
import io.dropwizard.jdbi3.JdbiFactory;
import io.dropwizard.jdbi3.bundles.JdbiExceptionsBundle;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;
import org.jdbi.v3.core.Jdbi;
import telraam.api.BatonResource;
import telraam.api.HelloworldResource;
import telraam.beacon.BeaconAggregator;
import telraam.database.daos.BatonDAO;
import telraam.database.models.Baton;
import telraam.database.models.Id;
import telraam.healthchecks.TemplateHealthCheck;




public class App extends Application<AppConfiguration> {
    private static Logger logger = Logger.getLogger(App.class.getName());
    private AppConfiguration config;
    private Environment environment;
    private Jdbi database;

    public static void main(String[] args) throws Exception {
        BeaconAggregator ba = new BeaconAggregator(4564);
        ba.onError((e) -> {logger.warning(e.getMessage()); return null;});
        ba.onData((e) -> {logger.info(e.toString()); return null;});
        ba.onConnect((_e) -> {logger.info("Connect"); return null;});
        ba.onDisconnect((_e) -> {logger.info("Disconnected"); return null;});
        // new App().run(args);
        ba.run();
    }

    @Override
    public String getName() {
        return "hello-world";
    }

    @Override
    public void initialize(Bootstrap<AppConfiguration> bootstrap) {
        // nothing to do yet
        bootstrap.addBundle(new JdbiExceptionsBundle());
    }


    @Override
    public void run(AppConfiguration configuration, Environment environment) throws Exception {
        this.config = configuration;
        this.environment = environment;
        // Add database
        final JdbiFactory factory = new JdbiFactory();
        database = factory.build(environment, configuration.getDataSourceFactory(), "postgresql");


        // Add api resources
        final HelloworldResource resource = new HelloworldResource(
                configuration.getTemplate(),
                configuration.getDefaultName()
        );
        environment.jersey().register(resource);
        environment.jersey().register(new BatonResource(database.onDemand(BatonDAO.class)));

        // Register healthcheck
        // environment.healthChecks().register("database", new DatabaseHealthCheck(database));
        environment.healthChecks().register("template", new TemplateHealthCheck(configuration.getTemplate()));
        logger.warning("TEST LOG");
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
