package telraam;

import io.dropwizard.Application;
import io.dropwizard.jdbi3.JdbiFactory;
import io.dropwizard.jdbi3.bundles.JdbiExceptionsBundle;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;
import org.jdbi.v3.core.Jdbi;
import telraam.api.BatonResource;
import telraam.api.HelloworldResource;
import telraam.database.daos.BatonDAO;
import telraam.database.models.Baton;
import telraam.database.models.Id;
import telraam.healthchecks.TemplateHealthCheck;

import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.stream.Collectors;


public class App extends Application<AppConfiguration> {
    private static Logger logger = Logger.getLogger(App.class.getName());

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
    }

    @Override
    public void run(AppConfiguration configuration, Environment environment) throws Exception {
        // Add database
        final JdbiFactory factory = new JdbiFactory();
        final Jdbi database = factory.build(environment, configuration.getDataSourceFactory(), "postgresql");

        final BatonDAO dao = database.onDemand(BatonDAO.class);
        Id id = dao.insert(new Baton("Hjhgfdfghjhgfeto"));

        // TODO By default everything should be logged to stdout (see dropwizard logging docs) but it isn't
        List<Baton> batons = dao.listBatons();
        if(logger.isLoggable(Level.INFO)) {
            logger.info("Baton testing information");
            logger.info(batons.stream().map(Baton::getName).collect(Collectors.joining(" : ")));
            logger.info(String.valueOf(dao.findBatonById(id.getId())));
        }

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
}
