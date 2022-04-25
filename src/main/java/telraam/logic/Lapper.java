package telraam.logic;

import io.dropwizard.jersey.setup.JerseyEnvironment;
import telraam.database.models.Detection;

public interface Lapper {
    void handle(Detection msg);
    void registerAPI(JerseyEnvironment jersey);
}
