package telraam.api;

import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;
import telraam.database.daos.DAO;
import telraam.database.models.Baton;

@Path("/baton") // dropwizard
@Tag(name = "Baton")
@Produces(MediaType.APPLICATION_JSON)
public class BatonResource extends AbstractListableResource<Baton> {
    public BatonResource(DAO<Baton> dao) {
        super(dao);
    }
}

