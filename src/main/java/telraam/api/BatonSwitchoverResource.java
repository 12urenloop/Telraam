package telraam.api;

import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;
import telraam.database.daos.BatonSwitchoverDAO;
import telraam.database.models.BatonSwitchover;

@Path("/batonswitchover") // dropwizard
@Tag(name = "Baton Switchover")
@Produces(MediaType.APPLICATION_JSON)
public class BatonSwitchoverResource extends AbstractListableResource<BatonSwitchover> {
    public BatonSwitchoverResource(BatonSwitchoverDAO dao) {
        super(dao);
    }
}

