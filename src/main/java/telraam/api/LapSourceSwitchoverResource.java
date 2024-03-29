package telraam.api;

import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;
import telraam.database.daos.LapSourceSwitchoverDAO;
import telraam.database.models.LapSourceSwitchover;

@Path("/lapsourceswitchover") // dropwizard
@Tag(name = "Lap Source Switchover")
@Produces(MediaType.APPLICATION_JSON)
public class LapSourceSwitchoverResource extends AbstractListableResource<LapSourceSwitchover> {
    public LapSourceSwitchoverResource(LapSourceSwitchoverDAO dao) {
        super(dao);
    }
}

