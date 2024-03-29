package telraam.api;

import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;
import telraam.database.daos.DAO;
import telraam.database.models.Station;

@Path("/station")
@Tag(name = "Station")
@Produces(MediaType.APPLICATION_JSON)
public class StationResource extends AbstractListableResource<Station> {
    public StationResource(DAO<Station> dao) {
        super(dao);
    }
}
