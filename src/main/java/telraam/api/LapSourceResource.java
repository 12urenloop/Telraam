package telraam.api;

import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;
import telraam.database.daos.DAO;
import telraam.database.models.LapSource;

@Path("/lap-source")
@Tag(name = "Lap Source")
@Produces(MediaType.APPLICATION_JSON)
public class LapSourceResource extends AbstractListableResource<LapSource> {
    public LapSourceResource(DAO<LapSource> dao) {
        super(dao);
    }
}
