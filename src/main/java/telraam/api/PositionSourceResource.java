package telraam.api;

import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;
import telraam.database.daos.DAO;
import telraam.database.models.PositionSource;


import java.awt.*;

@Path("/position-source")
@Tag(name = "Position Source")
@Produces(MediaType.APPLICATION_JSON)
public class PositionSourceResource extends AbstractListableResource<PositionSource> {
    public PositionSourceResource(DAO<PositionSource> dao) {
        super(dao);
    }
}
