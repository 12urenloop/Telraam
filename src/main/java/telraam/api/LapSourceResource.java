package telraam.api;

import telraam.database.daos.DAO;
import telraam.database.models.LapSource;

import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

@Path("/lap-source")
@Produces(MediaType.APPLICATION_JSON)
public class LapSourceResource extends AbstractListableResource<LapSource> {
    public LapSourceResource(DAO<LapSource> dao) {
        super(dao);
    }
}
