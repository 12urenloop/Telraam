package telraam.api;

import telraam.database.daos.DAO;
import telraam.database.models.Lap;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;

@Path("/lap")
@Produces(MediaType.APPLICATION_JSON)
public class LapResource extends AbstractResource<Lap> {
    public LapResource(DAO<Lap> dao) {
        super(dao);
    }
}
