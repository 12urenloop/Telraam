package telraam.api;

import telraam.database.daos.BatonDAO;
import telraam.database.models.Baton;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;

@Path("/baton")
@Produces(MediaType.APPLICATION_JSON)
public class BatonResource extends AbstractResource<Baton>{
    public BatonResource(BatonDAO dao) {
        super(dao);
    }
}

