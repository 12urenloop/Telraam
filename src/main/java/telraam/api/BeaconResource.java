package telraam.api;

import telraam.database.daos.DAO;
import telraam.database.models.Beacon;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;

@Path("/beacon")
@Produces(MediaType.APPLICATION_JSON)
public class BeaconResource extends AbstractListableResource<Beacon> {
    public BeaconResource(DAO<Beacon> dao) {
        super(dao);
    }
}
