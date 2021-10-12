package telraam.api;

import telraam.database.daos.DAO;
import telraam.database.models.Detection;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;

@Path("/detection")
@Produces(MediaType.APPLICATION_JSON)
public class DetectionResource extends AbstractListableResource<Detection> {

    public DetectionResource(DAO<Detection> dao) {
        super(dao);
    }

}
