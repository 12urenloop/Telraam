package telraam.api;

import telraam.database.daos.BatonDAO;
import telraam.database.models.Baton;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.Optional;

@Path("/baton")
@Produces(MediaType.APPLICATION_JSON)
public class BatonResource {

    private BatonDAO batonDAO;

    public BatonResource(BatonDAO batonDAO) {
        this.batonDAO = batonDAO;
    }

    @GET
    public Baton sayHello(@QueryParam("id") Optional<Integer> id) {
        if(id.isPresent()){
            Optional<Baton> optionalBaton = batonDAO.findBatonById(id.get());
            if(optionalBaton.isPresent()){
                return optionalBaton.get();
            } else {
                throw new WebApplicationException(String.format("Baton with id: %d not found", id.get()), Response.Status.NOT_FOUND);
            }
        } else {
            throw new WebApplicationException("You did not pass an id", Response.Status.BAD_REQUEST);
        }
    }

}
