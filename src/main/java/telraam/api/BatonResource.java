package telraam.api;

import telraam.App;
import telraam.database.daos.BatonDAO;
import telraam.database.models.Baton;
import telraam.database.models.Id;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.List;
import java.util.Optional;

@Path("/baton")
@Produces(MediaType.APPLICATION_JSON)
public class BatonResource {
    private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(App.class.getName());

    private static final String ID_NAME = "batonId";
    private static final String ENTITY_PATH = "/{batonId: [0-9]*}";

    private BatonDAO batonDAO;

    public BatonResource(BatonDAO batonDAO) {
        this.batonDAO = batonDAO;
    }

    /**
     * @return All the baton's in the database
     */
    @GET
    public List<Baton> getListOfBatons() {
        return batonDAO.listBatons();
    }

    /**
     * Create a new baton
     *
     * @param baton Passed as json via the request body
     * @return The generated id of the baton
     */
    @POST
    public Id createBaton(Baton baton) {
        return batonDAO.insert(baton);
    }

    /**
     * @param id the id to search for
     * @return a specific baton on the id
     */
    @GET @Path(ENTITY_PATH)
    public Baton getBaton(@PathParam(ID_NAME) Optional<Integer> id) {
        if (id.isPresent()) {
            Optional<Baton> optionalBaton = batonDAO.findBatonById(id.get());
            if (optionalBaton.isPresent()) {
                return optionalBaton.get();
            } else {
                throw new WebApplicationException(String.format("Baton with id: %d not found", id.get()), Response.Status.NOT_FOUND);
            }
        } else {
            throw new WebApplicationException("You did not pass an id", Response.Status.BAD_REQUEST);
        }
    }

    /**
     * Update a specific baton with the specified information
     * @param id the id of the baton to update
     * @return the response
     */
    @PUT @Path(ENTITY_PATH)
    public Response updateBaton(@PathParam(ID_NAME) Optional<Integer> id) {
        if (id.isPresent()) {
            Optional<Baton> optionalBaton = batonDAO.findBatonById(id.get());
            if (optionalBaton.isPresent()) {
                Baton baton = optionalBaton.get();
                // TODO update the baton in the database
                // batonDAO.update(baton);
                // TODO return the updated baton
                return Response.noContent().build();
            } else {
                throw new WebApplicationException(String.format("Baton with id: %d not found", id.get()), Response.Status.NOT_FOUND);
            }
        } else {
            throw new WebApplicationException("You did not pass an id", Response.Status.BAD_REQUEST);
        }
    }



    @DELETE @Path(ENTITY_PATH)
    public boolean deleteBaton(@PathParam(ID_NAME) Optional<Integer> id) {
        // TODO delete the baton
        return true;
    }
}

