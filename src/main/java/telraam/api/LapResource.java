package telraam.api;

import telraam.App;
import telraam.database.daos.LapDAO;
import telraam.database.models.Id;
import telraam.database.models.Lap;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.List;
import java.util.Optional;

@Path("/lap")
@Produces(MediaType.APPLICATION_JSON)
public class LapResource {

    private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(App.class.getName());

    private static final String ID_NAME = "lapId";
    private static final String ENTITY_PATH = "{lapId: [0-9]*}";

    private LapDAO lapDAO;

    public LapResource(LapDAO lapDAO) {
        this.lapDAO = lapDAO;
    }

    /**
     * @return All the laps in the database
     */
    @GET
    public List<Lap> getListOfLaps(){
        return lapDAO.getAll();
    }

    /**
     * Create a new lap
     *
     * @param lap Passed as json via the request body
     * @return The generated id of the lap
     */
    @POST
    public Id createLap(Lap lap){
        return lapDAO.insert(lap);
    }

    /**
     * @return a specific lap on the id
     */
    @GET @Path(ENTITY_PATH)
    public Lap getLap(@PathParam(ID_NAME)Optional<Integer> id){
        if (id.isPresent()){
            Optional<Lap> optionalLap = lapDAO.getById(id.get());
            if (optionalLap.isPresent()){
                return optionalLap.get();
            }else{
                throw new WebApplicationException(String.format("Lap with id: %d not found", id.get()), Response.Status.NOT_FOUND);
            }
        }else{
            throw new WebApplicationException("You did not pass an id", Response.Status.BAD_REQUEST);
        }
    }

    /**
     * Update a specific lap with the specified information
     */
    @PUT @Path(ENTITY_PATH)
    public Response updateLap(@PathParam(ID_NAME) Optional<Integer> id){
        if (id.isPresent()){
            Optional<Lap> optionalLap = lapDAO.getById(id.get());
            if (optionalLap.isPresent()){
                Lap lap = optionalLap.get();
                //todo update the lap in database
                //lapDAO.update(baton)
                //todo return updated lap
                return Response.noContent().build();
            } else {
                throw new WebApplicationException(String.format("Lap with id: %d not found", id.get()), Response.Status.NOT_FOUND);
            }
        } else {
            throw new WebApplicationException("You did not pass an id", Response.Status.BAD_REQUEST);
        }
    }

    /**
     *
     * @return a boolean if the lap could be deleted
     */

    @DELETE @Path(ENTITY_PATH)
    public boolean deleteLap(@PathParam(ID_NAME) Optional<Integer> id) {
        // TODO delete the lap
        return true;
    }

}
