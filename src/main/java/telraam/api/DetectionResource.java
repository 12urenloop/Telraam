package telraam.api;

import telraam.App;
import telraam.database.daos.DetectionDAO;
import telraam.database.models.Detection;
import telraam.database.models.Id;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.List;
import java.util.Optional;

@Path("/detection")
@Produces(MediaType.APPLICATION_JSON)
public class DetectionResource {

    private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(App.class.getName());

    private static final String ID_NAME = "detectionId";
    private static final String ENTITY_PATH = "/{detectionId: [0-9]*}";

    private DetectionDAO detectionDAO;

    public DetectionResource(DetectionDAO detectionDAO) {
        this.detectionDAO = detectionDAO;
    }

    /**
     * @return All the detections in the database
     */
    @GET
    public List<Detection> getListOfDetections(){
        return detectionDAO.getAll();
    }

    /**
     * Create a new detection
     *
     * @param detection Passed as json via the request body
     * @return The generated id of the detection
     */
    @POST
    public Id createDetection(Detection detection){
        return detectionDAO.insert(detection);
    }

    /**
     * @return a specific detection on the id
     */
    @GET @Path(ENTITY_PATH)
    public Detection getDetection(@PathParam(ID_NAME)Optional<Integer> id){
        if (id.isPresent()){
            Optional<Detection> optionalDetection = detectionDAO.getById(id.get());
            if (optionalDetection.isPresent()){
                return optionalDetection.get();
            }else {
                throw new WebApplicationException(String.format("Detection with id: %d not found", id.get()), Response.Status.NOT_FOUND);
            }
        }else{
            throw new WebApplicationException("You did not pass an id", Response.Status.BAD_REQUEST);
        }
    }

    /**
     * Update a specific detection with the specified information
     */
    @PUT @Path(ENTITY_PATH)
    public Response updateDetection(@PathParam(ID_NAME) Optional<Integer> id){
        if (id.isPresent()) {
            Optional<Detection> optionalDetection = detectionDAO.getById(id.get());
            if (optionalDetection.isPresent()) {
                Detection detection = optionalDetection.get();
                // TODO update the baton in the database
                // batonDAO.update(baton);
                // TODO return the updated baton
                return Response.noContent().build();
            } else {
                throw new WebApplicationException(String.format("Detection with id: %d not found", id.get()), Response.Status.NOT_FOUND);
            }
        } else {
            throw new WebApplicationException("You did not pass an id", Response.Status.BAD_REQUEST);
        }
    }

    /**
     *
     * @return a boolean if the detection could be deleted
     */

    @DELETE @Path(ENTITY_PATH)
    public boolean deleteDetection(@PathParam(ID_NAME) Optional<Integer> id) {
        // TODO delete the baton
        return true;
    }

}
