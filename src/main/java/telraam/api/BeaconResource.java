package telraam.api;

import telraam.App;
import telraam.database.daos.BeaconDAO;
import telraam.database.models.Beacon;
import telraam.database.models.Id;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.List;
import java.util.Optional;

@Path("/baton")
@Produces(MediaType.APPLICATION_JSON)
public class BeaconResource {

    private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(App.class.getName());

    private static final String ID_NAME = "beaconId";
    private static final String ENTITY_PATH = "/{beaconId: [0-9]*}";

    private BeaconDAO beaconDao;

    public BeaconResource(BeaconDAO beaconDao) {
        this.beaconDao = beaconDao;
    }

    /**
     * @return All the beacons in the database
     */
    @GET
    public List<Beacon> getListOfBeacons(){
        return beaconDao.getAll();
    }

    /**
     * Create a new beacon
     *
     * @param beacon Passed as json via the request body
     * @return The generated id of the beacon
     */
    @POST
    public Id createBeacon(Beacon beacon){
        return beaconDao.insert(beacon);
    }

    /**
     * @return a specific beacon on the id
     */
    @GET @Path(ENTITY_PATH)
    public Beacon getBeacon(@PathParam(ID_NAME) Optional<Integer> id) {
        if (id.isPresent()) {
            Optional<Beacon> optionalBeacon = beaconDao.getById(id.get());
            if (optionalBeacon.isPresent()) {
                return optionalBeacon.get();
            } else {
                throw new WebApplicationException(String.format("Beacon with id: %d not found", id.get()), Response.Status.NOT_FOUND);
            }
        } else {
            throw new WebApplicationException("You did not pass an id", Response.Status.BAD_REQUEST);
        }
    }

    /**
     *
     * @return a boolean if the beacon could be deleted
     */

    @PUT @Path(ENTITY_PATH)
    public Response updateBeacon(@PathParam(ID_NAME) Optional<Integer> id){
        if (id.isPresent()) {
            Optional<Beacon> optionalBeacon = beaconDao.getById(id.get());
            if (optionalBeacon.isPresent()) {
                Beacon beacon = optionalBeacon.get();
                // TODO update the baton in the database
                // batonDAO.update(baton);
                // TODO return the updated baton
                return Response.noContent().build();
            } else {
                throw new WebApplicationException(String.format("Beacon with id: %d not found", id.get()), Response.Status.NOT_FOUND);
            }
        } else {
            throw new WebApplicationException("You did not pass an id", Response.Status.BAD_REQUEST);
        }
    }
}
