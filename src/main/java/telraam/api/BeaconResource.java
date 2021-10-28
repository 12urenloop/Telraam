package telraam.api;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import telraam.database.daos.DAO;
import telraam.database.models.Beacon;

import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.util.List;
import java.util.Optional;

@Path("/beacon")
@Api(value = "/beacon") // Swagger
@Produces(MediaType.APPLICATION_JSON)
public class BeaconResource extends AbstractListableResource<Beacon> {
    public BeaconResource(DAO<Beacon> dao) {
        super(dao);
    }

    @Override
    @ApiOperation(value = "Find all beacons")
    public List<Beacon> getListOf() {
        return super.getListOf();
    }

    @Override
    @ApiOperation(value = "Add a new beacon to the database")
    public int create(Beacon beacon) {
        return super.create(beacon);
    }

    @Override
    @ApiOperation(value = "Find beacon by ID")
    public Beacon get(Optional<Integer> id) {
        return super.get(id);
    }

    @Override
    @ApiOperation(value = "Update an existing beacon")
    public Beacon update(Beacon beacon, Optional<Integer> id) {
        return super.update(beacon, id);
    }

    @Override
    @ApiOperation(value = "Delete an existing beacon")
    public boolean delete(Optional<Integer> id) {
        return super.delete(id);
    }
}
