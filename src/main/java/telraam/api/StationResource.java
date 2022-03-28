package telraam.api;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import telraam.database.daos.DAO;
import telraam.database.models.Station;

import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.util.List;
import java.util.Optional;

@Path("/station")
@Api(value = "/station") // Swagger
@Produces(MediaType.APPLICATION_JSON)
public class StationResource extends AbstractListableResource<Station> {
    public StationResource(DAO<Station> dao) {
        super(dao);
    }

    @Override
    @ApiOperation(value = "Find all stations")
    public List<Station> getListOf() {
        return super.getListOf();
    }

    @Override
    @ApiOperation(value = "Add a new station to the database")
    public int create(Station station) {
        return super.create(station);
    }

    @Override
    @ApiOperation(value = "Find station by ID")
    public Station get(Optional<Integer> id) {
        return super.get(id);
    }

    @Override
    @ApiOperation(value = "Update an existing station")
    public Station update(Station station, Optional<Integer> id) {
        return super.update(station, id);
    }

    @Override
    @ApiOperation(value = "Delete an existing station")
    public boolean delete(Optional<Integer> id) {
        return super.delete(id);
    }
}
