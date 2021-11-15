package telraam.api;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import telraam.database.daos.LapDAO;
import telraam.database.models.Lap;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import java.util.List;
import java.util.Optional;

@Path("/lap")
@Api("/lap")
@Produces(MediaType.APPLICATION_JSON)
public class LapResource extends AbstractResource<Lap> {
    private final LapDAO lapDAO;

    public LapResource(LapDAO dao) {
        super(dao);
        lapDAO = dao;
    }

    @GET
    @ApiOperation(value = "Find all laps")
    public List<Lap> getListOf(@QueryParam("source") final Integer source) {
        if (source == null) {
            return lapDAO.getAll();
        } else {
            return lapDAO.getAllBySource(source);
        }
    }

    @Override
    @ApiOperation(value = "Add a new lap to the database")
    public int create(Lap lap) {
        return super.create(lap);
    }

    @Override
    @ApiOperation(value = "Find lap by ID")
    public Lap get(Optional<Integer> id) {
        return super.get(id);
    }

    @Override
    @ApiOperation(value = "Update an existing lap")
    public Lap update(Lap lap, Optional<Integer> id) {
        return super.update(lap, id);
    }

    @Override
    @ApiOperation(value = "Delete an existing lap")
    public boolean delete(Optional<Integer> id) {
        return super.delete(id);
    }
}
