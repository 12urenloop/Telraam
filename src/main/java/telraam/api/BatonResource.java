package telraam.api;

import io.swagger.annotations.*;
import telraam.database.daos.BatonDAO;
import telraam.database.models.Baton;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import java.util.List;
import java.util.Optional;

@Path("/baton") // dropwizard
@Api(value = "/baton") // Swagger
@Produces(MediaType.APPLICATION_JSON)
public class BatonResource extends AbstractListableResource<Baton> {
    public BatonResource(BatonDAO dao) {
        super(dao);
    }

    @Override
    @ApiOperation(value = "Find all batons")
    public List<Baton> getListOf() {
        return super.getListOf();
    }

    @Override
    @ApiOperation(value = "Add a new baton to the database")
    public int create(Baton baton) {
        return super.create(baton);
    }

    @Override
    @ApiOperation(value = "Find baton by ID")
    public Baton get(Optional<Integer> id) {
        return super.get(id);
    }

    @Override
    @ApiOperation(value = "Update an existing baton")
    public Baton update(Baton baton, Optional<Integer> id) {
        return super.update(baton, id);
    }

    @Override
    @ApiOperation(value = "Delete an existing baton")
    public boolean delete(Optional<Integer> id) {
        return super.delete(id);
    }
}

