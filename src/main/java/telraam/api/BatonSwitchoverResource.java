package telraam.api;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import telraam.database.daos.BatonSwitchoverDAO;
import telraam.database.models.BatonSwitchover;

import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.util.List;
import java.util.Optional;

@Path("/batonswitchover") // dropwizard
@Api(value = "/batonswitchover") // Swagger
@Produces(MediaType.APPLICATION_JSON)
public class BatonSwitchoverResource extends AbstractListableResource<BatonSwitchover> {
    public BatonSwitchoverResource(BatonSwitchoverDAO dao) {
        super(dao);
    }

    @Override
    @ApiOperation(value = "Find all baton switchovers")
    public List<BatonSwitchover> getListOf() {
        return super.getListOf();
    }

    @Override
    @ApiOperation(value = "Find baton switchover by ID")
    public BatonSwitchover get(Optional<Integer> id) {
        return super.get(id);
    }

    @Override
    @ApiOperation(value = "Add a new baton switchover to the database")
    public int create(BatonSwitchover batonSwitchover) {
        return super.create(batonSwitchover);
    }
}

