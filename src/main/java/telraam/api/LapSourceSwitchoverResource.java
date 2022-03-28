package telraam.api;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import telraam.database.daos.LapSourceSwitchoverDAO;
import telraam.database.models.LapSourceSwitchover;

import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.util.List;
import java.util.Optional;

@Path("/lapsourceswitchover") // dropwizard
@Api(value = "/lapsourceswitchover") // Swagger
@Produces(MediaType.APPLICATION_JSON)
public class LapSourceSwitchoverResource extends AbstractListableResource<LapSourceSwitchover> {
    public LapSourceSwitchoverResource(LapSourceSwitchoverDAO dao) {
        super(dao);
    }

    @Override
    @ApiOperation(value = "Find all lap source switchovers")
    public List<LapSourceSwitchover> getListOf() {
        return super.getListOf();
    }

    @Override
    @ApiOperation(value = "Find lap source switchover by ID")
    public LapSourceSwitchover get(Optional<Integer> id) {
        return super.get(id);
    }
}

