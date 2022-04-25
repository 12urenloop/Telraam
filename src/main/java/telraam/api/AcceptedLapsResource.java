package telraam.api;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import telraam.database.daos.LapDAO;
import telraam.database.daos.LapSourceSwitchoverDAO;
import telraam.database.models.Lap;
import telraam.database.models.LapSourceSwitchover;
import telraam.util.AcceptedLapsUtil;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

@Path("/accepted-laps")
@Api("/accepted-laps")
@Produces(MediaType.APPLICATION_JSON)
public class AcceptedLapsResource {
    @GET
    @ApiOperation("Get all accepted laps")
    public List<Lap> getLaps() {
        return AcceptedLapsUtil.getInstance().getAcceptedLaps();
    }
}
