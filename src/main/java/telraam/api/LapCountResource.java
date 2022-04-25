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
import java.util.*;

@Path("/lap-counts")
@Api("/lap-counts")
@Produces(MediaType.APPLICATION_JSON)
public class LapCountResource {
    @GET
    @ApiOperation("Get the current lap counts per team")
    public Map<Integer, Integer> getLapCounts() {
        Map<Integer, Integer> perId = new HashMap<>();
        for (Lap lap : AcceptedLapsUtil.getInstance().getAcceptedLaps()) {
            int teamId = lap.getTeamId();

            if (!perId.containsKey(teamId)) {
                perId.put(teamId, 0);
            }

            perId.put(teamId, perId.get(teamId) + 1);
        }

        return perId;
    }
}
