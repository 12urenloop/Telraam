package telraam.api;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import telraam.database.daos.LapDAO;
import telraam.database.daos.LapSourceSwitchoverDAO;
import telraam.database.models.Lap;
import telraam.database.models.LapSourceSwitchover;

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
    LapDAO lapDAO;
    LapSourceSwitchoverDAO lapSourceSwitchoverDAO;

    public AcceptedLapsResource(LapDAO lapDAO, LapSourceSwitchoverDAO lapSourceSwitchoverDAO) {
        this.lapDAO = lapDAO;
        this.lapSourceSwitchoverDAO = lapSourceSwitchoverDAO;
    }

    @GET
    @ApiOperation("Get all accepted laps")
    public List<Lap> getLaps() {
        List<Lap> laps = this.lapDAO.getAll();
        // TODO: this should be done in SQL
        laps.sort(Comparator.comparing(Lap::getTimestamp));

        List<LapSourceSwitchover> lapSourceSwitchovers = this.lapSourceSwitchoverDAO.getAll();
        lapSourceSwitchovers.sort(Comparator.comparing(LapSourceSwitchover::getTimestamp));

        List<Lap> ret = new ArrayList<>();

        int lapSourceSwitchoverIndex = 0;
        int currentLapSource = -1;

        for (Lap lap : laps) {
            while (lapSourceSwitchoverIndex < lapSourceSwitchovers.size() && lapSourceSwitchovers.get(lapSourceSwitchoverIndex).getTimestamp().before(lap.getTimestamp())) {
                currentLapSource = lapSourceSwitchovers.get(lapSourceSwitchoverIndex).getId();
                System.out.println("New lap source: " + currentLapSource);
                lapSourceSwitchoverIndex++;
            }

            if (currentLapSource == -1 || lap.getLapSourceId() != currentLapSource) {
                ret.add(lap);
            } else {
                System.out.println("Rejecting lap source with lap source " + lap.getLapSourceId() + " (" + currentLapSource + ")");
            }
        }

        return ret;
    }
}
