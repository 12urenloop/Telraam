package telraam.api;

import telraam.database.daos.LapDAO;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.MediaType;
import telraam.database.daos.TeamDAO;
import telraam.database.models.Lap;
import telraam.database.models.LapCount;
import telraam.database.models.Team;
import telraam.util.AcceptedLapsUtil;

import java.sql.Timestamp;
import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

@Path("/lap-counts")
@Tag(name="Lap Counts")
@Produces(MediaType.APPLICATION_JSON)
public class LapCountResource {
    private TeamDAO teamDAO;
    private LapDAO lapDAO;

    public LapCountResource(TeamDAO teamDAO, LapDAO lapDAO) {
        this.teamDAO = teamDAO;
        this.lapDAO = lapDAO;
    }

    @GET
    @Operation(summary = "Get the current lap counts per team")
    public Map<String, Integer> getLapCounts() {
        Map<Integer, Integer> perId = new HashMap<>();
        for (Lap lap : AcceptedLapsUtil.getInstance().getAcceptedLaps()) {
            int teamId = lap.getTeamId();

            if (!perId.containsKey(teamId)) {
                perId.put(teamId, 0);
            }

            perId.put(teamId, perId.get(teamId) + 1);
        }

        Map<String, Integer> perName = new HashMap<>();

        for (int id : perId.keySet()) {
            Optional<Team> maybeTeam = teamDAO.getById(id);
            String teamName;
            if (maybeTeam.isPresent()) {
                teamName = maybeTeam.get().getName();
            } else {
                teamName = "Team " + id;
            }

            perName.put(teamName, perId.get(id));
        }

        return perName;
    }
    
    @GET
    @Path("/{lapSourceId}")
    public List<LapCount> getLapCountForLapSource(@PathParam("lapSourceId") Integer id, @QueryParam("end") Optional<String> endTimestamp) {
        LocalDateTime dateTime = LocalDateTime.now();
        if (endTimestamp.isPresent()) {
            dateTime = LocalDateTime.parse(endTimestamp.get());
        }
        List<LapCount> laps = lapDAO.getAllBeforeTime(id, Timestamp.valueOf(dateTime));
        return laps;
    }

    // EndTimestamp should be a ISO formatted date timestamp
    @GET
    @Path("/{lapSourceId}/{teamId}")
    public Integer getLapCountForLapSource(@PathParam("lapSourceId") Integer id, @PathParam("teamId") Integer teamId, @QueryParam("end") Optional<String> endTimestamp) {
        LocalDateTime dateTime = LocalDateTime.now();
        if (endTimestamp.isPresent()) {
            dateTime = LocalDateTime.parse(endTimestamp.get());
        }
        LapCount lapInfo = lapDAO.getAllForTeamBeforeTime(id, teamId, Timestamp.valueOf(dateTime));
        return lapInfo.getCount();
    }
}
