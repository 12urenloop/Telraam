package telraam.api;

import io.swagger.v3.oas.annotations.Operation;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.MediaType;
import telraam.database.daos.TeamDAO;
import telraam.database.models.Lap;
import telraam.database.models.Team;
import telraam.util.AcceptedLapsUtil;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

@Path("/lap-counts")
@Produces(MediaType.APPLICATION_JSON)
public class LapCountResource {
    TeamDAO teamDAO;

    public LapCountResource(TeamDAO teamDAO) {
        this.teamDAO = teamDAO;
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
}
