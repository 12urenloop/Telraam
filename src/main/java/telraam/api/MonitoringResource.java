package telraam.api;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.jdbi.v3.core.Jdbi;
import telraam.database.daos.BatonDAO;
import telraam.database.daos.DetectionDAO;
import telraam.database.daos.LapDAO;
import telraam.database.daos.TeamDAO;
import telraam.database.models.Lap;
import telraam.database.models.Team;
import telraam.database.models.TeamLapCount;
import telraam.monitoring.*;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import java.util.*;

@Path("/monitoring")
@Api("/monitoring")
@Produces(MediaType.APPLICATION_JSON)
public class MonitoringResource {
    private final BatonStatusHolder batonStatusHolder;
    private final BatonDetectionManager batonDetectionManager;
    private final TeamDAO teamDAO;
    private final LapDAO lapDAO;

    public MonitoringResource(BatonDAO BDAO, DetectionDAO DDAO, TeamDAO TDAO, LapDAO LDAO) {
        this.batonStatusHolder = new BatonStatusHolder(BDAO, DDAO);
        this.batonDetectionManager = new BatonDetectionManager(DDAO, TDAO);
        this.teamDAO = TDAO;
        this.lapDAO = LDAO;
    }

    @GET
    @Path("/batons")
    @ApiOperation(value = "Get the status of all the batons, including unused batons which are toggleable via a parameter")
    public List<BatonStatus> getBatonMetrics() {
        // TODO: filter assigned parameter in request
        return batonStatusHolder.GetAllBatonStatuses();
    }

    @GET
    @Path("/team-detection-times")
    @ApiOperation(value = "A map of all detections per batons")
    public Map<Integer, List<BatonDetection>> getTeamDetectionTimes() {
        return batonDetectionManager.getBatonDetections();
    }

    @GET
    @Path("/team-lap-times/{lapperId}")
    @ApiOperation(value = "Get monitoring data that can be used as grafana datasource")
    public Map<Integer, List<TeamLapInfo>> getTeamLapTimes(@PathParam("lapperId") Integer id) {
        List<Lap> laps = lapDAO.getAllBySourceSorted(id);
        List<Team> teams = teamDAO.getAll();
        Map<Integer, Team> teamMap = new HashMap<>();
        for (Team team : teams) {
            teamMap.put(team.getId(), team);
        }
        Map<Integer, List<TeamLapInfo>> teamLapInfos = new HashMap<>();
        Map<Integer, Lap> previousLap = new HashMap<>();
        for (Lap lap : laps) {
            if (!previousLap.containsKey(lap.getTeamId())) {
                previousLap.put(lap.getTeamId(), lap);
                continue;
            }
            Lap prevLap = previousLap.get(lap.getTeamId());
            previousLap.put(lap.getTeamId(), lap);
            if (!teamLapInfos.containsKey(lap.getTeamId())) {
                teamLapInfos.put(lap.getTeamId(), new ArrayList<>());
            }
            Team team = teamMap.get(lap.getTeamId());
            teamLapInfos.get(lap.getTeamId()).add(new TeamLapInfo((lap.getTimestamp().getTime() - prevLap.getTimestamp().getTime()) / 1000, lap.getTimestamp().getTime() / 1000, lap.getTeamId(), team.getName()));
        }
        return teamLapInfos;
    }

    @GET
    @Path("/team-lap-counts")
    @ApiOperation(value = "Get monitoring data that can be used as grafana datasource")
    public Map<String, Map<Integer, Integer>> getTeamLapCounts() {
        List<Team> teams = teamDAO.getAll();
        Map<String, Map<Integer, Integer>> teamLapCounts = new HashMap<>();
        for (Team team : teams) {
            var teamLapsCount = lapDAO.getAllBySourceAndTeam(team.getId());
            Map<Integer, Integer> lapCount = new HashMap<>();
            for (TeamLapCount teamLapCount : teamLapsCount) {
                lapCount.put(teamLapCount.getLapSourceId(), teamLapCount.getLapCount());
            }
            teamLapCounts.put(team.getName(), lapCount);
        }
        return teamLapCounts;
    }
}
