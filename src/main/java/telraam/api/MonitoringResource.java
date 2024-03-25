package telraam.api;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.jdbi.v3.core.Jdbi;
import telraam.database.daos.*;
import telraam.database.models.Lap;
import telraam.database.models.LapSource;
import telraam.database.models.Team;
import telraam.database.models.TeamLapCount;
import telraam.monitoring.BatonDetectionManager;
import telraam.monitoring.BatonStatusHolder;
import telraam.monitoring.StationDetectionManager;
import telraam.monitoring.models.BatonDetection;
import telraam.monitoring.models.BatonStatus;
import telraam.monitoring.models.LapCountForTeam;
import telraam.monitoring.models.TeamLapInfo;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import java.util.*;

@Path("/monitoring")
@Api("/monitoring")
@Produces(MediaType.APPLICATION_JSON)
public class MonitoringResource {
    private final BatonStatusHolder batonStatusHolder;
    private final BatonDetectionManager batonDetectionManager;
    private final StationDetectionManager stationDetectionManager;
    private final TeamDAO teamDAO;
    private final LapDAO lapDAO;
    private final LapSourceDAO lapSourceDAO;

    public MonitoringResource(Jdbi jdbi) {
        this.teamDAO = jdbi.onDemand(TeamDAO.class);
        this.lapDAO = jdbi.onDemand(LapDAO.class);
        this.lapSourceDAO = jdbi.onDemand(LapSourceDAO.class);
        this.batonStatusHolder = new BatonStatusHolder(jdbi.onDemand(BatonDAO.class), jdbi.onDemand(DetectionDAO.class));
        this.batonDetectionManager = new BatonDetectionManager(jdbi.onDemand(DetectionDAO.class), this.teamDAO, jdbi.onDemand(BatonSwitchoverDAO.class));
        this.stationDetectionManager = new StationDetectionManager(jdbi.onDemand(DetectionDAO.class), jdbi.onDemand(StationDAO.class));
    }

    @GET
    @Path("/batons")
    @ApiOperation(value = "Get the status of all the batons, including unused batons which are toggleable via a parameter")
    public List<BatonStatus> getBatonMetrics(@QueryParam("filter_assigned") boolean filterAssigned) {
        List<BatonStatus> batonStatuses = batonStatusHolder.GetAllBatonStatuses();
        if (filterAssigned) {
            List<Team> teams = teamDAO.getAll();
            Set<Integer> usedBatonIds = new HashSet<>();
            for (Team team : teams) {
                usedBatonIds.add(team.getBatonId());
            }
            return batonStatuses.stream().filter(batonStatus -> usedBatonIds.contains(batonStatus.getId())).toList();
        }
        return batonStatuses;
    }

    @POST
    @Path("/reset-rebooted/{batonId}")
    @ApiOperation(value = "Reset the rebooted flag of a baton")
    public void resetRebooted(@PathParam("batonId") Integer batonId) {
        batonStatusHolder.resetRebooted(batonId);
    }

    @GET
    @Path("/team-detection-times")
    @ApiOperation(value = "A map of all detections per batons")
    public Map<Integer, List<BatonDetection>> getTeamDetectionTimes() {
        return batonDetectionManager.getBatonDetections();
    }

    @GET
    @Path("/stations-latest-detection-time")
    @ApiOperation(value = "Get the map of all station name to time since last detection")
    public Map<String, Long> getStationIDToLatestDetectionTimeMap() {
        return stationDetectionManager.timeSinceLastDetectionPerStation();
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
    public List<LapCountForTeam> getTeamLapCounts() {
        List<Team> teams = teamDAO.getAll();
        List<LapSource> lapSources = lapSourceDAO.getAll();
        List<LapCountForTeam> lapCountForTeams = new ArrayList<>();
        for (Team team : teams) {
            var teamLapsCount = lapDAO.getAllBySourceAndTeam(team.getId());
            Map<Integer, Integer> lapCounts = new HashMap<>();
            lapSources.forEach(lapSource -> lapCounts.put(lapSource.getId(), 0));
            for (TeamLapCount teamLapCount : teamLapsCount) {
                lapCounts.put(teamLapCount.getLapSourceId(), teamLapCount.getLapCount());
            }
            lapCountForTeams.add(new LapCountForTeam(team.getName(), lapCounts));
        }
        return lapCountForTeams;
    }
}
