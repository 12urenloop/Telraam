package telraam.api;

import telraam.database.daos.DetectionDAO;
import telraam.database.daos.LapDAO;
import telraam.database.daos.TeamDAO;
import telraam.database.models.Lap;
import telraam.database.models.LastBeaconDetection;
import telraam.database.models.Team;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.util.List;
import java.util.StringJoiner;

@Path("/export")
@Produces(MediaType.TEXT_PLAIN)
public class ExporterResource {

    private LapDAO lapDAO;
    private TeamDAO teamDAO;
    private DetectionDAO detectionDAO;

    public ExporterResource(LapDAO lapDAO, TeamDAO teamDAO, DetectionDAO detectionDAO) {
        this.lapDAO = lapDAO;
        this.teamDAO = teamDAO;
        this.detectionDAO = detectionDAO;
    }

    @GET
    public String returnExport() {
        StringJoiner stringJoiner = new StringJoiner(System.lineSeparator());
        for (Team team : teamDAO.getAll()) {
            List<Lap> laps = lapDAO.getLapsForTeam(team.getId());
            int lapCount = laps.size();

            stringJoiner.add(String.format("telraam_team_lapcount{team_id=\"%s\", team_name=\"%s\"} %S", team.getId(), team.getName(), lapCount));
        }

        for (LastBeaconDetection lastBeaconDetection : detectionDAO.lastBeaconDetections()) {
            stringJoiner.add(String.format("telraam_last_beacon_detection{beacon_id=\"%s\"} %s", lastBeaconDetection.getBeaconId(), lastBeaconDetection.getTimestamp().getTime()));
        }


        return stringJoiner.toString();
    }

}
