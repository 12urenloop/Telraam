package telraam.logic.positioners;

import org.jdbi.v3.core.Jdbi;
import telraam.database.daos.BatonSwitchoverDAO;
import telraam.database.daos.StationDAO;
import telraam.database.daos.TeamDAO;
import telraam.database.models.BatonSwitchover;
import telraam.database.models.Detection;
import telraam.database.models.Station;
import telraam.database.models.Team;

import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class SimplePositioner {
    private final int QUEUE_SIZE = 50;
    private final int MIN_RSSI = -85;
    private Map<Integer, Team> teamIdToTeam = new HashMap<>();
    private Map<Team, CircularQueue<Detection>> teamDetections = new HashMap<>();
    private List<Integer> stations;

    public SimplePositioner(Jdbi jdbi) {
        TeamDAO teamDAO = jdbi.onDemand(TeamDAO.class);
        List<Team> teams = teamDAO.getAll();
        for (Team team: teams) {
            this.teamDetections.put(team, new CircularQueue<>(QUEUE_SIZE));
        }
        List<BatonSwitchover> switchovers = jdbi.onDemand(BatonSwitchoverDAO.class).getAll();
        switchovers.sort(Comparator.comparing(BatonSwitchover::getTimestamp));

        for (BatonSwitchover switchover: switchovers) {
            this.teamIdToTeam.put(switchover.getNewBatonId(), teamDAO.getById(switchover.getTeamId()).get());
        }

        List<Station> stationList = jdbi.onDemand(StationDAO.class).getAll();
        stationList.sort(Comparator.comparing(Station::getDistanceFromStart));
        this.stations = stationList.stream().map(Station::getId).toList();
    }

    private void calculatePosition(Team team) {
        List<Detection> detections = this.teamDetections.get(team);
        detections.sort(Comparator.comparing(Detection::getTimestamp));

        int currentStationRssi = MIN_RSSI;
        int currentStationPosition = 0;
        for (Detection detection: detections) {
            if (detection.getRssi() > currentStationRssi) {
                currentStationRssi = detection.getRssi();
                currentStationPosition = detection.getStationId();
            }
        }

        float progress = ((float) 100 / this.stations.size()) * this.stations.indexOf(currentStationPosition);

        System.out.printf("Team: %s | Progress: %f | Acceleration: %f%n", team.getName(), progress, 1.0);
    }

    public void handle(Detection detection) {
        Team team = this.teamIdToTeam.get(detection.getBatonId());
        this.teamDetections.get(team).add(detection);
        this.calculatePosition(team);
    }

}
