package telraam.logic;

import org.jdbi.v3.core.Jdbi;
import telraam.database.daos.BatonDAO;
import telraam.database.daos.BeaconDAO;
import telraam.database.daos.LapDAO;
import telraam.database.daos.TeamDAO;
import telraam.database.models.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class SimpleLapper implements Lapper {
    private static final int MAX_SPEED = 50;
    private List<Team> teams;
    private List<Baton> batons;
    private List<Beacon> beacons;
    private Jdbi jdbi;
    private LapDAO lapDAO;
    private Map<Integer, List<Detection>> detections;
    private Map<Integer, Integer> positionMap;
    private Team testTeam;
    private TeamDAO teamDAO;
    private BatonDAO batonDAO;
    private BeaconDAO beaconDAO;


    public SimpleLapper(Jdbi jdbi) {
        this.jdbi = jdbi;
        this.lapDAO = jdbi.onDemand(LapDAO.class);
        this.teamDAO = jdbi.onDemand(TeamDAO.class);
        this.batonDAO = jdbi.onDemand(BatonDAO.class);
        this.beaconDAO = jdbi.onDemand(BeaconDAO.class);

        this.teams = teamDAO.getAll();
        this.batons = batonDAO.getAll();
        this.beacons = beaconDAO.getAll();

        this.positionMap = new HashMap<>();
        this.detections = new HashMap<>();


        this.positionMap.put(1, 1);
        // in this simple implementation this map is redundant, but
        // it demonstrates a solution to the problem of assigning beacons
        // to positions on the track
        for (int i = 0; i < beacons.size(); i++) {
            this.positionMap.put(beacons.get(i).getId(), i);
        }


        for (Team team : teams) {
            detections.put(team.getBatonId(), new ArrayList<>());
        }
    }

    @Override
    public void handle(Detection msg) {
        List<Detection> currentDetections = detections.get(msg.getBatonId());
        currentDetections.add(msg);
        generateLap(currentDetections);
    }

    private void generateLap(List<Detection> detections) {
        Detection first = detections.get(0);

        for (int i = 1; i < detections.size(); i++) {
            Detection detection = detections.get(i);

            if (positionMap.get(first.getBeaconId()).equals(
                    positionMap.get(detection.getBeaconId())) &&
                    detections.size() == beacons.size() + 1) {
                this.lapDAO.insert(new Lap(detection.getBatonId(),
                        detection.getTimestamp()));
                detections.clear();
                detections.add(detection);
                break;
            }
        }
    }
}
