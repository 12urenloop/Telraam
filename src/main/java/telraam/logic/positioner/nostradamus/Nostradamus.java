package telraam.logic.positioner.nostradamus;

import org.jdbi.v3.core.Jdbi;
import telraam.database.daos.BatonDAO;
import telraam.database.daos.BatonSwitchoverDAO;
import telraam.database.daos.StationDAO;
import telraam.database.daos.TeamDAO;
import telraam.database.models.*;
import telraam.logic.positioner.PositionSender;
import telraam.logic.positioner.Positioner;

import java.util.*;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Logger;
import java.util.stream.Collectors;

public class Nostradamus implements Positioner {
    private static final Logger logger = Logger.getLogger(Nostradamus.class.getName());
    private final int INTERVAL_CALCULATE_MS = 500; // How often to handle new detections
    private final int INTERVAL_FETCH_MS = 60000; // Interval between fetching all stations, teams, ...
    private final int INTERVAL_DETECTIONS_MS = 3000; // Amount of milliseconds to group detections by
    private final int AVERAGE_AMOUNT = 10; // Calculate the average running speed the last x intervals
    private final double AVERAGE_SPRINTING_SPEED_M_MS = 0.00684; // Average sprinting speed m / ms
    private final int MIN_RSSI = -84;
    private final  int FINISH_OFFSET = 0;
    private final Jdbi jdbi;
    private final List<Detection> newDetections; // Contains not yet handled detections
    private final Lock detectionLock;
    private final Lock dataLock;
    private Map<Integer, Integer> batonToTeam; // Baton ID to Team ID
    private Map<Integer, TeamData> teamData; // All team data
    private final PositionSender positionSender;

    public Nostradamus(Jdbi jdbi) {
        this.jdbi = jdbi;
        this.newDetections = new ArrayList<>();
        this.detectionLock = new ReentrantLock();
        this.dataLock = new ReentrantLock();

        // Will be filled by fetch
        this.batonToTeam = new HashMap<>();
        setTeamData();

        this.positionSender = new PositionSender();

        new Thread(this::fetch).start();
        new Thread(this::calculatePosition).start();
    }

    private void setTeamData() {
        List<Station> stations = jdbi.onDemand(StationDAO.class).getAll();
        stations.sort(Comparator.comparing(Station::getDistanceFromStart));
        List<Team> teams = jdbi.onDemand(TeamDAO.class).getAll();

        teamData = teams.stream().collect(Collectors.toMap(
                Team::getId,
                team -> new TeamData(team.getId(), INTERVAL_DETECTIONS_MS, stations, AVERAGE_AMOUNT, AVERAGE_SPRINTING_SPEED_M_MS, FINISH_OFFSET)
        ));
    }

    private void fetch() {
        List<BatonSwitchover> switchovers = jdbi.onDemand(BatonSwitchoverDAO.class).getAll();

        Map<Integer, Integer> batonToTeam = switchovers.stream().sorted(
                Comparator.comparing(BatonSwitchover::getTimestamp)
        ).collect(Collectors.toMap(
                BatonSwitchover::getNewBatonId,
                BatonSwitchover::getTeamId,
                (existing, replacement) -> replacement
        ));

        if (!this.batonToTeam.equals(batonToTeam)) {
            dataLock.lock();
            this.batonToTeam = batonToTeam;
            dataLock.unlock();
        }

        // zzzzzzzz
        try {
            Thread.sleep(INTERVAL_FETCH_MS);
        } catch (InterruptedException e) {
            logger.severe(e.getMessage());
        }
    }

    private void calculatePosition() {
        Set<Integer> changedTeams = new HashSet<>(); // List of teams that have changed station
        while (true) {
            dataLock.lock();
            changedTeams.clear();
            detectionLock.lock();
            for (Detection detection: newDetections) {
                if (batonToTeam.containsKey(detection.getBatonId())) {
                    Integer teamId = batonToTeam.get(detection.getBatonId());
                    if (teamData.get(teamId).addDetection(detection)) {
                        changedTeams.add(teamId);
                    }
                }
            }
            newDetections.clear();
            detectionLock.unlock(); // Use lock as short as possible

            if (!changedTeams.isEmpty()) {
                // Update
                for (Integer teamId: changedTeams) {
                    teamData.get(teamId).updatePosition();
                }

                // Send new data to the websocket
                positionSender.send(
                        changedTeams.stream().map(team -> teamData.get(team).getPosition()).toList()
                );
            }

            dataLock.unlock();

            // zzzzzzzz
            try {
                Thread.sleep(INTERVAL_CALCULATE_MS);
            } catch (InterruptedException e) {
                logger.severe(e.getMessage());
            }
        }
    }

    @Override
    public void handle(Detection detection) {
        if (detection.getRssi() > MIN_RSSI) {
            detectionLock.lock();
            newDetections.add(detection);
            detectionLock.unlock();
        }
    }
}
