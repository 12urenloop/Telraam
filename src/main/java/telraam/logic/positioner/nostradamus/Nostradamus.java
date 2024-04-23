package telraam.logic.positioner.nostradamus;

import org.jdbi.v3.core.Jdbi;
import telraam.database.daos.BatonSwitchoverDAO;
import telraam.database.daos.StationDAO;
import telraam.database.daos.TeamDAO;
import telraam.database.models.BatonSwitchover;
import telraam.database.models.Detection;
import telraam.database.models.Station;
import telraam.database.models.Team;
import telraam.logic.positioner.Position;
import telraam.logic.positioner.PositionSender;
import telraam.logic.positioner.Positioner;

import java.util.*;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Logger;
import java.util.stream.Collectors;

public class Nostradamus implements Positioner {
    private static final Logger logger = Logger.getLogger(Nostradamus.class.getName());
    private final int INTERVAL_CALCULATE_MS = 500; // How often to handle new detections (in milliseconds)
    private final int INTERVAL_FETCH_MS = 10000; // Interval between fetching baton switchovers (in milliseconds)
    private final int INTERVAL_DETECTIONS_MS = 3000; // Amount of milliseconds to group detections by
    private final int MAX_NO_DATA_MS = 30000; // Send a stationary position after receiving no station update for x amount of milliseconds
    private final int MEDIAN_AMOUNT = 10; // Calculate the median running speed of the last x intervals
    private final double AVERAGE_SPRINTING_SPEED_M_MS = 0.00684; // Average sprinting speed meters / milliseconds
    private final int MIN_RSSI = -84; // Minimum rssi strength for a detection
    private final int FINISH_OFFSET_M = 0; // Distance between the last station and the finish in meters
    private final Jdbi jdbi;
    private final List<Detection> newDetections; // Contains not yet handled detections
    private Map<Integer, Integer> batonToTeam; // Baton ID to Team ID
    private final Map<Integer, TeamData> teamData; // All team data
    private final PositionSender positionSender;
    private final Lock detectionLock;
    private final Lock dataLock;

    public Nostradamus(Jdbi jdbi) {
        this.jdbi = jdbi;
        this.newDetections = new ArrayList<>();
        this.detectionLock = new ReentrantLock();
        this.dataLock = new ReentrantLock();

        // Will be filled by fetch
        this.batonToTeam = new HashMap<>();
        this.teamData = getTeamData();

        this.positionSender = new PositionSender();

        new Thread(this::fetch).start();
        new Thread(this::calculatePosition).start();
    }

    // Initiate the team data map
    private Map<Integer, TeamData> getTeamData() {
        List<Station> stations = jdbi.onDemand(StationDAO.class).getAll();
        stations.sort(Comparator.comparing(Station::getDistanceFromStart));
        List<Team> teams = jdbi.onDemand(TeamDAO.class).getAll();

        return teams.stream().collect(Collectors.toMap(
                Team::getId,
                team -> new TeamData(team.getId(), INTERVAL_DETECTIONS_MS, stations, MEDIAN_AMOUNT, AVERAGE_SPRINTING_SPEED_M_MS, FINISH_OFFSET_M)
        ));
    }

    // Fetch all baton switchovers and replace the current one if there are any changes
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

        // Sleep tight
        try {
            Thread.sleep(INTERVAL_FETCH_MS);
        } catch (InterruptedException e) {
            logger.severe(e.getMessage());
        }
    }

    // handle all new detections and update positions accordingly
    private void calculatePosition() {
        Set<Integer> changedTeams = new HashSet<>(); // List of teams that have changed station
        while (true) {
            changedTeams.clear();
            dataLock.lock();
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
            dataLock.unlock();

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

            // Send a stationary position if no new station data was received recently
            long now = System.currentTimeMillis();
            for (Map.Entry<Integer, TeamData> entry: teamData.entrySet()) {
                if (now - entry.getValue().getPreviousStationArrival() > MAX_NO_DATA_MS) {
                    positionSender.send(
                            Collections.singletonList(new Position(
                                    entry.getKey(),
                                    entry.getValue().getPosition().getProgress()
                            ))
                    );
                }
            }

            // Goodnight
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
