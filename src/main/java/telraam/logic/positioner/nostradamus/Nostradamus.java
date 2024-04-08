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
    private final int INTERVAL_CALCULATE = 500; // How often to handle new detections
    private final int INTERVAL_FETCH = 60000; // Interval between fetching all stations, teams, ...
    private final int INTERVAL_DETECTIONS = 1; // Amount of seconds to group detections by
    private final int AVERAGE_AMOUNT = 10; // Calculate the average running speed the last x intervals
    private final int MIN_RSSI = -84;
    private final Jdbi jdbi;
    private final List<Detection> newDetections; // Contains not yet handled detections
    private final Lock detectionLock;
    private final Lock dataLock;
    private Map<Integer, Team> batonToTeam; // Baton ID to Team
    private Map<Team, TeamData> teamData; // All team data
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

        new Thread(this::fetch);
        new Thread(this::calculatePosition);
    }

    private void setTeamData() {
        List<Station> stations = jdbi.onDemand(StationDAO.class).getAll();
        List<Team> teams = jdbi.onDemand(TeamDAO.class).getAll();

        teamData = teams.stream().collect(Collectors.toMap(
                team -> team,
                team -> new TeamData(team.getId(), INTERVAL_DETECTIONS, stations, AVERAGE_AMOUNT)
        ));
    }

    private void fetch() {
        List<BatonSwitchover> switchovers = jdbi.onDemand(BatonSwitchoverDAO.class).getAll();
        List<Team> teams = jdbi.onDemand(TeamDAO.class).getAll();

        Map<Integer, Team> teamIdToTeam = teams.stream().collect(Collectors.toMap(Team::getId, team -> team));
        Map<Integer, Team> batonToTeam = switchovers.stream().collect(Collectors.toMap(
                BatonSwitchover::getNewBatonId,
                switchover -> teamIdToTeam.get(switchover.getTeamId())
        ));

        if (!this.batonToTeam.equals(batonToTeam)) {
            dataLock.lock();
            this.batonToTeam = batonToTeam;
            dataLock.unlock();
        }

        // zzzzzzzz
        try {
            Thread.sleep(INTERVAL_FETCH);
        } catch (InterruptedException e) {
            logger.severe(e.getMessage());
        }
    }

    private void calculatePosition() {
        Set<Team> changedTeams = new HashSet<>(); // List of teams that have changed station
        while (true) {
            dataLock.lock();
            changedTeams.clear();
            detectionLock.lock();
            for (Detection detection: newDetections) {
                if (batonToTeam.containsKey(detection.getBatonId())) {
                    Team team = batonToTeam.get(detection.getBatonId());
                    if (teamData.get(team).addDetection(detection)) {
                        changedTeams.add(team);
                    }
                }
            }
            newDetections.clear();
            detectionLock.unlock(); // Use lock as short as possible

            if (!changedTeams.isEmpty()) {
                // Update
                for (Team team: changedTeams) {
                    teamData.get(team).updatePosition();
                }

                // Send new data to the websocket
                positionSender.send(
                        changedTeams.stream().map(team -> teamData.get(team).getPosition()).toList()
                );
            }

            dataLock.unlock();

            // zzzzzzzz
            try {
                Thread.sleep(INTERVAL_CALCULATE);
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
