package telraam.logic.positioner.nostradamus;

import org.jdbi.v3.core.Jdbi;
import telraam.database.models.Detection;
import telraam.database.models.Team;
import telraam.logic.positioner.PositionSender;
import telraam.logic.positioner.Positioner;

import java.util.*;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Logger;

public class Nostradamus implements Positioner {
    private static final Logger logger = Logger.getLogger(Nostradamus.class.getName());
    private final int INTERVAL_CALCULATE = 500; // How often to handle new detections
    private final int INTERVAL_FETCH = 30000; // Interval between fetching all stations, teams, ...
    private final int INTERVAL_DETECTIONS = 1; // Amount of seconds to group detections by
    private final int AVERAGE_AMOUNT = 10; // Calculate the average running speed the last x intervals
    private final int MIN_RSSI = -84;
    private final List<Detection> newDetections; // Contains not yet handled detections
    private final Lock detectionLock;
    private Map<Integer, Team> batonToTeam; // Baton ID to Team
    private Map<Team, TeamData> teamData; // All team data
    private final PositionSender positionSender;

    public Nostradamus(Jdbi jdbi) {
        this.newDetections = new ArrayList<>();
        this.detectionLock = new ReentrantLock(); // TODO: Right kind of lock?

        this.positionSender = new PositionSender();

        new Thread(this::calculatePosition);
    }

    private void calculatePosition() {
        Set<Team> changedTeams = new HashSet<>(); // List of teams that have changed station
        while (true) {
            changedTeams.clear();
            detectionLock.lock();
            for (Detection detection: newDetections) {
                if (batonToTeam.containsKey(detection.getBatonId())) {
                    Team team = batonToTeam.get(detection.getBatonId());
                    if (teamData.containsKey(team) && teamData.get(team).addDetection(detection)) {
                        changedTeams.add(team);
                    }
                }
            }
            detectionLock.unlock(); // Use lock as short as possible

            if (!changedTeams.isEmpty()) {
                // Update
                for (Team team: changedTeams) {
                    teamData.get(team).updatePosition();
                }

                positionSender.send(
                        changedTeams.stream().map(team -> teamData.get(team).getPosition()).toList()
                );
            }

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
