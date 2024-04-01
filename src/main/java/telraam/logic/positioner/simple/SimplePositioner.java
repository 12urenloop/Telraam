package telraam.logic.positioner.simple;

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
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Level;
import java.util.logging.Logger;

public class SimplePositioner implements Positioner {
    private static final Logger logger = Logger.getLogger(SimplePositioner.class.getName());
    private final int QUEUE_SIZE = 50;
    private final int MIN_RSSI = -85;
    private final int DEBOUNCE_TIMEOUT = 1;
    private boolean debounceScheduled;
    private final ScheduledExecutorService scheduler;
    private final PositionSender positionSender;
    private final Map<Integer, Team> batonIdToTeam;
    private final Map<Integer, CircularQueue<Detection>> teamDetections;
    private final List<Integer> stations;
    private final Map<Integer, Position> teamPositions;

    public SimplePositioner(Jdbi jdbi) {
        this.debounceScheduled = false;
        this.scheduler = Executors.newScheduledThreadPool(1);
        this.positionSender = new PositionSender();
        this.batonIdToTeam = new HashMap<>();
        this.teamDetections = new HashMap<>();
        this.teamPositions = new HashMap<>();

        TeamDAO teamDAO = jdbi.onDemand(TeamDAO.class);
        List<Team> teams = teamDAO.getAll();
        for (Team team : teams) {
            teamDetections.put(team.getId(), new CircularQueue<>(QUEUE_SIZE));
            teamPositions.put(team.getId(), new Position(team.getId()));
        }
        List<BatonSwitchover> switchovers = jdbi.onDemand(BatonSwitchoverDAO.class).getAll();
        switchovers.sort(Comparator.comparing(BatonSwitchover::getTimestamp));

        for (BatonSwitchover switchover : switchovers) {
            batonIdToTeam.put(switchover.getNewBatonId(), teamDAO.getById(switchover.getTeamId()).get());
        }

        List<Station> stationList = jdbi.onDemand(StationDAO.class).getAll();
        stationList.sort(Comparator.comparing(Station::getDistanceFromStart));
        stations = stationList.stream().map(Station::getId).toList();
    }

    private void calculatePositions() {
        logger.info("SimplePositioner: Calculating positions...");
        for (Map.Entry<Integer, CircularQueue<Detection>> entry : teamDetections.entrySet()) {
            List<Detection> detections = teamDetections.get(entry.getKey());
            detections.sort(Comparator.comparing(Detection::getTimestamp));

            int currentStationRssi = MIN_RSSI;
            int currentStationPosition = 0;
            for (Detection detection : detections) {
                if (detection.getRssi() > currentStationRssi) {
                    currentStationRssi = detection.getRssi();
                    currentStationPosition = detection.getStationId();
                }
            }

            float progress = ((float) 100 / stations.size()) * currentStationPosition;
            teamPositions.get(entry.getKey()).setProgress(progress);
        }

        positionSender.send(teamPositions.values().stream().toList());
        logger.info("SimplePositioner: Done calculating positions");
    }

    public synchronized void handle(Detection detection) {
        Team team = batonIdToTeam.get(detection.getBatonId());
        teamDetections.get(team.getId()).add(detection);

        if (!debounceScheduled) {
            debounceScheduled = true;
            scheduler.schedule(() -> {
                try {
                    calculatePositions();
                } catch (Exception e) {
                    logger.log(Level.SEVERE, e.getMessage(), e);
                }
                debounceScheduled = false;
            }, DEBOUNCE_TIMEOUT, TimeUnit.SECONDS);
        }
    }
}
