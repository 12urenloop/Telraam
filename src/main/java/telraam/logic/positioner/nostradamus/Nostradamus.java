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
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.logging.Logger;
import java.util.stream.Collectors;

public class Nostradamus implements Positioner {
    private static final Logger logger = Logger.getLogger(Nostradamus.class.getName());

    private final int INTERVAL_FETCH = 10000;
    private final int DEBOUNCE_TIMEOUT = 1;
    private final int MAX_SIZE = 10000;
    private final int MIN_RSSI = -85;
    private final int INTERVAL = 2;
    private final ScheduledExecutorService scheduler;
    private boolean debounceScheduled;
    private Lock lock;

    private final Jdbi jdbi;
    private final TeamDAO teamDAO;
    private final StationDAO stationDAO;
    private final BatonSwitchoverDAO batonSwitchoverDAO;
    private final PositionSender positionSender;
    private Map<Team, CircularPriorityQueue> teamDetections;
    private Map<Team, TeamData> teamData;
    private Map<Integer, Team> batonIdToTeam;
    private Map<Integer, Station> idToStation;

    public Nostradamus(Jdbi jdbi) {
        this.scheduler = Executors.newScheduledThreadPool(1);
        this.debounceScheduled = false;
        this.lock = new ReentrantLock();

        this.jdbi = jdbi;
        this.teamDAO = jdbi.onDemand(TeamDAO.class);
        this.stationDAO = jdbi.onDemand(StationDAO.class);
        this.batonSwitchoverDAO = jdbi.onDemand(BatonSwitchoverDAO.class);

        this.positionSender = new PositionSender();

        new Thread(this::fetch);
    }

    // Update variables that depend on teams, stations and / or batonswitchover
    private void fetch() {
        while (true) {
            List<Team> teams = teamDAO.getAll();
            List<BatonSwitchover> switchovers = batonSwitchoverDAO.getAll();
            List<Station> stations = stationDAO.getAll();

            lock.lock();

            teamDetections = teams.stream()
                    .collect(
                            Collectors.toMap(
                                    team -> team,
                                    team -> teamDetections.getOrDefault(team, new CircularPriorityQueue(MAX_SIZE))
                            )
                    );
            teamData = teams.stream()
                    .collect(
                            Collectors.toMap(
                                    team -> team,
                                    team -> teamData.getOrDefault(team, new TeamData(team.getId()))
                            )
                    );
            batonIdToTeam = switchovers.stream()
                    .collect(
                            Collectors.toMap(
                                    BatonSwitchover::getNewBatonId,
                                    switchover -> teamDAO.getById(switchover.getTeamId()).get()
                            )
                    );
            idToStation = stations.stream()
                    .collect(
                            Collectors.toMap(
                                    Station::getId,
                                    station -> station
                            )
                    );

            lock.unlock();

            try {
                Thread.sleep(INTERVAL_FETCH);
            } catch (InterruptedException e) {
                logger.severe(e.getMessage());
            }
        }
    }

    // TODO: Add more detection filtering, high enough rssi, only one detection / timestamp, ...
    // TODO: Calculate average times in separate thread
    // TODO: If multiple detections come out of order -> restart
    // TODO: If something in fetch changes -> restart

    // TODO: Start simple, if arrives at new station -> send location and average time. Else send location given speed
    private void calculatePositions() {
        logger.info("Nostradamus: Calculating positions...");

        for (Map.Entry<Team, CircularPriorityQueue> entry: teamDetections.entrySet()) {
            Map<Integer, Float> averageTimes = new HashMap<>();

            int lastStationid = -1;
            long currentStationTime = 0;
            int currentStationRssi = MIN_RSSI;
            int currentStationId = 0;

            for (Detection detection: entry.getValue()) {
                if (detection.getTimestamp().getTime() - currentStationTime < INTERVAL) {
                    // Same interval
                    // Keep station with the highest RSSI
                    if (detection.getRssi() > currentStationRssi) {
                        currentStationId = detection.getStationId();
                        currentStationRssi = detection.getRssi();
                    }
                } else {
                    // New interval
                    // Save old station id
                    lastStationid = currentStationId;
                    currentStationTime = detection.getTimestamp().getTime();
                    currentStationRssi = detection.getRssi();
                    currentStationId = detection.getStationId();
                }
            }

            // Keep result of last interval if it exists
            Station currentStation = idToStation.getOrDefault(lastStationid, idToStation.get(currentStationId));


        }

        positionSender.send(teamData.values().stream().map(TeamData::getPosition).toList());
        logger.info("Nostradamus: Done calculating positions");
    }

    @Override
    public void handle(Detection detection) {
        Team team = batonIdToTeam.get(detection.getBatonId());
        teamDetections.get(team).add(detection);

        if (! debounceScheduled) {
            debounceScheduled = true;
            scheduler.schedule(() -> {
                try {
                    calculatePositions();
                } catch (Exception e) {
                    logger.severe(e.getMessage());
                }
                debounceScheduled = false;
            }, DEBOUNCE_TIMEOUT, TimeUnit.SECONDS);
        }
    }
}
