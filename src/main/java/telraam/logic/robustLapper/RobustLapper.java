package telraam.logic.robustLapper;

import io.dropwizard.jersey.setup.JerseyEnvironment;
import org.jdbi.v3.core.Jdbi;
import telraam.database.daos.*;
import telraam.database.models.*;
import telraam.logic.Lapper;
import telraam.logic.viterbi.ViterbiLapper;

import java.sql.Timestamp;
import java.util.*;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;
import java.util.stream.Collectors;

// Implement Lapper for easier use in App and Fetcher
public class RobustLapper implements Lapper {

    private final String SOURCE_NAME = "robust-lapper";     // Id in database
    private final int DEBOUNCE_TIMEOUT = 10;
    private final int INTERVAL_TIME = 2000;                 // Interval to group detections (in ms)
    private final int MIN_RSSI = -85;                       // Min rssi

    private final Jdbi jdbi;
    private final ScheduledExecutorService scheduler;
    private final Logger logger;
    private final LapDAO lapDAO;
    private boolean debounceScheduled;
    private int lapSourceId;
    private Map<Integer, List<Detection>> teamDetections;
    private List<Team> teams;
    private List<Station> stations;
    private Map<Integer, List<Timestamp>> teamLaps;

    public RobustLapper(Jdbi jdbi) {
        this.jdbi = jdbi;
        this.scheduler = Executors.newScheduledThreadPool(1);
        this.logger = Logger.getLogger(ViterbiLapper.class.getName());
        this.lapDAO = jdbi.onDemand(LapDAO.class);
        this.debounceScheduled = false;

        // Get the lapSourceId, create the source if needed
        LapSourceDAO lapSourceDAO = jdbi.onDemand(LapSourceDAO.class);
        lapSourceDAO.getByName(SOURCE_NAME).ifPresentOrElse(
                lapSource -> this.lapSourceId = lapSource.getId(),
                () -> this.lapSourceId = lapSourceDAO.insert(new LapSource(SOURCE_NAME))
        );
    }

    private void processData() {
        // Maps a baton id to the current team using it
        Map<Integer, Team> batonTeam = new HashMap<>();
        // Map containing all detections belonging to a team
        teamDetections = new HashMap<>();

        TeamDAO teamDAO = this.jdbi.onDemand(TeamDAO.class);
        DetectionDAO detectionDAO = this.jdbi.onDemand(DetectionDAO.class);
        BatonSwitchoverDAO batonSwitchoverDAO = this.jdbi.onDemand(BatonSwitchoverDAO.class);

        teams = teamDAO.getAll();
        List<Detection> detections = detectionDAO.getAll();
        List<BatonSwitchover> switchovers = batonSwitchoverDAO.getAll();

        switchovers.sort(Comparator.comparing(BatonSwitchover::getTimestamp));
        detections.sort(Comparator.comparing(Detection::getTimestamp));

        Map<Integer, Team> teamById = teams.stream().collect(Collectors.toMap(Team::getId, team -> team));
        teams.forEach(team -> teamDetections.put(team.getId(), new ArrayList<>()));

        int switchoverIndex = 0;
        for (Detection detection : detections) {
            // Switch teams batons if it happened before the current detection
            while (switchoverIndex < switchovers.size() && switchovers.get(switchoverIndex).getTimestamp().before(detection.getTimestamp())) {
                BatonSwitchover switchover = switchovers.get(switchoverIndex);
                batonTeam.put(switchover.getNewBatonId(), teamById.get(switchover.getTeamId()));
                batonTeam.remove(switchover.getPreviousBatonId());
                switchoverIndex++;
            }

            // Check if detection belongs to a team, and it's signal is strong enough
            if (batonTeam.containsKey(detection.getBatonId()) && detection.getRssi() > MIN_RSSI) {
                List<Detection> currentDetections = teamDetections.get(batonTeam.get(detection.getBatonId()).getId());
                // If team already has a detection for that timestamp keep the one with the strongest signal
                if (! currentDetections.isEmpty() && currentDetections.get(currentDetections.size() - 1).getTimestamp().compareTo(detection.getTimestamp()) == 0) {
                    if (currentDetections.get(currentDetections.size() - 1).getRssi() < detection.getRssi()) {
                        currentDetections.remove(currentDetections.size() - 1);
                        currentDetections.add(detection);
                    }
                } else {
                    currentDetections.add(detection);
                }
            }
        }
    }

    public void calculateLaps() {
        logger.info("RobustLapper: Calculating laps...");
        processData();

        // Each teams laps
        teamLaps = new HashMap<>();

        StationDAO stationDAO = jdbi.onDemand(StationDAO.class);
        stations = stationDAO.getAll();
        stations.sort(Comparator.comparing(Station::getDistanceFromStart));
        // List containing station id's and sorted based on their distance from the start
        List<Integer> stationIdToPosition = stations.stream().map(Station::getId).toList();

        for (Team team : teams) {
            List<Timestamp> lapTimes = new ArrayList<>();

            // Station that is used for current interval
            int lastStationPosition = 0;
            long currentStationTime = 0;
            int currentStationRssi = MIN_RSSI;
            int currentStationPosition = 0;

            List<Detection> detections = teamDetections.get(team.getId());

            for (Detection detection : detections) {
                // Group all detections based on INTERVAL_TIME
                if (detection.getTimestamp().getTime() - currentStationTime < INTERVAL_TIME) {
                    // We're still in the same interval

                    // Use the station with the highest RSSI from this interval
                    if (detection.getRssi() > currentStationRssi) {
                        currentStationPosition = stationIdToPosition.indexOf(detection.getStationId());
                        currentStationRssi = detection.getRssi();
                    }
                } else {
                    // We're in a new interval, use the detection with the highest RSSI to update trajectory
                    // Check if new station is more likely to be in front of the runner
                    if (! (backwardPathDistance(lastStationPosition, currentStationPosition) <= 2)) {
                        if (isStartBetween(lastStationPosition, currentStationPosition)) {
                            // Add lap if we passed the start line
                            lapTimes.add(detection.getTimestamp());
                        }
                        lastStationPosition = currentStationPosition;
                    }
                    // Prepare new interval
                    currentStationTime = detection.getTimestamp().getTime();
                    currentStationPosition = stationIdToPosition.indexOf(detection.getStationId());
                    currentStationRssi = detection.getRssi();
                }
            }
            // Save result for team
            teamLaps.put(team.getId(), lapTimes);
        }

        save();
        logger.info("RobustLapper: Done calculating laps");
    }

    // Amount of stations between from_station and to_station, counted when going backward
    private int backwardPathDistance(int fromStation, int toStation) {
        return (((fromStation - toStation) % stations.size()) + stations.size()) % stations.size();
    }

    // Is the finish line between from_station and to_station when running forwards?
    private boolean isStartBetween(int fromStation, int toStation) {
        return fromStation > toStation;
    }

    private void save() {
        lapDAO.deleteByLapSourceId(this.lapSourceId);

        LinkedList<Lap> laps = new LinkedList<>();
        for (Map.Entry<Integer, List<Timestamp>> entries : teamLaps.entrySet()) {
            for (Timestamp timestamp : entries.getValue()) {
                laps.add(new Lap(entries.getKey(), lapSourceId, timestamp));
            }
        }

        lapDAO.insertAll(laps.iterator());
    }

    @Override
    public synchronized void handle(Detection msg) {
        if (!this.debounceScheduled) {
            // TODO: this might be better as an atomic
            this.debounceScheduled = true;
            this.scheduler.schedule(() -> {
                try {
                    this.calculateLaps();
                } catch (Exception e) {
                    logger.severe(e.getMessage());
                }
                this.debounceScheduled = false;
            }, DEBOUNCE_TIMEOUT, TimeUnit.SECONDS);
        }
    }

    @Override
    public void registerAPI(JerseyEnvironment jersey) {

    }
}
