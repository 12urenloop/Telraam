package telraam.logic.robustLapper;

import io.dropwizard.jersey.setup.JerseyEnvironment;
import org.jdbi.v3.core.Jdbi;
import telraam.database.daos.*;
import telraam.database.models.*;
import telraam.logic.Lapper;

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
    private List<Station> stations;
    private Map<Integer, List<Lap>> teamLaps;

    public RobustLapper(Jdbi jdbi) {
        this.jdbi = jdbi;
        this.scheduler = Executors.newScheduledThreadPool(1);
        this.logger = Logger.getLogger(RobustLapper.class.getName());
        this.lapDAO = jdbi.onDemand(LapDAO.class);
        this.debounceScheduled = false;

        // Get the lapSourceId, create the source if needed
        LapSourceDAO lapSourceDAO = jdbi.onDemand(LapSourceDAO.class);
        lapSourceDAO.getByName(SOURCE_NAME).ifPresentOrElse(
                lapSource -> this.lapSourceId = lapSource.getId(),
                () -> this.lapSourceId = lapSourceDAO.insert(new LapSource(SOURCE_NAME))
        );
    }

    // Group all detections by time and keep only one detection per timestamp
    private void processData() {
        DetectionDAO detectionDAO = this.jdbi.onDemand(DetectionDAO.class);
        List<Detection> detections = detectionDAO.getAllWithTeamId(MIN_RSSI);
        detections.sort(Comparator.comparing(Detection::getTimestamp));
        teamDetections = new HashMap<>();

        for (Detection detection : detections) {
            if (detection.getTeamId() != null) {
                if (teamDetections.containsKey(detection.getTeamId())) {
                    // teamDetections already contains teamId
                    List<Detection> teamDetectionsList = teamDetections.get(detection.getTeamId());
                    if (teamDetectionsList.get(teamDetectionsList.size() - 1).getTimestamp().compareTo(detection.getTimestamp()) == 0) {
                        // There's already a detection for that timestamp, keep the one with the highest rssi
                        if (teamDetectionsList.get(teamDetectionsList.size() - 1).getRssi() < detection.getRssi()) {
                            teamDetectionsList.remove(teamDetectionsList.size() - 1);
                            teamDetectionsList.add(detection);
                        }
                    } else {
                        // No detection yet for that timestamp so let's add it
                        teamDetectionsList.add(detection);
                    }
                } else {
                    // Team id isn't in teamDetections yet so let's add it
                    teamDetections.put(detection.getTeamId(), new ArrayList<>());
                    teamDetections.get(detection.getTeamId()).add(detection);
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

        for (Map.Entry<Integer, List<Detection>> entry : teamDetections.entrySet()) {
            List<Timestamp> lapTimes = new ArrayList<>();

            // Station that is used for current interval
            int lastStationPosition = 0;
            long currentStationTime = 0;
            int currentStationRssi = MIN_RSSI;
            int currentStationPosition = 0;

            for (Detection detection : entry.getValue()) {
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
            teamLaps.put(entry.getKey(), lapTimes.stream().map(time -> new Lap(entry.getKey(), lapSourceId, time)).collect(Collectors.toList()));
        }

        save();
        logger.info("RobustLapper: Done calculating laps");
    }

    // Amount of stations between from_station and to_station, counted when going backward
    private int backwardPathDistance(int fromStation, int toStation) {
        return (((fromStation - toStation) % stations.size()) + stations.size()) % stations.size();
    }

    // Returns whether the finish line is between from_station and to_station when running forwards
    private boolean isStartBetween(int fromStation, int toStation) {
        return fromStation > toStation;
    }

    private void save() {
        // Get all the old laps and sort by team
        List<Lap> laps = lapDAO.getAllBySource(lapSourceId);
        laps = laps.stream().filter(lap -> ! lap.getManual()).collect(Collectors.toList());
        Map<Integer, List<Lap>> oldLaps = new HashMap<>();

        for (Integer teamId : teamLaps.keySet()) {
            oldLaps.put(teamId, new ArrayList<>());
        }

        for (Lap lap : laps) {
            oldLaps.get(lap.getTeamId()).add(lap);
        }

        List<Lap> lapsToUpdate = new ArrayList<>();
        List<Lap> lapsToInsert = new ArrayList<>();
        List<Lap> lapsToDelete = new ArrayList<>();

        for (Map.Entry<Integer, List<Lap>> entries : teamLaps.entrySet()) {
            List<Lap> newLapsTeam = entries.getValue();
            List<Lap> oldLapsTeam = oldLaps.get(entries.getKey());
            oldLapsTeam.sort(Comparator.comparing(Lap::getTimestamp));
            int i = 0;
            // Go over each lap and compare timestamp
            while (i < oldLapsTeam.size() && i < newLapsTeam.size()) {
                // Update the timestamp if it isn't equal
                if (! oldLapsTeam.get(i).getTimestamp().equals(newLapsTeam.get(i).getTimestamp())) {
                    oldLapsTeam.get(i).setTimestamp(newLapsTeam.get(i).getTimestamp());
                    lapsToUpdate.add(oldLapsTeam.get(i));
                }
                i++;
            }

            // More old laps so delete the surplus
            while (i < oldLapsTeam.size()) {
                lapsToDelete.add(oldLapsTeam.get(i));
                i++;
            }

            // Add the new laps
            while (i < newLapsTeam.size()) {
                lapsToInsert.add(newLapsTeam.get(i));
                i++;
            }
        }

        lapDAO.updateAll(lapsToUpdate.iterator());
        lapDAO.insertAll(lapsToInsert.iterator());
        lapDAO.deleteAll(lapsToDelete.iterator());
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
