package telraam.logic.robustLapper.robustLapperIncremental;

import org.jdbi.v3.core.Jdbi;
import telraam.database.daos.DetectionDAO;
import telraam.database.daos.LapDAO;
import telraam.database.daos.StationDAO;
import telraam.database.models.Detection;
import telraam.database.models.Lap;
import telraam.database.models.Station;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;

public class RobustLapperTeam implements Runnable {

    private final Jdbi jdbi;

    private final int MIN_RSSI = -85;

    private Detection lastDetection = new Detection(0, new Timestamp(0));
    private List<Detection> detections;
    private int lastStationPosition = 0;
    private long currentStationTime = 0;
    private int currentStationRssi = Integer.MIN_VALUE;
    private int currentStationPosition = 0;

    private final int INTERVAL_TIME = 2000;
    private final int teamId;
    private final int lapSourceId;
    private final LapDAO lapDAO;
    private List<Integer> stationIdToPosition;
    private int stationsSize;

    public RobustLapperTeam(Jdbi jdbi, int teamId, int lapSourceId) {
        this.jdbi = jdbi;
        this.teamId = teamId;
        this.lapSourceId = lapSourceId;
        this.lapDAO = jdbi.onDemand(LapDAO.class);
        makeStationIdToPosition();
    }

    private void makeStationIdToPosition() {
        StationDAO stationDAO = jdbi.onDemand(StationDAO.class);
        List<Station> stations = stationDAO.getAll();
        stations.sort(Comparator.comparing(Station::getDistanceFromStart));
        stationsSize = stations.size();
        stationIdToPosition = stations.stream().map(Station::getId).toList();
    }

    public void calculateLaps() {
        if (detections.get(0).getTimestamp().before(lastDetection.getTimestamp())) {
            getOldData(detections);
        }
        List<Lap> laps = new ArrayList<>();

        for (Detection detection : detections) {
            // Group all detections based on INTERVAL_TIME
            if (detection.getTimestamp().getTime() - currentStationTime < INTERVAL_TIME) {
                // We're still in the same interval

                // Use the station with the highest RSSI from this interval
                if (detection.getRssi() > currentStationRssi) {
                    currentStationPosition = getStationIndex(detection.getStationId());
                    currentStationRssi = detection.getRssi();
                }
            } else {
                // We're in a new interval, use the detection with the highest RSSI to update trajectory
                // Check if new station is more likely to be in front of the runner
                if (! (backwardPathDistance(lastStationPosition, currentStationPosition) <= 2)) {
                    if (isStartBetween(lastStationPosition, currentStationPosition)) {
                        // Add lap if we passed the start line
                        laps.add(new Lap(teamId, lapSourceId, detection.getTimestamp()));
                    }
                    lastStationPosition = currentStationPosition;
                }
                // Prepare new interval
                currentStationTime = detection.getTimestamp().getTime();
                currentStationPosition = getStationIndex(detection.getStationId());
                currentStationRssi = detection.getRssi();
            }
        }

        lastDetection = detections.get(detections.size() - 1);
        save(laps);
    }

    // Get all detections since the first new detection
    private void getOldData(List<Detection> detections) {
        DetectionDAO detectionDAO = jdbi.onDemand(DetectionDAO.class);
        LapDAO lapDAO = jdbi.onDemand(LapDAO.class);
        Optional<Lap> lastLapOptional = lapDAO.getTeamLastLapBeforeWithSourceId(teamId, detections.get(0).getTimestamp(), lapSourceId);
        Timestamp timestamp;
        if (lastLapOptional.isPresent()) {
            Lap lastLap = lastLapOptional.get();
            timestamp = lastLap.getTimestamp();
            lapDAO.deleteTeamLapsSinceIdWithSourceId(teamId, lastLap.getId(), lapSourceId);
        } else {
            timestamp = new Timestamp(0);
        }
        detections.clear();
        detections.addAll(detectionDAO.getAfterTimestampFilterTeamId(teamId, timestamp, MIN_RSSI));
    }

    // Return station index
    // Supports station that are added midway the event
    private int getStationIndex(int stationId) {
        int index = stationIdToPosition.indexOf(stationId);
        if (index == -1) {
            makeStationIdToPosition();
            index = stationIdToPosition.indexOf(stationId);
        }

        return index;
    }

    // Amount of stations between from_station and to_station, counted when going backward
    private int backwardPathDistance(int fromStation, int toStation) {
        return (((fromStation - toStation) % stationsSize) + stationsSize) % stationsSize;
    }

    // Is the finish line between from_station and to_station when running forwards?
    private boolean isStartBetween(int fromStation, int toStation) {
        return fromStation > toStation;
    }

    // Save new lap times
    private void save(List<Lap> laps) {
        lapDAO.insertAll(laps.iterator());
    }

    public void setDetections(List<Detection> detections) {
        this.detections = detections;
    }

    @Override
    public void run() {
        calculateLaps();
    }
}