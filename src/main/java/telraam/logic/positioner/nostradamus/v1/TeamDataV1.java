package telraam.logic.positioner.nostradamus.v1;

import lombok.Getter;
import lombok.Setter;
import telraam.database.models.Detection;
import telraam.database.models.Station;
import telraam.logic.positioner.Position;

import java.util.*;
import java.util.logging.Logger;
import java.util.stream.Collectors;

public class TeamDataV1 {
    private static final Logger logger = Logger.getLogger(TeamDataV1.class.getName());
    private final DetectionListV1 detections; // List with all relevant detections
    private final Map<Integer, StationDataV1> stations;  // Station list
    private StationDataV1 currentStation; // Current station location
    private StationDataV1 previousStation; // Previous station location
    @Getter @Setter
    private long previousStationArrival; // Arrival time of previous station. Used to calculate the average times
    private final int totalDistance; // Total distance of the track
    private final float maxDeviance; // Maximum deviance the animation can have from the reality
    @Getter
    private Position position; // Data to send to the websocket
    private final int teamId;


    public TeamDataV1(int teamId, int interval, List<Station> stations, int averageAmount, double sprintingSpeed, int finishOffset) {
        stations.sort(Comparator.comparing(Station::getDistanceFromStart));
        this.totalDistance = (int) (stations.get(stations.size() - 1).getDistanceFromStart() + finishOffset);
        this.stations = stations.stream().collect(Collectors.toMap(
                Station::getId,
                station -> new StationDataV1(
                        stations,
                        stations.indexOf(station),
                        averageAmount,
                        totalDistance
                )
        ));
        // Pre-populate with some default values
        this.stations.forEach((stationId, stationData) -> stationData.times().add(
                (long) (((stationData.nextStation().getDistanceFromStart() - stationData.station().getDistanceFromStart() + totalDistance) % totalDistance) / sprintingSpeed)
        ));
        this.detections = new DetectionListV1(interval, stations);
        this.previousStationArrival = System.currentTimeMillis();
        this.currentStation = new StationDataV1(); // Will never trigger `isNextStation` for the first station
        this.maxDeviance = (float) 1 / stations.size();
        this.teamId = teamId;
        this.position = new Position(teamId, 0, 0, 0, System.currentTimeMillis());
    }

    // Add a new detection
    // Returns true if the team is at a new station
    public boolean addDetection(Detection e) {
        boolean newStation = detections.add(e);

        if ((newStation && isForwardStation(currentStation.index(), stations.get(e.getStationId()).index())) || currentStation.index() == -10) {
            // Is at a new station that is in front of the previous one.
            previousStation = currentStation;
            currentStation = stations.get(e.getStationId());
            long now = System.currentTimeMillis();
            if (isNextStation()) {
                // Only add the time if it goes forward by exactly one station
                previousStation.times().add(now - previousStationArrival);
            }
            previousStationArrival = now;

            return true;
        }

        return false;
    }

    private boolean isForwardStation(int oldStation, int newStation) {
        int stationDiff = (newStation - oldStation + stations.size()) % stations.size();
        return stationDiff < 3;
    }

    private boolean isNextStation() {
        return Objects.equals(previousStation.nextStation().getId(), currentStation.station().getId());
    }

    private double normalize(double number) {
        return (number + 1) % 1;
    }

    // Update the position data
    public void updatePosition() {
        long currentTime = System.currentTimeMillis();

        // Animation is currently at progress x
        long milliSecondsSince = currentTime - position.timestamp();
        double theoreticalProgress = normalize(position.progress() + (position.speed() * milliSecondsSince));

        // Arrive at next station at timestamp y and progress z
        double median = getMedian();
        double nextStationArrival = currentTime + median;
        double goalProgress = currentStation.nextProgress();

        double speed, progress;
        // Determine whether to speed up / slow down the animation or teleport it
        double difference = normalize(currentStation.currentProgress() - theoreticalProgress);
        if ((difference >= maxDeviance && difference <= 1 - maxDeviance)) {
            // Animation was too far behind or ahead so teleport
            progress = currentStation.currentProgress();
            speed = normalize(currentStation.nextProgress() - progress) / median;
        } else {
            // Animation is close enough, adjust so that we're synced at the next station
            progress = theoreticalProgress;
            speed = normalize(goalProgress - theoreticalProgress) / (nextStationArrival - currentTime);
        }

        position = new Position(teamId, progress, speed, 0, currentTime);
    }

    // Get the medium of the average times
    private long getMedian() {
        List<Long> sortedList = new ArrayList<>(currentStation.times());
        Collections.sort(sortedList);

        int size = sortedList.size();
        return size % 2 == 0 ? (sortedList.get(size / 2 - 1) + sortedList.get(size / 2)) / 2 : (sortedList.get(size / 2));
    }
}
