package telraam.logic.positioner.nostradamus;

import lombok.Getter;
import telraam.database.models.Detection;
import telraam.database.models.Station;
import telraam.logic.positioner.Position;

import java.util.*;
import java.util.stream.Collectors;

public class TeamData {
    private final PositionList detections; // List with all relevant detections
    private final Map<Integer, StationData> stations;  // Station list
    private long previousStationArrival; // Arrival time of previous station. Used to calculate the average times
    private StationData currentStation; // Current station location
    private StationData previousStation; // Previous station location
    private final int totalDistance; // Total distance of the track
    @Getter
    private final Position position; // Data to send to the websocket
    private final int maxDeviance; // Maximum deviance the animation can have from the reality


    public TeamData(int teamId, int interval, List<Station> stations, int averageAmount, double sprintingSpeed, int finishOffset) {
        stations.sort(Comparator.comparing(Station::getDistanceFromStart));
        this.totalDistance = (int) (stations.get(stations.size() - 1).getDistanceFromStart() + finishOffset);
        this.stations = stations.stream().collect(Collectors.toMap(
                Station::getId,
                station -> new StationData(
                        stations,
                        stations.indexOf(station),
                        averageAmount,
                        totalDistance
                )
        ));
        // Pre-populate with some data
        this.stations.forEach((stationId, stationData) -> stationData.averageTimes().add(
                (long) (((stationData.nextStation().getDistanceFromStart() - stationData.station().getDistanceFromStart() + totalDistance) % totalDistance) / sprintingSpeed)
        ));
        this.detections = new PositionList(interval, stations);
        this.previousStationArrival = System.currentTimeMillis();
        this.currentStation = new StationData(new Station(-10), new Station(-9), new CircularQueue<>(0), -10, -10, -10); // Will never trigger `isNextStation` for the first station
        this.position = new Position(teamId);
        this.maxDeviance = 1 / stations.size();
    }

    // Add a new detection
    // Returns true if the team is at a next station
    public boolean addDetection(Detection e) {
        boolean newStation = detections.add(e);

        if ((newStation && isForwardStation(currentStation.index(), stations.get(e.getStationId()).index())) || currentStation.index() == -10) {
            // Is at a new station that is in front of the previous one.
            previousStation = currentStation;
            currentStation = stations.get(e.getStationId());
            if (isNextStation()) {
                // Only add the time if it goes forward by exactly one station
                previousStation.averageTimes().add(System.currentTimeMillis() - previousStationArrival);
            }
            previousStationArrival = System.currentTimeMillis();

            return true;
        }

        return false;
    }

    private boolean isForwardStation(int oldStation, int newStation) {
        int stationDiff = (newStation - oldStation + stations.size()) % stations.size();
        return stationDiff > 0 && stationDiff < 3;
    }

    private boolean isNextStation() {
        return Objects.equals(previousStation.nextStation().getId(), currentStation.station().getId());
    }

    // Update the position data
    public void updatePosition() {
        long currentTime = System.currentTimeMillis();

        // Animation is currently at progress x
        long milliSecondsSince = currentTime - position.getTimestamp();
        double theoreticalProgress = (position.getProgress() + (position.getSpeed() * milliSecondsSince)) % 1;

        // Arrive at next station at timestamp y and progress z
        long nextStationArrival = currentTime + getMedian();
        double goalProgress = currentStation.nextProgress();

        double speed, progress;
        // Determine whether to speed up / slow down the animation or teleport it
        double difference = (goalProgress - theoreticalProgress + 1) % 1;
        if ((difference >= maxDeviance && difference <= 1 - maxDeviance) || previousStation.averageTimes().size() < 5) {
            // Animation was too far behind or ahead so teleport
            progress = currentStation.currentProgress();
            speed = ((currentStation.nextProgress() - progress + 1) % 1) / getMedian();
        } else {
            // Animation is close enough, adjust so that we're synced at the next station
            progress = theoreticalProgress;
            speed = ((goalProgress - theoreticalProgress + 1) % 1) / (nextStationArrival - currentTime);
        }

        position.update(progress, speed);
    }

    // Get the medium of the average times
    private long getMedian() {
        List<Long> sortedList = new ArrayList<>(currentStation.averageTimes());
        Collections.sort(sortedList);

        int size = sortedList.size();
        return size % 2 == 0 ? (sortedList.get(size / 2 - 1) + sortedList.get(size / 2)) / 2 : (sortedList.get(size / 2));
    }
}

// TODO: Possible problem: Might arrive to early at station -> Move backwards stations by 5 meter
