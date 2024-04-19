package telraam.logic.positioner.nostradamus;

import lombok.Getter;
import telraam.database.models.Detection;
import telraam.database.models.Station;
import telraam.logic.positioner.Position;

import java.util.*;
import java.util.stream.Collectors;

public class TeamData {
    private final PositionList detections;
    private final List<Station> stations; // Station list, ordered by distance from start
    private final Map<Integer, Station> idToStation; // Map going from a station id to the station
    private final Map<Station, List<Long>> averageTimes; // List of average times for each station. Going from station 2 to 3 is saved in 3.
    private long previousStationArrival; // Arrival time of previous station. Used to calculate the average times
    private Station currentStation; // Current station location
    private Station previousStation; // Previous station location
    private final Double totalDistance;
    @Getter
    private final Position position;


    public TeamData(int teamId, int interval, List<Station> stations, int averageAmount, double sprintingSpeed) {
        this.stations = stations;
        this.stations.sort(Comparator.comparing(Station::getDistanceFromStart));
        this.detections = new PositionList(interval, this.stations);
        this.idToStation = stations.stream().collect(Collectors.toMap(Station::getId, station -> station));
//        this.averageTimes = stations.stream().collect(Collectors.toMap(station -> station, station -> new CircularQueue<>(averageAmount)));
        this.previousStationArrival = System.currentTimeMillis();
        this.currentStation = new Station(-10); // Will never trigger `isNextStation` for the first station
        this.totalDistance = this.stations.get(this.stations.size() - 1).getDistanceFromStart(); //  Requires last station to be located at the start
        this.averageTimes = getAverageTimes(averageAmount, sprintingSpeed);
        this.position = new Position(teamId);
    }

    // Construct the average times and add a default value
    // TODO: Populate with existing detections
    private Map<Station, List<Long>> getAverageTimes(int averageAmount, double sprintingSpeed) {
        Map<Station, List<Long>> averageTimes = new HashMap<>();
        for (Station station: stations) {
            int index = stations.indexOf(station);
            averageTimes.put(station, new CircularQueue<>(averageAmount));
            double distance = (stations.get((index + 1) % stations.size()).getDistanceFromStart() - station.getDistanceFromStart() + totalDistance) % totalDistance;
            averageTimes.get(station).add((long) (distance / sprintingSpeed));
        }

        return averageTimes;
    }

    public boolean addDetection(Detection e) {
        boolean newStation = detections.add(e);

        if ((newStation && isForwardStation(currentStation, idToStation.get(e.getStationId()))) || currentStation.getId() == -1) {
            previousStation = currentStation;
            currentStation = idToStation.get(e.getStationId());
            if (isNextStation()) {
                // Only add the time if it goes forward by exactly one station
                if (averageTimes.containsKey(previousStation)) {
                    // While only be false for the first time an interval in ran as previousStation is still null
                    averageTimes.get(previousStation).add(System.currentTimeMillis() - previousStationArrival);
                }
            }

            previousStationArrival = System.currentTimeMillis();

            return true;
        }

        return false;
    }

    private int getStationDiff(Station oldStation, Station newStation) {
        return (stations.indexOf(newStation) - stations.indexOf(oldStation) + stations.size()) % stations.size();
    }

    private boolean isForwardStation(Station oldStation, Station newStation) {
        int stationDiff = getStationDiff(oldStation, newStation);
        return stationDiff > 0 && stationDiff < 3;
    }

    private boolean isNextStation() {
        return getStationDiff(previousStation, currentStation) == 1;
    }

    public void updatePosition() {
        // Arrive at millisecond x
        double currentTime = System.currentTimeMillis();
        double nextStationArrival = currentTime + getMedian();

        // Current at progress y
        double milliSecondsSince = currentTime - position.getTimestamp();
        double theoreticalProgress = (position.getProgress() + (position.getSpeed() * milliSecondsSince)) % 1;

        // Progress in z amount of seconds
        Station nextStation = stations.get((stations.indexOf(currentStation) + 1) % stations.size());
        double goalProgress = nextStation.getDistanceFromStart() / totalDistance;
        double speed, progress;
        // TODO: Do in a different way
        // TODO: Right now assumes the difference between each station is the same
        if ((goalProgress - theoreticalProgress + 1) % 1 >= 1 - ((double) 1 / stations.size()) || averageTimes.get(currentStation).size() < 5) {
            // Animation was already further than the next station so just tp
            progress = currentStation.getDistanceFromStart() / totalDistance;
            speed = (((nextStation.getDistanceFromStart() / totalDistance) - progress + 1) % 1) / getMedian();
        } else {
            // Animation is either behind or maximum in front by one station. Adjust so we're synced at the next station
            progress = theoreticalProgress;
            speed = ((goalProgress - theoreticalProgress + 1) % 1) / (nextStationArrival - currentTime);
        }

        position.update(progress, speed);
    }

    private double getMedian() {
        List<Long> sortedList = new ArrayList<>(averageTimes.get(currentStation));
        Collections.sort(sortedList);

        int size = sortedList.size();
        return size % 2 == 0 ? (sortedList.get(size / 2 - 1) + sortedList.get(size / 2)) / 2D : (sortedList.get(size / 2));
    }
}

// TODO: Possible problem: Might arrive to early at station -> Move backwards stations by 5 m
