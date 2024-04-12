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
        this.position = new Position(teamId);
        this.averageTimes = getAverageTimes(averageAmount, sprintingSpeed);
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

        if (newStation) {
            previousStation = currentStation;
            currentStation = idToStation.get(e.getStationId());
            if (isNextStation()) {
                // Only add the time if it goes forward by exactly one station
                if (averageTimes.containsKey(previousStation)) {
                    // While only be false for the first time an interval in ran as previousStation is still null
                    averageTimes.get(previousStation).add(System.currentTimeMillis() / 1000 - previousStationArrival);
                }
            }
            previousStationArrival = System.currentTimeMillis() / 1000;
        }

        return newStation;
    }

    private boolean isNextStation() {
        return (stations.indexOf(currentStation) - stations.indexOf(previousStation) + stations.size()) % stations.size() == 1;
    }

    // TODO: Smoothen out
    public void updatePosition() {
        double progress = currentStation.getDistanceFromStart() / totalDistance;
        Station nextStation = stations.get((stations.indexOf(currentStation) + 1) % stations.size());
//        double distance = (nextStation.getDistanceFromStart() - currentStation.getDistanceFromStart() + totalDistance) % totalDistance;
        double speed = (((nextStation.getDistanceFromStart() / totalDistance) - progress + 1) % 1) / getAverage(); // Progress / second

        position.update(progress, speed);
    }

    private double getAverage() {
        return averageTimes.get(currentStation).stream().mapToDouble(time -> time).average().orElse(0.1);
    }
}
