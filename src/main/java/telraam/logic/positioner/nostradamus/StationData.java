package telraam.logic.positioner.nostradamus;

import telraam.database.models.Station;

import java.util.List;

public record StationData(
        Station station, // The station
        Station nextStation, // The next station
        List<Long> averageTimes, // List containing the times (in ms) that was needed to run from this station to the next one.
        int index, // Index of this station when sorting a station list by distanceFromStart
        double currentProgress, // The progress value of this station
        double nextProgress // The progress value of the next station
) {
    public StationData(List<Station> stations, int index, int averageAmount, int totalDistance) {
        this(
                stations.get(index),
                stations.get((index + 1) % stations.size()),
                new CircularQueue<>(averageAmount),
                index,
                stations.get(index).getDistanceFromStart() / totalDistance,
                stations.get((index + 1) % stations.size()).getDistanceFromStart() / totalDistance
        );
    }
}
