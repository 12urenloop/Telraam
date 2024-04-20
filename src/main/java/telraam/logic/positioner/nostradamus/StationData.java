package telraam.logic.positioner.nostradamus;

import telraam.database.models.Station;

import java.util.ArrayList;
import java.util.List;

// Record containing all data necessary for TeamData
public record StationData(
        Station station, // The station
        Station nextStation, // The next station
        List<Long> times, // List containing the times (in ms) that was needed to run from this station to the next one.
        int index, // Index of this station when sorting a station list by distanceFromStart
        float currentProgress, // The progress value of this station
        float nextProgress // The progress value of the next station
) {
    public StationData() {
        this(
                new Station(-10),
                new Station(-9),
                new ArrayList<>(0),
                -10,
                0F,
                0F
        );
    }

    public StationData(List<Station> stations, int index, int averageAmount, int totalDistance) {
        this(
                stations.get(index),
                stations.get((index + 1) % stations.size()),
                new CircularQueue<>(averageAmount),
                index,
                (float) (stations.get(index).getDistanceFromStart() / totalDistance),
                (float) (stations.get((index + 1) % stations.size()).getDistanceFromStart() / totalDistance)
        );
    }
}
