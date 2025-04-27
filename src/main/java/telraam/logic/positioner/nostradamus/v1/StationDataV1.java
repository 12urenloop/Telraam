package telraam.logic.positioner.nostradamus.v1;

import telraam.database.models.Station;

import java.util.ArrayList;
import java.util.List;

// Record containing all data necessary for TeamData
public record StationDataV1(
        Station station, // The station
        Station nextStation, // The next station
        List<Long> times, // List containing the times (in ms) that was needed to run from this station to the next one.
        int index, // Index of this station when sorting a station list by distanceFromStart
        float currentProgress, // The progress value of this station
        float nextProgress // The progress value of the next station
) {
    public StationDataV1() {
        this(
                new Station(-10),
                new Station(-9),
                new ArrayList<>(0),
                -10,
                0F,
                0F
        );
    }

    public StationDataV1(List<Station> stations, int index, int averageAmount, int totalDistance) {
        this(
                stations.get(index),
                stations.get((index + 1) % stations.size()),
                new CircularQueueV1<>(averageAmount),
                index,
                (float) (stations.get(index).getDistanceFromStart() / totalDistance),
                (float) (stations.get((index + 1) % stations.size()).getDistanceFromStart() / totalDistance)
        );
    }
}
