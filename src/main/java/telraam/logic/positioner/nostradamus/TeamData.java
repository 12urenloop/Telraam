package telraam.logic.positioner.nostradamus;

import lombok.Getter;
import telraam.database.models.Detection;
import telraam.database.models.Station;
import telraam.logic.positioner.Position;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class TeamData {
    private PositionList detections;
    private List<Station> stations; // Station list, ordered by distance from start
    private Map<Integer, List<Long>> averageTimes; // List of average times for each station. Going from station 2 to 3 is saved in 3.
    private long previousStationArrival; // Arrival time of previous station. Used to calculate the average times
    @Getter
    private Position position;
    private Station currentStation;
    private int totalDistance;


    public TeamData(int teamId, int interval, List<Station> stations, int averageAmount) {
        this.detections = new PositionList(interval);
        this.stations = stations;

        averageTimes = new HashMap<>();
        for (Station station: stations) {
            averageTimes.put(station.getId(), new CircularQueue<>(averageAmount));
        }

        this.previousStationArrival = System.currentTimeMillis();
        this.position = new Position(teamId);
    }

    public boolean addDetection(Detection e) {
        boolean newStation = detections.add(e);
        Station previousStation = detections.getCurrentPosition().getStationId()

        // TODO: If station missed big problem
        if (newStation && averageTimes.containsKey(detections.getCurrentPosition())) {
            long now = System.currentTimeMillis();
            averageTimes.get(detections.getCurrentPosition()).add(now - previousStationArrival);
            previousStationArrival = now;
        }

        return newStation;
    }

    // TODO: Requires the last station to be on the finish. Convert to variable
    public void updatePosition() {
        float progress = (float) (currentStation.getDistanceFromStart() / totalDistance);
        Station nextStation = stations.get(stations.indexOf(currentStation) + 1 % stations.size());



        position.setProgress(progress);
    }
}
