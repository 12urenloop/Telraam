package telraam.logic.positioner.nostradamus.v1;

import lombok.Getter;
import telraam.database.models.Detection;
import telraam.database.models.Station;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

public class DetectionListV1 extends ArrayList<Detection> {

    private final int interval;
    private final List<Integer> stations;
    @Getter
    private Detection currentPosition;
    private Timestamp newestDetection;

    public DetectionListV1(int interval, List<Station> stations) {
        this.interval = interval;
        this.stations = stations.stream().sorted(Comparator.comparing(Station::getDistanceFromStart)).map(Station::getId).toList();
        this.currentPosition = new Detection(-1, 0, -100);
        this.newestDetection = new Timestamp(0);
    }

    // Returns True if the added detection results in a new station
    @Override
    public boolean add(Detection e) {
        super.add(e);

        if (e.getTimestamp().after(newestDetection)) {
            newestDetection = e.getTimestamp();
        }

        if (!e.getStationId().equals(currentPosition.getStationId()) && stationAfter(currentPosition.getStationId(), e.getStationId())) {
            // Possible new position
            if (e.getRssi() > currentPosition.getRssi() || !inInterval(currentPosition.getTimestamp(), newestDetection)) {
                // Detection stored in currentPosition will change
                int oldPosition = currentPosition.getStationId();
                // Filter out old detections
                removeIf(detection -> !inInterval(detection.getTimestamp(), newestDetection));

                // Get new position
                currentPosition = stream().max(Comparator.comparing(Detection::getRssi)).get();

                return oldPosition != currentPosition.getStationId();
            }
        }

        return false;
    }

    private boolean stationAfter(int oldStationId, int newStationId) {
        return (stations.indexOf(newStationId) - stations.indexOf(oldStationId) + stations.size()) % stations.size() > 0;
    }

    private boolean inInterval(Timestamp oldest, Timestamp newest) {
        return newest.getTime() - oldest.getTime() < interval;
    }
}
