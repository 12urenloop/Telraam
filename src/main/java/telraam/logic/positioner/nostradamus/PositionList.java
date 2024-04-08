package telraam.logic.positioner.nostradamus;

import lombok.Getter;
import telraam.database.models.Detection;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Comparator;

public class PositionList extends ArrayList<Detection> {

    private final int interval;
    @Getter
    private Detection currentPosition;
    private Timestamp newestDetection;

    public PositionList(int interval) {
        this.interval = interval;
        this.currentPosition = new Detection(-1, 0, -100);
        this.newestDetection = new Timestamp(0);
    }

    /**
     * @param e element to add
     * @return True if the current position has changed
     */
    @Override
    public boolean add(Detection e) {
        super.add(e);

        if (e.getTimestamp().after(newestDetection)) {
            newestDetection = e.getTimestamp();
        }

        if (!e.getStationId().equals(currentPosition.getStationId())) {
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

    private boolean inInterval(Timestamp oldest, Timestamp newest) {
        return newest.getNanos() - oldest.getNanos() > interval;
    }
}
