package telraam.monitoring;

import telraam.database.daos.DetectionDAO;
import telraam.database.daos.TeamDAO;
import telraam.database.models.Detection;

import java.util.*;
import java.util.stream.Collectors;

public class StationDetectionManager {
    private DetectionDAO detectionDAO;

    public StationDetectionManager(DetectionDAO detectionDAO) {
        this.detectionDAO = detectionDAO;
    }

    public Map<Integer, Long> timeSinceLastDetectionPerStation() {
        Set<Integer> stationIdList = this.detectionDAO.getAll().stream().map(Detection::getStationId).collect(Collectors.toSet());
        Map<Integer, Long> stationIdToTimeSinceLatestDetection = new HashMap<>();
        for (Integer stationId : stationIdList) {
            Optional<Detection> maybeDetection = this.detectionDAO.latestDetectionByStationId(stationId);

            if (maybeDetection.isPresent()) {
                Long time = maybeDetection.get().getTimestamp().getTime();
                stationIdToTimeSinceLatestDetection.put(stationId, (System.currentTimeMillis() - time)/1000);
            } else {
                stationIdToTimeSinceLatestDetection.put(stationId, null);
            }
        }
        return stationIdToTimeSinceLatestDetection;
    }
}
