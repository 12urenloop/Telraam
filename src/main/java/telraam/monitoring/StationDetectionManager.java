package telraam.monitoring;

import telraam.database.daos.DetectionDAO;
import telraam.database.daos.StationDAO;
import telraam.database.daos.TeamDAO;
import telraam.database.models.Detection;
import telraam.database.models.Station;

import java.util.*;
import java.util.stream.Collectors;

public class StationDetectionManager {
    private DetectionDAO detectionDAO;

    private StationDAO stationDAO;

    public StationDetectionManager(DetectionDAO detectionDAO, StationDAO stationDAO) {
        this.detectionDAO = detectionDAO;
        this.stationDAO = stationDAO;
    }

    public Map<Integer, Long> timeSinceLastDetectionPerStation() {
        List<Integer> stationIdList = stationDAO.getAll().stream().map(Station::getId).toList();
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
