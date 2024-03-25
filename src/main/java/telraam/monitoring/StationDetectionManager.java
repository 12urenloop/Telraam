package telraam.monitoring;

import telraam.database.daos.DetectionDAO;
import telraam.database.daos.StationDAO;
import telraam.database.models.Detection;
import telraam.database.models.Station;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public class StationDetectionManager {
    private DetectionDAO detectionDAO;

    private StationDAO stationDAO;

    public StationDetectionManager(DetectionDAO detectionDAO, StationDAO stationDAO) {
        this.detectionDAO = detectionDAO;
        this.stationDAO = stationDAO;
    }

    public Map<String, Long> timeSinceLastDetectionPerStation() {
        List<Integer> stationIdList = stationDAO.getAll().stream().map(Station::getId).toList();
        Map<String, Long> stationIdToTimeSinceLatestDetection = new HashMap<>();
        for (Integer stationId : stationIdList) {
            Optional<Detection> maybeDetection = this.detectionDAO.latestDetectionByStationId(stationId);
            Optional<Station> maybeStation = this.stationDAO.getById(stationId);
            if (maybeDetection.isPresent() && maybeStation.isPresent()) {
                Long time = maybeDetection.get().getTimestamp().getTime();
                stationIdToTimeSinceLatestDetection.put(maybeStation.get().getName(), (System.currentTimeMillis() - time) / 1000);
            }
        }
        return stationIdToTimeSinceLatestDetection;
    }
}
