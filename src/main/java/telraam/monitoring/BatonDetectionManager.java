package telraam.monitoring;

import telraam.database.daos.BatonDAO;
import telraam.database.daos.DetectionDAO;
import telraam.database.daos.TeamDAO;
import telraam.database.models.Baton;
import telraam.database.models.Team;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class BatonDetectionManager {
    private DetectionDAO detectionDAO;
    private TeamDAO teamDAO;

    // Map of a baton id to it's detections
    private Map<Integer, List<BatonDetection>> batonDetectionMap = new HashMap<>();
    private Integer handledDetectionId = 0;

    public BatonDetectionManager(DetectionDAO detectionDAO, TeamDAO teamDAO) {
        this.detectionDAO = detectionDAO;
        this.teamDAO = teamDAO;
    }

    public Map<Integer, List<BatonDetection>> getBatonDetections() {
        var detections = detectionDAO.getSinceId(handledDetectionId, Integer.MAX_VALUE);
        // Map of batonIds to team info
        var teamMap = new HashMap<Integer, Team>();
        var teams = teamDAO.getAll();
        teams.forEach(t -> teamMap.put(t.getBatonId(), t));
        detections.forEach(d -> {
            if (d.getId() > handledDetectionId) {
                handledDetectionId = d.getId();
            }
            Integer batonId = d.getBatonId();
            if (!batonDetectionMap.containsKey(batonId)) {
                batonDetectionMap.put(batonId, new ArrayList<>());
            }
            var batonDetections = batonDetectionMap.get(batonId);
            var team = teamMap.get(batonId);
            var batonDetection = new BatonDetection(Math.toIntExact(d.getTimestamp().getTime() / 1000), d.getRssi(), batonId, team.getName());
            batonDetections.add(batonDetection);
        });
        return batonDetectionMap;
    }
}
