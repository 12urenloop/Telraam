package telraam.monitoring;

import telraam.database.daos.BatonSwitchoverDAO;
import telraam.database.daos.DetectionDAO;
import telraam.database.daos.TeamDAO;
import telraam.database.models.Team;
import telraam.monitoring.models.BatonDetection;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class BatonDetectionManager {
    private DetectionDAO detectionDAO;
    private TeamDAO teamDAO;
    private BatonSwitchoverDAO batonSwitchoverDAO;

    // Map of a baton id to it's detections
    private Map<Integer, List<BatonDetection>> batonDetectionMap = new HashMap<>();
    private Integer handledDetectionId = 0;

    public BatonDetectionManager(DetectionDAO detectionDAO, TeamDAO teamDAO, BatonSwitchoverDAO batonSwitchoverDAO) {
        this.detectionDAO = detectionDAO;
        this.teamDAO = teamDAO;
        this.batonSwitchoverDAO = batonSwitchoverDAO;
    }

    public Map<Integer, List<BatonDetection>> getBatonDetections() {
        var detections = detectionDAO.getSinceId(handledDetectionId, Integer.MAX_VALUE);
        var batonSwitchOvers = batonSwitchoverDAO.getAll();
        // Map of batonIds to team info
        var teamMap = new HashMap<Integer, Team>();
        var teams = teamDAO.getAll();
        teams.forEach(t -> teamMap.put(t.getId(), t));
        detections.forEach(d -> {
            Map<Integer, Integer> batonTeamMap = new HashMap<>();
            batonSwitchOvers.forEach(b -> {
                if (b.getTimestamp().after(d.getTimestamp())) {
                    return;
                }
                if (b.getPreviousBatonId() != null && batonTeamMap.containsKey(b.getPreviousBatonId())) {
                    if (batonTeamMap.get(b.getPreviousBatonId()).equals(b.getTeamId())) {
                        batonTeamMap.remove(b.getPreviousBatonId());
                    }
                }
                if (b.getNewBatonId() != null) {
                    batonTeamMap.put(b.getNewBatonId(), b.getTeamId());
                }
            });
            if (!batonTeamMap.containsKey(d.getBatonId())) {
                return;
            }
            if (d.getId() > handledDetectionId) {
                handledDetectionId = d.getId();
            }
            Integer batonId = d.getBatonId();
            if (!batonDetectionMap.containsKey(batonId)) {
                batonDetectionMap.put(batonId, new ArrayList<>());
            }
            var batonDetections = batonDetectionMap.get(batonId);
            var team = teamMap.get(batonTeamMap.get(batonId));
            var batonDetection = new BatonDetection(Math.toIntExact(d.getTimestamp().getTime() / 1000), d.getRssi(),d.getStationId(), batonId, team.getName());
            batonDetections.add(batonDetection);
        });
        return batonDetectionMap;
    }
}
