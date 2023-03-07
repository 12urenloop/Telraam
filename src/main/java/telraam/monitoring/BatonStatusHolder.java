package telraam.monitoring;

import telraam.database.daos.BatonDAO;
import telraam.database.daos.DetectionDAO;
import telraam.database.models.Baton;
import telraam.database.models.Detection;
import telraam.monitoring.models.BatonStatus;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class BatonStatusHolder {
    // Map from batonMac to batonStatus
    private HashMap<String, BatonStatus> batonStatusMap = new HashMap<>();
    private HashMap<Integer, String> batonIdToMac = new HashMap<>();

    private BatonDAO batonDAO;
    private DetectionDAO detectionDAO;

    public BatonStatusHolder(BatonDAO BDAO, DetectionDAO DDAO) {
        batonDAO = BDAO;
        detectionDAO = DDAO;
        this.initStatus();
    }

    private void initStatus() {
        var batons = batonDAO.getAll();
        for (Baton baton : batons) {
            BatonStatus batonStatus = new BatonStatus(
                    baton.getMac().toLowerCase(),
                    baton.getName(),
                    0,
                    0,
                    false,
                    null,
                    -1
            );
            batonStatusMap.put(baton.getMac().toLowerCase(), batonStatus);
        }

    }

    public List<BatonStatus> GetAllBatonStatuses() {
        // For each baton, fetch latest detection
        var batons = batonDAO.getAll();
        for (Baton baton : batons) {
            var batonStatus = GetBatonStatus(baton.getId());
            var detection = detectionDAO.latestDetectionByBatonId(baton.getId(), batonStatus.getLastSeen());
            detection.ifPresent(this::updateState);
        }
        return new ArrayList<>(batonStatusMap.values());
    }

    private void updateState(Detection msg) {
        BatonStatus batonStatus = GetBatonStatus(msg.getBatonId());
        if (batonStatus == null) {
            batonStatus = createBatonStatus(msg.getBatonId());
            batonStatusMap.put(batonStatus.getMac(), batonStatus);
            return;
        }
        if (batonStatus.getLastSeen() == null) {
            batonStatus.setLastSeen(msg.getTimestamp());
        }
        if (batonStatus.getLastSeen().after(msg.getTimestamp())) {
            return;
        }
        if (msg.getUptimeMs() < batonStatus.getUptime() - 3000) {
            batonStatus.setRebooted(true);
        }
        batonStatus.setLastSeenSecondsAgo((System.currentTimeMillis() - msg.getTimestamp().getTime()) / 1000);
        batonStatus.setLastSeen(msg.getTimestamp());
        batonStatus.setUptime(msg.getUptimeMs() / 1000);
        batonStatus.setBattery(msg.getBattery());
        batonStatus.setLastDetectedAtStation(msg.getStationId());
    }

    public BatonStatus GetBatonStatus(Integer batonId) {
        if (!batonIdToMac.containsKey(batonId)) {
            var baton = batonDAO.getById(batonId);
            baton.ifPresent(value -> batonIdToMac.put(batonId, value.getMac().toLowerCase()));
        }
        String batonMac = batonIdToMac.get(batonId);
        return batonStatusMap.get(batonMac);
    }

    public BatonStatus createBatonStatus(Integer batonId) {
        String batonMac = batonIdToMac.get(batonId);
        if (batonMac != null) {
            return batonStatusMap.get(batonMac);
        }
        var baton = batonDAO.getById(batonId);
        if (baton.isEmpty()) {
            // Non-existing baton
            return null;
        }
        BatonStatus batonStatus = new BatonStatus(
                baton.get().getMac().toLowerCase(),
                baton.get().getName(),
                0,
                0,
                false,
                null,
                -1
        );
        batonStatusMap.put(batonStatus.getMac(), batonStatus);
        batonIdToMac.put(batonId, batonStatus.getMac());
        return batonStatus;
    }
}
