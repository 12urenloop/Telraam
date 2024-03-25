package telraam.monitoring;

import telraam.database.daos.BatonDAO;
import telraam.database.daos.DetectionDAO;
import telraam.database.models.Baton;
import telraam.database.models.Detection;
import telraam.monitoring.models.BatonStatus;

import java.sql.Timestamp;
import java.time.Instant;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Optional;

public class BatonStatusHolder {
    // Map from batonMac to batonStatus
    private HashMap<String, BatonStatus> batonStatusMap = new HashMap<>();
    private HashMap<Integer, String> batonIdToMac = new HashMap<>();

    private BatonDAO batonDAO;
    private DetectionDAO detectionDAO;

    public BatonStatusHolder(BatonDAO BDAO, DetectionDAO DDAO) {
        batonDAO = BDAO;
        detectionDAO = DDAO;
    }

    private BatonStatus getStatusForBaton(String batonMac) {
        BatonStatus batonStatus = batonStatusMap.get(batonMac);
        if (batonStatus == null) {
            Optional<Baton> optionalBaton  = batonDAO.getByMac(batonMac);
            if (optionalBaton.isEmpty()) {
                return null;
            }
            Baton baton = optionalBaton.get();

            batonStatus = new BatonStatus(
                    baton.getMac().toLowerCase(),
                    baton.getId(),
                    baton.getName(),
                    0,
                    0,
                    false,
                    null,
                    -1
            );
            batonStatusMap.put(baton.getMac().toLowerCase(), batonStatus);
        }
        return batonStatus;

    }

    public List<BatonStatus> GetAllBatonStatuses() {
        // For each baton, fetch latest detection
        var batons = batonDAO.getAll();
        for (Baton baton : batons) {
            var batonStatus = GetBatonStatus(baton.getId());
            var detection = detectionDAO.latestDetectionByBatonId(baton.getId(), batonStatus.getLastSeen() == null ? Timestamp.from(Instant.ofEpochSecond(0)) : batonStatus.getLastSeen());
            detection.ifPresent(this::updateState);
        }
        return new ArrayList<>(batonStatusMap.values());
    }

    private void updateState(Detection msg) {
        BatonStatus batonStatus = GetBatonStatus(msg.getBatonId());
        if (batonStatus == null) {
            batonStatus = createBatonStatus(msg.getBatonId());
            batonStatusMap.put(batonStatus.getMac(), batonStatus);
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
        return getStatusForBaton(batonMac);
    }

    public BatonStatus createBatonStatus(Integer batonId) {
        String batonMac = batonIdToMac.get(batonId);
        if (batonMac != null) {
            return getStatusForBaton(batonMac);
        }
        var baton = batonDAO.getById(batonId);
        if (baton.isEmpty()) {
            // Non-existing baton
            return null;
        }
        BatonStatus batonStatus = new BatonStatus(
                baton.get().getMac().toLowerCase(),
                baton.get().getId(),
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

    public void resetRebooted(int batonId) {
        var batonStatus = GetBatonStatus(batonId);
        if (batonStatus == null) {
            return;
        }
        batonStatus.setRebooted(false);
    }
}
