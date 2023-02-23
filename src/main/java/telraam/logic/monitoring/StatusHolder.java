package telraam.logic.monitoring;

import telraam.database.daos.BatonDAO;
import telraam.database.models.Baton;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class StatusHolder {
    // Map from batonMac to batonStatus
    private HashMap<String, BatonStatus> batonStatusMap = new HashMap<>();
    private HashMap<Integer, String> batonIdToMac = new HashMap<>();

    private BatonDAO batonDAO;

    public StatusHolder(BatonDAO BDAO) {
        batonDAO = BDAO;
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
        return new ArrayList<>(batonStatusMap.values());
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
