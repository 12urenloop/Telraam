package telraam.logic;

import org.jdbi.v3.core.Jdbi;
import telraam.database.daos.BeaconDAO;
import telraam.database.models.Beacon;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Track {
    private final BeaconDAO beaconDAO;
    private final List<Beacon> beacons;
    private final Map<Integer, Beacon> beaconMap;

    public Track(Jdbi jdbi) {
        this.beaconDAO = jdbi.onDemand(BeaconDAO.class);
        this.beacons = beaconDAO.getAll();
        beaconMap = new HashMap<>();
        for (Beacon beacon : beacons) {
            beaconMap.put(beacon.getId(), beacon);
        }
    }

    public Integer getDistance(Integer beaconId) {
        return beaconMap.get(beaconId).getDistance();
    }

    public Beacon getBeacon(Integer beaconId) {
        return beaconMap.get(beaconId);
    }

    public int beaconCount() {
        return beaconMap.size();
    }

}
