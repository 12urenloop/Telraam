package telraam.logic;

import org.jdbi.v3.core.Jdbi;
import telraam.database.daos.BatonDAO;
import telraam.database.daos.LapDAO;
import telraam.database.daos.TeamDAO;
import telraam.database.models.*;

import java.sql.Timestamp;
import java.util.*;

public class DistanceLapper implements Lapper {
    private final LapDAO lapDAO;
    private final TeamDAO teamDAO;
    private final BatonDAO batonDAO;
    private final List<Baton> batons;
    private final Map<Integer, Deque<Detection>> lastDetected;
    private final Track track;
    private Jdbi jdbi;


    public DistanceLapper(Jdbi jdbi) {
        this.lapDAO = jdbi.onDemand(LapDAO.class);
        this.teamDAO = jdbi.onDemand(TeamDAO.class);
        this.batonDAO = jdbi.onDemand(BatonDAO.class);

        this.batons = batonDAO.getAll();
        this.track = new Track(jdbi);
        this.lastDetected = new HashMap<>();
        for (Baton baton : batons) {
            this.lastDetected.put(baton.getId(), new ArrayDeque<>());
        }

    }

    @Override
    public void handle(Detection msg) {
        int batonId = msg.getBatonId();
        Deque<Detection> detections = this.lastDetected.get(batonId);

        if (detections.isEmpty()) {
            detections.add(msg);
        } else {
            if (msg.getTimestamp().compareTo(
                    detections.getLast().getTimestamp()) > 0) {
                detections.add(msg);
                if (detections.size() > track.beaconCount()) {
                    detections.removeFirst();
                }
            }
        }
        calculateLap(detections);
    }

    public void calculateLap(Deque<Detection> detections) {
        if (detections.size() >= 2) {
            Detection last = detections.removeLast();

            if (track.getDistance(last.getBeaconId()) <=
                    track.getDistance(detections.getLast().getBeaconId())) {
                teamDAO.getByBatonId(last.getBatonId())
                        .ifPresent(t -> lapDAO.insert(new Lap(t.getId(),
                                new Timestamp(System.currentTimeMillis()))));
            }
            detections.add(last);
        }

    }
}
