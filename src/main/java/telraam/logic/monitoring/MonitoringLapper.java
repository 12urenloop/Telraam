package telraam.logic.monitoring;

import io.dropwizard.jersey.setup.JerseyEnvironment;
import org.jdbi.v3.core.Jdbi;
import telraam.database.daos.BatonDAO;
import telraam.database.daos.DetectionDAO;
import telraam.database.models.Detection;
import telraam.logic.Lapper;

import java.util.List;

// We can reuse the lapper logic built-in to the fetcher to get the statuses of our batons
public class MonitoringLapper implements Lapper {
    private final Jdbi jdbi;
    private final StatusHolder statusHolder;

    public MonitoringLapper(Jdbi jdbi) {
        this.jdbi = jdbi;
        this.statusHolder = new StatusHolder(jdbi.onDemand(BatonDAO.class));
    }

    @Override
    public void handle(Detection msg) {
        BatonStatus batonStatus = this.statusHolder.GetBatonStatus(msg.getBatonId());
        if (batonStatus == null) {
            batonStatus = this.statusHolder.createBatonStatus(msg.getBatonId());
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

    @Override
    public void registerAPI(JerseyEnvironment jersey) {
        jersey.register(new MonitoringResource(this));
    }

    public List<BatonStatus> getBatonStatuses() {
        return this.statusHolder.GetAllBatonStatuses();
    }
}
