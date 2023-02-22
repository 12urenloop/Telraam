package telraam.logic.external;

import io.dropwizard.jersey.setup.JerseyEnvironment;
import org.jdbi.v3.core.Jdbi;
import telraam.database.daos.LapDAO;
import telraam.database.daos.LapSourceDAO;
import telraam.database.models.Detection;
import telraam.database.models.Lap;
import telraam.database.models.LapSource;
import telraam.logic.Lapper;

import java.sql.Timestamp;
import java.time.Instant;
import java.util.LinkedList;
import java.util.List;

public class ExternalLapper implements Lapper {

    static final String SOURCE_NAME = "external-lapper";
    private final LapDAO lapDAO;
    private int lapSourceId;
    public ExternalLapper(Jdbi jdbi) {
        this.lapDAO = jdbi.onDemand(LapDAO.class);

        LapSourceDAO lapSourceDAO = jdbi.onDemand(LapSourceDAO.class);
        lapSourceDAO.getByName(SOURCE_NAME).ifPresentOrElse(
                lapSource -> this.lapSourceId = lapSource.getId(),
                () -> this.lapSourceId = lapSourceDAO.insert(new LapSource(SOURCE_NAME))
        );
    }

    @Override
    public void handle(Detection msg) {}

    public void saveLaps(List<ExternalLapperTeamLaps> teamLaps) {
        lapDAO.deleteByLapSourceId(this.lapSourceId);

        LinkedList<Lap> laps = new LinkedList<>();

        for (ExternalLapperTeamLaps teamLap: teamLaps) {
            for (ExternalLapperLap lap: teamLap.laps) {
                laps.add(new Lap(teamLap.teamId, this.lapSourceId, new Timestamp((long) (lap.timestamp))));
            }
        }

        lapDAO.insertAll(laps.iterator());
    }

    @Override
    public void registerAPI(JerseyEnvironment jersey) {
        jersey.register(new ExternalLapperResource(this));
    }
}
