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
import java.util.Comparator;
import java.util.LinkedList;
import java.util.List;

public class ExternalLapper implements Lapper {

    static final String SOURCE_NAME = "external-lapper";
    private final LapDAO lapDAO;
    private int lapSourceId;

    public ExternalLapper(Jdbi jdbi) {
        this.lapDAO = jdbi.onDemand(LapDAO.class);

        // Get the lapSourceId, create the source if needed
        LapSourceDAO lapSourceDAO = jdbi.onDemand(LapSourceDAO.class);
        lapSourceDAO.getByName(SOURCE_NAME).ifPresentOrElse(
                lapSource -> this.lapSourceId = lapSource.getId(),
                () -> this.lapSourceId = lapSourceDAO.insert(new LapSource(SOURCE_NAME))
        );
    }

    @Override
    public void handle(Detection msg) {
        // Do nothing here. The external lappers polls periodically using the general api.
    }

    public void saveLaps(List<ExternalLapperTeamLaps> teamLaps) {
        List<Lap> laps = lapDAO.getAllBySource(lapSourceId);

        // Remember laps we have to take actions on
        List<Lap> lapsToDelete = new LinkedList<>();
        List<Lap> lapsToAdd = new LinkedList<>();

        // Find which laps are no longer needed or have to be added
        for (ExternalLapperTeamLaps teamLap : teamLaps) {
            List<Lap> lapsForTeam = laps.stream().filter(l -> l.getTeamId() == teamLap.teamId).sorted(Comparator.comparing(Lap::getTimestamp)).toList();
            List<Lap> newLapsForTeam = teamLap.laps.stream().map(nl -> new Lap(teamLap.teamId, lapSourceId, new Timestamp((long) (nl.timestamp)))).sorted(Comparator.comparing(Lap::getTimestamp)).toList();

            int lapsIndex = 0;
            int newLapsIndex = 0;
            while (lapsIndex != lapsForTeam.size() || newLapsIndex != newLapsForTeam.size()) {
                if (lapsIndex != lapsForTeam.size() && newLapsIndex != newLapsForTeam.size()) {
                    Lap lap = lapsForTeam.get(lapsIndex);
                    Lap newLap = newLapsForTeam.get(newLapsIndex);
                    if (lap.getTimestamp().before(newLap.getTimestamp())) {
                        lapsToDelete.add(lap);
                        lapsIndex++;
                    } else if (lap.getTimestamp().after(newLap.getTimestamp())) {
                        lapsToAdd.add(newLap);
                        newLapsIndex++;
                    } else { // Lap is present in both lists. Keep it.
                        lapsIndex++;
                        newLapsIndex++;
                    }
                } else if (lapsIndex != lapsForTeam.size()) {
                    lapsToDelete.add(lapsForTeam.get(lapsIndex));
                    lapsIndex++;
                } else {
                    lapsToAdd.add(newLapsForTeam.get(newLapsIndex));
                    newLapsIndex++;
                }
            }
        }

        // Do the required actions
        lapDAO.deleteAllById(lapsToDelete.iterator());
        lapDAO.insertAll(lapsToAdd.iterator());
    }

    @Override
    public void registerAPI(JerseyEnvironment jersey) {
        jersey.register(new ExternalLapperResource(this));
    }
}
