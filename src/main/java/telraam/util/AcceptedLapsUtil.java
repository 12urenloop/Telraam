package telraam.util;

import org.jdbi.v3.core.Jdbi;
import telraam.database.daos.LapDAO;
import telraam.database.daos.LapSourceSwitchoverDAO;
import telraam.database.models.Lap;
import telraam.database.models.LapSourceSwitchover;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

public class AcceptedLapsUtil {
    LapDAO lapDAO;
    LapSourceSwitchoverDAO lapSourceSwitchoverDAO;

    private static AcceptedLapsUtil instance;

    public AcceptedLapsUtil(LapDAO lapDAO, LapSourceSwitchoverDAO lapSourceSwitchoverDAO) {
        this.lapDAO = lapDAO;
        this.lapSourceSwitchoverDAO = lapSourceSwitchoverDAO;
    }

    public static void createInstance(Jdbi jdbi) {
        instance = new AcceptedLapsUtil(
                jdbi.onDemand(LapDAO.class),
                jdbi.onDemand(LapSourceSwitchoverDAO.class)
        );
    }

    public static AcceptedLapsUtil getInstance() {
        if (instance == null) {
            throw new RuntimeException("AcceptedLapsUtil is not initialized");
        }
        return instance;
    }

    public List<Lap> getAcceptedLaps() {
        List<Lap> laps = this.lapDAO.getAll();
        // TODO: this should be done in SQL
        laps.sort(Comparator.comparing(Lap::getTimestamp));

        List<LapSourceSwitchover> lapSourceSwitchovers = this.lapSourceSwitchoverDAO.getAll();
        lapSourceSwitchovers.sort(Comparator.comparing(LapSourceSwitchover::getTimestamp));

        List<Lap> ret = new ArrayList<>();

        int lapSourceSwitchoverIndex = 0;
        int currentLapSource = -1;

        for (Lap lap : laps) {
            while (lapSourceSwitchoverIndex < lapSourceSwitchovers.size() && lapSourceSwitchovers.get(lapSourceSwitchoverIndex).getTimestamp().before(lap.getTimestamp())) {
                currentLapSource = lapSourceSwitchovers.get(lapSourceSwitchoverIndex).getNewLapSource();
                // System.out.println("New lap source: " + currentLapSource);
                lapSourceSwitchoverIndex++;
            }

            if (lap.getLapSourceId() == currentLapSource) {
                ret.add(lap);
            }
        }

        return ret;
    }
}
