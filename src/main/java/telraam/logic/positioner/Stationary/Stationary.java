package telraam.logic.positioner.Stationary;

import org.jdbi.v3.core.Jdbi;
import telraam.database.daos.PositionSourceDAO;
import telraam.database.daos.TeamDAO;
import telraam.database.models.Detection;
import telraam.database.models.PositionSource;
import telraam.database.models.Team;
import telraam.logic.positioner.Position;
import telraam.logic.positioner.PositionSender;
import telraam.logic.positioner.Positioner;
import telraam.logic.positioner.nostradamus.Nostradamus;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

public class Stationary implements Positioner {
    private static final Logger logger = Logger.getLogger(Stationary.class.getName());
    private final String SOURCE_NAME = "stationary";
    private final int INTERVAL_UPDATE_MS = 60000;
    private final Jdbi jdbi;
    private final PositionSender positionSender;
    public Stationary(Jdbi jdbi) {
        this.jdbi = jdbi;
        this.positionSender = new PositionSender(SOURCE_NAME);

        // Add as source
        PositionSourceDAO positionSourceDAO = jdbi.onDemand(PositionSourceDAO.class);
        if (positionSourceDAO.getByName(SOURCE_NAME).isEmpty()) {
            positionSourceDAO.insert(new PositionSource(SOURCE_NAME));
        }

        new Thread(this::update).start();
    }

    private void update() {
        // Keep sending updates in case Loxsi ever restarts
        while (true) {
            long timestamp = System.currentTimeMillis();
            List<Team> teams = jdbi.onDemand(TeamDAO.class).getAll();

            List<Position> positions = teams.stream().map(t -> new Position(t.getId(), 0, 0, timestamp)).toList();
            positionSender.send(positions);

            try {
                Thread.sleep(INTERVAL_UPDATE_MS);
            } catch (InterruptedException e) {
                logger.severe(e.getMessage());
            }
        }
    }

    @Override
    public void handle(Detection detection) {}
}
