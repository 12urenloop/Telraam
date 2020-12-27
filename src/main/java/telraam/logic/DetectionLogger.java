package telraam.logic;

import java.util.Optional;
import java.util.logging.Logger;

import org.jdbi.v3.core.Jdbi;

import telraam.beacon.BeaconMessage;
import telraam.beacon.Callback;
import telraam.database.daos.BatonDAO;
import telraam.database.daos.BeaconDAO;
import telraam.database.daos.DetectionDAO;
import telraam.database.models.Detection;

public class DetectionLogger implements Callback<Void, BeaconMessage> {
    private static Logger logger = Logger.getLogger(DetectionLogger.class.getName());

    private BatonDAO batonDAO;
    private BeaconDAO beaconDAO;
    private DetectionDAO detectionDAO;

    public DetectionLogger(Jdbi database) {
        this.batonDAO = database.onDemand(BatonDAO.class);
        this.beaconDAO = database.onDemand(BeaconDAO.class);
        this.detectionDAO = database.onDemand(DetectionDAO.class);
    }

    public Void handle(BeaconMessage message) {
        Optional<Integer> batton = batonDAO.getIdByMac(message.battonMAC);
        Optional<Integer> beacon = beaconDAO.getIdByMac(message.stationMAC);
        if (batton.isEmpty() || beacon.isEmpty()) {
            // Warn
            logger.warning("Unknown mac addresses in message "+message);
            return null;
        }

        Detection det = new Detection(batton.get(), beacon.get(), message.time);
        detectionDAO.insert(det);


        logger.info("New detection saved to database " + det);
        return null;
    }
}