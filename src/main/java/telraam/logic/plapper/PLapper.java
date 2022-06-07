package telraam.logic.plapper;

import io.dropwizard.jersey.setup.JerseyEnvironment;
import org.jdbi.v3.core.Jdbi;
import org.jpl7.Atom;
import org.jpl7.JRef;
import org.jpl7.Query;
import telraam.database.daos.DetectionDAO;
import telraam.database.models.Detection;
import telraam.logic.Lapper;

import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

public class PLapper implements Lapper {

    private final Thread runner = new Thread(this::run);
    private final Logger logger = Logger.getLogger(PLapper.class.getName());
    private Jdbi jdbi;

    private List<Detection> detections;

    public PLapper(Jdbi jdbi) {
        this.jdbi = jdbi;
        new Query("use_module", new Atom("src/prolog/main")).hasSolution();
        new Query("init", new JRef(logger)).hasSolution();
        runner.start();
    }

    private void run() {
        DetectionDAO detectionDAO = jdbi.onDemand(DetectionDAO.class);

        if (detections == null || detections.size() == 0) {
            detections = detectionDAO.getAll();
        } else {
            detections.addAll(detectionDAO.getAll());
        }


        List<Detection> detections = detectionDAO.getAll();

        long start = System.nanoTime();


        List<String> detection_terms = detections.stream()
                .map(PSerializer::serialize)
                .toList();

        new Query("assert_detections", new Atom(PSerializer.serialize(detection_terms))).hasSolution();

        logger.log(Level.INFO, "Took: " + (System.nanoTime() - start) / 1_000_000_000.0 + "s");
    }

    @Override
    public void handle(Detection msg) {
        if (!runner.isAlive()) {
            runner.start();
        }
    }

    @Override
    public void registerAPI(JerseyEnvironment jersey) {
    }
}