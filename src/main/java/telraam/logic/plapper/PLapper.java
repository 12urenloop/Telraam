package telraam.logic.plapper;

import io.dropwizard.jersey.setup.JerseyEnvironment;
import org.jdbi.v3.core.Jdbi;
import org.jpl7.Atom;
import org.jpl7.JRef;
import org.jpl7.Query;
import telraam.database.daos.BatonSwitchoverDAO;
import telraam.database.daos.DetectionDAO;
import telraam.database.models.BatonSwitchover;
import telraam.database.models.Detection;
import telraam.logic.Lapper;

import java.lang.reflect.Field;
import java.sql.Timestamp;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

public class PLapper implements Lapper {

    private final Thread runner = new Thread(this::run);

    /* Public so prolog can access this easily */
    public final Logger logger = Logger.getLogger(PLapper.class.getName());

    private final DetectionDAO detectionDAO;
    private final BatonSwitchoverDAO batonSwitchoverDAO;
    private final Jdbi jdbi;

    private List<Detection> detections;
    private List<BatonSwitchover> batonSwitchovers;

    public PLapper(Jdbi jdbi) {
        this.jdbi = jdbi;
        this.detectionDAO = jdbi.onDemand(DetectionDAO.class);
        this.batonSwitchoverDAO = jdbi.onDemand(BatonSwitchoverDAO.class);

        new Query("use_module", new Atom("src/prolog/main")).hasSolution();
        new Query("init", new JRef(this)).hasSolution();
        runner.start();
    }

    private void run() {
        /* Update detection list */
        if (detections == null || detections.size() == 0) {
            detections = detectionDAO.getAll();
        } else {
            detections.addAll(detectionDAO.getAllSinceId(detections.get(detections.size() - 1).getId()));
        }
        /* Update batonSwitchover list */
        if (batonSwitchovers == null || batonSwitchovers.size() == 0) {
            batonSwitchovers = batonSwitchoverDAO.getAll();
        } else {
            batonSwitchovers.addAll(batonSwitchoverDAO.getAllSinceId(batonSwitchovers.get(batonSwitchovers.size() - 1).getId()));
        }


        long start = System.nanoTime();

        new Query("run").hasSolution();

        logger.log(Level.INFO, "Took: " + (System.nanoTime() - start) / 1_000_000_000.0 + "s");
    }

    public String getBatonSwitchovers() {
        return getList(batonSwitchovers, this::buildObject);
    }

    public String getDetections() {
        return getList(detections, this::buildObject);
    }

    private void buildObject(StringBuilder builder, Object object) {
        Class<?> clazz = object.getClass();

        builder.append(clazz.getSimpleName().toLowerCase()).append('{');

        Field[] fields = clazz.getDeclaredFields();
        Field lastField = fields[fields.length - 1];

        for (Field field : fields) {

            builder.append(field.getName()).append(':');

            builder.append("(");

            if (field.canAccess(object)) {
                buildField(builder, field, object);
            } else {
                field.setAccessible(true);
                buildField(builder, field, object);
                field.setAccessible(false);
            }

            builder.append(')');

            if (field != lastField) {
                builder.append(',');
            }
        }

        builder.append('}');
    }

    private void buildField(StringBuilder builder, Field field, Object object) {
        try {
            if (field.getType() == Timestamp.class) {
                builder.append(((Timestamp) field.get(object)).getTime());
            } else {
                builder.append(field.get(object));
            }
        } catch (IllegalAccessException e) {
            e.printStackTrace();
        }
    }

    private interface BuildFunction<T> {
        public void apply(StringBuilder builder, T t);
    }

    private <T> String getList(List<T> list, BuildFunction<T> buildFunction) {
        if (list.isEmpty()) {
            return "[]";
        }

        StringBuilder builder = new StringBuilder();

        builder.append('[');

        int index = 0;
        while (index + 1 < list.size()) {
            buildFunction.apply(builder, list.get(index));
            builder.append(',');
            index++;
        }
        buildFunction.apply(builder, list.get(index));

        builder.append(']');

        return builder.toString();
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
