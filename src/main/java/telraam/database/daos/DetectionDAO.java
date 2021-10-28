package telraam.database.daos;

import org.jdbi.v3.sqlobject.config.RegisterBeanMapper;
import org.jdbi.v3.sqlobject.customizer.Bind;
import org.jdbi.v3.sqlobject.customizer.BindBean;
import org.jdbi.v3.sqlobject.statement.GetGeneratedKeys;
import org.jdbi.v3.sqlobject.statement.SqlQuery;
import org.jdbi.v3.sqlobject.statement.SqlUpdate;
import telraam.database.models.Detection;
import telraam.database.models.LastBeaconDetection;

import java.util.List;
import java.util.Optional;

public interface DetectionDAO extends DAO<Detection> {
    @SqlQuery("SELECT * FROM detection")
    @RegisterBeanMapper(Detection.class)
    List<Detection> getAll();

    @Override
    @SqlUpdate("INSERT INTO detection (beacon_id, baton_id, timestamp) " +
            "VALUES (:beaconId, :batonId, :timestamp)")
    @GetGeneratedKeys({"id"})
    int insert(@BindBean Detection detection);

    @SqlQuery("SELECT * FROM detection WHERE id = :id")
    @RegisterBeanMapper(Detection.class)
    Optional<Detection> getById(@Bind("id") int id);

    @Override
    @SqlUpdate("DELETE FROM detection WHERE id = :id")
    int deleteById(@Bind("id") int id);

    @Override
    @SqlUpdate("UPDATE detection SET " +
            "baton_id = :batonId, " +
            "beacon_id = :beaconId, " +
            "timestamp = :timestamp")
    int update(@BindBean Detection modelObj);

    @SqlQuery("select max(timestamp) as timestamp, beacon_id from detection group by beacon_id")
    @RegisterBeanMapper(LastBeaconDetection.class)
    List<LastBeaconDetection> lastBeaconDetections();
}
