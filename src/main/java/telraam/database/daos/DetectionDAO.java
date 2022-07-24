package telraam.database.daos;

import org.jdbi.v3.sqlobject.config.RegisterBeanMapper;
import org.jdbi.v3.sqlobject.customizer.Bind;
import org.jdbi.v3.sqlobject.customizer.BindBean;
import org.jdbi.v3.sqlobject.customizer.BindBeanList;
import org.jdbi.v3.sqlobject.statement.GetGeneratedKeys;
import org.jdbi.v3.sqlobject.statement.SqlBatch;
import org.jdbi.v3.sqlobject.statement.SqlQuery;
import org.jdbi.v3.sqlobject.statement.SqlUpdate;
import telraam.database.models.Detection;

import java.util.List;
import java.util.Optional;

public interface DetectionDAO extends DAO<Detection> {
    @SqlQuery("SELECT * FROM detection")
    @RegisterBeanMapper(Detection.class)
    List<Detection> getAll();

    @Override
    @SqlUpdate("""
            INSERT INTO detection (station_id, baton_id, timestamp, rssi, battery, remote_id, uptime_ms) \
            VALUES (:stationId, :batonId, :timestamp, :rssi, :battery, :remoteId, :uptimeMs)
            """)
    @GetGeneratedKeys({"id"})
    int insert(@BindBean Detection detection);

    @SqlBatch("""
            INSERT INTO detection (station_id, baton_id, timestamp, rssi, battery, remote_id, uptime_ms) \
            VALUES (:stationId, :batonId, :timestamp, :rssi, :battery, :remoteId, :uptimeMs)
            """)
    @GetGeneratedKeys({"id"})
    int insertAll(@BindBean List<Detection> detection);

    @SqlQuery("SELECT * FROM detection WHERE id = :id")
    @RegisterBeanMapper(Detection.class)
    Optional<Detection> getById(@Bind("id") int id);

    @Override
    @SqlUpdate("DELETE FROM detection WHERE id = :id")
    int deleteById(@Bind("id") int id);

    @Override
    @SqlUpdate("UPDATE detection SET " +
            "baton_id = :batonId, " +
            "station_id = :stationId, " +
            "timestamp = :timestamp WHERE id = :id")
    int update(@Bind("id") int id, @BindBean Detection modelObj);

    @SqlQuery("SELECT * FROM detection WHERE remote_id = (SELECT MAX(remote_id) FROM detection WHERE station_id = :stationId) AND station_id = :stationId LIMIT 1")
    @RegisterBeanMapper(Detection.class)
    Optional<Detection> latestDetectionByStationId(@Bind("stationId") int stationId);

    @SqlQuery("SELECT * FROM detection WHERE id > :id")
    @RegisterBeanMapper(Detection.class)
    List<Detection> getAllSinceId(@Bind("id") int id);
}
