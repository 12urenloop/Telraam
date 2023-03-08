package telraam.database.daos;

import org.jdbi.v3.sqlobject.config.RegisterBeanMapper;
import org.jdbi.v3.sqlobject.customizer.Bind;
import org.jdbi.v3.sqlobject.customizer.BindBean;
import org.jdbi.v3.sqlobject.statement.GetGeneratedKeys;
import org.jdbi.v3.sqlobject.statement.SqlBatch;
import org.jdbi.v3.sqlobject.statement.SqlQuery;
import org.jdbi.v3.sqlobject.statement.SqlUpdate;
import telraam.database.models.Detection;

import java.sql.Timestamp;
import java.util.List;
import java.util.Optional;

public interface DetectionDAO extends DAO<Detection> {
    @SqlQuery("SELECT * FROM detection")
    @RegisterBeanMapper(Detection.class)
    List<Detection> getAll();

    @Override
    @SqlUpdate("""
            INSERT INTO detection (station_id, baton_id, timestamp, rssi, battery, remote_id, uptime_ms, timestamp_ingestion) \
            VALUES (:stationId, :batonId, :timestamp, :rssi, :battery, :remoteId, :uptimeMs, :timestampIngestion)
            """)
    @GetGeneratedKeys({"id"})
    int insert(@BindBean Detection detection);

    @SqlBatch("""
            INSERT INTO detection (station_id, baton_id, timestamp, rssi, battery, remote_id, uptime_ms, timestamp_ingestion) \
            VALUES (:stationId, :batonId, :timestamp, :rssi, :battery, :remoteId, :uptimeMs, :timestampIngestion)
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

    @SqlQuery("SELECT * FROM detection WHERE id > :id ORDER BY id LIMIT :limit")
    @RegisterBeanMapper(Detection.class)
    List<Detection> getSinceId(@Bind("id") int id, @Bind("limit") int limit);


    @SqlQuery("SELECT * FROM detection WHERE baton_id = :batonId AND timestamp > :timestamp ORDER BY timestamp DESC LIMIT 1")
    @RegisterBeanMapper(Detection.class)
    Optional<Detection> latestDetectionByBatonId(@Bind("batonId") int batonId, @Bind("timestamp") Timestamp timestamp);

    @SqlQuery("SELECT * FROM detection WHERE station_id = :stationId AND timestamp > :timestamp ORDER BY timestamp DESC LIMIT 1")
    @RegisterBeanMapper(Detection.class)
    Optional<Detection> latestDetectionByStationId(@Bind("stationId") int stationId, @Bind("timestamp") Timestamp timestamp);


    @SqlQuery("""
            WITH bso AS (SELECT teamid, newbatonid, timestamp AS current_timestamp, LEAD(timestamp) OVER (PARTITION BY teamid ORDER BY timestamp) next_baton_switch FROM batonswitchover)
            SELECT baton_id, station_id, rssi, timestamp, teamid FROM detection d LEFT JOIN bso ON d.baton_id = bso.newbatonid AND d.timestamp BETWEEN bso.current_timestamp AND bso.next_baton_switch WHERE rssi > :minRssi
            """)
    @RegisterBeanMapper(Detection.class)
    List<Detection> getAllWithTeamId(@Bind("minRssi") int minRssi);
}
