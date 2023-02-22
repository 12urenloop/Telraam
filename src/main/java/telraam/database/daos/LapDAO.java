package telraam.database.daos;

import org.jdbi.v3.sqlobject.config.RegisterBeanMapper;
import org.jdbi.v3.sqlobject.customizer.Bind;
import org.jdbi.v3.sqlobject.customizer.BindBean;
import org.jdbi.v3.sqlobject.customizer.BindBeanList;
import org.jdbi.v3.sqlobject.statement.GetGeneratedKeys;
import org.jdbi.v3.sqlobject.statement.SqlBatch;
import org.jdbi.v3.sqlobject.statement.SqlQuery;
import org.jdbi.v3.sqlobject.statement.SqlUpdate;
import telraam.database.models.Lap;

import java.util.Iterator;
import java.util.List;
import java.util.Optional;

public interface LapDAO extends DAO<Lap> {
    @Override
    @SqlQuery("SELECT * FROM lap")
    @RegisterBeanMapper(Lap.class)
    List<Lap> getAll();

    @SqlQuery("SELECT * FROM lap WHERE lap_source_id = :lapSourceId")
    @RegisterBeanMapper(Lap.class)
    List<Lap> getAllBySource(@Bind("lapSourceId") Integer lapSourceId);

    @SqlUpdate("INSERT INTO lap (team_id, lap_source_id, timestamp) " +
            "VALUES (:teamId, :lapSourceId, :timestamp)")
    @GetGeneratedKeys({"id"})
    int insert(@BindBean Lap lap);

    @SqlQuery("SELECT * FROM lap where id = :id")
    @RegisterBeanMapper(Lap.class)
    Optional<Lap> getById(@Bind("id") int id);

    @Override
    @SqlUpdate("DELETE FROM lap WHERE id = :id")
    int deleteById(@Bind("id") int id);

    @Override
    @SqlUpdate("UPDATE lap SET " +
            "team_id = :teamId, " +
            "lap_source_id = :lapSourceId, " +
            "timestamp = :timestamp " +
            "WHERE id = :id")
    int update(@Bind("id") int id, @BindBean Lap modelObj);

    @SqlUpdate("DELETE FROM lap WHERE lap_source_id = :lapSourceId")
    void deleteByLapSourceId(@Bind("lapSourceId") int lapSourceId);

    @SqlBatch("INSERT INTO lap (team_id, lap_source_id, timestamp) VALUES (:teamId, :lapSourceId, :timestamp)")
    void insertAll(@BindBean Iterator<Lap> laps);
}
