package telraam.database.daos;

import org.jdbi.v3.sqlobject.config.RegisterBeanMapper;
import org.jdbi.v3.sqlobject.customizer.Bind;
import org.jdbi.v3.sqlobject.customizer.BindBean;
import org.jdbi.v3.sqlobject.statement.GetGeneratedKeys;
import org.jdbi.v3.sqlobject.statement.SqlQuery;
import org.jdbi.v3.sqlobject.statement.SqlUpdate;
import telraam.database.models.Lap;
import telraam.database.models.Id;

import java.util.List;
import java.util.Optional;

public interface LapDAO extends DAO<Lap> {
    @Override
    @SqlQuery("SELECT * FROM lap")
    @RegisterBeanMapper(Lap.class)
    List<Lap> getAll();


    @SqlUpdate("INSERT INTO lap (team_id, timestamp) " +
            "VALUES (:teamId, :timestamp)")
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
            "timestamp = :timestamp " +
            "WHERE id = :id")
    int update(@BindBean Lap modelObj);

    @SqlQuery("SELECT * FROM lap " +
            "WHERE team_id = :teamId " +
            "ORDER BY timestamp DESC ")
    @RegisterBeanMapper(Lap.class)
    List<Lap> getLapsForTeam(@Bind("teamId") int teamId);
}
