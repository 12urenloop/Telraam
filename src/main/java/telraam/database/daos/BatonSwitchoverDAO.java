package telraam.database.daos;

import org.jdbi.v3.sqlobject.config.RegisterBeanMapper;
import org.jdbi.v3.sqlobject.customizer.Bind;
import org.jdbi.v3.sqlobject.customizer.BindBean;
import org.jdbi.v3.sqlobject.statement.GetGeneratedKeys;
import org.jdbi.v3.sqlobject.statement.SqlQuery;
import org.jdbi.v3.sqlobject.statement.SqlUpdate;
import telraam.database.models.BatonSwitchover;
import telraam.database.models.Detection;

import java.util.List;
import java.util.Optional;

public interface BatonSwitchoverDAO extends DAO<BatonSwitchover> {

    @Override
    @SqlQuery("SELECT * FROM batonswitchover ORDER BY timestamp")
    @RegisterBeanMapper(BatonSwitchover.class)
    List<BatonSwitchover> getAll();

    @Override
    @SqlUpdate("INSERT INTO batonswitchover (teamId, previousBatonId, newBatonId, timestamp) VALUES (:teamId, :previousBatonId, :newBatonId, :timestamp)")
    @GetGeneratedKeys({"id"})
    int insert(@BindBean BatonSwitchover batonSwitchover);

    @Override
    @SqlQuery("SELECT * FROM batonswitchover WHERE id = :id")
    @RegisterBeanMapper(BatonSwitchover.class)
    Optional<BatonSwitchover> getById(@Bind("id") int id);

    @Override
    @SqlUpdate("DELETE FROM batonswitchover WHERE id = :id")
    @RegisterBeanMapper(BatonSwitchover.class)
    int deleteById(@Bind("id") int id);

    @Override
    @SqlUpdate("UPDATE batonswitchover SET teamId = :teamId, previousBatonId = :previousBatonId, newBatonId = :newBatonId, timestamp = :timestamp WHERE id = :id")
    int update(@Bind("id") int id, @BindBean BatonSwitchover batonSwitchover);

    @SqlQuery("SELECT * FROM batonswitchover WHERE id > :id")
    @RegisterBeanMapper(BatonSwitchover.class)
    List<BatonSwitchover> getAllSinceId(@Bind("id") int id);
}
