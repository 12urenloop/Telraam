package telraam.database.daos;

import org.jdbi.v3.sqlobject.config.RegisterBeanMapper;
import org.jdbi.v3.sqlobject.customizer.Bind;
import org.jdbi.v3.sqlobject.customizer.BindBean;
import org.jdbi.v3.sqlobject.statement.GetGeneratedKeys;
import org.jdbi.v3.sqlobject.statement.SqlQuery;
import org.jdbi.v3.sqlobject.statement.SqlUpdate;
import telraam.database.models.LapSourceSwitchover;

import java.util.List;
import java.util.Optional;

public interface LapSourceSwitchoverDAO extends DAO<LapSourceSwitchover> {

    @Override
    @SqlQuery("SELECT * FROM lapsourceswitchover")
    @RegisterBeanMapper(LapSourceSwitchover.class)
    List<LapSourceSwitchover> getAll();

    @SqlQuery("SELECT * FROM lapsourceswitchover ORDER BY timestamp")
    @RegisterBeanMapper(LapSourceSwitchover.class)
    List<LapSourceSwitchover> getAllOrderByTimestamp();

    @Override
    @SqlUpdate("INSERT INTO lapsourceswitchover (newLapSource, timestamp) VALUES (:newLapSource, :timestamp)")
    @GetGeneratedKeys({"id"})
    int insert(@BindBean LapSourceSwitchover lapSourceSwitchover);

    @Override
    @SqlQuery("SELECT * FROM lapsourceswitchover WHERE id = :id")
    @RegisterBeanMapper(LapSourceSwitchover.class)
    Optional<LapSourceSwitchover> getById(@Bind("id") int id);

    @Override
    @SqlUpdate("DELETE FROM lapsourceswitchover WHERE id = :id")
    @RegisterBeanMapper(LapSourceSwitchover.class)
    int deleteById(@Bind("id") int id);

    @Override
    @SqlUpdate("UPDATE lapsourceswitchover SET newLapSource=:newLapSource, timestamp=:timestamp WHERE id = :id")
    int update(@Bind("id") int id, @BindBean LapSourceSwitchover lapSourceSwitchover);
}
