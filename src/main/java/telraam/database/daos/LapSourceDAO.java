package telraam.database.daos;

import org.jdbi.v3.sqlobject.config.RegisterBeanMapper;
import org.jdbi.v3.sqlobject.customizer.Bind;
import org.jdbi.v3.sqlobject.customizer.BindBean;
import org.jdbi.v3.sqlobject.statement.GetGeneratedKeys;
import org.jdbi.v3.sqlobject.statement.SqlQuery;
import org.jdbi.v3.sqlobject.statement.SqlUpdate;
import telraam.database.models.LapSource;

import java.util.List;
import java.util.Optional;

public interface LapSourceDAO extends DAO<LapSource> {

    @Override
    @SqlQuery("SELECT * FROM lap_source")
    @RegisterBeanMapper(LapSource.class)
    List<LapSource> getAll();


    @SqlUpdate("INSERT INTO lap_source (name) VALUES (:name)")
    @GetGeneratedKeys({"id"})
    int insert(@BindBean LapSource lapSource);

    @SqlQuery("SELECT * FROM lap_source where id = :id")
    @RegisterBeanMapper(LapSource.class)
    Optional<LapSource> getById(@Bind("id") int id);

    @SqlQuery("SELECT * FROM lap_source where name = :name")
    @RegisterBeanMapper(LapSource.class)
    Optional<LapSource> getByName(@Bind("name") String name);

    @Override
    @SqlUpdate("DELETE FROM lap_source WHERE id = :id")
    int deleteById(@Bind("id") int id);

    @Override
    @SqlUpdate("UPDATE lap_source SET name = :name WHERE id = :id")
    int update(@BindBean LapSource modelObj);

    @SqlQuery("SELECT COUNT(id) FROM lap_source")
    long count();
}
