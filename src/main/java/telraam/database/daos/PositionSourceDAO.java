package telraam.database.daos;

import org.jdbi.v3.sqlobject.config.RegisterBeanMapper;
import org.jdbi.v3.sqlobject.customizer.Bind;
import org.jdbi.v3.sqlobject.customizer.BindBean;
import org.jdbi.v3.sqlobject.statement.GetGeneratedKeys;
import org.jdbi.v3.sqlobject.statement.SqlQuery;
import org.jdbi.v3.sqlobject.statement.SqlUpdate;
import telraam.database.models.PositionSource;

import java.util.List;
import java.util.Optional;

public interface PositionSourceDAO extends DAO<PositionSource> {
    @Override
    @SqlQuery("SELECT * FROM position_source")
    @RegisterBeanMapper(PositionSource.class)
    List<PositionSource> getAll();

    @SqlUpdate("INSERT INTO position_source (name) VALUES (:name)")
    @GetGeneratedKeys({"id"})
    int insert(@BindBean PositionSource positionSource);

    @SqlQuery("SELECT * FROM position_source WHERE name = :name")
    @RegisterBeanMapper(PositionSource.class)
    Optional<PositionSource> getByName(@Bind("name") String name);
}
