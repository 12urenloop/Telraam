package telraam.database.daos;

import org.jdbi.v3.sqlobject.config.RegisterBeanMapper;
import org.jdbi.v3.sqlobject.customizer.Bind;
import org.jdbi.v3.sqlobject.customizer.BindBean;
import org.jdbi.v3.sqlobject.statement.GetGeneratedKeys;
import org.jdbi.v3.sqlobject.statement.SqlQuery;
import org.jdbi.v3.sqlobject.statement.SqlUpdate;
import telraam.database.models.Detection;
import telraam.database.models.Id;
import java.util.List;
import java.util.Optional;

public interface DetectionDAO {
    // @SqlQuery("select * from baton")
    // @RegisterBeanMapper(Baton.class)
    List<Detection> getAll();

    // @SqlUpdate("insert into baton (name) values (:name)")
    // @GetGeneratedKeys({"id"})
    // @RegisterBeanMapper(Id.class)
    Id insert(@BindBean Detection detection);

    // @SqlQuery("select * from baton where id = :id")
    // @RegisterBeanMapper(Baton.class)
    Optional<Detection> getById(@Bind("id") int id);
}
