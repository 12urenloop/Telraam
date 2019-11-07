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

public interface LapDAO {
    // @SqlQuery("select * from baton")
    // @RegisterBeanMapper(Baton.class)
    List<Lap> getAll();

    // @SqlUpdate("insert into baton (name) values (:name)")
    // @GetGeneratedKeys({"id"})
    // @RegisterBeanMapper(Id.class)
    Id insert(@BindBean Lap lap);

    // @SqlQuery("select * from baton where id = :id")
    // @RegisterBeanMapper(Baton.class)
    Optional<Lap> getById(@Bind("id") int id);
}
