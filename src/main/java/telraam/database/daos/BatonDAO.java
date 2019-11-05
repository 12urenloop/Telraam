package telraam.database.daos;

import org.jdbi.v3.sqlobject.config.RegisterBeanMapper;
import org.jdbi.v3.sqlobject.customizer.Bind;
import org.jdbi.v3.sqlobject.customizer.BindBean;
import org.jdbi.v3.sqlobject.statement.GetGeneratedKeys;
import org.jdbi.v3.sqlobject.statement.SqlQuery;
import org.jdbi.v3.sqlobject.statement.SqlUpdate;
import telraam.database.models.Baton;
import telraam.database.models.Id;

import java.util.List;
import java.util.Optional;

public interface BatonDAO {

    @SqlQuery("select * from batons")
    List<Baton> listBatons();

    @SqlUpdate("insert into baton (name) values (:name)")
    @GetGeneratedKeys({"id"})
    @RegisterBeanMapper(Id.class)
    Id insert(@BindBean Baton baton);

    @SqlQuery("select * from baton where id = :id")
    Optional<Baton> findBatonById(@Bind("id") int id);
}