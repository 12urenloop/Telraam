package telraam.database.daos;

import org.jdbi.v3.sqlobject.config.RegisterBeanMapper;
import org.jdbi.v3.sqlobject.customizer.Bind;
import org.jdbi.v3.sqlobject.customizer.BindBean;
import org.jdbi.v3.sqlobject.statement.GetGeneratedKeys;
import org.jdbi.v3.sqlobject.statement.SqlQuery;
import org.jdbi.v3.sqlobject.statement.SqlUpdate;
import telraam.database.models.Beacon;
import telraam.database.models.Id;

import java.util.List;
import java.util.Optional;

public interface BeaconDAO {

    @SqlQuery("SELECT * FROM beacon")
    @RegisterBeanMapper(Beacon.class)
    List<Beacon> getAll();

    @SqlUpdate("INSERT INTO beacon (name) VALUES (:name)")
    @GetGeneratedKeys({"id"})
    @RegisterBeanMapper(Id.class)
    Id insert(@BindBean Beacon beacon);

    @SqlQuery("select * from baton where id = :id")
    @RegisterBeanMapper(Beacon.class)
    Optional<Beacon> getById(@Bind("id") int id);

    //@SqlQuery("")
    int deleteById(@Bind("id") int id);
}
