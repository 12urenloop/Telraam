package telraam.database.daos;

import org.jdbi.v3.sqlobject.config.RegisterBeanMapper;
import org.jdbi.v3.sqlobject.customizer.Bind;
import org.jdbi.v3.sqlobject.customizer.BindBean;
import org.jdbi.v3.sqlobject.statement.GetGeneratedKeys;
import org.jdbi.v3.sqlobject.statement.SqlQuery;
import org.jdbi.v3.sqlobject.statement.SqlUpdate;
import telraam.database.models.Baton;
import telraam.database.models.Beacon;
import telraam.database.models.Id;

import java.util.List;
import java.util.Optional;

public interface BeaconDAO extends DAO<Beacon>{

    @Override
    @SqlQuery("SELECT * FROM beacon")
    @RegisterBeanMapper(Beacon.class)
    List<Beacon> getAll();

    @Override
    @SqlUpdate("INSERT INTO beacon (name) VALUES (:name)")
    @GetGeneratedKeys({"id"})
    int insert(@BindBean Beacon beacon);

    @Override
    @SqlQuery("SELECT * FROM beacon WHERE id = :id")
    @RegisterBeanMapper(Beacon.class)
    Optional<Beacon> getById(@Bind("id") int id);

    @Override
    @SqlUpdate("DELETE FROM beacon WHERE id = :id")
    int deleteById(@Bind("id") int id);

    @Override
    @SqlUpdate("UPDATE beacon SET name = :name WHERE id = :id")
    int update(@BindBean Baton baton);

}
