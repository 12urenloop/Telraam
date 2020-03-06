package telraam.database.daos;

import org.jdbi.v3.sqlobject.config.RegisterBeanMapper;
import org.jdbi.v3.sqlobject.customizer.Bind;
import org.jdbi.v3.sqlobject.customizer.BindBean;
import org.jdbi.v3.sqlobject.statement.GetGeneratedKeys;
import org.jdbi.v3.sqlobject.statement.SqlQuery;
import org.jdbi.v3.sqlobject.statement.SqlUpdate;
import telraam.database.models.Baton;

import java.util.List;
import java.util.Optional;

public interface BatonDAO extends DAO<Baton> {

    @Override
    @SqlQuery("SELECT * FROM baton")
    @RegisterBeanMapper(Baton.class)
    List<Baton> getAll();

    @Override
    @SqlUpdate("INSERT INTO baton (name, mac) VALUES (:name, :mac)")
    @GetGeneratedKeys({"id"})
    int insert(@BindBean Baton baton);

    @Override
    @SqlQuery("SELECT * FROM baton WHERE id = :id")
    @RegisterBeanMapper(Baton.class)
    Optional<Baton> getById(@Bind("id") int id);

    @SqlQuery("SELECT * FROM baton WHERE mac = :mac")
    @RegisterBeanMapper(Baton.class)
    Optional<Baton> getByMac(@Bind("mac") String mac);

    @Override
    @SqlUpdate("DELETE FROM baton WHERE id = :id")
    @RegisterBeanMapper(Baton.class)
    int deleteById(@Bind("id") int id);

    @Override
    @SqlUpdate("UPDATE baton SET name = :name WHERE id = :id")
    int update(@BindBean Baton baton);
}