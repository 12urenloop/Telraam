package telraam.database.daos;

import org.jdbi.v3.sqlobject.config.RegisterBeanMapper;
import org.jdbi.v3.sqlobject.customizer.Bind;
import org.jdbi.v3.sqlobject.customizer.BindBean;
import org.jdbi.v3.sqlobject.statement.GetGeneratedKeys;
import org.jdbi.v3.sqlobject.statement.SqlQuery;
import org.jdbi.v3.sqlobject.statement.SqlUpdate;
import telraam.database.models.Team;
import telraam.database.models.Id;

import java.util.List;
import java.util.Optional;

public interface TeamDAO {
    @SqlQuery("SELECT * FROM team")
    @RegisterBeanMapper(Team.class)
    List<Team> getAll();

    @SqlUpdate("INSERT INTO team (name, baton_id) VALUES (:name, :baton_id)")
    @GetGeneratedKeys({"id"})
    @RegisterBeanMapper(Id.class)
    Id insert(@BindBean Team team);

    @SqlQuery("SELECT * FROM team WHERE id = :id")
    @RegisterBeanMapper(Team.class)
    Optional<Team> getById(@Bind("id") int id);

    void deleteById(@Bind("id") int id);
}