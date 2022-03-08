package telraam.database.daos;

import org.jdbi.v3.sqlobject.config.RegisterBeanMapper;
import org.jdbi.v3.sqlobject.customizer.Bind;
import org.jdbi.v3.sqlobject.customizer.BindBean;
import org.jdbi.v3.sqlobject.statement.GetGeneratedKeys;
import org.jdbi.v3.sqlobject.statement.SqlQuery;
import org.jdbi.v3.sqlobject.statement.SqlUpdate;
import telraam.database.models.Baton;
import telraam.database.models.Team;

import java.util.List;
import java.util.Optional;

public interface TeamDAO extends DAO<Team> {

    @Override
    @SqlQuery("SELECT * FROM team")
    @RegisterBeanMapper(Team.class)
    List<Team> getAll();

    @Override
    @SqlUpdate("INSERT INTO team (name, baton_id) VALUES (:name, :batonId)")
    @GetGeneratedKeys({"id"})
    int insert(@BindBean Team team);

    @Override
    @SqlQuery("SELECT * FROM team WHERE id = :id")
    @RegisterBeanMapper(Team.class)
    Optional<Team> getById(@Bind("id") int id);

    @Override
    @SqlUpdate("DELETE FROM team where id = :id")
    int deleteById(@Bind("id") int id);

    @Override
    @SqlUpdate("UPDATE team SET " +
            "name = :name," +
            "baton_id = :batonId " +
            "WHERE id = :id")
    int update(@Bind("id") int id, @BindBean Team modelObj);

    @SqlQuery("SELECT batonId, MAX(timestamp) FROM baton_assignment where teamId = :id GROUP BY id")
    Optional<Integer> getCurrentBatonId(@BindBean Team team);
}
