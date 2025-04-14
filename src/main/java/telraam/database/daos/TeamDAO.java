package telraam.database.daos;

import org.jdbi.v3.sqlobject.config.RegisterBeanMapper;
import org.jdbi.v3.sqlobject.customizer.Bind;
import org.jdbi.v3.sqlobject.customizer.BindBean;
import org.jdbi.v3.sqlobject.statement.GetGeneratedKeys;
import org.jdbi.v3.sqlobject.statement.SqlQuery;
import org.jdbi.v3.sqlobject.statement.SqlUpdate;
import telraam.database.models.Team;

import java.util.List;
import java.util.Optional;

public interface TeamDAO extends DAO<Team> {

    @Override
    @SqlQuery("SELECT t.*, tb.baton_id FROM team t LEFT JOIN team_baton_ids tb ON tb.team_id = t.id")
    @RegisterBeanMapper(Team.class)
    List<Team> getAll();

    @Override
    @SqlUpdate("INSERT INTO team (name, jacket_nr) VALUES (:name, :jacketNr)")
    @GetGeneratedKeys({"id"})
    int insert(@BindBean Team team);

    @Override
    @SqlQuery("SELECT t.*, tb.baton_id FROM team t LEFT JOIN team_baton_ids tb ON tb.team_id = t.id WHERE t.id = :id")
    @RegisterBeanMapper(Team.class)
    Optional<Team> getById(@Bind("id") int id);

    @Override
    @SqlUpdate("DELETE FROM team where id = :id")
    int deleteById(@Bind("id") int id);

    @Override
    @SqlUpdate("UPDATE team SET name = :name, jacket_nr = :jacketNr WHERE id = :id")
    int update(@Bind("id") int id, @BindBean Team modelObj);
}
