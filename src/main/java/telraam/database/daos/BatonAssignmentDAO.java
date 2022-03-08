package telraam.database.daos;

import org.jdbi.v3.sqlobject.config.RegisterBeanMapper;
import org.jdbi.v3.sqlobject.customizer.Bind;
import org.jdbi.v3.sqlobject.customizer.BindBean;
import org.jdbi.v3.sqlobject.statement.GetGeneratedKeys;
import org.jdbi.v3.sqlobject.statement.SqlQuery;
import org.jdbi.v3.sqlobject.statement.SqlUpdate;
import telraam.database.models.BatonAssignment;

import java.util.List;
import java.util.Optional;

public interface BatonAssignmentDAO extends DAO<BatonAssignment> {

    @Override
    @SqlQuery("SELECT * FROM baton_assignment")
    @RegisterBeanMapper(BatonAssignment.class)
    List<BatonAssignment> getAll();

    @Override
    @SqlUpdate("INSERT INTO baton_assignment (baton_id, team_id) VALUES (:batonId, :teamId)")
    @GetGeneratedKeys({"id"})
    int insert(@BindBean BatonAssignment batonAssignment);

    @Override
    @SqlQuery("SELECT * FROM baton_assignment WHERE id = :id")
    @RegisterBeanMapper(BatonAssignment.class)
    Optional<BatonAssignment> getById(@Bind("id") int id);

    @Override
    @SqlUpdate("DELETE FROM baton_assignment WHERE id = :id")
    @RegisterBeanMapper(BatonAssignment.class)
    int deleteById(@Bind("id") int id);

    @Override
    @SqlUpdate("UPDATE baton_assignment SET baton_id = :batonId, team_id = :teamId WHERE id = :id")
    int update(@Bind("id") int id, @BindBean BatonAssignment batonAssignment);
}
