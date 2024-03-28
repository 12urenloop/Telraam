package telraam.database.daos;

import org.jdbi.v3.sqlobject.config.RegisterBeanMapper;
import org.jdbi.v3.sqlobject.customizer.Bind;
import org.jdbi.v3.sqlobject.customizer.BindBean;
import org.jdbi.v3.sqlobject.statement.GetGeneratedKeys;
import org.jdbi.v3.sqlobject.statement.SqlQuery;
import org.jdbi.v3.sqlobject.statement.SqlUpdate;
import telraam.database.models.Station;

import java.util.List;
import java.util.Optional;

public interface StationDAO extends DAO<Station> {

    @Override
    @SqlQuery("SELECT * FROM station")
    @RegisterBeanMapper(Station.class)
    List<Station> getAll();

    @Override
    @SqlUpdate("INSERT INTO station (name, distance_from_start, broken, url, coord_x, coord_y) VALUES (:name, :distanceFromStart, :broken, :url, :coordX, :coordY)")
    @GetGeneratedKeys({"id"})
    int insert(@BindBean Station station);

    @Override
    @SqlQuery("SELECT * FROM station WHERE id = :id")
    @RegisterBeanMapper(Station.class)
    Optional<Station> getById(@Bind("id") int id);

    @Override
    @SqlUpdate("DELETE FROM station WHERE id = :id")
    int deleteById(@Bind("id") int id);

    @Override
    @SqlUpdate("UPDATE station SET name = :name, distance_from_start = :distanceFromStart, broken = :broken, url = :url, coord_x = :coordX, coord_y = :coordY WHERE id = :id")
    int update(@Bind("id") int id, @BindBean Station station);
}
