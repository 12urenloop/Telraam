package telraam.database.daos;

import java.util.List;
import java.util.Optional;

public interface DAO<T> {
    List<T> getAll();

    int insert(T modelObj);

    Optional<T> getById(int id);

    int deleteById(int id);

    int update(int id, T modelObj);

}
