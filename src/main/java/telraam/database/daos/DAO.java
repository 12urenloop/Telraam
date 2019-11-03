package telraam.database.daos;

import java.util.List;

/**
 * Generic interface all daos should extend
 */
public interface DAO<T> {

    /**
     * Get all objects present in the database.
     * @return the list of objects
     */
    List<T> getAll();

    T getById(Integer id);

    void insert(T newObject);

    void delete(Integer id);
}
