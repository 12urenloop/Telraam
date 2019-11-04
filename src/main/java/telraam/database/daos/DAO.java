package telraam.database.daos;

import telraam.database.models.Baton;

import java.util.List;

/**
 * Generic interface all daos should extend
 */
public interface DAO<T> {

    /**
     * Get all objects present in the database.
     *
     * @return the list of objects
     */
    List<T> getAll();

    T getById(Integer id);

    Baton insert(T newObject);

    void delete(Integer id);
}
