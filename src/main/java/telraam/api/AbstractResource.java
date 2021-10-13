package telraam.api;

import telraam.database.daos.DAO;

import javax.annotation.security.PermitAll;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response;
import java.util.Optional;

public abstract class AbstractResource<T> implements Resource<T> {

    protected final DAO<T> dao;

    protected AbstractResource(DAO<T> dao) {
        this.dao = dao;
    }

    @Override
    @PermitAll
    public int create(T t) {
        return dao.insert(t);
    }

    @Override
    public T get(Optional<Integer> id) {
        if (id.isPresent()) {
            Optional<T> optional = dao.getById(id.get());
            if (optional.isPresent()) {
                return optional.get();
            } else {
                throw new WebApplicationException(String.format("%s with id: %d not found", this.getClass().getSimpleName(), id.get()), Response.Status.NOT_FOUND);
            }
        } else {
            throw new MissingIdException();
        }
    }

    @Override
    @PermitAll
    public T update(T t, Optional<Integer> id) {
        if (id.isPresent()) {
            Optional<T> optionalBaton = dao.getById(id.get());
            if (optionalBaton.isPresent()) {
                dao.update(t);
                return t;
            } else {
                throw new WebApplicationException(String.format("%s with id: %d not found", Class.class.getName(), id.get()), Response.Status.NOT_FOUND);
            }
        } else {
            throw new MissingIdException();
        }
    }

    @Override
    @PermitAll
    public boolean delete(Optional<Integer> id) {
        if (id.isPresent()) {
            return dao.deleteById(id.get()) == 1;
        } else {
            throw new MissingIdException();
        }
    }

    private static class MissingIdException extends WebApplicationException {
        MissingIdException() {
            super("You did not pass an id", Response.Status.BAD_REQUEST);
        }
    }
}
