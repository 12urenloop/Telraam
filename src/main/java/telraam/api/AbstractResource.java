package telraam.api;

import io.swagger.annotations.ApiParam;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import telraam.database.daos.DAO;

import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Response;
import java.util.Optional;

public abstract class AbstractResource<T> implements Resource<T> {

    protected final DAO<T> dao;

    protected AbstractResource(DAO<T> dao) {
        this.dao = dao;
    }

    @Override
    // TODO Validate model and return 405 for wrong input
    public int create(@ApiParam(required = true) T t) {
        return dao.insert(t);
    }

    @Override
    @ApiResponses(value = {
            @ApiResponse(code = 400, message = "Invalid or no ID supplied"), // TODO validate ID, return 400 on wrong ID format
            @ApiResponse(code = 404, message = "Entity with specified ID not found")
    })
    public T get(@ApiParam(value = "ID of entity that needs to be fetched", required = true) Optional<Integer> id) {
        if (id.isPresent()) {
            Optional<T> optional = dao.getById(id.get());
            if (optional.isPresent()) {
                return optional.get();
            } else {
                throw new WebApplicationException(String.format("%s with id: %d not found", Class.class.getName(), id.get()), Response.Status.NOT_FOUND);
            }
        } else {
            throw new MissingIdException();
        }
    }

    @Override
    @ApiResponses(value = {
            @ApiResponse(code = 400, message = "Invalid or no ID supplied"), // TODO validate ID, return 400 on wrong ID format
            @ApiResponse(code = 404, message = "Entity with specified ID not found"),
            @ApiResponse(code = 405, message = "Validation exception")}) // TODO validate input, 405 on wrong input
    public T update(@ApiParam(value = "Entity object that needs to be updated in the database", required = true) T t,
                    @ApiParam(value = "ID of entity that needs to be fetched", required = true) Optional<Integer> id) {
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
    @ApiResponses(value = {
            @ApiResponse(code = 400, message = "Invalid or no ID supplied"), // TODO validate ID, return 400 on wrong ID format
    })
    public boolean delete(
            @ApiParam(value = "ID of entity that needs to be fetched", required = true) Optional<Integer> id) {
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
