package telraam.api;

import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.WebApplicationException;
import jakarta.ws.rs.core.Response;
import telraam.database.daos.DAO;

import java.util.Optional;

public abstract class AbstractResource<T> implements Resource<T> {

    protected final DAO<T> dao;

    protected AbstractResource(DAO<T> dao) {
        this.dao = dao;
    }

    @Override
    // TODO Validate model and return 405 for wrong input
    public int create(@Parameter(required = true) T t) {
        return dao.insert(t);
    }

    @Override
    @ApiResponse(responseCode = "400", description = "Invalid or no ID supplied") // TODO validate ID, return 400 on wrong ID format
    @ApiResponse(responseCode = "404", description = "Entity with specified ID not found")
    public T get(@Parameter(description = "ID of entity that needs to be fetched", required = true) Optional<Integer> id) {
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
    @ApiResponse(responseCode = "400", description = "Invalid or no ID supplied") // TODO validate ID, return 400 on wrong ID format
    @ApiResponse(responseCode = "404", description = "Entity with specified ID not found")
    @ApiResponse(responseCode = "405", description = "Validation exception") // TODO validate input, 405 on wrong input
    public T update(@Parameter(description = "Entity object that needs to be updated in the database", required = true) T t,
                    @Parameter(description = "ID of entity that needs to be fetched", required = true) Optional<Integer> id) {
        if (id.isPresent()) {
            Optional<T> optionalBaton = dao.getById(id.get());
            if (optionalBaton.isPresent()) {
                dao.update(id.get(), t);
                return t;
            } else {
                throw new WebApplicationException(String.format("%s with id: %d not found", t.getClass().getName(), id.get()), Response.Status.NOT_FOUND);
            }
        } else {
            throw new MissingIdException();
        }
    }

    @Override
    @ApiResponses(value = {
            @ApiResponse(responseCode = "400", description = "Invalid or no ID supplied"), // TODO validate ID, return 400 on wrong ID format
    })
    public boolean delete(
            @Parameter(description = "ID of entity that needs to be deleted", required = true) Optional<Integer> id) {
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
