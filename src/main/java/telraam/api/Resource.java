package telraam.api;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import java.util.List;
import java.util.Optional;

public interface Resource<T> {

    String ID_NAME = "Id";
    String ENTITY_PATH = "/{Id: [0-9]*}";

    /**
     * @param t the item to create in the database
     * @return whether or not the item was created
     */

    @POST
    int create(T t);

    /**
     * @param id the id of the item to return
     * @return the item you requested
     */

    @GET
    @Path(ENTITY_PATH)
    T get(@PathParam(ID_NAME) Optional<Integer> id);

    /**
     * @param t  the item to update
     * @param id the id of the item to update
     * @return the new and updated item
     */

    @PUT
    @Path(ENTITY_PATH)
    @Consumes(MediaType.APPLICATION_JSON)
    T update(T t, @PathParam(ID_NAME) Optional<Integer> id);

    /**
     * @param id the id of the item to delete
     * @return whether or not the item was deleted
     */

    @DELETE
    @Path(ENTITY_PATH)
    boolean delete(@PathParam(ID_NAME) Optional<Integer> id);


}
