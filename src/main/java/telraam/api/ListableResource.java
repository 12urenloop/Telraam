package telraam.api;

import jakarta.ws.rs.GET;
import java.util.List;

/**
 * A resource with a basic GET /resource page, without params.
 */
public interface ListableResource<T> extends Resource<T> {
    /**
     * @return a list of all the specified items
     */
    @GET
    List<T> getListOf();
}
