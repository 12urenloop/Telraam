package telraam.api;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.MediaType;
import telraam.database.daos.DetectionDAO;
import telraam.database.models.Detection;

import java.util.List;
import java.util.Optional;

@Path("/detection")
@Produces(MediaType.APPLICATION_JSON)
@Tag(name="Detection")
public class DetectionResource extends AbstractListableResource<Detection> {

    private final DetectionDAO detectionDAO;

    public DetectionResource(DetectionDAO dao) {
        super(dao);
        detectionDAO = dao;
    }

    @GET
    @Path("/since/{id}")
    @Operation(summary = "Get detections with ID larger than given ID")
    public List<Detection> getListSince(@PathParam("id") Integer id, @QueryParam("limit") Optional<Integer> limit) {
        return detectionDAO.getSinceId(id, limit.orElse(1000));
    }
}
