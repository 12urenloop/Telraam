package telraam.api;

import io.swagger.v3.oas.annotations.Operation;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.MediaType;
import telraam.database.daos.DetectionDAO;
import telraam.database.models.Detection;

import java.util.List;
import java.util.Optional;

@Path("/detection")
@Produces(MediaType.APPLICATION_JSON)
public class DetectionResource extends AbstractListableResource<Detection> {

    private final DetectionDAO detectionDAO;

    public DetectionResource(DetectionDAO dao) {
        super(dao);
        detectionDAO = dao;
    }

    @Override
    @Operation(summary = "Find all detections")
    public List<Detection> getListOf() {
        return super.getListOf();
    }

    @Override
    @Operation(summary = "Add a new detection to the database")
    public int create(Detection detection) {
        return super.create(detection);
    }

    @Override
    @Operation(summary = "Find detection by ID")
    public Detection get(Optional<Integer> id) {
        return super.get(id);
    }

    @Override
    @Operation(summary = "Update an existing detection")
    public Detection update(Detection detection, Optional<Integer> id) {
        return super.update(detection, id);
    }

    @Override
    @Operation(summary = "Delete an existing detection")
    public boolean delete(Optional<Integer> id) {
        return super.delete(id);
    }

    @GET
    @Path("/since/{id}")
    @Operation(summary = "Get detections with ID larger than given ID")
    public List<Detection> getListSince(@PathParam("id") Integer id, @QueryParam("limit") Optional<Integer> limit) {
        return detectionDAO.getSinceId(id, limit.orElse(1000));
    }
}
