package telraam.api;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import telraam.database.daos.DetectionDAO;
import telraam.database.models.Detection;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;
import java.util.List;
import java.util.Optional;

@Path("/detection")
@Api(value = "/detection") // Swagger
@Produces(MediaType.APPLICATION_JSON)
public class DetectionResource extends AbstractListableResource<Detection> {

    private final DetectionDAO detectionDAO;

    public DetectionResource(DetectionDAO dao) {
        super(dao);
        detectionDAO = dao;
    }

    @Override
    @ApiOperation(value = "Find all detections")
    public List<Detection> getListOf() {
        return super.getListOf();
    }

    @Override
    @ApiOperation(value = "Add a new detection to the database")
    public int create(Detection detection) {
        return super.create(detection);
    }

    @Override
    @ApiOperation(value = "Find detection by ID")
    public Detection get(Optional<Integer> id) {
        return super.get(id);
    }

    @Override
    @ApiOperation(value = "Update an existing detection")
    public Detection update(Detection detection, Optional<Integer> id) {
        return super.update(detection, id);
    }

    @Override
    @ApiOperation(value = "Delete an existing detection")
    public boolean delete(Optional<Integer> id) {
        return super.delete(id);
    }

    @GET
    @Path("/since/{id}")
    @ApiOperation(value = "Get detections with ID larger than given ID")
    public List<Detection> getListSince(@PathParam("id") Integer id, @QueryParam("limit") Optional<Integer> limit) {
        return detectionDAO.getSinceId(id, limit.orElse(1000));
    }
}
