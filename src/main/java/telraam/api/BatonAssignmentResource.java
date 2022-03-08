package telraam.api;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import telraam.database.daos.BatonAssignmentDAO;
import telraam.database.daos.BatonDAO;
import telraam.database.models.Baton;
import telraam.database.models.BatonAssignment;

import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.util.List;
import java.util.Optional;

@Path("/baton-assignment") // dropwizard
@Api(value = "/baton-assignment") // Swagger
@Produces(MediaType.APPLICATION_JSON)
public class BatonAssignmentResource extends AbstractListableResource<BatonAssignment> {
    public BatonAssignmentResource(BatonAssignmentDAO dao) {
        super(dao);
    }

    @Override
    @ApiOperation(value = "Find all batons assignments")
    public List<BatonAssignment> getListOf() {
        return super.getListOf();
    }

    @Override
    @ApiOperation(value = "Add a new baton assignment to the database")
    public int create(BatonAssignment batonAssignment) {
        return super.create(batonAssignment);
    }

//    @Override
//    @ApiOperation(value = "Find baton assignment by ID")
//    public BatonAssignment get(Optional<Integer> id) {
//        return super.get(id);
//    }

//    @Override
//    @ApiOperation(value = "Update an existing baton assignment")
//    public BatonAssignment update(BatonAssignment batonAssignment, Optional<Integer> id) {
//        return super.update(batonAssignment, id);
//    }
//

    @Override
    @ApiOperation(value = "Delete an existing baton assignment")
    public boolean delete(Optional<Integer> id) {
        return super.delete(id);
    }
}

