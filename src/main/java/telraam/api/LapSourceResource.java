package telraam.api;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import telraam.database.daos.DAO;
import telraam.database.models.LapSource;

import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.util.List;
import java.util.Optional;

@Path("/lap-source")
@Api("/lap-source")
@Produces(MediaType.APPLICATION_JSON)
public class LapSourceResource extends AbstractListableResource<LapSource> {
    public LapSourceResource(DAO<LapSource> dao) {
        super(dao);
    }

    @Override
    @ApiOperation(value = "Find all lap sources")
    public List<LapSource> getListOf() {
        return super.getListOf();
    }

    @Override
    @ApiOperation(value = "Add a new lap source to the database")
    public int create(LapSource lapSource) {
        return super.create(lapSource);
    }

    @Override
    @ApiOperation(value = "Find lap source by ID")
    public LapSource get(Optional<Integer> id) {
        return super.get(id);
    }

    @Override
    @ApiOperation(value = "Update an existing lap source")
    public LapSource update(LapSource lapSource, Optional<Integer> id) {
        return super.update(lapSource, id);
    }

    @Override
    @ApiOperation(value = "Delete an existing lap source")
    public boolean delete(Optional<Integer> id) {
        return super.delete(id);
    }
}
