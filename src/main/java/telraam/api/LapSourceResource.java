package telraam.api;

import io.swagger.v3.oas.annotations.Operation;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.MediaType;
import telraam.database.daos.DAO;
import telraam.database.models.LapSource;

import java.util.List;
import java.util.Optional;

@Path("/lap-source")
@Produces(MediaType.APPLICATION_JSON)
public class LapSourceResource extends AbstractListableResource<LapSource> {
    public LapSourceResource(DAO<LapSource> dao) {
        super(dao);
    }

    @Override
    @Operation(summary = "Find all lap sources")
    public List<LapSource> getListOf() {
        return super.getListOf();
    }

    @Override
    @Operation(summary = "Add a new lap source to the database")
    public int create(LapSource lapSource) {
        return super.create(lapSource);
    }

    @Override
    @Operation(summary = "Find lap source by ID")
    public LapSource get(Optional<Integer> id) {
        return super.get(id);
    }

    @Override
    @Operation(summary = "Update an existing lap source")
    public LapSource update(LapSource lapSource, Optional<Integer> id) {
        return super.update(lapSource, id);
    }

    @Override
    @Operation(summary = "Delete an existing lap source")
    public boolean delete(Optional<Integer> id) {
        return super.delete(id);
    }
}
