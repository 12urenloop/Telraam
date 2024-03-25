package telraam.api;

import io.swagger.v3.oas.annotations.Operation;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.MediaType;
import telraam.database.daos.LapDAO;
import telraam.database.models.Lap;

import java.util.List;
import java.util.Optional;

@Path("/lap")
@Produces(MediaType.APPLICATION_JSON)
public class LapResource extends AbstractResource<Lap> {
    private final LapDAO lapDAO;

    public LapResource(LapDAO dao) {
        super(dao);
        lapDAO = dao;
    }

    @GET
    @Operation(summary = "Find all laps")
    public List<Lap> getListOf(@QueryParam("source") final Integer source) {
        if (source == null) {
            return lapDAO.getAll();
        } else {
            return lapDAO.getAllBySource(source);
        }
    }

    @Override
    @Operation(summary = "Add a new lap to the database")
    public int create(Lap lap) {
        return super.create(lap);
    }

    @Override
    @Operation(summary = "Find lap by ID")
    public Lap get(Optional<Integer> id) {
        return super.get(id);
    }

    @Override
    @Operation(summary = "Update an existing lap")
    public Lap update(Lap lap, Optional<Integer> id) {
        return super.update(lap, id);
    }

    @Override
    @Operation(summary = "Delete an existing lap")
    public boolean delete(Optional<Integer> id) {
        return super.delete(id);
    }
}
