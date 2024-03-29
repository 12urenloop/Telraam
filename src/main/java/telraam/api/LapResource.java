package telraam.api;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.MediaType;
import telraam.database.daos.LapDAO;
import telraam.database.models.Lap;

import java.util.List;
import java.util.Optional;

@Path("/lap")
@Tag(name="Lap")
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
}
