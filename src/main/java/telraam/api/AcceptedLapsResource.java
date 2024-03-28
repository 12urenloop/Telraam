package telraam.api;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;
import telraam.database.models.Lap;
import telraam.util.AcceptedLapsUtil;

import java.util.List;

@Path("/accepted-laps")
@Tag(name="Accpted Laps")
@Produces(MediaType.APPLICATION_JSON)
public class AcceptedLapsResource {
    @GET
    @Operation(summary = "Get all accepted laps")
    public List<Lap> getLaps() {
        return AcceptedLapsUtil.getInstance().getAcceptedLaps();
    }
}
