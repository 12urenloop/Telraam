package telraam.api;

import io.swagger.v3.oas.annotations.Operation;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;
import telraam.database.models.Lap;
import telraam.util.AcceptedLapsUtil;

import java.util.List;

@Path("/accepted-laps")
@Produces(MediaType.APPLICATION_JSON)
public class AcceptedLapsResource {
    @GET
    @Operation(summary = "Get all accepted laps")
    public List<Lap> getLaps() {
        return AcceptedLapsUtil.getInstance().getAcceptedLaps();
    }
}
