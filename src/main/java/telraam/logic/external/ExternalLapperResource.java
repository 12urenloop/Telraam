package telraam.logic.external;

import io.swagger.v3.oas.annotations.Operation;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.MediaType;
import telraam.logic.external.models.ExternalLapperStats;
import telraam.logic.external.models.ExternalLapperTeamLaps;

import java.util.List;


@Path("/lappers/external")
@Produces(MediaType.APPLICATION_JSON)
public class ExternalLapperResource {
    private final ExternalLapper lapper;

    private ExternalLapperStats externalLapperStats;

    public ExternalLapperResource(ExternalLapper lapper) {
        this.lapper = lapper;
        this.externalLapperStats = new ExternalLapperStats();
    }

    @POST
    @Path("/laps")
    @Operation(summary = "Post the current laps")
    public void postLaps(List<ExternalLapperTeamLaps> teamLaps) {
        this.lapper.saveLaps(teamLaps);
    }

    @GET
    @Path("/stats")
    @Operation(summary = "Get lapper statistics")
    public ExternalLapperStats getStats() {
        return externalLapperStats;
    }

    @POST
    @Path("/stats")
    @Operation(summary = "Post lapper statistics")
    public void postStats(ExternalLapperStats externalLapperStats) {
        this.externalLapperStats = externalLapperStats;
    }
}

