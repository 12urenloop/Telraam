package telraam.logic.external;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.util.List;


@Path("/lappers/external")
@Api("/lappers/external")
@Produces(MediaType.APPLICATION_JSON)
public class ExternalLapperResource {
    private final ExternalLapper lapper;

    public ExternalLapperResource(ExternalLapper lapper) {
        this.lapper = lapper;
    }

    @POST
    @Path("/laps")
    @ApiOperation(value = "Post the current laps")
    public void postLaps(List<ExternalLapperTeamLaps> teamLaps) {
        this.lapper.saveLaps(teamLaps);
    }

    //@GET
    //@Path("/stats")
    //@ApiOperation(value = "Get lapper statistics")
    //public Map<Integer, Map<Integer, Integer>> getStats() {
    //    //return this.lapper.getLapCounts();
    //}
//
    //@POST
    //@Path("/stats")
    //@ApiOperation(value = "Post lapper statistics")
    //public Map<Integer, Map<Integer, Integer>> postStats() {
    //    return this.lapper.getLapCounts();
    //}
}

