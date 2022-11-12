package telraam.logic.baumwelch;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.util.Map;

@Path("/lappers/baumwelch")
@Api("/lappers/baumwelch")
@Produces(MediaType.APPLICATION_JSON)
public class BaumWelchLapperResource {

    private final BaumWelchLapper lapper;

    public BaumWelchLapperResource(BaumWelchLapper lapper) {
        this.lapper = lapper;
    }

    @GET
    @Path("/transition_matrix")
    @ApiOperation(value = "Get lapper transition matrices")
    public Map<Integer, Map<Integer, Double>> getTransitionProbabilities() {
        return this.lapper.getHmm().getTransitionProbabilities();
    }

    @GET
    @Path("/emission_matrix")
    @ApiOperation(value = "Get lapper emission matrices")
    public Map<Integer, Map<Integer, Double>> getEmissionProbabilities() {
        return this.lapper.getHmm().getEmissionProbabilities();
    }
}
