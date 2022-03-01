package telraam.logic.viterbi;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.util.Map;

@Path("/lappers/viterbi")
@Api("/lappers/viterbi")
@Produces(MediaType.APPLICATION_JSON)
public class ViterbiLapperResource {
    private final ViterbiLapper lapper;

    public ViterbiLapperResource(ViterbiLapper lapper) {
        this.lapper = lapper;
    }

    @GET
    @Path("/probabilities")
    @ApiOperation(value = "Get lapper position probabilities")
    public Map<Integer, Map<Integer, Double>> getProbabilities() {
        return this.lapper.getProbabilities();
    }

    @GET
    @Path("/configuration")
    @ApiOperation(value = "Get lapper configuration")
    public ViterbiLapperConfiguration getConfiguration() {
        return this.lapper.getConfig();
    }
}