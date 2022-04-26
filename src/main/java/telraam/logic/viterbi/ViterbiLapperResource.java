package telraam.logic.viterbi;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import telraam.logic.viterbi.algorithm.ViterbiModel;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.sql.Timestamp;
import java.util.List;
import java.util.Map;
import java.util.Set;

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
    @Path("/lap-times")
    @ApiOperation(value = "Get lapper estimated laps")
    public Map<Integer, Map<Integer, Set<Timestamp>>> getLapTimes() {
        return this.lapper.getLapTimes();
    }

    @GET
    @Path("/configuration")
    @ApiOperation(value = "Get lapper configuration")
    public ViterbiLapperConfiguration getConfiguration() {
        return this.lapper.getConfig();
    }

    @GET
    @Path("/model")
    @ApiOperation(value = "Get Viterbi model")
    public ViterbiModel<Integer, Integer> getModel() {
        return this.lapper.getModel();
    }

    @GET
    @Path("/recalculate")
    @ApiOperation(value = "Recalculate Viterbi rounds")
    public String recalculateRounds() {
        this.lapper.calculateLaps();
        return "Recalculated rounds";
    }
}
