package telraam.logic.monitoring;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import telraam.database.daos.BatonDAO;
import telraam.database.daos.DetectionDAO;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.util.List;

@Path("/monitoring")
@Api("/monitoring")
@Produces(MediaType.APPLICATION_JSON)
public class MonitoringResource {

    private MonitoringLapper monitoringLapper;
    public MonitoringResource(MonitoringLapper lapper) {
        this.monitoringLapper = lapper;
    }

    @GET
    @Path("/batons")
    @ApiOperation(value = "Get monitoring data that can be used as grafana datasource")
    public List<BatonStatus> getBatonMetrics() {
        // TODO: filter assigned parameter in request
        return monitoringLapper.getBatonStatuses();
    }
}
