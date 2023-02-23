package telraam.api;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import telraam.database.daos.BatonDAO;
import telraam.database.daos.DetectionDAO;
import telraam.monitoring.BatonStatus;
import telraam.monitoring.StatusHolder;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.util.List;

@Path("/monitoring")
@Api("/monitoring")
@Produces(MediaType.APPLICATION_JSON)
public class MonitoringResource {
    private final StatusHolder statusHolder;
    public MonitoringResource(BatonDAO BDAO, DetectionDAO DDAO) {
        this.statusHolder = new StatusHolder(BDAO, DDAO);
    }

    @GET
    @Path("/batons")
    @ApiOperation(value = "Get monitoring data that can be used as grafana datasource")
    public List<BatonStatus> getBatonMetrics() {
        // TODO: filter assigned parameter in request
        return statusHolder.GetAllBatonStatuses();
    }
}
