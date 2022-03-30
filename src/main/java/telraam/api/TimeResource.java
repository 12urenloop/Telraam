package telraam.api;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.time.Instant;

@Path("/time")
@Api("/time")
@Produces(MediaType.APPLICATION_JSON)
public class TimeResource {
    static class TimeResponse {
        public long timestamp;

        public TimeResponse() {
            this.timestamp = System.currentTimeMillis();
        }
    }

    @GET
    @ApiOperation(value = "Find all laps")
    public TimeResponse get() {
        return new TimeResponse();
    }
}
