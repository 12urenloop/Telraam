package telraam.api;

import io.swagger.v3.oas.annotations.Operation;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;

@Path("/time")
@Produces(MediaType.APPLICATION_JSON)
public class TimeResource {
    static class TimeResponse {
        public long timestamp;

        public TimeResponse() {
            this.timestamp = System.currentTimeMillis();
        }
    }

    @GET
    @Operation(summary = "Get current time")
    public TimeResponse get() {
        return new TimeResponse();
    }
}
