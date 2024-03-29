package telraam.api;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;

@Path("/time")
@Tag(name = "Time")
@Produces(MediaType.APPLICATION_JSON)
public class TimeResource {
    public static class TimeResponse {
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
