package telraam.api;

import com.codahale.metrics.annotation.Timed;
import telraam.api.responses.Saying;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import java.util.Optional;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Warning
 *
 * Resource classes are used by multiple threads concurrently. In general, we recommend that resources be stateless/immutable, but it’s important to keep the context in mind.
 */

@Path("/hello-world")
@Produces(MediaType.APPLICATION_JSON)
public class HelloworldResource {
    private final String template;
    private final String defaultName;
    private final AtomicLong counter;

    public HelloworldResource(String template, String defaultName) {
        this.template = template;
        this.defaultName = defaultName;
        this.counter = new AtomicLong();
    }

    @GET
    @Timed
    public Saying sayHello(@QueryParam("name") Optional<String> name) {
        final String value = String.format(template, name.orElse(defaultName));
        return new Saying(counter.incrementAndGet(), value);
    }
}
