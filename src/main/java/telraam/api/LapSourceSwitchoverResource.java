package telraam.api;

import io.swagger.v3.oas.annotations.Operation;
import jakarta.ws.rs.*;
import jakarta.ws.rs.core.MediaType;
import telraam.database.daos.LapSourceSwitchoverDAO;
import telraam.database.models.LapSourceSwitchover;

import java.util.List;
import java.util.Optional;

@Path("/lapsourceswitchover") // dropwizard
@Produces(MediaType.APPLICATION_JSON)
public class LapSourceSwitchoverResource extends AbstractListableResource<LapSourceSwitchover> {
    public LapSourceSwitchoverResource(LapSourceSwitchoverDAO dao) {
        super(dao);
    }

    @Override
    @Operation(summary = "Find all lap source switchovers")
    public List<LapSourceSwitchover> getListOf() {
        return super.getListOf();
    }

    @Override
    @Operation(summary = "Find lap source switchover by ID")
    public LapSourceSwitchover get(Optional<Integer> id) {
        return super.get(id);
    }

    @Override
    @Operation(summary = "Add a new lap source switchover to the database")
    public int create(LapSourceSwitchover lapSourceSwitchover) {
        return super.create(lapSourceSwitchover);
    }
}

