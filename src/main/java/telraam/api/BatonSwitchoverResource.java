package telraam.api;

import io.swagger.v3.oas.annotations.Operation;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;
import telraam.database.daos.BatonSwitchoverDAO;
import telraam.database.models.BatonSwitchover;

import java.util.List;
import java.util.Optional;

@Path("/batonswitchover") // dropwizard
@Produces(MediaType.APPLICATION_JSON)
public class BatonSwitchoverResource extends AbstractListableResource<BatonSwitchover> {
    public BatonSwitchoverResource(BatonSwitchoverDAO dao) {
        super(dao);
    }

    @Override
    @Operation(summary = "Find all baton switchovers")
    public List<BatonSwitchover> getListOf() {
        return super.getListOf();
    }

    @Override
    @Operation(summary = "Find baton switchover by ID")
    public BatonSwitchover get(Optional<Integer> id) {
        return super.get(id);
    }

    @Override
    @Operation(summary = "Add a new baton switchover to the database")
    public int create(BatonSwitchover batonSwitchover) {
        return super.create(batonSwitchover);
    }
}

