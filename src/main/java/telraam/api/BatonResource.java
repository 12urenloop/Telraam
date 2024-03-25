package telraam.api;

import io.swagger.v3.oas.annotations.Operation;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;
import telraam.database.daos.BatonDAO;
import telraam.database.models.Baton;

import java.util.List;
import java.util.Optional;

@Path("/baton") // dropwizard
@Produces(MediaType.APPLICATION_JSON)
public class BatonResource extends AbstractListableResource<Baton> {
    public BatonResource(BatonDAO dao) {
        super(dao);
    }

    @Override
    @Operation(summary = "Find all batons")
    public List<Baton> getListOf() {
        return super.getListOf();
    }

    @Override
    @Operation(summary = "Add a new baton to the database")
    public int create(Baton baton) {
        return super.create(baton);
    }

    @Override
    @Operation(summary = "Find baton by ID")
    public Baton get(Optional<Integer> id) {
        return super.get(id);
    }

    @Override
    @Operation(summary = "Update an existing baton")
    public Baton update(Baton baton, Optional<Integer> id) {
        return super.update(baton, id);
    }

    @Override
    @Operation(summary = "Delete an existing baton")
    public boolean delete(Optional<Integer> id) {
        return super.delete(id);
    }
}

