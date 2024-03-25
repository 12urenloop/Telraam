package telraam.api;

import io.swagger.v3.oas.annotations.Operation;
import jakarta.ws.rs.Produces;
import telraam.database.daos.DAO;
import telraam.database.models.Station;

import jakarta.ws.rs.Path;
import jakarta.ws.rs.core.MediaType;
import java.util.List;
import java.util.Optional;

@Path("/station")
@Produces(MediaType.APPLICATION_JSON)
public class StationResource extends AbstractListableResource<Station> {
    public StationResource(DAO<Station> dao) {
        super(dao);
    }

    @Override
    @Operation(summary = "Find all stations")
    public List<Station> getListOf() {
        return super.getListOf();
    }

    @Override
    @Operation(summary = "Add a new station to the database")
    public int create(Station station) {
        return super.create(station);
    }

    @Override
    @Operation(summary = "Find station by ID")
    public Station get(Optional<Integer> id) {
        return super.get(id);
    }

    @Override
    @Operation(summary = "Update an existing station")
    public Station update(Station station, Optional<Integer> id) {
        return super.update(station, id);
    }

    @Override
    @Operation(summary = "Delete an existing station")
    public boolean delete(Optional<Integer> id) {
        return super.delete(id);
    }
}
