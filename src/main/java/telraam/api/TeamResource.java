package telraam.api;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import telraam.database.daos.TeamDAO;
import telraam.database.models.Team;

import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.util.List;
import java.util.Optional;


@Path("/team")
@Api("/team")
@Produces(MediaType.APPLICATION_JSON)
public class TeamResource extends AbstractListableResource<Team> {

    public TeamResource(TeamDAO teamDAO) {
        super(teamDAO);
    }

    @Override
    @ApiOperation(value = "Find all teams")
    public List<Team> getListOf() {
        return super.getListOf();
    }

    @Override
    @ApiOperation(value = "Add a new team to the database")
    public int create(Team team) {
        return super.create(team);
    }

    @Override
    @ApiOperation(value = "Find team by ID")
    public Team get(Optional<Integer> id) {
        return super.get(id);
    }

    @Override
    @ApiOperation(value = "Update an existing team")
    public Team update(Team team, Optional<Integer> id) {
        return super.update(team, id);
    }

    @Override
    @ApiOperation(value = "Delete an existing team")
    public boolean delete(Optional<Integer> id) {
        return super.delete(id);
    }
}
