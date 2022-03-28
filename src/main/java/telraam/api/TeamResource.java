package telraam.api;

import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import telraam.database.daos.BatonSwitchoverDAO;
import telraam.database.daos.TeamDAO;
import telraam.database.models.BatonSwitchover;
import telraam.database.models.Team;

import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import java.sql.Timestamp;
import java.time.Instant;
import java.util.List;
import java.util.Objects;
import java.util.Optional;


@Path("/team")
@Api("/team")
@Produces(MediaType.APPLICATION_JSON)
public class TeamResource extends AbstractListableResource<Team> {
    BatonSwitchoverDAO batonSwitchoverDAO;
    public TeamResource(TeamDAO teamDAO, BatonSwitchoverDAO batonSwitchoverDAO) {
        super(teamDAO);
        this.batonSwitchoverDAO = batonSwitchoverDAO;
    }

    @Override
    @ApiOperation(value = "Find all teams")
    public List<Team> getListOf() {
        return super.getListOf();
    }

    @Override
    @ApiOperation(value = "Add a new team to the database")
    public int create(Team team) {
        int ret = super.create(team);

        if (team.getBatonId() != null) {
            this.batonSwitchoverDAO.insert(new BatonSwitchover(
                    ret,
                    null,
                    team.getBatonId(),
                    Timestamp.from(Instant.now())
            ));
        }

        return ret;
    }

    @Override
    @ApiOperation(value = "Find team by ID")
    public Team get(Optional<Integer> id) {
        return super.get(id);
    }

    @Override
    @ApiOperation(value = "Update an existing team")
    public Team update(Team team, Optional<Integer> id) {
        Team previousTeam = this.get(id);
        Team ret = super.update(team, id);

        System.out.println(previousTeam.getBatonId());
        System.out.println(team.getBatonId());

        if (!Objects.equals(previousTeam.getBatonId(), team.getBatonId())) {
            this.batonSwitchoverDAO.insert(new BatonSwitchover(
                team.getId(),
                previousTeam.getBatonId(),
                team.getBatonId(),
                Timestamp.from(Instant.now())
            ));
        }

        return ret;
    }

    @Override
    @ApiOperation(value = "Delete an existing team")
    public boolean delete(Optional<Integer> id) {
        return super.delete(id);
    }
}
