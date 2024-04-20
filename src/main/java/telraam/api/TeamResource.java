package telraam.api;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;
import telraam.database.daos.BatonSwitchoverDAO;
import telraam.database.daos.TeamDAO;
import telraam.database.models.BatonSwitchover;
import telraam.database.models.Team;

import java.sql.Timestamp;
import java.time.Instant;
import java.util.List;
import java.util.Objects;
import java.util.Optional;


@Path("/team")
@Tag(name="Team")
@Produces(MediaType.APPLICATION_JSON)
public class TeamResource extends AbstractListableResource<Team> {
    BatonSwitchoverDAO batonSwitchoverDAO;

    public TeamResource(TeamDAO teamDAO, BatonSwitchoverDAO batonSwitchoverDAO) {
        super(teamDAO);
        this.batonSwitchoverDAO = batonSwitchoverDAO;
    }

    @Override
    @Operation(summary = "Add a new team to the database")
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
    @Operation(summary = "Update an existing team")
    public Team update(Team team, Optional<Integer> id) {
        Team previousTeam = this.get(id);
        Team ret = super.update(team, id);

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
}
