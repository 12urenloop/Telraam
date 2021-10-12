package telraam.api;

import telraam.database.daos.TeamDAO;
import telraam.database.models.Team;

import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;


@Path("/team")
@Produces(MediaType.APPLICATION_JSON)
public class TeamResource extends AbstractListableResource<Team> {

    public TeamResource(TeamDAO teamDAO) {
        super(teamDAO);
    }

}
