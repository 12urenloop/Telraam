package telraam.api;

import telraam.database.daos.TeamDAO;
import telraam.database.models.Team;

import javax.ws.rs.*;
import javax.ws.rs.core.MediaType;


@Path("/team")
@Produces(MediaType.APPLICATION_JSON)
public class TeamResource extends AbstractResource<Team> {

    public TeamResource(TeamDAO teamDAO) {
        super(teamDAO);
    }

}
