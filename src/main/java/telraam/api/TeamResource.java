package telraam.api;

import telraam.App;
import telraam.database.daos.TeamDAO;
import telraam.database.models.Id;
import telraam.database.models.Team;

import javax.ws.rs.*;
import javax.ws.rs.core.Response;
import java.util.List;
import java.util.Optional;

public class TeamResource {

    private static java.util.logging.Logger logger = java.util.logging.Logger.getLogger(App.class.getName());

    private static final String ID_NAME = "teamId";
    private static final String ENTITY_PATH = "{teamId: [0-9]*}";

    private TeamDAO teamDAO;

    public TeamResource(TeamDAO teamDAO) {
        this.teamDAO = teamDAO;
    }

    /**
     * @return All the teams in the database
     */
    @GET
    public List<Team> getListOfTeams(){
        return teamDAO.getAll();
    }

    /**
     * Create a new team
     *
     * @param team Passed as json via the request body
     * @return The generated id of the team
     */
    @POST
    public Id createTeam(Team team){
        return teamDAO.insert(team);
    }

    /**
     * @return a specific team on the id
     */
    @GET @Path(ENTITY_PATH)
    public Team getTeam(@PathParam(ID_NAME) Optional<Integer> id){
        if (id.isPresent()){
            Optional<Team> optionalTeam = teamDAO.getById(id.get());
            if (optionalTeam.isPresent()){
                return optionalTeam.get();
            }else{
                throw new WebApplicationException(String.format("Team with id: %d not found", id.get()), Response.Status.NOT_FOUND);
            }
        }else{
            throw new WebApplicationException("You did not pass an id", Response.Status.BAD_REQUEST);
        }
    }

    /**
     * Update a specific team with the specified information
     */
    @PUT @Path(ENTITY_PATH)
    public Response updateTeam(@PathParam(ID_NAME) Optional<Integer> id){
        if (id.isPresent()){
            Optional<Team> optionalTeam = teamDAO.getById(id.get());
            if (optionalTeam.isPresent()){
                Team team = optionalTeam.get();
                //todo update the lap in database
                //lapDAO.update(baton)
                //todo return updated lap
                return Response.noContent().build();
            } else {
                throw new WebApplicationException(String.format("Team with id: %d not found", id.get()), Response.Status.NOT_FOUND);
            }
        } else {
            throw new WebApplicationException("You did not pass an id", Response.Status.BAD_REQUEST);
        }
    }

    /**
     *
     * @return a boolean if the team could be deleted
     */

    @DELETE @Path(ENTITY_PATH)
    public boolean deleteTeam(@PathParam(ID_NAME) Optional<Integer> id) {
        // TODO delete the lap
        return true;
    }
}
