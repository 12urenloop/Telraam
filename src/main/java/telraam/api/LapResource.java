package telraam.api;

import telraam.database.daos.LapDAO;
import telraam.database.models.Lap;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import java.util.List;

@Path("/lap")
@Produces(MediaType.APPLICATION_JSON)
public class LapResource extends AbstractResource<Lap> {
    private final LapDAO lapDAO;

    public LapResource(LapDAO dao) {
        super(dao);
        lapDAO = dao;
    }

    @GET
    public List<Lap> getListOf(@QueryParam("source") final Integer source) {
        if (source == null) {
            return lapDAO.getAll();
        } else {
            return lapDAO.getAllBySource(source);
        }
    }
}
