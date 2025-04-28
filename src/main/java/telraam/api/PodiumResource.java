package telraam.api;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;
import lombok.NoArgsConstructor;
import telraam.database.daos.LapDAO;
import telraam.database.models.Lap;
import telraam.database.models.Podium;
import telraam.util.AcceptedLapsUtil;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Path("/podium")
@Tag(name = "Podium")
@Produces(MediaType.APPLICATION_JSON)
public class PodiumResource {
    @GET
    @Operation(summary = "Get the podium")
    public List<Podium> getPodium() {
        Map<Integer, Integer> perId = new HashMap<>();
        Map<Integer, Lap> lastLap = new HashMap<>();

        for (Lap lap : AcceptedLapsUtil.getInstance().getAcceptedLaps()) {
            int teamId = lap.getTeamId();
            perId.put(teamId, perId.getOrDefault(teamId, 0) + 1);
            lastLap.put(teamId, lap);
        }

        List<Map.Entry<Integer, Integer>> sortedTeams = perId.entrySet()
                .stream()
                .sorted((a, b) -> {
                    int cmp = Integer.compare(b.getValue(), a.getValue());
                    if (cmp != 0) {
                        return cmp;
                    }
                    Timestamp t1 = lastLap.get(a.getKey()).getTimestamp();
                    Timestamp t2 = lastLap.get(b.getKey()).getTimestamp();
                    return t1.compareTo(t2);
                })
                .limit(3)
                .toList();

        List<Podium> podium = new ArrayList<>();
        int rank = 1;
        for (Map.Entry<Integer, Integer> entry : sortedTeams) {
            Podium p = new Podium(entry.getKey(), rank++, entry.getValue());
            podium.add(p);
        }

        return podium;
    }

}
