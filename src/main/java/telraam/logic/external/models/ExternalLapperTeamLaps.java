package telraam.logic.external.models;

import lombok.Getter;
import lombok.Setter;

import java.util.List;


@Getter @Setter
public class ExternalLapperTeamLaps {
    private int teamId;
    private List<ExternalLapperLap> laps;
}
