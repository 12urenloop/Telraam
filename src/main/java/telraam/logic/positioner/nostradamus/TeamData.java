package telraam.logic.positioner.nostradamus;

import lombok.Getter;
import lombok.Setter;
import telraam.database.models.Station;
import telraam.logic.positioner.Position;

import java.sql.Timestamp;
import java.util.HashMap;
import java.util.Map;

@Getter @Setter
public class TeamData {
    private Position position;
    private Station lastStation;
    private Timestamp lastUpdate;

    public TeamData(int teamId) {
        this.position = new Position(teamId);
        this.lastStation = null;
        this.lastUpdate = null;
    }
}
