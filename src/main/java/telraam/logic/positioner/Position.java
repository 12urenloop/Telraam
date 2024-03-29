package telraam.logic.positioner;

import lombok.Getter;
import lombok.Setter;
import telraam.database.models.Team;
import telraam.websocket.WebSocketMessageSingleton;

@Getter @Setter
public class Position {
    private int teamId;
    private float progress; // Progress of the lap. Between 0-1
    private float speed; // Current speed. Progress / second

    public Position(int teamId) {
        this.teamId = teamId;
        this.progress = 0;
        this.speed = 0;
    }
}
