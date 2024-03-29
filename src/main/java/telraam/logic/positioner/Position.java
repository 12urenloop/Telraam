package telraam.logic.positioners;

import lombok.Getter;
import lombok.Setter;
import telraam.database.models.Team;
import telraam.websocket.WebSocketMessageSingleton;

@Getter @Setter
public class Position {
    private Team team;
    private float progress; // Progress of the lap. Between 0-1
    private float speed; // Current speed. Progress / second

    public Position(Team team) {
        this.team = team;
        this.progress = 0;
        this.speed = 0;
    }

    private String toWebsocketMessage() {
        return String.format("{\"topic\": \"position\", \"data\": {\"team\": %d, \"progress\": %.4f, \"speed\": %.5f}}", team.getId(), this.progress, this.speed);
    }

    public void send() {
        WebSocketMessageSingleton.getInstance().sendToAll(
                this.toWebsocketMessage()
        );
    }
}
